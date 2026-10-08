# The public entry point for household formation, and the two passes that need more than a
# single matching call: placing children with their parents, and absorbing the members that
# attach to an already-formed household.

#' Partition a Synthetic Population Into Households
#'
#' Groups an already-fitted synthetic population into households according to the role each
#' agent holds, without changing any attribute the population carries. Couples are matched
#' on any number of determinants, children are grouped into sibling sets and matched to
#' their parents, and agents whose role attaches them to an existing household are used to
#' reach a published mean household size.
#'
#' @param df The synthetic population, one row per agent.
#' @param structure A [household_structure()] declaring what each value of the role column
#'   does when households are formed.
#' @param group_by Character vector of spatial columns from finest to coarsest, for example
#'   `c("PC4", "district", "municipality")`. Matching is attempted at the finest level and
#'   agents left without a partner are carried up to the next, with each relaxation counted.
#' @param neighbours Optional edge list of which areas border which, as a data frame whose
#'   first two columns hold pairs of values from the finest `group_by` column. When given it
#'   becomes a rung between the finest level and the next column: agents still unmatched are
#'   offered a partner across one shared boundary before the search widens to a whole
#'   district or municipality. Areas absent from the list, and islands with no neighbour,
#'   fall through to the next column.
#' @param relocate Whether the members of a household formed across a boundary move to one
#'   area, so that every household sits in a single place. `TRUE` by default, since the
#'   members of a household share a dwelling. The area is chosen so that the moves between
#'   any two areas cancel, and the number moved and the net flow per area are reported. Set
#'   `FALSE` to keep every agent in the area it was fitted to, at the cost of households
#'   whose members live in two places.
#' @param agent_id Name of the agent identifier column. Default `"agent_id"`.
#' @param role Name of the household-role column. Default `"household_role"`.
#' @param couple_determinants List of determinants governing who partners whom, built with
#'   [match_on_gap()] and [match_on_pairing()]. May be empty, in which case couples are
#'   formed at random within a stratum.
#' @param child_determinants List of determinants governing which children join which
#'   household, most often one [match_on_gap()] on age with a `floor`.
#' @param sex_pairing Optional [match_on_pairing()] on the sex column giving the target mix
#'   of couple sex combinations. Without it, couples are formed without regard to sex.
#' @param children_per_household Data frame with columns `household_type`, `n_children` and
#'   `count`. May carry one of the `group_by` columns to vary spatially. Its shape is kept
#'   and shifted so the households in each area hold exactly the children present.
#' @param sibling_spacing A [gap_distribution()] of the age spacing between consecutive
#'   siblings. Optional.
#' @param parent_reference Optional list of `column` and `level` naming which partner the
#'   parent-child gap is measured from, for instance the mother where the distribution is a
#'   mother's age at birth.
#' @param target_household_size Optional data frame with the finest `group_by` column and a
#'   `size` column giving the published mean household size, which decides how many attached
#'   members each area absorbs.
#' @param attach_to Optional character vector of household types that may receive attached
#'   members. Defaults to every type.
#' @param household_prefix Prefix for generated household identifiers. Default `"HH"`.
#'
#' @return A list of `population`, the input with a `household_id` column added;
#'   `households`, one row per household with its area, type and size; and `report`, the
#'   diagnostics [verify_households()] prints.
#'
#' @details
#' Every step reports rather than repairs. Where a stratum holds an odd number of people in
#' a couple role, or more children than its households can hold, the surplus is carried up
#' the spatial ladder and then left without a household, and the counts appear in the
#' report. No agent's role is ever rewritten to make a match possible, so the distributions
#' fitted before this step survive it intact.
#'
#' @examples
#' \donttest{
#' nl <- household_structure(
#'   hh_role("LivingAlone",         kind = "partner", type = "single",        n_adults = 1),
#'   hh_role("PartnerNoChildren",   kind = "partner", type = "couple",        n_adults = 2),
#'   hh_role("PartnerWithChildren", kind = "partner", type = "couple_kids",   n_adults = 2),
#'   hh_role("SingleParent",        kind = "partner", type = "single_parent", n_adults = 1),
#'   hh_role("ChildAtHome",         kind = "child",
#'           type = c("couple_kids", "single_parent")),
#'   hh_role("OtherHouseholdMember",   kind = "attached"),
#'   hh_role("InstitutionalHousehold", kind = "excluded"))
#'
#' out <- assign_households(agent_df, nl, group_by = c("PC4", "municipality"),
#'                          couple_determinants = list(age_gap, origin_homogamy),
#'                          child_determinants  = list(mother_gap))
#' }
#'
#' @seealso [household_structure()], [household_person_margins()], [verify_households()]
#' @importFrom stats setNames
#' @export
assign_households <- function(df, structure, group_by, agent_id = "agent_id",
                              role = "household_role",
                              couple_determinants = list(), child_determinants = list(),
                              sex_pairing = NULL,
                              children_per_household = NULL, sibling_spacing = NULL,
                              parent_reference = NULL, target_household_size = NULL,
                              attach_to = NULL, neighbours = NULL, relocate = TRUE,
                              household_prefix = "HH") {

    if (!inherits(structure, "gsp_hh_structure")) {
        stop("'structure' must come from household_structure().")
    }
    for (col in c(agent_id, role, group_by)) {
        if (!col %in% colnames(df)) stop("Column '", col, "' is not in the population.")
    }
    unknown <- setdiff(unique(as.character(df[[role]])), c(structure$levels, NA))
    if (length(unknown)) {
        stop("The population holds role(s) the structure does not declare: ",
             paste(sQuote(unknown), collapse = ", "), ".")
    }

    couple_spec <- split_determinants(couple_determinants)
    if (!is.null(sex_pairing)) {
        couple_spec$pairing <- c(list(sex_pairing), couple_spec$pairing)
    }
    child_spec <- split_determinants(child_determinants)

    n <- nrow(df)
    roles <- as.character(df[[role]])
    areas <- stats::setNames(lapply(group_by, function(g) as.character(df[[g]])), group_by)
    finest <- areas[[group_by[1]]]

    # The ladder the search climbs. Each rung turns the agents still unmatched into pools to
    # try. A column rung splits them by that column; the neighbour rung offers each pair of
    # bordering areas in turn, which holds a relaxed match to one shared boundary instead of
    # letting it reach across a whole municipality.
    rungs <- list(list(kind = "column", column = group_by[1], label = group_by[1]))
    edges <- NULL
    if (!is.null(neighbours)) {
        edges <- build_edges(neighbours, unique(finest))
        if (nrow(edges)) {
            rungs <- c(rungs, list(list(kind = "edges", label = "bordering areas")))
        } else {
            warning("'neighbours' matched no pair of areas in '", group_by[1],
                    "'; the neighbour rung is skipped.", call. = FALSE)
        }
    }
    if (length(group_by) > 1) {
        rungs <- c(rungs, lapply(group_by[-1], function(g)
            list(kind = "column", column = g, label = g)))
    }

    hh_of <- rep(NA_integer_, n)
    next_hh <- 0L
    # `ref` is the member the gap distribution is drawn against; `bound` is the youngest
    # member of the adult core, which is what any gap floor has to hold for.
    cores <- data.frame(household = integer(0), type = character(0), area = character(0),
                        ref = integer(0), bound = numeric(0), stringsAsFactors = FALSE)
    gap_values <- if (!is.null(child_spec$gap) && child_spec$gap$column %in% colnames(df)) {
        suppressWarnings(as.numeric(df[[child_spec$gap$column]]))
    } else rep(NA_real_, n)
    report <- list(relaxed = list(), unmatched = list(), floor_hits = 0L, floor_blocked = 0L,
                   notes = character(0), childless_cores = 0L, unplaced_children = 0L)

    # ---- adult cores held by one person -------------------------------------------
    solo <- structure$levels[structure$kind_of[structure$levels] == "partner" &
                             structure$adults_of[structure$levels] == 1]
    for (r in solo) {
        pos <- which(roles == r)
        if (!length(pos)) next
        ids <- next_hh + seq_along(pos)
        next_hh <- next_hh + length(pos)
        hh_of[pos] <- ids
        cores <- rbind(cores, data.frame(household = ids, type = structure$type_of[[r]],
                                         area = finest[pos], ref = pos,
                                         bound = gap_values[pos],
                                         stringsAsFactors = FALSE))
    }

    # ---- adult cores held by two, matched over the spatial ladder -------------------
    couples <- structure$levels[structure$kind_of[structure$levels] == "partner" &
                                structure$adults_of[structure$levels] == 2]
    for (r in couples) {
        pending <- which(roles == r)
        for (li in seq_along(rungs)) {
            if (length(pending) < 2) break
            pools <- rung_pools(rungs[[li]], pending, areas, finest, edges)
            taken <- logical(n)
            ma <- integer(0); mb <- integer(0); area_pick <- character(0)
            for (pool in pools) {
                # Pools on the neighbour rung overlap, because an area borders several
                # others, so anyone matched on an earlier edge is dropped here rather than
                # being matched twice.
                pool <- pool[!taken[pool]]
                if (length(pool) < 2) next
                res <- pair_recursive(pool, pool, df, couple_spec, 1L, TRUE, couple_spec$gap)
                report$floor_hits <- report$floor_hits + res$floor_hits
                report$floor_blocked <- report$floor_blocked + res$floor_blocked
                if (!length(res$a)) next
                taken[res$a] <- TRUE; taken[res$b] <- TRUE
                ma <- c(ma, res$a); mb <- c(mb, res$b)
                # Decided per pool, so the households formed across one boundary are split
                # evenly between its two sides and the moves between them cancel.
                area_pick <- c(area_pick, balanced_area(finest, res$a, res$b))
            }
            if (length(ma)) {
                ids <- next_hh + seq_along(ma)
                next_hh <- next_hh + length(ma)
                hh_of[ma] <- ids; hh_of[mb] <- ids
                cores <- rbind(cores, data.frame(
                    household = ids, type = structure$type_of[[r]], area = area_pick,
                    ref = choose_reference(df, ma, mb, parent_reference,
                                           value_column = child_spec$gap$column),
                    bound = pmin(gap_values[ma], gap_values[mb]),
                    stringsAsFactors = FALSE))
                if (li > 1) {
                    report$relaxed[[paste0(r, " at ", rungs[[li]]$label)]] <- 2L * length(ma)
                }
            }
            pending <- setdiff(pending, c(ma, mb))
        }
        if (length(pending)) report$unmatched[[r]] <- length(pending)
    }

    # ---- children -------------------------------------------------------------------
    child_roles <- roles_of_kind(structure, "child")
    if (length(child_roles) && nrow(cores)) {
        placed <- place_children(df, roles, child_roles, cores, structure, areas, group_by,
                                 children_per_household, sibling_spacing, child_spec,
                                 report, rungs, finest, edges)
        hh_of[placed$rows] <- placed$household
        report <- placed$report
    }

    # ---- attached members ------------------------------------------------------------
    attached <- roles_of_kind(structure, "attached")
    if (length(attached)) {
        pos <- which(roles %in% attached)
        if (length(pos)) {
            att <- attach_members(pos, hh_of, cores, finest, target_household_size,
                                  attach_to, group_by[1], next_hh)
            hh_of[att$rows] <- att$household
            next_hh <- att$next_hh
            if (nrow(att$new_cores)) cores <- rbind(cores, att$new_cores)
            report$notes <- c(report$notes, att$note)
        }
    }

    excluded <- roles_of_kind(structure, "excluded")
    report$excluded <- if (length(excluded)) sum(roles %in% excluded) else 0L

    # ---- assemble --------------------------------------------------------------------
    df$household_id <- ifelse(is.na(hh_of), NA_character_,
                              sprintf(paste0(household_prefix, "%08d"), hh_of))
    sizes <- table(hh_of[!is.na(hh_of)])
    key <- names(sizes)
    households <- data.frame(
        household_id   = sprintf(paste0(household_prefix, "%08d"), as.integer(key)),
        area           = cores$area[match(as.integer(key), cores$household)],
        household_type = cores$type[match(as.integer(key), cores$household)],
        size           = as.integer(sizes),
        stringsAsFactors = FALSE)
    names(households)[names(households) == "area"] <- group_by[1]

    # Matching that climbed the ladder pairs people from different areas, so a household can
    # be built out of members of more than one. Counted against the areas the agents were
    # fitted to, before anybody moves.
    spanning <- 0L
    placed_rows <- which(!is.na(hh_of))
    if (length(placed_rows)) {
        # Counted by reducing to the distinct household-area pairs and asking which
        # households occur more than once. Splitting a million rows by household and
        # counting distinct areas inside each gives the same answer and costs far more.
        pair <- !duplicated(paste(hh_of[placed_rows], finest[placed_rows], sep = "\r"))
        h <- hh_of[placed_rows][pair]
        spanning <- length(unique(h[duplicated(h)]))
    }
    report$spanning_areas <- as.integer(spanning)

    # A household occupies one dwelling, so its members move to one area. Which area was
    # decided per pool, so the moves between any two areas cancel; whatever net flow is left
    # is reported rather than assumed away, because moving anyone perturbs the spatial
    # margins the population was fitted to.
    report$relocated <- 0L
    report$net_flow <- NULL
    if (relocate && length(placed_rows)) {
        area_of <- stats::setNames(cores$area, as.character(cores$household))
        target_area <- unname(area_of[as.character(hh_of[placed_rows])])
        moved <- !is.na(target_area) & target_area != finest[placed_rows]
        if (any(moved)) {
            rows <- placed_rows[moved]
            from <- finest[rows]; to <- target_area[moved]
            df[[group_by[1]]][rows] <- to
            flow <- merge(
                stats::aggregate(list(out = rep(1L, length(from))),
                                 by = list(area = from), FUN = sum),
                stats::aggregate(list(into = rep(1L, length(to))),
                                 by = list(area = to), FUN = sum),
                by = "area", all = TRUE)
            flow$out[is.na(flow$out)] <- 0L
            flow$into[is.na(flow$into)] <- 0L
            flow$net <- flow$into - flow$out
            report$net_flow <- flow[order(-abs(flow$net)), ]
            report$relocated <- length(rows)
        }
    }

    report$population <- n
    report$assigned   <- sum(!is.na(hh_of))
    report$households <- nrow(households)
    report$group_by   <- group_by

    list(population = df, households = households, report = report)
}

# Separate the pairing determinants from the single gap determinant that orders each cell.
split_determinants <- function(ds) {
    gaps <- Filter(function(d) d$kind == "gap", ds)
    if (length(gaps) > 1) {
        warning("More than one gap determinant was given; only the first, on '",
                gaps[[1]]$column, "', orders the match. Express the others as pairing ",
                "determinants over banded levels if they should also constrain it.",
                call. = FALSE)
    }
    list(pairing = Filter(function(d) d$kind == "pairing", ds),
         gap     = if (length(gaps)) gaps[[1]] else NULL)
}

# Which partner carries the value the parent-child gap is measured from, and so the one any
# gap floor binds. Where a reference level is named and exactly one partner holds it, that
# partner. Otherwise the younger of the two, so that a floor applied to the reference also
# holds for the other partner and no adult in the household ends up implausibly close in age
# to the children. A same-sex couple, or a population with no reference column, takes this
# second route; picking a partner by position instead would leave the floor binding an
# arbitrary one of them.
choose_reference <- function(df, a, b, parent_reference, value_column = NULL) {
    age_a <- if (!is.null(value_column) && value_column %in% colnames(df))
        suppressWarnings(as.numeric(df[[value_column]][a])) else numeric(0)
    age_b <- if (!is.null(value_column) && value_column %in% colnames(df))
        suppressWarnings(as.numeric(df[[value_column]][b])) else numeric(0)
    younger <- if (length(age_a) && !anyNA(age_a) && !anyNA(age_b)) {
        ifelse(age_a <= age_b, a, b)
    } else a

    if (is.null(parent_reference) || is.null(parent_reference$column) ||
        !parent_reference$column %in% colnames(df)) {
        return(younger)
    }
    va <- as.character(df[[parent_reference$column]][a])
    vb <- as.character(df[[parent_reference$column]][b])
    is_a <- va == parent_reference$level
    is_b <- vb == parent_reference$level
    # Exactly one partner at the reference level identifies them; none or both falls back.
    ifelse(is_a & !is_b, a, ifelse(is_b & !is_a, b, younger))
}

# Normalise an edge list to unordered pairs of areas that occur in the population. Self
# loops and duplicates are dropped, so each shared boundary is offered exactly once.
build_edges <- function(neighbours, valid) {
    if (!is.data.frame(neighbours) || ncol(neighbours) < 2) {
        stop("'neighbours' must be a data frame whose first two columns are pairs of areas.")
    }
    a <- as.character(neighbours[[1]])
    b <- as.character(neighbours[[2]])
    keep <- !is.na(a) & !is.na(b) & a != b & a %in% valid & b %in% valid
    a <- a[keep]; b <- b[keep]
    lo <- pmin(a, b); hi <- pmax(a, b)
    dup <- duplicated(paste(lo, hi, sep = "\r"))
    data.frame(a = lo[!dup], b = hi[!dup], stringsAsFactors = FALSE)
}

# The pools of agents to try at one rung. A column rung splits them by that column; the
# neighbour rung offers the agents of each bordering pair, in random order so that no area
# is systematically served first.
rung_pools <- function(rung, pending, areas, finest, edges) {
    if (rung$kind == "column") {
        return(unname(split(pending, areas[[rung$column]][pending])))
    }
    by_area <- split(pending, finest[pending])
    ord <- sample.int(nrow(edges))
    pools <- lapply(ord, function(e) c(by_area[[edges$a[e]]], by_area[[edges$b[e]]]))
    pools[lengths(pools) > 0]
}

# The same for children, which have to be offered together with the adult cores of the same
# pool. The label is passed through because a spatially varying children-per-household
# distribution is looked up by it.
rung_pool_pairs <- function(rung, kids, cores_idx, areas, finest, core_area, core_rows,
                            edges) {
    if (rung$kind == "column") {
        ckey <- areas[[rung$column]][kids]
        # At the finest level a core sits in the area its household was placed in, which can
        # differ from the area of the agent representing it; a coarser column is read off
        # that agent.
        hkey <- if (identical(rung$column, names(areas)[1])) core_area
                else areas[[rung$column]][core_rows]
        kb <- split(kids, ckey); cb <- split(cores_idx, hkey)
        shared <- intersect(names(kb), names(cb))
        return(lapply(shared, function(s) list(kids = kb[[s]], cores = cb[[s]], label = s)))
    }
    kb <- split(kids, finest[kids])
    cb <- split(cores_idx, core_area)
    ord <- sample.int(nrow(edges))
    out <- lapply(ord, function(e) {
        a <- edges$a[e]; b <- edges$b[e]
        list(kids = c(kb[[a]], kb[[b]]), cores = c(cb[[a]], cb[[b]]), label = a)
    })
    out[vapply(out, function(x) length(x$kids) > 0 && length(x$cores) > 0, logical(1))]
}

# Choose one area per pair so that, within a pool, the households formed across a boundary
# are split evenly between its two sides. Pairs already sharing an area are untouched, so
# nobody moves without reason.
balanced_area <- function(finest, a, b) {
    fa <- finest[a]; fb <- finest[b]
    out <- fa
    differ <- which(fa != fb)
    if (length(differ)) {
        take_b <- resample(differ, floor(length(differ) / 2))
        out[take_b] <- fb[take_b]
    }
    out
}

# ---------------------------------------------------------------------------------------
# Children
# ---------------------------------------------------------------------------------------

# Place children with adult cores, working up the spatial ladder. Within each stratum the
# children are first divided between the household types that may host them, in proportion
# to how many children those types are expected to hold, then grouped into sibling sets
# whose sizes reproduce the published distribution, and finally matched to cores.
place_children <- function(df, roles, child_roles, cores, structure, areas, group_by,
                           children_per_household, sibling_spacing, child_spec, report,
                           rungs, finest, edges) {

    age_col <- if (!is.null(child_spec$gap)) child_spec$gap$column else NULL
    ages <- if (!is.null(age_col)) as.numeric(df[[age_col]]) else rep(0, nrow(df))

    eligible <- unique(unlist(lapply(child_roles, function(r) structure$type_of[[r]])))

    # Indexed by row so the matching can look it up for whichever core it is considering,
    # built once rather than per area.
    bound_vec <- rep(NA_real_, nrow(df))
    bound_vec[cores$ref] <- cores$bound

    # Placement is tracked with masks rather than by removing matched agents from a growing
    # vector: repeatedly taking the set difference against everything matched so far would
    # cost a pass over the whole population for every area.
    child_done <- rep(FALSE, nrow(df))
    is_child <- roles %in% child_roles
    core_done <- rep(FALSE, nrow(cores))
    core_done[!(cores$type %in% eligible)] <- TRUE

    out_rows <- integer(0); out_hh <- integer(0)

    for (li in seq_along(rungs)) {
        pending_children <- which(is_child & !child_done)
        core_pool <- which(!core_done)
        if (!length(pending_children) || !length(core_pool)) break

        # Children join the area their household already sits in, so each pool pairs the
        # children of an area with the cores placed there.
        core_area <- cores$area[core_pool]
        core_rows <- cores$ref[core_pool]
        rung_col <- if (rungs[[li]]$kind == "column") rungs[[li]]$column else NA_character_
        pools <- rung_pool_pairs(rungs[[li]], pending_children, core_pool, areas,
                                 finest, core_area, core_rows, edges)
        relaxed_here <- 0L

        for (pl in pools) {
            kids_here  <- pl$kids[!child_done[pl$kids]]
            cores_here <- pl$cores[!core_done[pl$cores]]
            s <- pl$label
            if (!length(kids_here) || !length(cores_here)) next
            type_here <- cores$type[cores_here]

            demand <- vapply(eligible, function(t) {
                sum(type_here == t) * mean_children_for(children_per_household, t,
                                                        rung_col, s)
            }, numeric(1))
            names(demand) <- eligible
            if (sum(demand) <= 0) next

            for (r in child_roles) {
                pos_r <- kids_here[roles[kids_here] == r]
                if (!length(pos_r)) next
                types_r <- intersect(structure$type_of[[r]], eligible)
                w <- demand[types_r]
                if (!length(w) || sum(w) <= 0) next
                share <- GenSynthPop::calculate_group_counts(w / sum(w), length(pos_r))
                pos_r <- resample(pos_r)
                at <- 0L
                for (ti in seq_along(types_r)) {
                    k <- share[ti]
                    if (k <= 0) next
                    kids_t <- pos_r[at + seq_len(k)]; at <- at + k
                    cores_t <- cores_here[type_here == types_r[ti]]
                    cores_t <- cores_t[!core_done[cores_t]]
                    if (!length(cores_t)) next

                    res <- fill_type(df, kids_t, cores_t, cores, ages,
                                     size_weights_for(children_per_household, types_r[ti],
                                                      rung_col, s),
                                     sibling_spacing, child_spec, bound_vec)
                    if (!length(res$rows)) next
                    out_rows <- c(out_rows, res$rows)
                    out_hh   <- c(out_hh, res$household)
                    child_done[res$rows] <- TRUE
                    core_done[res$cores_used] <- TRUE
                    relaxed_here <- relaxed_here + length(res$rows)
                    report$floor_hits <- report$floor_hits + res$floor_hits
                    report$floor_blocked <- report$floor_blocked + res$floor_blocked
                report$floor_blocked <- report$floor_blocked + res$floor_blocked
                }
            }
        }
        if (li > 1 && relaxed_here) {
            report$relaxed[[paste0("children at ", rungs[[li]]$label)]] <- relaxed_here
        }
    }

    pending_children <- which(is_child & !child_done)
    core_pool <- which(!core_done)
    report$unplaced_children <- length(pending_children)
    report$childless_cores <- length(core_pool)
    list(rows = out_rows, household = out_hh, report = report)
}

# The household size distribution for one type, restricted to the spatial level in play.
size_weights_for <- function(cph, type, rung_column, stratum) {
    if (is.null(cph)) return(NULL)
    keep <- as.character(cph$household_type) == type
    # A spatially varying distribution applies only on the rung split by that same column;
    # on any other rung, including the neighbour rung, the pooled distribution is used.
    if (!is.na(rung_column) && rung_column %in% colnames(cph)) {
        keep <- keep & as.character(cph[[rung_column]]) == stratum
    }
    sub <- cph[keep, , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    list(sizes = as.numeric(sub$n_children), weights = as.numeric(sub$count))
}

mean_children_for <- function(cph, type, rung_column, s) {
    w <- size_weights_for(cph, type, rung_column, s)
    if (is.null(w) || !sum(w$weights)) return(1)
    sum(w$sizes * w$weights) / sum(w$weights)
}

# Group one stratum's children of one household type into sibling sets and match the sets to
# adult cores.
fill_type <- function(df, kids, cores_t, cores, ages, weights, sibling_spacing, child_spec,
                      bound_vec = NULL) {
    n_kids <- length(kids)
    n_cores <- length(cores_t)
    if (!n_kids || !n_cores) {
        return(list(rows = integer(0), household = integer(0), cores_used = integer(0),
                    floor_hits = 0L, floor_blocked = 0L))
    }
    if (is.null(weights)) {
        m <- n_kids / n_cores
        sizes <- unique(c(max(1, floor(m)), max(1, ceiling(m))))
        weights <- list(sizes = sizes, weights = rep(1, length(sizes)))
    }
    sizes <- weights$sizes[weights$sizes >= 1]
    wts <- weights$weights[weights$sizes >= 1]
    if (!length(sizes)) { sizes <- 1; wts <- 1 }

    # A household of this type holds at least one child, so no more households can be filled
    # than there are children.
    n_units <- min(n_cores, floor(n_kids / min(sizes)))
    if (n_units < 1) {
        return(list(rows = integer(0), household = integer(0), cores_used = integer(0),
                    floor_hits = 0L, floor_blocked = 0L))
    }
    counts <- fit_size_counts(sizes, wts, n_units, n_kids)
    # fit_size_counts may extend an open-ended top category, so the sizes it actually used
    # are the names it returns, not the ones passed in.
    set_sizes <- rep(as.numeric(names(counts)), times = as.integer(counts))
    if (!length(set_sizes)) {
        return(list(rows = integer(0), household = integer(0), cores_used = integer(0),
                    floor_hits = 0L, floor_blocked = 0L))
    }

    sets <- group_siblings(kids, ages, set_sizes, sibling_spacing)
    if (!length(sets)) {
        return(list(rows = integer(0), household = integer(0), cores_used = integer(0),
                    floor_hits = 0L, floor_blocked = 0L))
    }

    # Each sibling set stands in the match for its oldest child, and each core for the
    # partner the gap is measured from, so the pairing machinery applies unchanged.
    set_ref <- vapply(sets, function(m) m[which.max(ages[m])], integer(1))
    core_ref <- cores$ref[cores_t]
    res <- pair_recursive(core_ref, set_ref, df, child_spec, 1L, FALSE, child_spec$gap,
                          bounds = bound_vec)

    matched_set <- match(res$b, set_ref)
    matched_core <- match(res$a, core_ref)
    hh_ids <- cores$household[cores_t][matched_core]

    rows <- unlist(sets[matched_set], use.names = FALSE)
    household <- rep(hh_ids, lengths(sets[matched_set]))
    list(rows = rows, household = household, cores_used = cores_t[matched_core],
         floor_hits = res$floor_hits, floor_blocked = res$floor_blocked)
}

# ---------------------------------------------------------------------------------------
# Attached members
# ---------------------------------------------------------------------------------------

# Absorb the roles that join an existing household. How many an area takes is set by the
# difference between its published mean household size and the size its households have
# reached, which is the one use the published mean size has: everything else in the
# reconciliation is a count. Anyone left over forms a household with other leftovers rather
# than being dropped.
attach_members <- function(pos, hh_of, cores, finest, target_household_size, attach_to,
                           area_col, next_hh) {
    eligible_types <- if (is.null(attach_to)) unique(cores$type) else attach_to
    note <- character(0)
    rows <- integer(0); household <- integer(0)
    new_cores <- data.frame(household = integer(0), type = character(0),
                            area = character(0), ref = integer(0), bound = numeric(0),
                            stringsAsFactors = FALSE)

    current <- table(hh_of[!is.na(hh_of)])
    size_of <- stats::setNames(as.integer(current), names(current))

    by_area <- split(pos, finest[pos])
    core_by_area <- split(seq_len(nrow(cores)), cores$area)

    target_lookup <- NULL
    if (!is.null(target_household_size) && area_col %in% colnames(target_household_size) &&
        "size" %in% colnames(target_household_size)) {
        target_lookup <- stats::setNames(as.numeric(target_household_size$size),
                                         as.character(target_household_size[[area_col]]))
    }

    attached_total <- 0L
    for (a in names(by_area)) {
        avail <- by_area[[a]]
        ci <- core_by_area[[a]]
        if (is.null(ci) || !length(ci)) next
        ok <- ci[cores$type[ci] %in% eligible_types]
        if (!length(ok)) next

        deficit <- length(avail)
        if (!is.null(target_lookup) && !is.na(target_lookup[a])) {
            placed <- sum(size_of[as.character(cores$household[ci])], na.rm = TRUE)
            deficit <- max(0L, as.integer(round(target_lookup[a] * length(ci)) - placed))
        }
        take <- min(deficit, length(avail))
        if (take > 0) {
            chosen <- resample(avail, take)
            hosts <- cores$household[resample(ok, take, replace = TRUE)]
            rows <- c(rows, chosen); household <- c(household, hosts)
            attached_total <- attached_total + take
            avail <- setdiff(avail, chosen)
        }
        # Whatever the area cannot absorb forms shared households of two.
        if (length(avail)) {
            avail <- resample(avail)
            n_pairs <- floor(length(avail) / 2)
            if (n_pairs > 0) {
                ids <- next_hh + seq_len(n_pairs)
                next_hh <- next_hh + n_pairs
                rows <- c(rows, avail[seq_len(2 * n_pairs)])
                household <- c(household, rep(ids, each = 2))
                new_cores <- rbind(new_cores, data.frame(
                    household = ids, type = "attached_shared", area = a,
                    ref = avail[seq_len(n_pairs)], bound = NA_real_,
                    stringsAsFactors = FALSE))
                avail <- avail[-seq_len(2 * n_pairs)]
            }
            if (length(avail)) {
                ids <- next_hh + seq_along(avail)
                next_hh <- next_hh + length(avail)
                rows <- c(rows, avail); household <- c(household, ids)
                new_cores <- rbind(new_cores, data.frame(
                    household = ids, type = "attached_shared", area = a, ref = avail,
                    bound = NA_real_, stringsAsFactors = FALSE))
            }
        }
    }
    if (attached_total > 0) {
        note <- paste0(attached_total, " attached member(s) joined an existing household",
                       if (!is.null(target_lookup)) " to reach the published mean size" else "",
                       "; ", length(rows) - attached_total,
                       " formed households of their own.")
    }
    list(rows = rows, household = household, next_hh = next_hh, new_cores = new_cores,
         note = note)
}
