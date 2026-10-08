# Reporting on a household assignment.
#
# Same posture as verify_target_attribute(): say what the result looks like against what
# was asked for, and leave the judgement to the person running it. Nothing here changes the
# assignment.

#' Report How Well an Assignment Reproduced Its Targets
#'
#' Compares the households [assign_households()] built against the distributions it was
#' given and, when supplied, against the household counts published for each area. Prints a
#' readable summary and returns the comparisons.
#'
#' @param result The list returned by [assign_households()].
#' @param structure The [household_structure()] the assignment used. Needed to tell partners
#'   apart from the other people in a household when the realised gaps are read back; without
#'   it the partner-gap comparison is skipped.
#' @param couple_determinants The determinants passed to [assign_households()], so the
#'   realised gap and pairing distributions can be compared with the targets.
#' @param child_determinants The child determinants, compared the same way.
#' @param target_households Optional data frame of published household counts with the
#'   area column, a `household_type` column and `count`, as returned in the `households`
#'   element of [household_person_margins()].
#' @param role Name of the household-role column. Default `"household_role"`.
#'
#' @return Invisibly, a list of `coverage`, `sizes`, `gap` and `counts` comparisons.
#'
#' @details
#' Agreement between a realised and a target distribution is reported as total variation
#' distance, the same measure the package uses when checking a fitted attribute: half the
#' summed absolute difference between two sets of shares, so zero is exact agreement and one
#' is no overlap. A few percent is normal, because a target distribution can only be
#' reproduced as far as the people present allow.
#'
#' @examples
#' \donttest{
#' out <- assign_households(agent_df, nl, group_by = c("PC4", "municipality"))
#' verify_households(out, couple_determinants = list(age_gap))
#' }
#'
#' @seealso [assign_households()], [verify_target_attribute()]
#' @export
verify_households <- function(result, structure = NULL, couple_determinants = list(),
                              child_determinants = list(), target_households = NULL,
                              role = "household_role") {

    pop <- result$population
    hh  <- result$households
    rep <- result$report
    out <- list()

    cat("\n-- household assignment --------------------------------------------\n")
    assigned <- rep$assigned
    cat(sprintf("  %d of %d agents placed in %d households (%.1f%%)\n",
                assigned, rep$population, rep$households,
                100 * assigned / max(rep$population, 1)))
    if (!is.null(rep$excluded) && rep$excluded > 0) {
        cat(sprintf("  %d agent(s) held out by an excluded role\n", rep$excluded))
    }
    unplaced <- rep$population - assigned - (rep$excluded %||% 0)
    if (unplaced > 0) {
        cat(sprintf("  %d agent(s) left without a household\n", unplaced))
    }
    out$coverage <- c(population = rep$population, assigned = assigned,
                      households = rep$households, excluded = rep$excluded %||% 0,
                      unplaced = unplaced)

    if (length(rep$relaxed)) {
        cat("\n  matched only after widening the spatial level:\n")
        for (nm in names(rep$relaxed)) {
            cat(sprintf("    %-40s %d agent(s)\n", nm, rep$relaxed[[nm]]))
        }
    }
    if (length(rep$unmatched)) {
        cat("\n  no partner found at any level:\n")
        for (nm in names(rep$unmatched)) {
            cat(sprintf("    %-40s %d agent(s)\n", nm, rep$unmatched[[nm]]))
        }
    }
    if (!is.null(rep$childless_cores) && rep$childless_cores > 0) {
        cat(sprintf("\n  %d household(s) of a type that should hold children got none\n",
                    rep$childless_cores))
    }
    if (!is.null(rep$unplaced_children) && rep$unplaced_children > 0) {
        cat(sprintf("  %d child agent(s) found no household\n", rep$unplaced_children))
    }
    if (!is.null(rep$floor_hits) && rep$floor_hits > 0) {
        cat(sprintf("  %d pair(s) were pulled back to a gap floor\n", rep$floor_hits))
    }
    if (!is.null(rep$floor_blocked) && rep$floor_blocked > 0) {
        cat(sprintf("  %d match(es) refused because no candidate met the gap floor\n",
                    rep$floor_blocked))
    }
    if (!is.null(rep$spanning_areas) && rep$spanning_areas > 0) {
        cat(sprintf("  %d household(s) were formed out of members of more than one %s\n",
                    rep$spanning_areas, rep$group_by[1]))
    }
    if (!is.null(rep$relocated) && rep$relocated > 0) {
        cat(sprintf("  %d agent(s) moved so that each household occupies one %s\n",
                    rep$relocated, rep$group_by[1]))
        nf <- rep$net_flow
        if (!is.null(nf) && nrow(nf)) {
            cat(sprintf("    net flow per area sums to %d, largest %+d, %d area(s) beyond 10\n",
                        sum(nf$net), nf$net[which.max(abs(nf$net))],
                        sum(abs(nf$net) > 10)))
        }
    }
    for (nt in rep$notes) if (nzchar(nt)) cat("  ", nt, "\n", sep = "")

    # ---- realised household sizes ----------------------------------------------------
    cat("\n-- household size --------------------------------------------------\n")
    sz <- stats::aggregate(list(households = hh$size),
                           by = list(household_type = hh$household_type,
                                     size = hh$size), FUN = length)
    mean_by_type <- stats::aggregate(list(mean_size = hh$size),
                                     by = list(household_type = hh$household_type),
                                     FUN = mean)
    n_by_type <- stats::aggregate(list(households = hh$size),
                                  by = list(household_type = hh$household_type),
                                  FUN = length)
    tbl <- merge(mean_by_type, n_by_type, by = "household_type")
    for (i in seq_len(nrow(tbl))) {
        cat(sprintf("  %-22s %7d households, mean size %.2f\n",
                    tbl$household_type[i], tbl$households[i], tbl$mean_size[i]))
    }
    cat(sprintf("  %-22s %7d households, mean size %.2f\n", "all", nrow(hh), mean(hh$size)))
    out$sizes <- tbl

    # ---- realised against target distributions ---------------------------------------
    gap_report <- function(ds, label, pairs) {
        g <- Filter(function(d) d$kind == "gap", ds)
        if (!length(g) || is.null(g[[1]]$distribution) || is.null(pairs)) return(NULL)
        d <- g[[1]]
        realised <- pairs
        bands <- d$distribution
        idx <- vapply(realised, function(v) {
            hit <- which(v >= bands$from & v <= bands$to)
            if (length(hit)) hit[1] else NA_integer_
        }, integer(1))
        obs <- tabulate(idx[!is.na(idx)], nbins = nrow(bands))
        if (!sum(obs)) return(NULL)
        p <- obs / sum(obs)
        q <- bands$count / sum(bands$count)
        tvd <- 0.5 * sum(abs(p - q))
        cat(sprintf("\n  %s on '%s': total variation distance %.3f over %d pair(s)\n",
                    label, d$column, tvd, sum(obs)))
        if (sum(is.na(idx))) {
            cat(sprintf("    %d pair(s) fell outside every band\n", sum(is.na(idx))))
        }
        data.frame(from = bands$from, to = bands$to, target = q, realised = p)
    }

    cat("\n-- matching quality ------------------------------------------------\n")
    child_gaps <- realised_child_gaps(pop, child_determinants, structure, role)
    out$gap <- list(
        couples  = gap_report(couple_determinants, "partner gap",
                              realised_gaps(pop, couple_determinants, structure, role)),
        children = gap_report(child_determinants, "parent-child gap", child_gaps))
    if (is.null(out$gap$couples) && is.null(out$gap$children)) {
        cat("  no gap determinant with a target distribution to compare against\n")
    }

    # ---- household counts against the published ones ---------------------------------
    if (!is.null(target_households)) {
        area_col <- rep$group_by[1]
        if (all(c(area_col, "household_type", "count") %in% colnames(target_households))) {
            built <- stats::aggregate(list(built = hh$size),
                                      by = list(area = hh[[area_col]],
                                                household_type = hh$household_type),
                                      FUN = length)
            names(built)[1] <- area_col
            cmp <- merge(target_households, built,
                         by = c(area_col, "household_type"), all = TRUE)
            cmp$count[is.na(cmp$count)] <- 0
            cmp$built[is.na(cmp$built)] <- 0
            cmp$difference <- cmp$built - cmp$count
            cat("\n-- households against the published counts -------------------------\n")
            by_type <- stats::aggregate(cbind(count, built) ~ household_type, cmp, sum)
            for (i in seq_len(nrow(by_type))) {
                cat(sprintf("  %-22s published %8d, built %8d (%+.1f%%)\n",
                            by_type$household_type[i], by_type$count[i], by_type$built[i],
                            100 * (by_type$built[i] / max(by_type$count[i], 1) - 1)))
            }
            within <- mean(abs(cmp$difference) <= pmax(1, 0.1 * cmp$count))
            cat(sprintf("  %.1f%% of area-by-type cells within 10%% of the published count\n",
                        100 * within))
            out$counts <- cmp
        } else {
            warning("'target_households' needs columns '", area_col,
                    "', 'household_type' and 'count'; the comparison was skipped.",
                    call. = FALSE)
        }
    }
    cat("--------------------------------------------------------------------\n\n")
    invisible(out)
}

# Recover the realised differences for a gap determinant by reading them back off the
# households that were built, so the comparison uses the assignment rather than anything
# the matching recorded about itself.
realised_gaps <- function(pop, determinants, structure, role) {
    g <- Filter(function(d) d$kind == "gap", determinants)
    if (!length(g) || is.null(structure)) return(NULL)
    d <- g[[1]]
    if (!d$column %in% colnames(pop) || !"household_id" %in% colnames(pop)) return(NULL)

    # Only the two partners carry a partner gap. Reading every pair of household members
    # would compare parents with their children and report a distribution that has nothing
    # to do with the one that was asked for.
    partner_levels <- structure$levels[structure$kind_of[structure$levels] == "partner" &
                                       structure$adults_of[structure$levels] == 2]
    keep <- !is.na(pop$household_id) & as.character(pop[[role]]) %in% partner_levels
    if (!any(keep)) return(NULL)
    sub <- pop[keep, , drop = FALSE]

    parts <- split(seq_len(nrow(sub)), sub$household_id)
    parts <- parts[lengths(parts) == 2]
    if (!length(parts)) return(NULL)
    vals <- as.numeric(sub[[d$column]])
    ref <- if (!is.null(d$orient_by) && d$orient_by %in% colnames(sub)) {
        as.character(sub[[d$orient_by]])
    } else NULL

    ix <- matrix(unlist(parts, use.names = FALSE), ncol = 2, byrow = TRUE)
    if (!is.null(ref) && !is.null(d$orient_reference)) {
        first_is_ref  <- ref[ix[, 1]] == d$orient_reference
        second_is_ref <- ref[ix[, 2]] == d$orient_reference
        signed <- xor(first_is_ref, second_is_ref)
        out <- numeric(nrow(ix))
        out[signed & first_is_ref]  <- (vals[ix[, 1]] - vals[ix[, 2]])[signed & first_is_ref]
        out[signed & second_is_ref] <- (vals[ix[, 2]] - vals[ix[, 1]])[signed & second_is_ref]
        out[!signed] <- abs(vals[ix[, 1]] - vals[ix[, 2]])[!signed]
        return(out)
    }
    abs(vals[ix[, 1]] - vals[ix[, 2]])
}

# The realised gap between a household's oldest child and the adult the child gap was
# measured from, read back off the assignment the same way.
realised_child_gaps <- function(pop, determinants, structure, role) {
    g <- Filter(function(d) d$kind == "gap", determinants)
    if (!length(g) || is.null(structure) || is.null(g[[1]]$distribution)) return(NULL)
    d <- g[[1]]
    if (!d$column %in% colnames(pop) || !"household_id" %in% colnames(pop)) return(NULL)

    child_levels  <- roles_of_kind(structure, "child")
    parent_levels <- structure$levels[structure$kind_of[structure$levels] == "partner"]
    sub <- pop[!is.na(pop$household_id), , drop = FALSE]
    kinds <- as.character(sub[[role]])
    vals <- as.numeric(sub[[d$column]])

    oldest_child <- tapply(ifelse(kinds %in% child_levels, vals, NA_real_),
                           sub$household_id, function(v) suppressWarnings(max(v, na.rm = TRUE)))
    youngest_adult <- tapply(ifelse(kinds %in% parent_levels, vals, NA_real_),
                             sub$household_id, function(v) suppressWarnings(min(v, na.rm = TRUE)))
    both <- intersect(names(oldest_child), names(youngest_adult))
    gaps <- youngest_adult[both] - oldest_child[both]
    unname(gaps[is.finite(gaps)])
}

`%||%` <- function(a, b) if (is.null(a)) b else a
