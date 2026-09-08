library(dplyr)

# Compare the datasets the attribute is built from before fitting anything, so that a
# mismatch between them is reported as a data problem rather than surfacing later as an
# unexplained IPF non-convergence or a large divergence in the verification step.
#
# The usual cause is that the sources describe different populations: a national or
# regional contingency table alongside local aggregates for the spatial units, published
# for a different reference year, a different age range, or a subpopulation (education
# levels, for instance, are commonly tabulated only for adults). IPF can rescale totals,
# but it cannot reconcile sources whose composition genuinely disagrees.
check_data_alignment <- function(df, df_contingency, margins, margins_names, group_by,
                                target_attribute) {
    notes <- character(0)
    # Agents the contingency table can actually speak about: those whose categories all
    # appear in it. A table covering a subpopulation is compared against that
    # subpopulation, not against the whole synthetic population, so that deliberately
    # partial coverage is not reported as a much larger shortfall than it is.
    # The target attribute is excluded: df holds it as all-NA at this point, so
    # including it would mark every agent as uncovered.
    conditioning <- setdiff(intersect(colnames(df), colnames(df_contingency)),
                            c("count", target_attribute))
    covered <- rep(TRUE, nrow(df))
    for (column in conditioning) {
        covered <- covered & as.character(df[[column]]) %in%
            as.character(df_contingency[[column]])
    }
    n_agents <- sum(covered)
    if (n_agents == 0) n_agents <- nrow(df)

    for (index in seq_along(margins)) {
        margin_df <- margins[[index]]
        margin_name <- margins_names[[index]]
        if (!"count" %in% colnames(margin_df)) next

        n_missing <- sum(is.na(margin_df$count))
        if (n_missing > 0) {
            where <- if (length(group_by) > 0 && group_by[1] %in% colnames(margin_df)) {
                paste0(" across ",
                       length(unique(margin_df[[group_by[1]]][is.na(margin_df$count)])),
                       " spatial unit(s)")
            } else ""
            notes <- c(notes, paste0("margin '", margin_name, "' has ", n_missing,
                " missing count(s)", where, "; those units cannot be fitted to it"))
        }

        # Counted over the levels the contingency table shares with the margin, which is
        # what the fitting itself uses. A margin that merely reaches wider than a
        # subpopulation table - neighbourhood ages against a 15-and-over contingency - is
        # trimmed to those levels during fitting and is not a mismatch.
        in_scope <- if (margin_name %in% colnames(df_contingency)) {
            as.character(margin_df[[margin_name]]) %in%
                as.character(df_contingency[[margin_name]])
        } else rep(TRUE, nrow(margin_df))
        margin_total <- sum(margin_df$count[in_scope], na.rm = TRUE)
        coverage <- margin_total / n_agents
        # 5%: census margins routinely differ by a percent or two through disclosure
        # rounding, so only a clearly different population is worth reporting.
        if (is.finite(coverage) && abs(coverage - 1) > 0.05) {
            notes <- c(notes, paste0("margin '", margin_name, "' totals ", round(margin_total),
                " against ", n_agents, " agents the contingency table covers (",
                round(100 * coverage, 1), "%), so it describes a different population"))
        }

        # Composition, not just totals: a source table whose category mix differs from the
        # local aggregates cannot be reproduced jointly, however it is scaled.
        if (margin_name %in% colnames(df_contingency)) {
            contingency_share <- tapply(df_contingency$count, df_contingency[[margin_name]], sum)
            margin_share <- tapply(margin_df$count, margin_df[[margin_name]], function(x) sum(x, na.rm = TRUE))
            shared <- intersect(names(contingency_share), names(margin_share))
            if (length(shared) > 1) {
                p <- contingency_share[shared] / sum(contingency_share[shared])
                q <- margin_share[shared] / sum(margin_share[shared])
                composition_tvd <- 0.5 * sum(abs(p - q))
                if (is.finite(composition_tvd) && composition_tvd > 0.05) {
                    notes <- c(notes, paste0("the contingency table and margin '", margin_name,
                        "' disagree about the composition of '", margin_name, "' by ",
                        round(100 * composition_tvd, 1), "%"))
                }
            }
        }

        missing_levels <- setdiff(unique(as.character(margin_df[[margin_name]])),
                                  unique(as.character(df_contingency[[margin_name]])))
        if (margin_name %in% colnames(df_contingency) && length(missing_levels) > 0) {
            notes <- c(notes, paste0("margin '", margin_name, "' has categor(ies) ",
                paste(sQuote(missing_levels), collapse = ", "),
                " that the contingency table has no cell for; they are excluded from the ",
                "fit. That is intended when the contingency table deliberately covers a ",
                "subpopulation, and a spelling mismatch otherwise. Agents in those ",
                "categories are left NA and can be assigned separately"))
        }
    }

    if (length(notes) > 0) {
        warning(paste0("The underlying datasets may not align:\n  - ",
            paste(notes, collapse = "\n  - "),
            "\nThis typically means national or regional aggregates do not match up with ",
            "the local ones - a different reference year, age range or subpopulation. IPF ",
            "will still rescale each spatial unit to its own margins, and the conditional ",
            "propensities are preserved, but the joint distribution of the result cannot ",
            "reproduce sources that disagree with each other. Check the verification ",
            "output below before using the population."), call. = FALSE)
    }
    invisible(notes)
}

#' Add a Target Attribute to a Synthetic Population Using Contingency Table Fitting
#'
#' This function adds a target attribute to a synthetic population by fitting it to an input contingency table.
#' If margins are provided, the function will apply iterative proportional fitting (IPF) to adjust the target
#' attribute distribution based on these margins. After the attribute is added, the function performs validation checks.
#'
#' @param df A data frame representing the synthetic population, where each row corresponds to an individual agent.
#' @param df_contingency A data frame representing the contingency table that contains the target attribute's distribution.
#' This should include the column for the target attribute and any other grouping variables.
#' @param target_attribute A string specifying the name of the attribute to be added to the synthetic population. 
#' This attribute must be present in the `df_contingency`.
#' @param group_by if no margins are provided, pick a a vector of column names that exist in both `df` and `df_contingency`. These are the attributes 
#' by which the population and contingency table are grouped. If margins are provided pick a column  name 
#' that exists in both the 'df' and all the 'margins' subtables. These are the attributes 
#' by which the population and margins tables are grouped and whose subgroup marginal distributions are used to fit the contingency table using IPF. Group_by often represent spatial or categorical splits.
#' @param margins An optional list of data frames containing marginal distributions to be used for IPF. If NULL, 
#' only the contingency table is used.
#' @param margins_names An optional list of vectors containing the names of the margin columns, corresponding 
#' to the data frames in `margins`. Each vector should list the margin names for the respective data frame.
#'
#' @details This function adds a target attribute (e.g., age, income level, etc.) to a synthetic population, based on 
#' its distribution in a provided contingency table. If marginal distributions are supplied, IPF is applied to adjust the 
#' population contingency distribution according to the provided margins.
#'
#' The function works in the following steps:
#' 1. **Initial Data Validation**: It checks that all `group_by` columns are present in the synthetic population (`df`).
#' 2. **IPF or Contingency Table Fit**: If margins are provided, IPF is used to adjust the contingency table to match the 
#' margin distributions. Otherwise, the contingency table is applied directly.
#' 3. **Attribute Assignment**: For each group defined by `group_by`, and if margins provided also by the additional contingency variables, the target attribute values are assigned to agents in the 
#' synthetic population based on their distribution in the contingency table or the IPF-adjusted margins.
#' 4. **Validation**: After assigning the attribute, the function verifies that the added attribute's distribution matches the 
#' original contingency table.
#'
#' @return A data frame representing the synthetic population, now with the target attribute added.
#'
#' @examples
#' # Example synthetic population data
#' df_synthetic <- data.frame(
#'   neighb_code = c("neigh_123", "neigh_123", "neigh_456"),
#'   sex = c("male", "female", "male"),
#'   age_group = c("25-45", "15-25", "45-65")
#' )
#'
#' # Example contingency table
#' df_contingency <- data.frame(
#'   age_group = c("0-15", "15-25", "25-45", "45-65", "65+"),
#'   sex = c("male", "female", "male", "female", "male"),
#'   migrationbackground = c("Dutch", "Dutch", "Non-Dutch", "Dutch", "Non-Dutch"),
#'   count = c(19, 31, 66, 54, 29)
#' )
#' 
#' margins <- list(
#'   age_group = data.frame(
#'     neighb_code = c("neigh_123", "neigh_123", "neigh_123", "neigh_123", 
#'                    "neigh_123", "neigh_456", "neigh_456", "neigh_456", 
#'                      "neigh_456", "neigh_456"),
#'     age_group = c("0-15", "15-25", "25-45", "45-65", "65+", "0-15", 
#'                    "15-25", "25-45", "45-65", "65+"),
#'     count = c(50, 100, 150, 200, 75, 40, 90, 130, 180, 60)
#'   ),
#'   sex = data.frame(
#'     neighb_code = c("neigh_123", "neigh_123", "neigh_456", "neigh_456"),
#'     sex = c("male", "female", "male", "female"),
#'     count = c(250, 200, 220, 180)
#'   )
#' )
#'
#' # Apply the function without margins
#' df_updated <- Conditional_attribute_adder(
#'   df = df_synthetic, 
#'   df_contingency = df_contingency, 
#'   target_attribute = "migrationbackground", 
#'   group_by = c("neighb_code"),
#'   margins = margins,
#'   margins_names = c("age_group", "sex")
#' )
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom stats aggregate as.formula ave chisq.test complete.cases setNames
#' @export
Conditional_attribute_adder <- function(df, df_contingency, target_attribute, 
                              group_by = NULL, margins = NULL, margins_names = NULL) {
    # Reset and clean the synthetic population dataframe
    if ("index" %in% colnames(df)) {
        df <- df[ , !colnames(df) %in% "index"]
    }
    if ("level_0" %in% colnames(df)) {
        df <- df[ , !colnames(df) %in% "level_0"]
    }

    if (!all(group_by %in% colnames(df))) {
    stop("One or more columns in 'group_by' do not exist in the dataframe.")
    }
    if (!target_attribute %in% colnames(df_contingency)) {
    stop("The target attribute does not exist in df_contingency.")
    }

    # Add a new empty column for the target attribute
    df[[target_attribute]] <- NA
    
    # Determine the margins group if margins are provided
    if (!is.null(margins)) {
        print("margins provided: using IPF")
        contingency_cols <- colnames(df_contingency)
        uncoveredcontingency <- contingency_cols[(!contingency_cols %in% margins_names) & (contingency_cols != "count")]
        if (length(uncoveredcontingency) == 0) {
          uncoveredcontingency <- NULL
        } else{
          print(paste("uncoveredcontingency: ", uncoveredcontingency))
          }
        uncoveredmargins <- margins_names[!margins_names %in% contingency_cols]
        if (length(uncoveredmargins) > 0) {
          print(paste("Warning: Margins", uncoveredmargins, "not found in the contingency table. It will be ignored."))
          margins <- margins[margins_names %in% contingency_cols]
          margins_names <- margins_names[margins_names %in% contingency_cols]
        }

        margins_group <- unique(unlist(lapply(c(margins_names, uncoveredcontingency), function(x) x[x %in% colnames(df)])))
        marginorder <-lapply(margins_names, function(col_name) {
            unique(df_contingency[[col_name]])
        })
        # Reported up front, so that a mismatch between the sources is recognisable as a
        # data problem rather than appearing later as an unexplained fitting failure.
        check_data_alignment(df, df_contingency, margins, margins_names, group_by,
                             target_attribute)
    } else {
        print("no margins provided: using only supplied contigency table")
        margins_group <- group_by
        marginorder <- NULL
        uncoveredcontingency <- NULL
    }
    
    print(paste("margins_group: ", margins_group))

    # Columns that subdivide each group. With margins the attribute is assigned per
    # (group x margin-combination) cell; without margins the group itself is the cell.
    sub_group_by <- if (is.null(margins)) character(0) else
        margins_group[!(margins_group %in% group_by) & margins_group != target_attribute]

    # Resolve every agent to its cell in a single pass over the population.
    #
    # The previous implementation located each cell by scanning the whole population
    # with get_group_mask() once per group *and* once per sub-group column, then called
    # nrow(df[mask, ]) - which materialises a full copy of every column merely to count
    # rows. That made the loop O(groups x cells x columns x nrow(df)); profiling a
    # 861k-agent, 457-neighbourhood run spent ~99% of its time there and under 1% in the
    # actual IPF. Splitting the row indices once is O(nrow(df)) in total and leaves the
    # statistics untouched.
    #
    # Keys are compared as character: the group columns are typically factors, and
    # factor == character dispatches to Ops.factor/NextMethod, which alone accounted for
    # over a third of the old runtime.
    keycols <- c(group_by, sub_group_by)
    keydf <- df[, keycols, drop = FALSE]
    keydf[] <- lapply(keydf, as.character)
    cells <- unique(keydf)
    cell_key <- do.call(paste, c(cells, sep = "\r"))
    row_key <- do.call(paste, c(keydf, sep = "\r"))
    # levels = cell_key keeps cell_rows[[i]] aligned with cells[i, ]
    cell_rows <- split(seq_len(nrow(df)), factor(row_key, levels = cell_key))
    cells_by_group <- split(seq_along(cell_key),
                            do.call(paste, c(cells[group_by], sep = "\r")))

    # Collect the assignments in a plain vector. Writing them straight into df would go
    # through [<-.data.frame once per cell, copying the frame each time.
    assigned <- rep(NA_character_, nrow(df))

    # Main loop over the group-by attributes. Iterating over unique *combinations*
    # keeps a multi-column group_by working, as documented: df[[group_by]] would be
    # recursive indexing (df[["a"]][["b"]]) and fails for more than one column.
    # IPF reports non-convergence once per spatial unit it cannot fit. Collected here and
    # reported once, with the likely cause, instead of as hundreds of separate warnings.
    unconverged_groups <- character(0)

    uniquegroups <- unique(keydf[, group_by, drop = FALSE])
    for (group_index in seq_len(nrow(uniquegroups))) {
        group_name <- unlist(uniquegroups[group_index, ], use.names = FALSE)
        print(paste("Processing group", paste(group_name, collapse = " | "), "which is", group_index, "of", nrow(uniquegroups)))
        df_contingency_group <- withCallingHandlers(
            GenSynthPop::ipf_fit_contingency_table(df_contingency = df_contingency, group_name = group_name,
                                                            group_by = group_by, margins = margins ,
                                                            margins_names = margins_names,
                                                            marginorder = marginorder,
                                                            uncoveredcontingency = uncoveredcontingency),
            warning = function(w) {
                if (grepl("did not converged", w$message, fixed = TRUE)) {
                    unconverged_groups <<- c(unconverged_groups, paste(group_name, collapse = " | "))
                    invokeRestart("muffleWarning")
                }
            })
        group_fractions <- GenSynthPop::get_group_fractions( df_contingency = df_contingency_group, group_by = group_by, target_attribute = target_attribute, margins_names = margins_names, margins_group = margins_group )

        # Only the cells that actually contain agents; empty margin combinations that the
        # old expand.grid() produced and then skipped never get built in the first place.
        for (cell_index in cells_by_group[[paste(group_name, collapse = "\r")]]) {
            rows <- cell_rows[[cell_index]]
            if (!length(rows)) next
            cell_fractions <- group_fractions
            for (i in seq_along(sub_group_by)) {
                cell_fractions <- cell_fractions[as.character(cell_fractions[[sub_group_by[i]]]) ==
                                                     cells[cell_index, sub_group_by[i]], , drop = FALSE]
            }
            cell_fractions <- cell_fractions[, c(target_attribute, "fraction"), drop = FALSE]
            # A cell with no rows at all - a group absent from the contingency table - is
            # left NA, and verify_target_attribute() reports the unassigned agents. It is
            # not passed to calculate_group_counts(), whose correction loop cannot reach
            # the agent total from an empty fraction set and spins forever.
            # Cells that do have rows but sum to zero are still passed through, so the
            # existing behaviour of falling back to the first category is unchanged.
            if (nrow(cell_fractions) == 0) next
            assigned[rows] <- GenSynthPop::get_agent_values_from_fractions(group_fractions = cell_fractions,
                                                                          group_agent_count = length(rows),
                                                                          target_attribute = target_attribute)
        }
    }
    df[[target_attribute]] <- assigned

    if (length(unconverged_groups) > 0) {
        failed <- unique(unconverged_groups)
        examples <- paste(failed[seq_len(min(5, length(failed)))], collapse = ", ")
        warning(paste0("IPF did not converge for ", length(failed), " of ",
            nrow(uniquegroups), " group(s) (e.g. ", examples,
            "). Those groups keep the unfitted contingency distribution.\n",
            "This usually means the underlying data does not align: the margins of a ",
            "spatial unit ask for a combination that the contingency table gives zero ",
            "weight to - national or regional aggregates not matching up with the local ",
            "ones, for instance a category that only applies to part of the population ",
            "(such as education levels tabulated for adults only) being fitted against ",
            "age margins that cover everyone. Check that the sources describe the same ",
            "population, reference year and age range."), call. = FALSE)
    }

    print("Verifying attribute")
    GenSynthPop::verify_target_attribute(df, df_contingency, target_attribute, margins_group,
                                         margins = margins, margins_names = margins_names,
                                         group_by = group_by)
    return(df)
}
