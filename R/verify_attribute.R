library(dplyr)
library(stats)

#' Generate a contingency table from a synthetic population dataset
#'
#' This function constructs a contingency table from a synthetic population data frame. The table represents counts for 
#' each combination of specified attributes. If `full_crostab` is set to TRUE, it ensures all possible combinations 
#' of attribute levels are represented in the table, filling in any missing combinations with a count of zero.
#'
#' @param df_synthetic_population A data frame representing the synthetic population, where each row corresponds to an agent.
#' @param columns A character vector specifying the column names (attributes) to be used for building the contingency table. 
#' If NULL, all columns in the data frame will be used. Default is NULL.
#' @param full_crostab Logical; if TRUE, fills in missing combinations of attributes with zero counts. If FALSE, only observed 
#' combinations will be included in the table. Default is FALSE.
#'
#' @details This function creates a contingency table by counting the occurrences of each unique combination of the specified attributes
#' (or all attributes if none are specified). The result is a data frame with one row per unique combination of attribute levels.
#' 
#' When `full_crostab` is TRUE, the function computes all possible combinations of the attribute levels and merges them with the 
#' observed data. Missing combinations are filled with a count of 0. This is useful in cases where you want to ensure that 
#' every possible combination of attributes is explicitly represented in the contingency table, even if no agents have that combination.
#'
#' @return A data frame representing the contingency table, where each row is a unique combination of the specified attributes, 
#' and a `Freq` column indicates the count of occurrences for that combination.
#' 
#' @examples
#' # Example usage:
#' df_synthetic_population <- data.frame(
#'   age_group = c("0-15", "15-25", "25-45", "45-65", "65+", "0-15", "25-45"),
#'   sex = c("male", "female", "male", "female", "male", "female", "male"),
#'   migrationbackground = c("Dutch", "Dutch", "Non-Dutch", "Dutch", "Non-Dutch", "Non-Dutch", "Dutch")
#' )
#' 
#' # Create a contingency table for all columns
#' synthetic_population_to_contingency(df_synthetic_population)
#' 
#' # Create a contingency table for specific columns (age_group and sex)
#' synthetic_population_to_contingency(df_synthetic_population, columns = c("age_group", "sex"))
#' 

#' @export
synthetic_population_to_contingency <- function(df_synthetic_population, columns = NULL, full_crostab = FALSE) {
  if (is.null(columns)) {
    columns <- colnames(df_synthetic_population)
  }
  df <- as.data.frame(table(df_synthetic_population[, columns]))
  if (full_crostab) {
    if (!is.data.frame(df)) {
      df <- as.data.frame(table(df_synthetic_population[, columns], useNA = "ifany"))
      df[is.na(df)] <- 0
    } else {
      levels_list <- lapply(columns, function(col) unique(df_synthetic_population[[col]]))
      all_combinations <- expand.grid(levels_list)
      df <- merge(all_combinations, df, by = columns, all.x = TRUE)
      df[is.na(df)] <- 0
    }
  }
  return(df)
}


#' Split a Total Variation Distance into its Conditional and Marginal Parts
#'
#' Compares observed against expected counts and separates the two things that can make
#' them differ: the distribution *within* each group, and the relative sizes of the groups
#' themselves.
#'
#' The distinction matters because fitting an attribute by `group_by` controls only the
#' first of these. Group sizes come from the population, not from the contingency table.
#' Measuring the two together - normalising every cell by the grand total - therefore
#' reports a size mismatch as though it were a fitting error, and does so exactly: when
#' the conditional distributions are reproduced perfectly, the joint distance reduces
#' algebraically to the marginal one. A contingency table expressed as shares rather than
#' counts, a national table compared against local units, or a table covering only part of
#' the population will each produce a large joint distance with nothing wrong in the fit.
#'
#' @param observed_count Observed counts, one per cell
#' @param expected_count Expected counts, one per cell
#' @param group Vector naming the group each cell belongs to. A single constant value
#'   leaves the conditional distance equal to the joint one.
#' @return A list with `conditional` (group-size weighted mean of the within-group
#'   distances), `marginal` (distance between the two sets of group sizes), `joint` (the
#'   undivided distance over all cells), and the number of groups compared and skipped.
#' @keywords internal
#' @export
split_total_variation_distance <- function(observed_count, expected_count, group) {
  observed_count <- as.numeric(observed_count)
  expected_count <- as.numeric(expected_count)
  group <- as.character(group)

  joint <- 0.5 * sum(abs(observed_count / sum(observed_count) -
                           expected_count / sum(expected_count)))

  observed_group <- tapply(observed_count, group, sum)
  expected_group <- tapply(expected_count, group, sum)[names(observed_group)]
  marginal <- 0.5 * sum(abs(observed_group / sum(observed_group) -
                              expected_group / sum(expected_group)))

  # A group with nothing on one side has no distribution to compare; it is left out of
  # the conditional distance rather than counted as a perfect or a total mismatch.
  comparable <- observed_group > 0 & expected_group > 0
  per_group <- vapply(names(observed_group), function(g) {
    if (!isTRUE(comparable[[g]])) return(NA_real_)
    cells <- group == g
    0.5 * sum(abs(observed_count[cells] / sum(observed_count[cells]) -
                    expected_count[cells] / sum(expected_count[cells])))
  }, numeric(1))

  weight <- observed_group[names(per_group)]
  keep <- !is.na(per_group)
  conditional <- if (any(keep)) sum(per_group[keep] * weight[keep]) / sum(weight[keep]) else NA_real_

  list(conditional = conditional, marginal = marginal, joint = joint,
       n_groups = sum(keep), n_skipped = sum(!keep))
}


#' Verify the Target Attribute
#'
#' This function verifies the integrity of the target attribute in the synthetic population.
#' It checks if any agents have not been assigned a value for the target attribute, and then
#' compares the result against both sources it was built from:
#'
#' 1. the contingency table, i.e. the region-wide joint distribution of the target attribute
#'    with the other attributes, and
#' 2. the margins, i.e. the totals published for each spatial unit, if they were supplied.
#'
#' The two answer different questions. Divergence from the contingency table is expected
#' whenever the attribute is fitted to local margins or assigned to a subpopulation, whereas
#' the margins are a hard constraint the population is meant to reproduce, so a divergence
#' there points at a fitting problem.
#'
#' @param df A data frame representing the synthetic population that includes the target attribute.
#' @param df_contingency A data frame representing the original distribution from which the target attribute
#'                        is derived.
#' @param target_attribute A string representing the name of the target attribute to verify.
#' @param margins_group A vector of attribute names used to group the data for comparison.
#' @param margins An optional list of the marginal distribution data frames the attribute was fitted to.
#' @param margins_names An optional vector naming the margin column in each data frame in `margins`.
#' @param group_by An optional vector of column names identifying the spatial unit the margins apply to.
#'
#' @return NULL This function does not return any value; it generates warnings if there are issues
#'               with the target attribute.
#'
#' @importFrom stats chisq.test
#' @importFrom dplyr inner_join
#'
#' @export
verify_target_attribute <- function(df, df_contingency, target_attribute, margins_group,
                                    margins = NULL, margins_names = NULL, group_by = NULL) {
  if (any(is.na(df[[target_attribute]]))) {
    warning(paste("Not all agents were assigned a", target_attribute, "value. Caution advised."))
  }
  # Named separately from the group_by argument, which identifies the spatial unit the
  # margins apply to and is still needed further down.
  contingency_group <- unique(c(margins_group, target_attribute))
  contingency <- GenSynthPop::synthetic_population_to_contingency(df, contingency_group) %>%
    dplyr::inner_join(df_contingency, by = contingency_group)
  # Select the two count columns by name rather than by position: df_contingency may
  # carry columns beyond the join keys and the count, in which case the join is wider
  # than length(contingency_group) + 2 and positional renaming silently mislabels them.
  expected_col <- if ("count" %in% colnames(df_contingency)) "count" else
    setdiff(colnames(df_contingency), contingency_group)[1]
  contingency <- contingency[, c(contingency_group, "Freq", expected_col)]
  colnames(contingency) <- c(contingency_group, "observed_count", "expected_count")
  print("Final Contingency table:")
  print(contingency)

  # Goodness-of-fit, not independence: the question is whether the observed counts
  # follow the expected distribution. Passing the two count vectors as x and y
  # instead tests whether they are independent, whose p-value barely responds to
  # fit quality (it returns ~0.2 for anything from a 1% to an 80% deviation) and
  # errors outright when either vector is constant.
  fitcells <- contingency[contingency$expected_count > 0, ]
  # chisq.test() warns "Chi-squared approximation may be incorrect" as soon as one
  # expected count falls below 5, the rule of thumb for the approximation. Comparing per
  # spatial unit produces such cells by construction - small units, and source counts
  # rounded to the nearest 5 for disclosure control - and it says nothing about the
  # quality of the fit. The number of sparse cells is reported instead of the warning,
  # since the p-value is deliberately not used for any decision here (see below).
  expected_counts <- sum(as.numeric(fitcells$observed_count)) *
    as.numeric(fitcells$expected_count) / sum(as.numeric(fitcells$expected_count))
  n_sparse <- sum(expected_counts < 5)
  ChisqTestRes <- withCallingHandlers(
    chisq.test(x = fitcells$observed_count,
               p = fitcells$expected_count / sum(fitcells$expected_count)),
    warning = function(w) {
      if (grepl("Chi-squared approximation may be incorrect", w$message)) {
        invokeRestart("muffleWarning")
      }
    })
  print("Chi-squared test results:")
  print(ChisqTestRes)
  print(paste("Chi-squared test p-value:", ChisqTestRes$p.value))
  if (n_sparse > 0) {
    print(paste0("Note: ", n_sparse, " of ", nrow(fitcells), " compared cells have an ",
                 "expected count below 5, so the chi-squared approximation is unreliable. ",
                 "That is normal when comparing small spatial units and is not a sign of a ",
                 "fitting problem - judge the fit by the total variation distance below."))
  }

  # The p-value is reported but not warned on: for a large synthetic population it
  # rejects deviations far too small to matter. The magnitude is judged instead, and
  # judged on the distribution within each combination of the other attributes, since
  # that is what fitting the target actually determines - the number of agents holding
  # each combination was settled by earlier steps and by the population itself.
  conditioned_on <- setdiff(contingency_group, target_attribute)
  cell_group <- if (length(conditioned_on) == 0) rep("all", nrow(fitcells)) else
    do.call(paste, c(as.list(fitcells[conditioned_on]), sep = "\r"))
  distance <- GenSynthPop::split_total_variation_distance(fitcells$observed_count,
                                                          fitcells$expected_count, cell_group)
  total_variation_distance <- distance$conditional

  within <- if (length(conditioned_on) == 0) "the population as a whole" else
    paste("each", paste(conditioned_on, collapse = " x "))
  print(paste0("Contingency table - total variation distance within ", within, ": ",
               round(100 * distance$conditional, 2), "% of agents would have to be reassigned to ",
               "another ", target_attribute, " to reproduce the source distribution inside their ",
               "own group."))
  if (distance$n_skipped > 0) {
    print(paste0("(", distance$n_skipped, " of ", distance$n_groups + distance$n_skipped,
                 " groups were empty on one side and left out of that figure.)"))
  }
  print(paste0("Reported for context rather than judged: group sizes differ from the source table ",
               "by ", round(100 * distance$marginal, 2), "%, which taken together with the above ",
               "gives an undivided distance of ", round(100 * distance$joint, 2), "%. The number of ",
               "agents in each group comes from the population and from earlier steps, not from this ",
               "table, so a large value there says the two describe different populations - a ",
               "different reference year, a national table against local units, a table covering only ",
               "part of the population, or counts given as shares - rather than that the fit is wrong."))

  # Threshold set at 5%: an attribute drawn straight from the contingency table lands
  # near 0.1%, while fitting to local margins moves the distribution a few percent off
  # the region-wide table by design. Past 5% the divergence is larger than that intended
  # local and multi-variable variation comfortably accounts for.
  if (!is.na(total_variation_distance) && total_variation_distance > 0.05) {
    warning(paste0("The added attribute ", target_attribute, " does not reproduce the distribution ",
                   "the contingency table specifies within ", within, ". Total variation distance: ",
                   round(100 * total_variation_distance, 2), "%.\n",
                   "This measures only the composition inside each group, so it is not inflated by ",
                   "the source table and the population differing in size or coverage. Check the ",
                   "contingency table against the attributes already assigned, and the margin ",
                   "checks below, which test the constraints the attribute was fitted to."),
            call. = FALSE)
  }

  # Second check: the margins the attribute was actually fitted to. Unlike the contingency
  # table these are a hard constraint per spatial unit, so the same 5% is a much stricter
  # test here - a correctly fitted attribute sits near 0.1%, well under the threshold.
  if (!is.null(margins) && !is.null(margins_names) && !is.null(group_by)) {
    spatial_key <- function(d) do.call(paste, c(as.list(d[group_by]), sep = "\r"))
    # Hoisted out of the loop: it does not depend on the margin, and pasting one column
    # per agent for every margin in turn is the most expensive step in this check.
    df_spatial_key <- spatial_key(df)
    for (margin_index in seq_along(margins)) {
      margin_name <- margins_names[[margin_index]]
      margin_df <- margins[[margin_index]]
      if (!margin_name %in% colnames(df) || !margin_name %in% colnames(margin_df)) next

      observed <- as.data.frame(table(df_spatial_key, df[[margin_name]]), stringsAsFactors = FALSE)
      colnames(observed) <- c("spatial_unit", margin_name, "observed_count")
      margin_df$spatial_unit <- spatial_key(margin_df)
      compared <- merge(observed, margin_df[, c("spatial_unit", margin_name, "count")],
                        by = c("spatial_unit", margin_name))
      compared <- compared[!is.na(compared$count), ]
      if (nrow(compared) == 0 || sum(compared$observed_count) == 0 || sum(compared$count) == 0) next

      # Split the same way as above. A margin can cover a different population from the
      # agents being fitted - counts for ages 15 to 75 against a population of 15 and
      # over, say - and that difference belongs in the marginal part, not in the fit.
      margin_distance <- GenSynthPop::split_total_variation_distance(compared$observed_count,
                                                                     compared$count,
                                                                     compared$spatial_unit)
      print(paste0("Margin '", margin_name, "' - total variation distance within spatial units: ",
                   round(100 * margin_distance$conditional, 2),
                   "% (unit sizes differ by ", round(100 * margin_distance$marginal, 2),
                   "%, undivided ", round(100 * margin_distance$joint, 2), "%)"))
      if (!is.na(margin_distance$conditional) && margin_distance$conditional > 0.05) {
        warning(paste0("The added attribute ", target_attribute, " does not reproduce the '", margin_name,
                       "' composition of the spatial units it was fitted to. Total variation distance: ",
                       round(100 * margin_distance$conditional, 2), "%"))
      }
    }
  }
}
