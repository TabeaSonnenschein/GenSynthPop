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
  ChisqTestRes <- chisq.test(x = fitcells$observed_count,
                             p = fitcells$expected_count / sum(fitcells$expected_count))
  print("Chi-squared test results:")
  print(ChisqTestRes)
  print(paste("Chi-squared test p-value:", ChisqTestRes$p.value))

  # The p-value is reported but not warned on: for a large synthetic population it
  # rejects deviations far too small to matter. The magnitude is judged instead.
  observed_share <- fitcells$observed_count / sum(fitcells$observed_count)
  expected_share <- fitcells$expected_count / sum(fitcells$expected_count)
  total_variation_distance <- 0.5 * sum(abs(observed_share - expected_share))

  print(paste0("Contingency table - total variation distance: ", round(100 * total_variation_distance, 2),
               "% of agents would have to be reassigned to another category to reproduce ",
               "the source distribution exactly."))
  print(paste("Some divergence is expected and intended: the attribute is fitted to each",
              "spatial unit's own margins and jointly with the other attributes, so the",
              "population is meant to depart from the region-wide source table wherever",
              "local composition differs from it."))

  # Threshold set at 5%: an attribute drawn straight from the contingency table lands
  # near 0.1%, while fitting to local margins moves the joint distribution a few percent
  # off the region-wide table by design. Past 5% the divergence is larger than that
  # intended local and multi-variable variation comfortably accounts for.
  if (total_variation_distance > 0.05) {
    warning(paste0("The added attribute ", target_attribute, " diverges from the contingency table ",
                   "by more than intended local variation explains. Total variation distance: ",
                   round(100 * total_variation_distance, 2), "%"))
  }

  # Second check: the margins the attribute was actually fitted to. Unlike the contingency
  # table these are a hard constraint per spatial unit, so the same 5% is a much stricter
  # test here - a correctly fitted attribute sits near 0.1%, well under the threshold.
  if (!is.null(margins) && !is.null(margins_names) && !is.null(group_by)) {
    spatial_key <- function(d) do.call(paste, c(as.list(d[group_by]), sep = "\r"))
    for (margin_index in seq_along(margins)) {
      margin_name <- margins_names[[margin_index]]
      margin_df <- margins[[margin_index]]
      if (!margin_name %in% colnames(df) || !margin_name %in% colnames(margin_df)) next

      observed <- as.data.frame(table(spatial_key(df), df[[margin_name]]), stringsAsFactors = FALSE)
      colnames(observed) <- c("spatial_unit", margin_name, "observed_count")
      margin_df$spatial_unit <- spatial_key(margin_df)
      compared <- merge(observed, margin_df[, c("spatial_unit", margin_name, "count")],
                        by = c("spatial_unit", margin_name))
      compared <- compared[!is.na(compared$count), ]
      if (nrow(compared) == 0 || sum(compared$observed_count) == 0 || sum(compared$count) == 0) next

      margin_tvd <- 0.5 * sum(abs(compared$observed_count / sum(compared$observed_count) -
                                    compared$count / sum(compared$count)))
      print(paste0("Margin '", margin_name, "' - total variation distance across spatial units: ",
                   round(100 * margin_tvd, 2), "%"))
      if (margin_tvd > 0.05) {
        warning(paste0("The added attribute ", target_attribute, " does not reproduce the '", margin_name,
                       "' margins of the spatial units it was fitted to. Total variation distance: ",
                       round(100 * margin_tvd, 2), "%"))
      }
    }
  }
}
