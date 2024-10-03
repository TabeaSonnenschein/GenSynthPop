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
#' It checks if any agents have not been assigned a value for the target attribute and compares
#' the distribution of the target attribute with a given contingency table using a Z-squared test.
#'
#' @param df A data frame representing the synthetic population that includes the target attribute.
#' @param df_contingency A data frame representing the original distribution from which the target attribute
#'                        is derived.
#' @param target_attribute A string representing the name of the target attribute to verify.
#' @param margins_group A vector of attribute names used to group the data for comparison.
#'
#' @return NULL This function does not return any value; it generates warnings if there are issues
#'               with the target attribute.
#' 
#' @importFrom stats chisq.test
#' @importFrom dplyr inner_join
#' 
#' @export
verify_target_attribute <- function(df, df_contingency, target_attribute, margins_group) {
  if (any(is.na(df[[target_attribute]]))) {
    warning(paste("Not all agents were assigned a", target_attribute, "value. Caution advised."))
  }
  group_by <- unique(c(margins_group, target_attribute))
  contingency <- GenSynthPop::synthetic_population_to_contingency(df, group_by) %>%
    dplyr::inner_join(df_contingency, by = group_by) 
  colnames(contingency) <- c(group_by, "observed_count", "expected_count")
  print("Final Contingency table:")
  print(contingency)

  ChisqTestRes <- chisq.test(contingency$observed_count, 
                              contingency$expected_count, 
                              # simulate.p.value = TRUE #uses simulation conditional on the marginals, is a version of the Fisher exact test
                              ) 
  print("Chi-squared test results:")
  print(ChisqTestRes)
  print(paste("Chi-squared test p-value:", ChisqTestRes$p.value))
  if (ChisqTestRes$p.value > 0.05) {
    warning(paste("The added attribute", target_attribute, "does not statistically match the original distribution. P-value:", ChisqTestRes$p.value))
  }
}