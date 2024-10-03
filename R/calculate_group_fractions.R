library(dplyr)


#' Calculate Fractions from a Contingency Table
#'
#' This function computes the fractions of counts for each combination of conditional attributes in a contingency 
#' table. It takes a data frame containing counts and adds a new column representing the proportion of the count 
#' for each group of conditional attributes.
#'
#' @param df_contingency A data frame representing the contingency table, which must include conditional 
#' attributes and a `count` column indicating the frequency of each attribute combination.
#' @param group_by A character vector specifying the conditional attributes by which to group the data when calculating 
#' the fractions. This should include the attributes to consider for the fraction calculation.
#' @param target_attribute A character string representing the target attribute for which fractions are to be calculated. 
#' This attribute should be included in the `df_contingency` data frame.
#' @param margins_names A character vector of margin names to include in the grouping, if any. This is optional; 
#' if provided, it is combined with the `group_by` parameters.
#' @param margins_group A character vector representing the groups for margins, which can also influence the fraction calculations. 
#' This parameter is optional.
#'
#' @details 
#' The function calculates the fraction of counts for each group defined by the specified `group_by` attributes. 
#' If `margins_names` are provided, these will be included in the grouping for the fraction calculation. The function 
#' ensures that the target attribute is excluded from the grouping to avoid redundancy.
#'
#' A new column named `fraction` is added to the output data frame, showing the proportion of each count relative 
#' to the total count for the corresponding group. If the total count for a group is zero, the fraction will be set to zero to 
#' avoid division by zero errors.
#'
#' @return A data frame with an added `fraction` column, which displays the calculated fractions for each group of 
#' conditional attributes based on the provided contingency table.
#'
#' @examples
#' # Example: Calculate fractions for a simple contingency table
#' df_contingency <- data.frame(
#'   age_group = c("0-15", "15-25", "25-45", "45-65"),
#'   sex = c("male", "female", "male", "female"),
#'   count = c(30, 20, 50, 40)
#' )
#' 
#' fractions <- calculate_fractions(df_contingency, 
#'                                   group_by = c("age_group"), 
#'                                   target_attribute = "sex")
#' # Returns the data frame with an added 'fraction' column
#'
#' @keywords internal
#' @importFrom stats ave
#' @export
calculate_fractions <- function(df_contingency, group_by, target_attribute, margins_names = NULL, margins_group = NULL) {
  df_fractions <- df_contingency
  if (!is.null(margins_names)) {
    group_by <- unique(c(group_by, margins_group))
  }
  group_by <- group_by[group_by != target_attribute & group_by %in% colnames(df_contingency)]
  # Calculate the fractions for each group
  df_fractions$fraction <- ave(df_contingency$count, df_contingency[, group_by], FUN = function(x) {
    total <- sum(x)
    if (total != 0) {
      return(x / total)
    } else {
      return(0)
    }
  })
  return(df_fractions)
}

#' Calculate Group Fractions from Contingency Data
#'
#' This function computes the fractions (proportions) of synthetic agents for each combination of group attributes 
#' based on a contingency table. The resulting fractions represent the distribution of the target attribute across 
#' specified groups and optional margins.
#'
#' @param df_contingency A data frame representing the contingency table that contains the observed distribution of agents across combinations of attributes.
#' @param group_by A character vector specifying the columns in the contingency table to group the data by. This typically represents the dimensions or attributes along which the data is split.
#' @param target_attribute A string specifying the target attribute for which the fractions are calculated. This is the variable of interest in the contingency table.
#' @param margins_names A list of column names representing the margins (optional). If provided, it includes additional attributes to consider when calculating the fractions.
#' @param margins_group A character vector indicating the group for margins (optional). This allows for finer resolution in calculating fractions based on specific margin groups.
#'
#' @details 
#' The function first calls \code{calculate_fractions} to generate fractional values from the contingency table. 
#' These fractions represent the proportion of agents assigned to each combination of the target attribute and the 
#' group-by variables. The result is a filtered and ordered data frame of fractions, with indices set by the target 
#' attribute and any margin names, if applicable.
#'
#' If \code{margins_names} are provided, the function combines them with the \code{target_attribute} and any additional 
#' margin groups to ensure the returned fractions are indexed by all relevant attributes.
#'
#' @return A data frame of calculated fractions, with rows corresponding to combinations of the target attribute 
#' and any provided margins, and columns representing the fractions.
#'
#' @examples
#' # Example: Calculate fractions for target attribute 'age_group' 
#' # across 'sex' and 'migrationbackground' groups
#' df_contingency <- data.frame(
#'   sex = c("male", "female", "male", "female"),
#'   migrationbackground = c("Dutch", "Dutch", "Non-Dutch", "Non-Dutch"),
#'   age_group = c("0-15", "15-25", "25-45", "45-65"),
#'   count = c(30, 25, 50, 45)
#' )
#' 
#' fractions <- get_group_fractions(df_contingency, group_by = c("sex", "migrationbackground"), 
#'                                  target_attribute = "age_group")
#' # Returns a data frame of fractions based on 'age_group', 'sex', and 'migrationbackground'
#'
#' @keywords internal
#' @importFrom stats complete.cases
#' @export
get_group_fractions <- function(df_contingency, group_by, target_attribute, margins_names = NULL, margins_group = NULL) {
  df_fractions <- calculate_fractions(df_contingency, group_by, target_attribute, margins_names, margins_group)
  index <- target_attribute
  if (!is.null(margins_names)) {
    index <- unique(c(index, unlist(margins_group)))
  }
  df_fractions <- df_fractions[order(df_fractions[, index]), c(index,"fraction")]
  df_fractions <- df_fractions[complete.cases(df_fractions), ]
  return(df_fractions)
}

