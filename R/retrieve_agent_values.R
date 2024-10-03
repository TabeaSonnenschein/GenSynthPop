# Required packages
library(dplyr)

#' Calculate Adjusted Group Counts Based on Fractions
#'
#' This function calculates the number of synthetic agents for each group based on the fractional distribution 
#' of the target attribute. It ensures the total count of agents matches the specified total number of agents 
#' in the synthetic population.
#'
#' @param df_fractions A numeric vector representing the fractional distribution of agents for each group (the result of \code{ConditionalAttributeAdder$calculate_fractions}).
#' Each value represents the proportion of agents that should belong to a given group.
#' @param n_agents_total An integer representing the total number of synthetic agents in the population.
#'
#' @details 
#' The function initially calculates the agent counts for each group by multiplying the fractional distribution 
#' (`df_fractions`) by the total number of agents (`n_agents_total`). Since rounding these counts may result in a mismatch between 
#' the sum of the counts and the total number of agents, the function iteratively adjusts the counts:
#' \itemize{
#'   \item It identifies the group with the largest fractional difference between the computed counts and the expected 
#'         fractional value.
#'   \item It increments or decrements the count of that group until the sum of all counts equals \code{n_agents_total}.
#' }
#'
#' @return A numeric vector representing the adjusted counts of synthetic agents for each group, ensuring the total 
#' matches \code{n_agents_total}.
#'
#' @examples
#' # Example: Adjusting counts based on fractional distribution
#' df_fractions <- c(0.3, 0.5, 0.2)
#' n_agents_total <- 10
#' 
#' adjusted_counts <- calculate_group_counts(df_fractions, n_agents_total)
#' # The result is a vector of adjusted counts summing to 10.
#'
#' @export
calculate_group_counts <- function(df_fractions, n_agents_total) {
  # Apply the fraction to the total number of agents to obtain a first estimate of synthetic agent counts per group
  counts <- round(df_fractions * n_agents_total)
  while (sum(counts) != n_agents_total) {
    # Find the group with the largest fractional difference to the expected fraction
    differences <- (counts / n_agents_total) - df_fractions
    correction_target <- if (sum(counts) < n_agents_total) {
      which.min(differences)
    } else {
      which.max(differences)
    }
    # Adjust that group in the direction that moves the count towards `n_agents_total`
    if (sum(counts) < n_agents_total) {
      counts[correction_target] <- counts[correction_target] + 1
    } else {
      counts[correction_target] <- counts[correction_target] - 1
    }
  }
  return(counts)
}

#' Generate Agent Attribute Values Based on Group Fractions
#'
#' This function assigns values of a target attribute to agents within a group based on the relative proportions
#' (fractions) of those values in the group. The generated values match the proportions given by the input fractions.
#'
#' @param group_fractions A data frame containing the fractions for each value of the target attribute. 
#' The data frame must include a `fraction` column representing the proportion of each target attribute value within the group, 
#' and a column for the target attribute itself.
#' @param group_agent_count An integer representing the total number of agents in the group.
#' @param target_attribute A string specifying the name of the target attribute column in `group_fractions`. 
#' Default is "target_attribute".
#'
#' @details 
#' This function takes a group of agents and assigns values to the target attribute based on the provided `group_fractions`. 
#' The function works by:
#' 1. **Calculating group counts**: The number of agents corresponding to each fraction is computed by multiplying 
#' the total number of agents in the group (`group_agent_count`) by each fraction.
#' 2. **Generating values**: The function replicates each target attribute value according to its count and then 
#' randomly shuffles these values to avoid any ordering bias.
#'
#' @return A vector of the target attribute values for all agents in the group, with the values distributed according to the input fractions.
#'
#' @examples
#' # Example group fractions
#' group_fractions <- data.frame(
#'   target_attribute = c("young", "middle-aged", "senior"),
#'   fraction = c(0.2, 0.5, 0.3)
#' )
#' 
#' # Generate values for a group of 10 agents
#' agent_values <- get_agent_values_from_fractions(group_fractions, 
#'                                                group_agent_count = 10, 
#'                                                target_attribute = "target_attribute")
#'
#' @export
get_agent_values_from_fractions <- function(group_fractions, group_agent_count, target_attribute = "target_attribute") {
  group_fractions$group_counts <- calculate_group_counts(group_fractions$fraction, group_agent_count)
  group_values <- c()  # Initialize an empty vector to store group values
  for (attr_value in group_fractions[group_fractions$group_counts > 0, target_attribute]) {
    series <- group_fractions[group_fractions[target_attribute] == attr_value, "group_counts"]
    group_values <- c(group_values, rep(attr_value, series))
  }
  group_values <- sample(group_values, replace = FALSE) # Shuffle the group values to avoid any bias
  return(group_values)
}

