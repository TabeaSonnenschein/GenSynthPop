library(dplyr)
library(mipfp)
library(tidyr)
library(stats)

#' Suppress Specific Probability Warning
#'
#' This function evaluates an expression while suppressing a specific warning message:
#' "Target not consistents - shifting to probabilities!". All other warnings are displayed
#' as usual. This is useful when using functions like `Ipfp` where such warnings may occur, 
#' but you want to ignore them without suppressing other warnings.
#'
#' @param expr An R expression to be evaluated. It can be any function or block of code that
#'     potentially generates warnings.
#'
#' @return The result of the evaluated expression. Warnings other than the specified one are
#'     displayed, and the execution continues as normal.
#'
#' @details The function uses `withCallingHandlers` to handle warnings during the evaluation
#'     of the expression. It checks if the warning message contains the text
#'     "Target not consistents - shifting to probabilities!" and suppresses it. Other warnings
#'     are not affected.
#'
#' @export
suppress_probability_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("Target not consistents - shifting to probabilities!", w$message)) {
        # Suppress the specific warning by returning NULL
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Iterative Proportional Fitting (IPF) for Contingency Tables
#'
#' This function performs iterative proportional fitting (IPF) on a contingency table to adjust the table's counts to match
#' specified marginal distributions. If no margins are provided, it simply returns the relevant subset of the table.
#' The function supports multi-dimensional contingency tables and handles cases where some margins are uncovered.
#'
#' @param df_contingency A data frame representing the contingency table, with one column for each variable and a `count` column.
#' @param group_name A vector of values corresponding to the group to filter (the subset defined by `group_by`).
#' @param group_by A character vector of column names used to filter the contingency table for the given `group_name`.
#' @param margins A list of data frames containing marginal distributions (e.g., age, sex) for IPF. 
#'     Each data frame must contain a column for the margin values and a `count` column.
#' @param margins_names A list of column names corresponding to the margins, i.e., the names of the variables for which
#'     marginal distributions are provided.
#' @param marginorder A list of vectors, where each vector contains the levels (order) of the corresponding margin variable.
#'     The order of each margin will be used when transforming the contingency table to a multi-dimensional array.
#' @param uncoveredcontingency A list of column names for variables in the contingency table that do not have 
#'     associated marginal distributions. These will be incorporated into the multi-dimensional array for IPF but are not constrained by margins.
#'
#' @return A data frame of the fitted contingency table, where the counts have been adjusted to match the provided margins.
#'     The resulting data frame will include all the original variables along with the adjusted `count` column.
#'
#' @details
#' If `margins` is provided, the function performs IPF on the contingency table to fit the table to the specified marginal distributions.
#' If no margins are provided (`margins = NULL`), it returns the subset of the table based on the values of `group_name` and `group_by`.
#' The function first transforms the contingency table into a multi-dimensional array, fits the table using IPF, and 
#' then transforms the adjusted array back into a data frame.
#'
#' The uncovered contingency variables (`uncoveredcontingency`) are included in the fitting process but not constrained by 
#' the marginal distributions.
#'
#' @note
#' This function relies on the `Ipfp` function from the `mipfp` package for the iterative proportional fitting procedure.
#' If marginal totals are inconsistent, IPF shifts to probabilities to correct them. A custom handler is used to suppress 
#' specific warnings related to this adjustment.
#'
#' @seealso
#' \code{\link[mipfp]{Ipfp}} for the core IPF algorithm.
#'
#' @examples
#' # Example usage:
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
#'                "neigh_123", "neigh_456", "neigh_456", "neigh_456", "neigh_456", "neigh_456"),
#'     age_group = c("0-15", "15-25", "25-45", "45-65", "65+", "0-15", 
#'                  "15-25", "25-45", "45-65", "65+"),
#'     count = c(50, 100, 150, 200, 75, 40, 90, 130, 180, 60)
#'   ),
#'   sex = data.frame(
#'     neighb_code = c("neigh_123", "neigh_123", "neigh_456", "neigh_456"),
#'     sex = c("male", "female", "male", "female"),
#'     count = c(250, 200, 220, 180)
#'   )
#' )
#' 
#' fitted_table <- ipf_fit_contingency_table(
#'   df_contingency, 
#'   group_name = "neigh_123",  # Neighborhood code for filtering
#'   group_by = c("neighb_code"),  # Column representing the neighborhood grouping
#'   margins = margins,
#'   margins_names = c("age_group", "sex"),
#'   marginorder = list(c("0-15", "15-25", "25-45", "45-65", "65+"), c("male", "female")),
#'   uncoveredcontingency = c('migrationbackground')
#' )
#' 
#' print(fitted_table)
#' 
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom stats aggregate as.formula setNames
#' @importFrom mipfp Ipfp
#' @export
ipf_fit_contingency_table <- function(df_contingency, group_name, group_by, margins = NULL, margins_names = NULL, marginorder = NULL, uncoveredcontingency = NULL) {
  if (is.null(margins)) {
    mask <- GenSynthPop::get_group_mask(df_contingency, group_name, group_by)
    return(df_contingency[mask, ])
  } else {
    aggregates <- list()
    for (index in seq_along(margins)) {
      margin_df <- margins[[index]]
      mask <- GenSynthPop::get_group_mask(margin_df, group_name, group_by)
      margin_df_masked <- margin_df[mask, ]
      margin_df_masked[[margins_names[[index]]]] <- factor(margin_df_masked[[margins_names[[index]]]], levels = marginorder[[index]])
      margin_df_masked <- margin_df_masked[order(margin_df_masked[[margins_names[[index]]]]),]
      marginaggregates <- unlist(list(margin_df_masked$count)) 
      if (sum(marginaggregates) == 0) {
        print(paste("Warning: No data for margin", margins_names[[index]], "in group", group_name, ", using contingency margins instead."))
        contingencymargins <- aggregate(as.formula(paste("count ~", margins_names[[index]])), data = df_contingency, FUN = sum)
        contingencymargins[[margins_names[[index]]]] <- factor(contingencymargins[[margins_names[[index]]]], levels = marginorder[[index]])
        contingencymargins <- contingencymargins[order(contingencymargins[[margins_names[[index]]]]),]
        marginaggregates <- unlist(list(contingencymargins$count))
      }
      aggregates[[margins_names[[index]]]] <- array(data = marginaggregates, dimnames = list(marginorder[[index]]))
    }
    # transforming the contingency table to a multi-dimensional array to fit the IPF function
  
    if (!is.null(uncoveredcontingency)) {
      marginorder <- c(marginorder, lapply(uncoveredcontingency, function(var) var = unique(df_contingency[[var]])))
      contvars <- c(margins_names, uncoveredcontingency)
    } else {
      contvars <- margins_names
    }
    total_dimensions <- lapply(marginorder, length)
    df_to_fit <- df_contingency %>%
      pivot_wider(names_from = contvars[2:length(contvars)],                # Creating wide columns based on second to last margin
        values_from = "count",                            # Filling with count values
        values_fill = list(count = 0)                  # Fill NAs with 0
      )
    df_to_fit <- as.data.frame(df_to_fit)
    data_values <- as.numeric(as.matrix(df_to_fit[, -1]))
    multi_array <- array(
      data = data_values,     
      dim = total_dimensions,
      dimnames = marginorder
      )
    margindimensions <- seq_along(length(contvars))

    # Call the Ipfp function to perform the iterative proportional fitting
    ipf_results <- GenSynthPop::suppress_probability_warning(Ipfp(
      seed = multi_array,                    # Initial seed (multi-dimensional array)
      target.list = margindimensions,      # List of target dimensions
      target.data = aggregates,      # List of target data (marginal counts)
      # print = TRUE,                   # Optionally print progress
      # na.target = TRUE                # Set to TRUE if NA values are allowed in targets
    ))
    fitted_array = ipf_results$x.hat
    dim_names <- expand.grid(
      setNames(lapply(seq_along(contvars), function(i) dimnames(fitted_array)[[i]]), contvars)
    )
    counts <- as.vector(fitted_array)
    df_fitted <- cbind(dim_names, count = counts)
  return(as.data.frame(df_fitted))
  }
}
