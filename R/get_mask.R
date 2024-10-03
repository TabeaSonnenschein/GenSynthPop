
#' Generate a Logical Mask for Group-Based Filtering of Data Frames
#'
#' This function generates a logical mask (a vector of `TRUE`/`FALSE` values) for filtering rows in a data frame. 
#' The mask identifies rows where the values in the specified `group_by` columns match the corresponding 
#' values in `group_name`. The resulting mask can be used for subsetting or filtering the data frame.
#'
#' @param df A data frame to be filtered. The data frame should contain the columns specified in `group_by`.
#' @param group_name A vector of values that define the group. Each value corresponds to a specific grouping column in `group_by`.
#'     The length of `group_name` must match the length of `group_by`.
#' @param group_by A character vector of column names in the data frame. These columns will be used to group 
#'     the data based on the corresponding values in `group_name`.
#'
#' @return A logical vector of the same length as the number of rows in the data frame. The vector contains 
#'     `TRUE` for rows that match the group defined by `group_name` and `group_by`, and `FALSE` for those that do not.
#'
#' @details
#' The function iterates through each column specified in `group_by`, compares the values in that column 
#' to the corresponding value in `group_name`, and returns a logical vector that can be used to filter the data frame.
#' If the data frame is empty, a warning message is printed.
#'
#' @note
#' This function assumes that the length of `group_name` and `group_by` are equal, and that each element in `group_name`
#' corresponds to a column in `group_by` by position.
#'
#' @examples
#' # Example usage with a data frame:
#' df <- data.frame(
#'   age_group = c("0-15", "15-25", "25-45"),
#'   sex = c("male", "female", "male"),
#'   count = c(100, 150, 200)
#' )
#' 
#' group_name <- c("15-25", "female")
#' group_by <- c("age_group", "sex")
#' 
#' mask <- get_group_mask(df, group_name, group_by)
#' df_filtered <- df[mask, ]
#' 
#' @export
get_group_mask <- function(df, group_name, group_by) {
  if (length(df) == 0) {
    print("Warning: Data frame is empty. Likely due to mismatched variable names.")
  }
  mask <- rep(TRUE, nrow(df))
  
  for (i in seq_along(group_by)) {
    mask <- mask & df[[group_by[i]]] == group_name[i]
  }
  
  return(mask)
}
