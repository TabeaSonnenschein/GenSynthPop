% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mask.R
\name{get_group_mask}
\alias{get_group_mask}
\title{Generate a Logical Mask for Group-Based Filtering of Data Frames}
\usage{
get_group_mask(df, group_name, group_by)
}
\arguments{
\item{df}{A data frame to be filtered. The data frame should contain the columns specified in \code{group_by}.}

\item{group_name}{A vector of values that define the group. Each value corresponds to a specific grouping column in \code{group_by}.
The length of \code{group_name} must match the length of \code{group_by}.}

\item{group_by}{A character vector of column names in the data frame. These columns will be used to group
the data based on the corresponding values in \code{group_name}.}
}
\value{
A logical vector of the same length as the number of rows in the data frame. The vector contains
\code{TRUE} for rows that match the group defined by \code{group_name} and \code{group_by}, and \code{FALSE} for those that do not.
}
\description{
This function generates a logical mask (a vector of \code{TRUE}/\code{FALSE} values) for filtering rows in a data frame.
The mask identifies rows where the values in the specified \code{group_by} columns match the corresponding
values in \code{group_name}. The resulting mask can be used for subsetting or filtering the data frame.
}
\details{
The function iterates through each column specified in \code{group_by}, compares the values in that column
to the corresponding value in \code{group_name}, and returns a logical vector that can be used to filter the data frame.
If the data frame is empty, a warning message is printed.
}
\note{
This function assumes that the length of \code{group_name} and \code{group_by} are equal, and that each element in \code{group_name}
corresponds to a column in \code{group_by} by position.
}
\examples{
# Example usage with a data frame:
df <- data.frame(
  age_group = c("0-15", "15-25", "25-45"),
  sex = c("male", "female", "male"),
  count = c(100, 150, 200)
)

group_name <- c("15-25", "female")
group_by <- c("age_group", "sex")

mask <- get_group_mask(df, group_name, group_by)
df_filtered <- df[mask, ]

}
