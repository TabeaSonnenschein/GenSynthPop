% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{crosstabular_to_singleside_df}
\alias{crosstabular_to_singleside_df}
\title{Crosstabular Stratified Table to Single Sided Variable Combination - Counts Table}
\usage{
crosstabular_to_singleside_df(
  df,
  nrow_var,
  ncol_var,
  row_var_names,
  col_var_names
)
}
\arguments{
\item{df}{The stratified dataset that you want to process}

\item{nrow_var}{number of rows corresponding to variable names}

\item{ncol_var}{number of columns corresponding to variable names}

\item{row_var_names}{The variable names that you want to use to describe the variables in the rows. This will be used as variable column name for the output dataset.The order needs to be from top row to lower row variables.}

\item{col_var_names}{The variable names that you want to use to describe the variables in the columns. This will be used as variable column name for the output dataset. The order needs to be from left column to right column variables.}
}
\value{
Returns a processed stratefied dataframe with all variables as columns on the left. There will be all unique variable combinations. Additionally there will be the counts variable that shows how many people there are for the respective variable combination.
}
\description{
In order to facilitate the data preparation, this function can transform any stratified dataset structure in terms of number of row variables and column variable combinations into a dataset structure of all variable combinations as columns in the left and a single "counts" column on the right.
}
\details{
#Example Tables (random numbers)
\subsection{Turns a crosstabular stratified dataframe, such as this:}{
\subsection{*note that it can have any number of row variables (here 2) or column variables (here 1)}{\tabular{ccccccc}{
   age_groups \tab male \tab male \tab female \tab female \tab non-binary \tab non-binary \cr
    \tab employed \tab unemployed \tab employed \tab unemployed \tab employed \tab unemployed \cr
   A1 \tab 25 \tab 133 \tab 175 \tab 389 \tab 196 \tab 203 \cr
   A2 \tab 132 \tab 323 \tab 275 \tab 206 \tab 248 \tab 270 \cr
   A3 \tab 122 \tab 360 \tab 394 \tab 25 \tab 137 \tab 34 \cr
}

}

}

\subsection{Into a single side stratified dataframe such as this:}{\tabular{cccc}{
   age_group \tab sex \tab employ_status \tab counts \cr
   A1 \tab male \tab employed \tab 25 \cr
   A2 \tab male \tab employed \tab 132 \cr
   A3 \tab male \tab employed \tab 122 \cr
   A1 \tab male \tab unemployed \tab 133 \cr
   A2 \tab male \tab unemployed \tab 323 \cr
   A3 \tab male \tab unemployed \tab 360 \cr
   A1 \tab female \tab employed \tab 175 \cr
   A2 \tab female \tab employed \tab 275 \cr
   A3 \tab female \tab employed \tab 394 \cr
   A1 \tab female \tab unemployed \tab 389 \cr
   A2 \tab female \tab unemployed \tab 206 \cr
   A3 \tab female \tab unemployed \tab 25 \cr
   A1 \tab non-binary \tab employed \tab 196 \cr
   A2 \tab non-binary \tab employed \tab 248 \cr
   A3 \tab non-binary \tab employed \tab 137 \cr
   A1 \tab non-binary \tab unemployed \tab 203 \cr
   A2 \tab non-binary \tab unemployed \tab 270 \cr
   A3 \tab non-binary \tab unemployed \tab 34 \cr
}

}
}
\examples{
# some example mock data
# crosstabular stratified dataframe mock data
row1 = c("age_groups","male","male", "female", "female", "non-binary", "non-binary" )
row2 = c("", "employed", "unemployed", "employed", "unemployed", "employed", "unemployed")
row3 = c("A1", sample(1:400,6))
row4 = c("A2", sample(1:400,6))
row5 = c("A3", sample(1:400,6))
cross_tab_stratified_df = as.data.frame(rbind(row1, row2, row3, row4, row5))
print(cross_tab_stratified_df)

# function application
## the number of row variables are 2 (the sex and the employment status), while only age is a column variable, hence ncol_var = 1
singleside_df = crosstabular_to_singleside_df(df = cross_tab_stratified_df, nrow_var = 2, ncol_var = 1, row_var_names = c("sex", "employ_status"), col_var_names = c("age_group"))
print(singleside_df)

}
