% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_agent_values.R
\name{calculate_group_counts}
\alias{calculate_group_counts}
\title{Calculate Adjusted Group Counts Based on Fractions}
\usage{
calculate_group_counts(df_fractions, n_agents_total)
}
\arguments{
\item{df_fractions}{A numeric vector representing the fractional distribution of agents for each group (the result of \code{ConditionalAttributeAdder$calculate_fractions}).
Each value represents the proportion of agents that should belong to a given group.}

\item{n_agents_total}{An integer representing the total number of synthetic agents in the population.}
}
\value{
A numeric vector representing the adjusted counts of synthetic agents for each group, ensuring the total
matches \code{n_agents_total}.
}
\description{
This function calculates the number of synthetic agents for each group based on the fractional distribution
of the target attribute. It ensures the total count of agents matches the specified total number of agents
in the synthetic population.
}
\details{
The function initially calculates the agent counts for each group by multiplying the fractional distribution
(\code{df_fractions}) by the total number of agents (\code{n_agents_total}). Since rounding these counts may result in a mismatch between
the sum of the counts and the total number of agents, the function iteratively adjusts the counts:
\itemize{
\item It identifies the group with the largest fractional difference between the computed counts and the expected
fractional value.
\item It increments or decrements the count of that group until the sum of all counts equals \code{n_agents_total}.
}
}
\examples{
# Example: Adjusting counts based on fractional distribution
df_fractions <- c(0.3, 0.5, 0.2)
n_agents_total <- 10

adjusted_counts <- calculate_group_counts(df_fractions, n_agents_total)
# The result is a vector of adjusted counts summing to 10.

}
