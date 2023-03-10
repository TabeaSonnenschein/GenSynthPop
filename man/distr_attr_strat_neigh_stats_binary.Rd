% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distr_attributes.R
\name{distr_attr_strat_neigh_stats_binary}
\alias{distr_attr_strat_neigh_stats_binary}
\title{Distributing attributes across agent population based on conditional proabilities and neighborhood totals for binary attributes}
\usage{
distr_attr_strat_neigh_stats_binary(
  agent_df,
  neigh_df,
  neigh_ID,
  variable,
  list_var_classes_neigh_df,
  list_agent_propens,
  list_class_names,
  agent_exclude
)
}
\arguments{
\item{agent_df}{Dataframe of the unique agents with their attributes}

\item{neigh_df}{Dataframe of aggregate statistical data per neighborhood, specifically the total population per neighborhood and the counts per variable class.}

\item{neigh_ID}{the columnname that indicates the neighborhood ID in both the agent and neighborhood dataframe}

\item{variable}{the new variable that we want to add based on the stratified and neighborhood marginal distributions}

\item{list_var_classes_neigh_df}{a list of the column names in the Neighborhood dataset with the classes of the variable that will be modeled (e.g. c("Men", "Women", "Non-Binary"), which are the classes of sex).}

\item{list_agent_propens}{A list of the columns in the agent dataset that contain the propensities for the classes of the variable based on the other agents conditional attributes. This list has to be in the same order as the list_var_classes_neigh_df. Since we are dealing with a binary attribute, we can leave out the last propensity as it is 1 minus the other propensities.}

\item{list_class_names}{The list_class_names is optional and contains the values that the new created agent variable should have for the different variable classes. It has to be in the same order and of the same length as the list_var_classes_neigh_df. If left empty, the list_var_classes_neigh_df will become the default values for the classes.}

\item{agent_exclude}{an optional variable containing one or multiple variable names of the agent dataset on which basis agents should be excluded from the attribute assignment. These variables whould be binary columns, with 1 indicating that it should be excluded (e.g. "is_child" could be entered if agents with is_child = 1 should be exluded for the new attribute).}
}
\value{
returns the agent df with the newly distributed attribute and the random score, which is used to see if it is below or above the propensity. Moreover, the agent_df is shuffled to make sure no bias is caused by order
}
\description{
This function distributes attribute classes in the agent population based on the conditional propensities and the neighborhood statistics. It uses random scores and sees if the score is below the propensity, in which case the first attribute is assigned, if above the second attribute is assigned. Consequently, the attribute distribution in each neighborhood will be compared to the respective neighborhood's totals. If the distribution is not equal to the neighborhood distributions or has an absolute difference more than one person or 2\%, then first random scores will be assigned again, seeing if it results in a fitting distribution. If that does not work then the propensities of the number of agents equal to the rounded absolute difference of distribution in the neighborhood will be adjusted in the direction of the difference. This happens iteratively with assigning the attribute by setting the propensity in relation to the random score until the desired distribution is achieved.
}
\examples{
## generating some example mock data (based on random numbers)
### agent dataframe mock data ###
agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
agent_df$neigh_ID = sample(1:10, size =nrow(agent_df), replace = T)
agent_df$age_group = c("A1", "A2", "A3", "A4")[sample(1:4, size = nrow(agent_df), replace = T)]
agent_df$sex = c("female", "male", "non-binary")[sample(1:3, size = nrow(agent_df), replace = T)]
# the propensity variable this should be the ouput of the "calc_propens_agents" function or the "strat_prop_from_sep_cond_var" if no multi-joint distributions of conditional variable are available and only single conditional variable joint distributions.
# for illustration purposes it is only a random probability here
agent_df$prop_employed = sample(x= seq(from= 0, to = 1, by= 0.01), size = nrow(agent_df), replace = T)
colnames(agent_df) = c("agent_id", "neigh_ID", "age_group", "sex", "prop_employed")
print(agent_df)

### neighborhood totals dataframe ###
# since agents have been generated to represent the number of people in a previous step, here the neighborhood totals have to be equal to the count of the number of agents in the neighborhood
# if it is not exactly equal that is fine, the algorithm can handle that (it is common that in census data the population totals can vary depending on the variable)
neigh_df = as.data.frame(1:10)
colnames(neigh_df) = c("neigh_ID")
neigh_df$total = NA
neigh_df$employed = NA
for(id in neigh_df$neigh_ID){
   neigh_df$total[id] = length(which(agent_df$neigh_ID == id))
   neigh_df$employed[id] = sample(1:neigh_df$total[id], size = 1)
}
neigh_df$unemployed = neigh_df$total - neigh_df$employed
print(neigh_df)

## applying the function (without optional params)
agent_df_new = distr_attr_strat_neigh_stats_binary(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "unemployed"), list_agent_propens = c("prop_employed"))
print(agent_df_new)

## applying the function (with optional params)
# say we want to exclude people from age_group A1, we need to create a binary variable that indicates if an agent belongs to that group
agent_df$is_child = 0
agent_df$is_child[agent_df$age_group == "A1"] = 1
# additionally, say we want to call the employ_status attributes differently then "employed" and "unemployed" as given by the dataframe, for example c("has_work_income", "no_work_income")
# then we would use the function as such:
agent_df_new = distr_attr_strat_neigh_stats_binary(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "unemployed"), list_agent_propens = c("prop_employed"), list_class_names = c("has_work_income", "no_work_income"), agent_exclude = "is_child" )
print(agent_df_new)

}
