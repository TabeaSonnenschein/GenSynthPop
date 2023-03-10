
####### Generating an agent dataframe of the population size and assigning a unique ID

#' @title Generating an agent dataframe of the population size and assigning a unique ID
#' @description The function creates a new agent dataframe of an indicated population size to start the building of the synthetic agent population.  Each row in the dataframe represents an individual and is assigned a unique ID. This is basically the agent dataframe scelaton that will be iteratively extended with attribute data based on conditional and marginal distributions of demographics.
#' @param pop_size The size of the population for the are for which a synthetic agent population should be created.
#'
#' @return Returns a new dataframe of len(pop_size) with unique agent_ids.
#' @export
#' @details
#' # Example output table
#' | agent_ID |
#' |:---:|
#' | Agent_1 |
#' | Agent_2 |
#' | Agent_3 |
#' | Agent_4 |
#' | Agent_5 |
#' | Agent_6 |
#' (...)
#'
#' ### continues until population size. It is just the initiation of the agent_df, as a scelaton to be filled with representative attributes.
#' @md
#' @examples
#' ## Let us say we want to initiate a synthetic population of Amsterdam,
#' ## which has a population size of 910 146 as of 2022
#'
#' agent_df = gen_agent_df(910146)
#' print(agent_df)
#'
#' ## Because sometimes the sum of total census counts differs to the official overall city population count,
#' ## it makes sense to use the sum of the population of the first attribute you will add (for example age).
#'
gen_agent_df = function(pop_size){
  agent_ID = paste("Agent_",1:pop_size, sep="")
  agent_df = as.data.frame(agent_ID)
  return(agent_df)
}



#' @title Populating the agent_df with age_group and neigh_ID attributes distributed like a given neighborhood marginal distribution
#' @description This function is the second function to be used after gen_agent_df and adds the first attributes of age_group and neigh_id, such that it satisfies the distribution of the given neighborhood age_group marginal distributions. The location and age_group attributes are hence representative and can be used as conditional variables for the next attributes (e.g. sex, then migration-background) in the following steps. From now on the multi-variable joint distribution of variables can be taken into account and combined with the neighborhood totals (marginal distributions) in order to receive a population that is spatially representative and in which the covariation of multiple attributes are representative.
#' @param neigh_df a dataframe of neighborhoods in the rows, indentified by an neigh-id column, and the marginal distribution across age-groups in the other columns.
#' @param agent_df the initiated agent_df generated by gen_agent_df
#' @param neigh_id the columnname of the column that indicates the neighborhood IDs
#' @param age_colnames a list of columnnames of the age_group classes
#'
#' @return returns the agent_df with the new attribute of age_group and neigh_ID. It also shuffles the agent_df's roworder in order to not introduce bias in subsequent steps
#' @export
#'
#' @examples
#' #mock data
#' #agent_df mock data
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' colnames(agent_df) = "agent_ID"
#'
#' # neigh_df mock data
#' # the total population (sum of all agegroups across all neighborhoods) has to be equal to the length of the agent_df
#' neigh_df = as.data.frame(1:10)
#' colnames(neigh_df) = c("neigh_ID")
#' # we call our age_groups A1, A2, A3, but it can be any number of classes with any name
#' neigh_df$A1 = sample(x=1:20, size=10)
#' neigh_df$A2 = sample(x=1:20, size=10)
#' neigh_df$A3 = 50- (neigh_df$A1 + neigh_df$A2)
#'
#' #function application
#' agent_df= distr_agent_neigh_age_group(neigh_df = neigh_df, agent_df = agent_df, neigh_id = "neigh_ID", age_colnames = c("A1", "A2", "A3"))
#' print(agent_df)
#'
distr_agent_neigh_age_group = function(neigh_df, agent_df, neigh_id, age_colnames){
  agent_df$age_group = ""
  agent_df[,c(neigh_id)] = ""
  n = 0 # indice of agent population that is populated with attributes
  for(i in 1:nrow(neigh_df)){   # neighborhood indice
    for(g in age_colnames) {            # agegroup indice
      nr_people = neigh_df[i, c(g)]
      agent_df$age_group[(n+1):(n+nr_people)] = g
      agent_df[(n+1):(n+nr_people), c(neigh_id)] = neigh_df[i, c(neigh_id)]
      n = n + nr_people
    }
  }
  random_seq = sample(nrow(agent_df))
  agent_df = agent_df[random_seq,]
  return(agent_df)
}

