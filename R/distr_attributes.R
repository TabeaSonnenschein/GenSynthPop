
####### distributing attributes across agent population based on conditional proabilities and neighborhood totals

### EXPLANATION
# This function distributes attribute classes in the agent population based on the conditional propensities and the neighborhood statistics
# agent_df = Dataframe of the unique agents with their attributes
# neigh_df = Dataframe of aggregate statistical data per neighborhood, specifically the total population
# per neighborhood and the counts per variable class.
# variable = the new variable that we want to add based on the stratified and neighborhood marginal distributions
# list_var_classes_neigh_df = a list of the column names in the Neighborhood dataset with
# the classes of the variable that will be modeled (e.g. c("Men", "Women", "Non-Binary"), which are the classes of sex).
# list_agent_propens = A list of the columns in the agent dataset that contain the propensities for the classes of the variable based on the other agents conditional attributes.
# This list has to be in the same order as the list_var_classes_neigh_df, but can leave out the last propensity as it is 1 minus the other propensities.
# The list_class_names is optional and contains the values that the new created agent variable should have for the different variable classes.
# It has to be in the same order and of the same length as the list_var_classes_neigh_df. If left empty, the list_var_classes_neigh_df will become the default values for the classes.
# agent_exclude = an optional variable containing one or multiple variable names of the agent dataset on which basis agents should be excluded from the attribute assignment
# if that variable(s) is/are 1, then the agents will be excluded

## distr_attr_strat_neigh_stats_binary is for binary attributes

#' @title Distributing attributes across agent population based on conditional proabilities and neighborhood totals for binary attributes
#' @description  This function distributes attribute classes in the agent population based on the conditional propensities and the neighborhood statistics. It uses random scores and sees if the score is below the propensity, in which case the first attribute is assigned, if above the second attribute is assigned. Consequently, the attribute distribution in each neighborhood will be compared to the respective neighborhood's totals. If the distribution is not equal to the neighborhood distributions or has an absolute difference more than one person or 2%, then first random scores will be assigned again, seeing if it results in a fitting distribution. If that does not work then the propensities of the number of agents equal to the rounded absolute difference of distribution in the neighborhood will be adjusted in the direction of the difference. This happens iteratively with assigning the attribute by setting the propensity in relation to the random score until the desired distribution is achieved.
#' @param agent_df Dataframe of the unique agents with their attributes
#' @param neigh_df Dataframe of aggregate statistical data per neighborhood, specifically the total population per neighborhood and the counts per variable class.
#' @param neigh_ID the columnname that indicates the neighborhood ID in both the agent and neighborhood dataframe
#' @param variable the new variable that we want to add based on the stratified and neighborhood marginal distributions
#' @param list_var_classes_neigh_df a list of the column names in the Neighborhood dataset with the classes of the variable that will be modeled (e.g. c("Men", "Women", "Non-Binary"), which are the classes of sex).
#' @param list_agent_propens A list of the columns in the agent dataset that contain the propensities for the classes of the variable based on the other agents conditional attributes. This list has to be in the same order as the list_var_classes_neigh_df. Since we are dealing with a binary attribute, we can leave out the last propensity as it is 1 minus the other propensities.
#' @param list_class_names The list_class_names is optional and contains the values that the new created agent variable should have for the different variable classes. It has to be in the same order and of the same length as the list_var_classes_neigh_df. If left empty, the list_var_classes_neigh_df will become the default values for the classes.
#' @param agent_exclude an optional variable containing one or multiple variable names of the agent dataset on which basis agents should be excluded from the attribute assignment. These variables whould be binary columns, with 1 indicating that it should be excluded (e.g. "is_child" could be entered if agents with is_child = 1 should be exluded for the new attribute).
#'
#' @return returns the agent df with the newly distributed attribute and the random score, which is used to see if it is below or above the propensity. Moreover, the agent_df is shuffled to make sure no bias is caused by order
#' @export
#'
#' @examples
#' ## generating some example mock data (based on random numbers)
#' ### agent dataframe mock data ###
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' agent_df$neigh_ID = sample(1:10, size =nrow(agent_df), replace = T)
#' agent_df$age_group = c("A1", "A2", "A3", "A4")[sample(1:4, size = nrow(agent_df), replace = T)]
#' agent_df$sex = c("female", "male", "non-binary")[sample(1:3, size = nrow(agent_df), replace = T)]
#' # the propensity variable this should be the ouput of the "calc_propens_agents" function or the "strat_prop_from_sep_cond_var" if no multi-joint distributions of conditional variable are available and only single conditional variable joint distributions.
#' # for illustration purposes it is only a random probability here
#' agent_df$prop_employed = sample(x= seq(from= 0, to = 1, by= 0.01), size = nrow(agent_df), replace = T)
#' colnames(agent_df) = c("agent_id", "neigh_ID", "age_group", "sex", "prop_employed")
#' print(agent_df)
#'
#' ### neighborhood totals dataframe ###
#' # since agents have been generated to represent the number of people in a previous step, here the neighborhood totals have to be equal to the count of the number of agents in the neighborhood
#' # if it is not exactly equal that is fine, the algorithm can handle that (it is common that in census data the population totals can vary depending on the variable)
#' neigh_df = as.data.frame(1:10)
#' colnames(neigh_df) = c("neigh_ID")
#' neigh_df$total = NA
#' neigh_df$employed = NA
#' for(id in neigh_df$neigh_ID){
#'    neigh_df$total[id] = length(which(agent_df$neigh_ID == id))
#'    neigh_df$employed[id] = sample(1:neigh_df$total[id], size = 1)
#' }
#' neigh_df$unemployed = neigh_df$total - neigh_df$employed
#' print(neigh_df)
#'
#' ## applying the function (without optional params)
#' agent_df_new = distr_attr_strat_neigh_stats_binary(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "unemployed"), list_agent_propens = c("prop_employed"))
#' print(agent_df_new)
#'
#' ## applying the function (with optional params)
#' # say we want to exclude people from age_group A1, we need to create a binary variable that indicates if an agent belongs to that group
#' agent_df$is_child = 0
#' agent_df$is_child[agent_df$age_group == "A1"] = 1
#' # additionally, say we want to call the employ_status attributes differently then "employed" and "unemployed" as given by the dataframe, for example c("has_work_income", "no_work_income")
#' # then we would use the function as such:
#' agent_df_new = distr_attr_strat_neigh_stats_binary(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "unemployed"), list_agent_propens = c("prop_employed"), list_class_names = c("has_work_income", "no_work_income"), agent_exclude = "is_child" )
#' print(agent_df_new)
#'
distr_attr_strat_neigh_stats_binary = function(agent_df, neigh_df, neigh_ID, variable, list_var_classes_neigh_df, list_agent_propens, list_class_names, agent_exclude){
  print(Sys.time())
  agent_df[, c(variable, "random_scores")] = 0
  if(missing(list_class_names)){
    list_class_names = list_var_classes_neigh_df
  }
  if(!missing(agent_exclude)){
    agent_df[, c("excluded")] = 0
    for(i in 1:length(agent_exclude)){
      agent_df[which(agent_df[, c(agent_exclude[i])] == 1) , c("excluded")] =  1
    }
  }
  for (i in 1:nrow(neigh_df)){
    if(!missing(agent_exclude)){
      x = which(agent_df[, c(neigh_ID)] == neigh_df[i, c(neigh_ID)] & agent_df[, c("excluded")] != 1)
    }
    else{
      x = which(agent_df[, c(neigh_ID)] == neigh_df[i, c(neigh_ID)])
    }
    tot__var_class_neigh = neigh_df[i, list_var_classes_neigh_df]
    agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
    fitness = 0
    if(length(x) != 0){
      while(fitness == 0){
        if(length(list_var_classes_neigh_df)== 2){
          agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
          agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
          if(length(which(agent_df[x, c(variable)] == list_class_names[1])) >= tot__var_class_neigh[1] & length(which(agent_df[x, c(variable)] == list_class_names[2])) >= tot__var_class_neigh[2]){
            fitness = 1
          }
          else if(sum(tot__var_class_neigh) <= length(x)){
            agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
            agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
            agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
            if(length(which(agent_df[x, c(variable)] == list_class_names[1])) >= tot__var_class_neigh[1] & length(which(agent_df[x, c(variable)] == list_class_names[2])) >= tot__var_class_neigh[2]){
              fitness = 1
            }
            else if(length(which(agent_df[x, c(variable)] == list_class_names[1])) < tot__var_class_neigh[1]){
              abs_diff = as.integer(as.numeric(tot__var_class_neigh[1]) - length(which(agent_df[x, c(variable)] == list_class_names[1])))
              if(abs_diff <= 1|is.na(abs_diff)){
                fitness = 1
              }
              else{
                class = which(agent_df[x, c(variable)] == list_class_names[2])[1:as.numeric(abs_diff)]
                class = class[!is.na(class)]
                if(length(class) == 0){
                  fitness = 1
                }
                agent_df[x[class], c(list_agent_propens[1])] = agent_df[x[class], c(list_agent_propens[1])] + 0.5
              }
            }
            else if(length(which(agent_df[x, c(variable)] == list_class_names[2])) < tot__var_class_neigh[2]){
              abs_diff = as.integer(as.numeric(tot__var_class_neigh[2]) - length(which(agent_df[x, c(variable)] == list_class_names[2])))
              if(abs_diff <= 1|is.na(abs_diff)){
                fitness = 1
              }
              else{
                class = which(agent_df[x, c(variable)] == list_class_names[1])[1:as.numeric(abs_diff)]
                class = class[!is.na(class)]
                if(length(class) == 0){
                  fitness = 1
                }
                agent_df[x[class], c(list_agent_propens[1])] = agent_df[x[class], c(list_agent_propens[1])] - 0.5
              }
            }
          }
          else if(length(x) < 10){
            fitness = 1
          }
          else if(sum(tot__var_class_neigh) > length(x)){
            percent_diff1 = length(which(agent_df[x, c(variable)] == list_class_names[1]))/as.numeric(tot__var_class_neigh[1])
            percent_diff2 = length(which(agent_df[x, c(variable)] == list_class_names[2]))/as.numeric(tot__var_class_neigh[2])
            percent_diff_diff = percent_diff1 - percent_diff2
            if(abs(percent_diff_diff) < 0.02|is.na(percent_diff_diff)){
              fitness = 1
            }
            else{
              if( percent_diff_diff < 0){
                abs_diff = as.integer((((as.numeric(abs(percent_diff_diff)))/2) * as.numeric(tot__var_class_neigh[1]))/3)
                if(abs_diff <= 1|is.na(abs_diff)){
                  fitness = 1
                }
                else{
                  class = which(agent_df[x, c(variable)] == list_class_names[2])[1:abs_diff]
                  class = class[!is.na(class)]
                  if(length(class) == 0){
                    fitness = 1
                  }
                  agent_df[x[class], c(list_agent_propens[1])] = agent_df[x[class], c(list_agent_propens[1])] + 0.3
                }
              }
              else{
                abs_diff = as.integer((((as.numeric(abs(percent_diff_diff)))/2) * as.numeric(tot__var_class_neigh[2]))/3)
                if(abs_diff <= 1|is.na(abs_diff)){
                  fitness = 1
                }
                else{
                  class = which(agent_df[x, c(variable)] == list_class_names[1])[1:abs_diff]
                  class = class[!is.na(class)]
                  if(length(class) == 0){
                    fitness = 1
                  }
                  agent_df[x[class], c(list_agent_propens[1])] = agent_df[x[class], c(list_agent_propens[1])] - 0.3
                }
              }
            }
          }
        }
      }
    }
    print(paste("neighborhood:", i))
  }
  random_seq = sample(nrow(agent_df))
  agent_df = agent_df[random_seq,]
  print(Sys.time())
  return(agent_df)
}

#' @title Distributing attributes across agent population based on conditional proabilities and neighborhood totals for attributes with 3 or more classes
#' @description  This function distributes attribute classes in the agent population based on the conditional propensities and the neighborhood statistics. It uses random scores and sees if the score is below the propensity, in which case the first attribute is assigned, if above the second attribute is assigned. Consequently, the attribute distribution in each neighborhood will be compared to the respective neighborhood's totals. If the distribution is not equal to the neighborhood distributions or has an absolute difference more than one person or 2%, then first random scores will be assigned again, seeing if it results in a fitting distribution. If that does not work then the propensities of the number of agents equal to the rounded absolute difference of distribution in the neighborhood will be adjusted in the direction of the difference. This happens iteratively with assigning the attribute by setting the propensity in relation to the random score until the desired distribution is achieved.
#
#' @param agent_df Dataframe of the unique agents with their attributes
#' @param neigh_df Dataframe of aggregate statistical data per neighborhood, specifically the total population per neighborhood and the counts per variable class.
#' @param neigh_ID the columnname that indicates the neighborhood ID in both the agent and neighborhood dataframe
#' @param variable the new variable that we want to add based on the stratified and neighborhood marginal distributions
#' @param list_var_classes_neigh_df a list of the column names in the Neighborhood dataset with the classes of the variable that will be modeled (e.g. c("Men", "Women", "Non-Binary"), which are the classes of sex).
#' @param list_agent_propens A list of the columns in the agent dataset that contain the propensities for the classes of the variable based on the other agents conditional attributes. This list has to be in the same order as the list_var_classes_neigh_df. For the 3 or more attribute function, all propensities for all variables have to be included.
#' @param list_class_names The list_class_names is optional and contains the values that the new created agent variable should have for the different variable classes. It has to be in the same order and of the same length as the list_var_classes_neigh_df. If left empty, the list_var_classes_neigh_df will become the default values for the classes.
#' @param agent_exclude an optional variable containing one or multiple variable names of the agent dataset on which basis agents should be excluded from the attribute assignment. These variables whould be binary columns, with 1 indicating that it should be excluded (e.g. "is_child" could be entered if agents with is_child = 1 should be exluded for the new attribute).
#'
#' @return returns the agent df with the newly distributed attribute and the random score, which is used to see if it is below or above the propensity.
#' @export
#'
#' @examples
#' ## generating some example mock data (based on random numbers)
#' ### agent dataframe mock data ###
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' agent_df$neigh_ID = sample(1:10, size =nrow(agent_df), replace = T)
#' agent_df$age_group = c("A1", "A2", "A3", "A4")[sample(1:4, size = nrow(agent_df), replace = T)]
#' agent_df$sex = c("female", "male", "non-binary")[sample(1:3, size = nrow(agent_df), replace = T)]
#' # the propensity variable this should be the ouput of the "calc_propens_agents" function or the "strat_prop_from_sep_cond_var" if no multi-joint distributions of conditional variable are available and only single conditional variable joint distributions.
#' # for illustration purposes it is only a random probability here
#' # since this function is for 3 or more attributes of variables we conceptualize the employ_status variable to be c("employed", "pensioned", "unemployed").
#' agent_df$prop_employed = sample(x= seq(from= 0, to = 0.75, by= 0.01), size = nrow(agent_df), replace = T)
#' agent_df$prop_pension = sample(x= seq(from= 0, to = 0.25, by= 0.01), size = nrow(agent_df), replace = T)
#' agent_df$prop_unemployed = 1-(agent_df$prop_employed + agent_df$prop_pension)
#' colnames(agent_df) = c("agent_id", "neigh_ID", "age_group", "sex", "prop_employed", "prop_pension")
#' print(agent_df)
#'
#' ### neighborhood totals dataframe ###
#' # since agents have been generated to represent the number of people in a previous step, here the neighborhood totals have to be equal to the count of the number of agents in the neighborhood
#' # if it is not exactly equal that is fine, the algorithm can handle that (it is common that in census data the population totals can vary depending on the variable)
#' neigh_df = as.data.frame(1:10)
#' colnames(neigh_df) = c("neigh_ID")
#' neigh_df$total = NA
#' neigh_df$employed = NA
#' for(id in neigh_df$neigh_ID){
#'    neigh_df$total[id] = length(which(agent_df$neigh_ID == id))
#'    neigh_df$employed[id] = sample(1:(neigh_df$total[id]/1.3), size = 1)
#'    neigh_df$pensioned[id] = sample(1:(neigh_df$total[id]-neigh_df$employed[id]), size = 1)
#' }
#' neigh_df$unemployed = neigh_df$total - neigh_df$employed - neigh_df$pensioned
#' print(neigh_df)
#'
#' ## applying the function (without optional params)
#' agent_df_new = distr_attr_strat_neigh_stats_3plus(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "pensioned", "unemployed"), list_agent_propens = c("prop_employed", "prop_pension", "prop_unemployed"))
#' print(agent_df_new)
#'
#' ## applying the function (with optional params)
#' # say we want to exclude people from age_group A1, we need to create a binary variable that indicates if an agent belongs to that group
#' agent_df$is_child = 0
#' agent_df$is_child[agent_df$age_group == "A1"] = 1
#' # additionally, say we want to call the employ_status attributes differently then "employed" and "unemployed" as given by the dataframe, for example c("has_work_income", "no_work_income")
#' # then we would use the function as such:
#' agent_df_new = distr_attr_strat_neigh_stats_3plus(agent_df = agent_df, neigh_df = neigh_df, neigh_ID = "neigh_ID", variable = "employ_status", list_var_classes_neigh_df = c("employed", "pensioned", "unemployed"), list_agent_propens = c("prop_employed", "prop_pension", "prop_unemployed"), list_class_names = c("has_work_income", "has_pension_income", "no_work_income"), agent_exclude = "is_child" )
#' print(agent_df_new)
#'
distr_attr_strat_neigh_stats_3plus = function(agent_df, neigh_df, neigh_ID, variable, list_var_classes_neigh_df, list_agent_propens, list_class_names, agent_exclude){
  print(Sys.time())
  agent_df[, c(variable, "random_scores")] = 0
  if(missing(list_class_names)){
    list_class_names = list_var_classes_neigh_df
  }
  if(!missing(agent_exclude)){
    agent_df[, c("excluded")] = 0
    for(i in 1:length(agent_exclude)){
      agent_df[which(agent_df[, c(agent_exclude[i])] == 1) , c("excluded")] =  1
    }
  }
  lvar = length(list_var_classes_neigh_df)
  for (i in 1:nrow(neigh_df)){
    if(!missing(agent_exclude)){
      x = which(agent_df[, c(neigh_ID)] == neigh_df[i, c(neigh_ID)] & agent_df[, c("excluded")] != 1)
    }
    else{
      x = which(agent_df[, c(neigh_ID)] == neigh_df[i, c(neigh_ID)])
    }
    tot__var_class_neigh = neigh_df[i, list_var_classes_neigh_df]
    agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
    fitness = 0
    if(length(x) != 0){
      while(fitness == 0){
        if(lvar > 2){
          agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
          agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:2])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
          if(lvar >3){
            for(n in 3:(lvar -1)){
              agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(n-1)])]) < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:n])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[n]
            }
          }
          agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(lvar -1)])]) < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[lvar]
          if((length(which(agent_df[x, c(variable)] == list_class_names[1])) >= tot__var_class_neigh[1] & length(which(agent_df[x, c(variable)] == list_class_names[2])) >= tot__var_class_neigh[2] & length(which(agent_df[x, c(variable)] == list_class_names[3])) >= tot__var_class_neigh[3]) | sum(tot__var_class_neigh) == 0 | is.na(sum(tot__var_class_neigh))){
            fitness = 1
          }
          else if(sum(tot__var_class_neigh) <= length(x)){
            agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
            agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
            agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:2])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
            if(lvar>3){
              for(n in 3:(lvar-1)){
                agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(n-1)])]) < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:n])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[n]
              }
            }
            agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(lvar -1)])]) < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[lvar]
            if(length(which(agent_df[x, c(variable)] == list_class_names[1])) >= tot__var_class_neigh[1] & length(which(agent_df[x, c(variable)] == list_class_names[2])) >= tot__var_class_neigh[2] & length(which(agent_df[x, c(variable)] == list_class_names[3])) >= tot__var_class_neigh[3]){
              fitness = 1
            }
            else{
              underrepresented = c()
              for(n in 1:(lvar)){
                if(length(which(agent_df[x, c(variable)] == list_class_names[n])) < tot__var_class_neigh[n]){
                  underrepresented = append(underrepresented, 1)
                }
                else{
                  underrepresented = append(underrepresented, 0)
                }
              }
              for(n in 1:(lvar)){
                if(underrepresented[n] == 1){
                  abs_diff = (tot__var_class_neigh[n] - length(which(agent_df[x, c(variable)] == list_class_names[n])))
                  class = which(agent_df[x, c(variable)] %in% list_class_names[which(underrepresented == 0)])[1:as.numeric(abs_diff)]
                  class = class[!is.na(class)]
                  agent_df[x[class], c(variable)] = list_class_names[n]
                  agent_df[x[class], c(list_agent_propens[n])] = agent_df[x[class], c(list_agent_propens[n])] + 0.5
                  agent_df[x[class], c(list_agent_propens[which(underrepresented == 0)])] = agent_df[x[class], c(list_agent_propens[which(underrepresented == 0)])] - (0.5/length(which(underrepresented == 0)))
                }
              }
              if(length(which(agent_df[x, c(variable)] == list_class_names[1])) >= tot__var_class_neigh[1] & length(which(agent_df[x, c(variable)] == list_class_names[2])) >= tot__var_class_neigh[2] & length(which(agent_df[x, c(variable)] == list_class_names[3])) >= tot__var_class_neigh[3]){
                fitness = 1
              }
            }
          }
          else if(sum(tot__var_class_neigh) > length(x)){
            percent_diff = c()
            for(n in 1:(lvar)){
              if(tot__var_class_neigh[n] != 0){
                percent_diff = append(percent_diff, length(which(agent_df[x, c(variable)] == list_class_names[n]))/as.numeric(tot__var_class_neigh[n]))
              }
              else{
                percent_diff = append(percent_diff, NA)
              }
            }
            print(percent_diff)
            percent_diff_diff = as.data.frame(matrix(data = NA, nrow = length(list_var_classes_neigh_df), ncol = length(list_var_classes_neigh_df)))
            for(n in 1:(lvar)){
              for(k in 1:(lvar)){
                percent_diff_diff[n, k] = percent_diff[n] - percent_diff[k]
              }
            }
            if(all(abs(na.omit(percent_diff_diff)) < 0.03)){
              fitness = 1
            }
            else{
              tot_abs_diff = c()
              for(n in 1:(lvar)){
                m = which(percent_diff_diff[n,] < (-0.03))
                if(length(m)> 0){
                  abs_diff = as.numeric(((sum(as.numeric(abs(percent_diff_diff[n, m])))/length(m)) * as.numeric(tot__var_class_neigh[n])))
                  tot_abs_diff = append(tot_abs_diff, abs_diff )
                  for(l in m){
                    class = which(agent_df[x, c(variable)] %in% list_class_names[l])[1:as.integer((abs_diff/3)*(as.numeric(tot__var_class_neigh[l])/sum(as.numeric(tot__var_class_neigh[m]))))]
                    class = class[!is.na(class)]
                    agent_df[x[class], c(variable)] = list_class_names[n]
                    agent_df[x[class], c(list_agent_propens[n])] = agent_df[x[class], c(list_agent_propens[n])] + 0.3
                    agent_df[x[class], c(list_agent_propens[m])] = agent_df[x[class], c(list_agent_propens[m])] - (0.3/length(m))
                  }
                }
              }
              if(all(abs(tot_abs_diff) < 3)){
                fitness = 1
              }
              else{
                percent_diff = c()
                for(n in 1:(lvar)){
                  if(tot__var_class_neigh[n] != 0){
                    percent_diff = append(percent_diff, length(which(agent_df[x, c(variable)] == list_class_names[n]))/as.numeric(tot__var_class_neigh[n]))
                  }
                  else{
                    percent_diff = append(percent_diff, NA)
                  }                }
                print(percent_diff)
                percent_diff_diff = as.data.frame(matrix(data = NA, nrow = length(list_var_classes_neigh_df), ncol = length(list_var_classes_neigh_df)))
                for(n in 1:(lvar)){
                  for(k in 1:(lvar)){
                    percent_diff_diff[n, k] = percent_diff[n] - percent_diff[k]
                  }
                }
                if(all(abs(na.omit(percent_diff_diff)) < 0.05)){
                  fitness = 1
                }
                # else{
                #   agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
                # }
              }
            }
          }
        }
        else if(lvar == 2){
          print("use binary attribute function: distr_attr_strat_neigh_stats_binary() ")
        }
      }
    }
    print(paste("neighborhood:", i))
  }
  random_seq = sample(nrow(agent_df))
  agent_df = agent_df[random_seq,]
  print(Sys.time())
  return(agent_df)
}




#### assigning attributes purely based on conditional probabilities

#' @title Assigning Attributes purely based on conditional probabilities
#' @description Sometimes there are no marginal distributions for the neighborhood available for certain variable that we want to join to the agent dataframe. For these cases this function can be used to assign variables solely based on the conditional propensities (the likelihood to have a certain attribute because of other attributes). Conditional propensity does not require causal direction, but can be purely based on statistical correlation in a place for a group of people.
#' @param agent_df Dataframe of the unique agents with their attributes
#' @param variable the new variable that we want to add based on the stratified and neighborhood marginal distributions
#' @param list_agent_propens A list of the columns in the agent dataset that contain the propensities for the classes of the variable based on the other agents conditional attributes. This list has to be in the same order as the list_var_classes_neigh_df. We can leave out the last propensity as it is 1 minus the other propensities.
#' @param list_class_names The list_class_names is optional and contains the values that the new created agent variable should have for the different variable classes. It has to be in the same order and of the same length as the list_var_classes_neigh_df. If left empty, the list_var_classes_neigh_df will become the default values for the classes.
#' @param agent_exclude an optional variable containing one or multiple variable names of the agent dataset on which basis agents should be excluded from the attribute assignment. These variables whould be binary columns, with 1 indicating that it should be excluded (e.g. "is_child" could be entered if agents with is_child = 1 should be exluded for the new attribute).

#' @return Returns the Agent dataframe with the new assigned attribute and the random score used for the assignment.
#' @export
#'
#' @examples
#' ## generating some example mock data (based on random numbers)
#' ### agent dataframe mock data ###
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' agent_df$neigh_ID = sample(1:10, size =nrow(agent_df), replace = T)
#' agent_df$age_group = c("A1", "A2", "A3", "A4")[sample(1:4, size = nrow(agent_df), replace = T)]
#' agent_df$sex = c("female", "male", "non-binary")[sample(1:3, size = nrow(agent_df), replace = T)]
#' # the propensity variable this should be the ouput of the "calc_propens_agents" function or the "strat_prop_from_sep_cond_var" if no multi-joint distributions of conditional variable are available and only single conditional variable joint distributions.
#' # for illustration purposes it is only a random probability here
#' agent_df$prop_employed = sample(x= seq(from= 0, to = 0.75, by= 0.01), size = nrow(agent_df), replace = T)
#' agent_df$prop_pension = sample(x= seq(from= 0, to = 0.25, by= 0.01), size = nrow(agent_df), replace = T)
#' agent_df$prop_unemployed = 1-(agent_df$prop_employed + agent_df$prop_pension)
#' colnames(agent_df) = c("agent_id", "neigh_ID", "age_group", "sex", "prop_employed", "prop_pension")
#' print(agent_df)
#'
#' # function application (without excluding a subset of the population from attribute assignment)
#' agent_df_new = distr_attr_cond_prop(agent_df = agent_df, variable = "employ_status", list_agent_propens = c("prop_employed", "prop_pension", "prop_unemployed"), list_class_names = c("employed", "pensioned", "unemployed"))
#' print(agent_df_new)
#'
#' ## applying the function (with agent_exclude param)
#' # say we want to exclude people from age_group A1, we need to create a binary variable that indicates if an agent belongs to that group
#' agent_df$is_child = 0
#' agent_df$is_child[agent_df$age_group == "A1"] = 1
#' agent_df_new = distr_attr_cond_prop(agent_df = agent_df, variable = "employ_status", list_agent_propens = c("prop_employed", "prop_pension", "prop_unemployed"), list_class_names = c("employed", "pensioned", "unemployed"), agent_exclude = "is_child")
#' print(agent_df_new)
#'
distr_attr_cond_prop = function(agent_df, variable, list_agent_propens, list_class_names, agent_exclude){
  print(Sys.time())
  agent_df[, c(variable, "random_scores")] = 0
  if(!missing(agent_exclude)){
    agent_df[, c("excluded")] = 0
    for(i in 1:length(agent_exclude)){
      agent_df[which(agent_df[, c(agent_exclude[i])] == 1) , c("excluded")] =  1
    }
    x = which(agent_df[, c("excluded")] != 1)
  }
  else{
    x = which(agent_df[, c(variable)] == 0)
  }
  lvar = length(list_class_names)
  agent_df[x, c("random_scores")] = sample(x= seq(from= 0, to = 1, by= 0.01), size = length(x), replace = T)
  if(lvar == 2){
    agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
    agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
  }
  else if(lvar > 2){
    agent_df[x[which(agent_df[x, c(list_agent_propens[1])] >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[1]
    agent_df[x[which(agent_df[x, c(list_agent_propens[1])] < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:2])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[2]
    if(lvar >3){
      for(n in 3:(lvar -1)){
        agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(n-1)])]) < agent_df[x,c("random_scores")] & rowSums(agent_df[x, c(list_agent_propens[1:n])]) >= agent_df[x,c("random_scores")])], c(variable)] = list_class_names[n]
      }
    }
    agent_df[x[which(rowSums(agent_df[x, c(list_agent_propens[1:(lvar -1)])]) < agent_df[x,c("random_scores")])], c(variable)] = list_class_names[lvar]
  }
  random_seq = sample(nrow(agent_df))
  agent_df = agent_df[random_seq,]
  print(Sys.time())
  return(agent_df)
}


