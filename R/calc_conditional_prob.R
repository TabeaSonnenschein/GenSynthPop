
#' @title Calculating the conditional propensity to have an attribute based on conditional variables

#' @description This is a function to calculate the probability/propensity to have a specific class of a variable conditional on other variables. It requires a stratified dataframe (@dataframe) which includes all combinations of the conditional variables (the ones based on which we want to calculate the propensity), the number of people with the variable class (@variable) for which we want to calculate the propensity, and the total number of people within the combination of conditional variables (@total_population). The function calculates the propensity to have a specific class based on the conditional variables, by dividing the number of people with the class if interest by the total number of people within the combination of conditional variables. Further, the function joins these probabilities with the agent dataset (@agent_df) based on the list of conditional variables (@list_conditional_var). This requires the variable/column names of the conditional variables to be equal for the stratified and the agent dataframe. Finally, it shuffles the agent dataset to avoid biases due to a non random order in the next steps.

#' @param dataframe stratified dataframe which includes all combinations of the conditional variables (the ones based on which we want to calculate the propensity)
#' @param variable the number of people with the variable class for which we want to calculate the propensity
#' @param total_population the total number of people within the combination of conditional variables
#' @param agent_df agent dataset, the full dataset of individual agents that was previously created and that contains the conditional variables
#' @param list_conditional_var list of conditional variables, the variables on which basis the propensity to have an attribute should be created
#'
#' @return The output is the agent dataframe with an additional column of their propensity/probability to have the respective property/attribute.
#' @export
#'
#' @details
#' # Example Input- Output Tables (random numbers)
#' ## Stratified dataframe (input)
#' |age_group |sex | employed | un-employed | total |
#' |:----:|:-----:|:----:|:---:|:---:|
#' | A1 | male | 25 | 133 | 158 |
#' | A2 | male | 132 | 323 | 455 |
#' | A3 | male | 122 | 360 | 482 |
#' | A1 | female | 175 | 389 | 564 |
#' | A2 | female | 275 | 206 |481 |
#' | A3 | female | 394 |  25 | 419 |
#' | A1 | non-binary | 196 | 203 | 399 |
#' | A2 | non-binary | 248 | 270 | 518 |
#' | A3 | non-binary | 137 |  34 | 171 |
#'
#' ## agent dataframe (input)
#' | agent_id | age_group | sex |
#' |:---:|:---:|:---:|
#' | Agent_1 | A1 | male |
#' | Agent_2 | A3 | male |
#' | Agent_3 | A3 | non-binary |
#' | Agent_4 | A1 | female |
#' | Agent_5 | A4 | male |
#' | Agent_6 | A1 | female |
#' | Agent_7 | A4 | non-binary |
#' | Agent_8 | A3 | male |
#' | Agent_9 | A3 | male |
#' | Agent_10 | A3 | male |
#' | Agent_11 | A3 | non-binary |
#' | Agent_12 | A4 | non-binary |
#' | Agent_13 | A4 | non-binary |
#' | Agent_14 | A4 | female |
#' | Agent_15 | A2 | non-binary |
#' (...)
#'
#' ## agent dataframe with conditional propensity to have class of interest (here employed)
#' | agent_id | age_group | sex | prop_employed |
#' |:---:|:---:|:---:|:---:|
#' | Agent_91 | A2 | male | 0.66781411 |
#' | Agent_483 | A3 | female | 0.4893964 |
#' | Agent_418 | A1 | non-binary | 0.45307918 |
#' | Agent_124 | A1 | male | 0.64898990 |
#' | Agent_388 | A1 | female | 0.56937799 |
#' | Agent_406 | A1 | male | 0.64898990 |
#' | Agent_447 | A3 | male | 0.48939641 |
#' | Agent_459 | A1 | female | 0.56937799 |
#' | Agent_328 | A1 | female | 0.56937799 |
#' | Agent_223 | A1 | non-binary | 0.45307918 |
#' (...)
#'
#' ### * if you have more than two classes of a variable then apply the same function to the same datasets multiple times for the different classes of the variable. E.g., if you have "low_education", "middle_education", "high_eduction", just apply it for all of them.
#' @md
#' @examples
#' ## generating some example mock data ##
#' # stratified dataframe mock data
#' age_group = rep(c("A1", "A2", "A3", "A4"),3)
#' sex = rep(c("male", "female", "non-binary"), each = 4)
#' employed = sample(1:400,length(age_group))
#' unemployed = sample(1:400,length(age_group))
#' total_pop = employed + unemployed
#' stratified_df = data.frame(age_group, sex , employed, unemployed, total_pop)
#'
#' # agent dataframe mock data
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' agent_df$age_group = age_group[sample(1:length(age_group), size = nrow(agent_df), replace = T)]
#' agent_df$sex = sex[sample(1:length(age_group), size = nrow(agent_df), replace = T)]
#' colnames(agent_df) = c("agent_id",  "age_group", "sex" )
#' print(agent_df)
#'
#' # function application
#' agent_df = calc_propens_agents(stratified_df, "employed", "total_pop", agent_df, c("age_group", "sex"))
#' print(agent_df$prop_employed)
#' print(agent_df)
#'
calc_propens_agents = function(dataframe, variable, total_population, agent_df, list_conditional_var){
  if(!missing(total_population)){
    dataframe[,c(paste("prop_",variable, sep = ""))] = dataframe[,c(variable)]/dataframe[, c(total_population)]
  }
  order_agent_df = colnames(agent_df)
  if(paste("prop_",variable, sep = "") %in% order_agent_df){
    x = which(order_agent_df == paste("prop_",variable, sep = ""))
    agent_df = subset(agent_df, select = -c(x))
  }
  agent_df = merge(agent_df, dataframe[,c(list_conditional_var, paste("prop_",variable, sep = ""))], all.x = T, all.y= F, by = list_conditional_var)
  agent_df = agent_df[,c(order_agent_df, paste("prop_",variable, sep = ""))]
  random_seq = sample(nrow(agent_df))
  agent_df = agent_df[random_seq,]
  return(agent_df)
}


#' @title Creates a stratified propensity table from separate conditional variable joint distributions
#' @description This function combines multiple - joint distributions of conditional variables into one stratified dataframe of all-variable combinations and the propensity to have the variable classes of interest.
#' @param df A dataframe with one column in which all classes of the conditional variables are listed in seperate rows and additional columns that show the marginal distributions for a variable of interest
#' @param nested_cond_attr_list a nested list of classes of conditional variables. (e.g. list(c("female", "male", "non-binary"), c("employed", "unemployed")))
#' @param cond_var_names a list of the names that you want to give the variables to which the  classes indicated in the nested_cond_attr_list belong. It has to have the same order as that list. (e.g. for above that could be c("sex", "employ_status"))
#' @param cond_attr_column The column in which all separate conditional variable classes are listed.
#' @param var_for_pred The columnnames that indicate the classes of the variable for which the stratified dataframe should be generated, if it is a binary variable, then only one class needs to be given, since the propensity to have the other class is class is 1-the propensity of the indicated class.
#' @param total_population The column indicating the total population that has the conditional variable attribute of the row
#' @returns a stratified dataset with all combinations of the conditional variables and the propensity to be/have the indicated classes
#' @export
#' @details
#' #Example Table (random numbers)
#' ## This is the input table with marginal distributions of the variable of interest (here employment) for one conditional variables at a time.
#' | cond_variables | employed | unemployed | tot_pop |
#' |:---:|:---:|:---:|:---:|
#' | male | 262 | 259 | 521 |
#' | female | 253 | 139 | 392 |
#' | non-binary | 57 | 304 | 361 |
#' | single_household | 158 | 244 | 402 |
#' | multi_pers_household | 118 | 311 | 429 |
#'
#' ## This is the output table.
#' | sex | hh_structure | prop_employed | prop_unemployed |
#' |:---:|:---:|:---:|:---:|
#' | male | single_household | 0.4479570 | 0.5520430 |
#' | male | multi_pers_household | 0.3889687 | 0.6110313 |
#' | female | single_household | 0.5192215 | 0.4807785 |
#' | female | multi_pers_household | 0.4602332 | 0.5397668 |
#' | non-binary | single_household | 0.2754648 | 0.7245352 |
#' | non-binary | multi_pers_household | 0.2164765 | 0.7835235 |
#' @md
#' @examples
#' ## creating mock data
#' cond_variables = c("male", "female", "non-binary", "single_household", "multi_pers_household")
#' employed = sample(40:400,length(cond_variables))
#' unemployed = sample(40:400,length(cond_variables))
#' df = data.frame(cond_variables, employed , unemployed)
#' df$tot_pop = df$employed + df$unemployed
#' print(df)
#'
#' # function application
#' strat_df = Strat_prop_from_sep_cond_var(df = df, nested_cond_attr_list = list(c("male", "female", "non-binary"), c("single_household", "multi_pers_household")), cond_var_names = c("sex", "hh_structure"), cond_attr_column = "cond_variables", var_for_pred = c("employed", "unemployed"), total_population = "tot_pop" )
#' print(strat_df)
strat_prop_from_sep_cond_var = function(df, nested_cond_attr_list, cond_var_names, cond_attr_column, var_for_pred, total_population){
  ncondVar = length(cond_var_names)
  attr_length = c()
  for(i in 1:ncondVar){
    attr_length = append(attr_length, length(nested_cond_attr_list[[i]]))
  }
  new_strat_df = as.data.frame(matrix(nrow = prod(attr_length), ncol = (ncondVar + length(var_for_pred))))
  for(i in 1:ncondVar){
    if(i == ncondVar){
      new_strat_df[,i] = rep(nested_cond_attr_list[[i]], times =  (prod(attr_length)/attr_length[i]))
    }
    else{
      var_comb = c()
      for(n in 1:attr_length[i]){
        var_comb = append(var_comb, rep(nested_cond_attr_list[[i]][n], times = prod(attr_length[(i+1):ncondVar])))
      }
      new_strat_df[,i] = rep(var_comb, times = prod(attr_length)/prod(attr_length[(i):ncondVar]))
    }

  }
  colnames(new_strat_df) = c(cond_var_names, paste("prop_",var_for_pred, sep = ""))
  if(missing(total_population)){
    for(i in 1:nrow(new_strat_df)){
      for(n in 1:length(var_for_pred)){
        new_strat_df[i,n+ncondVar] = sum(df[which(df[,c(cond_attr_column)] %in% c(new_strat_df[i,1:ncondVar])),c(var_for_pred[n])])/ncondVar
      }
    }
  }
  else{
    for(i in 1:length(var_for_pred)){
      df[,c(paste("prop_",var_for_pred[i], sep = ""))] = df[,c(var_for_pred[i])]/df[, c(total_population)]
    }
    for(i in 1:nrow(new_strat_df)){
      for(n in 1:length(var_for_pred)){
        new_strat_df[i,n+ncondVar] = sum(df[which(df[,c(cond_attr_column)] %in% c(new_strat_df[i,1:ncondVar])),c(paste("prop_",var_for_pred[n], sep = ""))])/ncondVar
      }
    }
  }
  return(new_strat_df)
}

