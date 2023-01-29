####### Preparing stratified datasets

#' @title Crosstabular Stratified Table to Single Sided Variable Combination - Counts Table
#' @description In order to facilitate the data preparation, this function can transform any stratified dataset structure in terms of number of row variables and column variable combinations into a dataset structure of all variable combinations as columns in the left and a single "counts" column on the right.
#' @param df The stratified dataset that you want to process
#' @param nrow_var number of rows corresponding to variable names
#' @param ncol_var number of columns corresponding to variable names
#' @param row_var_names The variable names that you want to use to describe the variables in the rows. This will be used as variable column name for the output dataset.The order needs to be from top row to lower row variables.
#' @param col_var_names The variable names that you want to use to describe the variables in the columns. This will be used as variable column name for the output dataset. The order needs to be from left column to right column variables.
#' @return Returns a processed stratefied dataframe with all variables as columns on the left. There will be all unique variable combinations. Additionally there will be the counts variable that shows how many people there are for the respective variable combination.
#' @details
#' #Example Tables (random numbers)
#' ## Turns a crosstabular stratified dataframe, such as this:
#' ### *note that it can have any number of row variables (here 2) or column variables (here 1)
#' | age_groups | male | male | female | female | non-binary | non-binary |
#' |:----:|:----:|:----:|:----:|:----:|:----:|:----:|
#' |  |employed | unemployed | employed | unemployed | employed | unemployed |
#' | A1 | 25 | 133 | 175 | 389 | 196 | 203 |
#' | A2 | 132 | 323 | 275 | 206 | 248 | 270 |
#' | A3 | 122 | 360 | 394 | 25 | 137 | 34 |
#'
#' ## Into a single side stratified dataframe such as this:
#' |age_group |sex | employ_status | counts |
#' |:----:|:-----:|:----:|:---:|
#' | A1 | male | employed | 25 |
#' | A2 | male | employed | 132 |
#' | A3 | male | employed | 122 |
#' | A1 | male | unemployed | 133 |
#' | A2 | male | unemployed | 323 |
#' | A3 | male | unemployed | 360 |
#' | A1 | female | employed | 175 |
#' | A2 | female | employed | 275 |
#' | A3 | female | employed | 394 |
#' | A1 | female | unemployed | 389 |
#' | A2 | female | unemployed | 206 |
#' | A3 | female | unemployed |  25 |
#' | A1 | non-binary | employed | 196 |
#' | A2 | non-binary | employed | 248 |
#' | A3 | non-binary | employed | 137 |
#' | A1 | non-binary | unemployed | 203 |
#' | A2 | non-binary | unemployed | 270 |
#' | A3 | non-binary | unemployed |  34 |
#' @md
#' @examples
#' # some example mock data
#' # crosstabular stratified dataframe mock data
#' row1 = c("age_groups","male","male", "female", "female", "non-binary", "non-binary" )
#' row2 = c("", "employed", "unemployed", "employed", "unemployed", "employed", "unemployed")
#' row3 = c("A1", sample(1:400,6))
#' row4 = c("A2", sample(1:400,6))
#' row5 = c("A3", sample(1:400,6))
#' cross_tab_stratified_df = as.data.frame(rbind(row1, row2, row3, row4, row5))
#' print(cross_tab_stratified_df)
#'
#' # function application
#' ## the number of row variables are 2 (the sex and the employment status), while only age is a column variable, hence ncol_var = 1
#' singleside_df = crosstabular_to_singleside_df(df = cross_tab_stratified_df, nrow_var = 2, ncol_var = 1, row_var_names = c("sex", "employ_status"), col_var_names = c("age_group"))
#' print(singleside_df)
#'
#' @export
crosstabular_to_singleside_df =  function(df, nrow_var, ncol_var, row_var_names, col_var_names){
  len_rowvar_combi = ncol(df) - ncol_var # how many classes of the row variables are there
  len_colvar_combi = nrow(df) - nrow_var # how many classes of the column variables are there
  df_len = len_rowvar_combi * len_colvar_combi
  df_colvar = as.data.frame(df[(nrow_var+1):nrow(df),1:ncol_var])
  colnames(df_colvar) = col_var_names
  df_new = df_colvar
  for(i in 1:(len_rowvar_combi-1)){
    df_new = rbind(df_new, df_colvar)
  }
  for(x in row_var_names){
    df_new[,c(x)] = ""
  }
  df_new$counts = NA
  print(nrow(df_new))
  for(i in 1:len_rowvar_combi){
    # print(i)
    # print(nrow(df_new))
    for(x in (1:len_colvar_combi)){
      df_new[((i-1)*len_colvar_combi)+x,c(row_var_names)]=df[1:nrow_var,i+ncol_var]
    }
    df_new[((i-1)*len_colvar_combi)+1:(i*len_colvar_combi),c("counts")] = df[(1+nrow_var):(len_colvar_combi+nrow_var),i+ncol_var]
  }
  return(df_new[1:df_len,])
}

# this function restructures the dataframe so that the classes of one column/variable are seperate columns
#' @title Restructures a single-sided stratified dataframe so that the classes of one column/variable of interest are seperate columns
#' @description This function takes a single-sided stratified dataframe, such as the output of the crosstabular_to_singleside_df function and restructures it as such that the unique classes of one variable of interest (any of the variablecolumns), will become seperate columns. Hence the output will be a dataframe of all variable combinations excluding the variable of interest and the marginal distributions of the variable combinations along the classes of the variable of interest.
#' @param df The original stratified dataframe with all varable combinations as columns on the left side.
#' @param variable The variable of interest, for which on wants to generate seperate columns of the subclasses.
#' @param countsname The columnname of the column in the original datafram, which indicates the counts for all variable combinations.
#' @return the output will be a dataframe of all variable combinations excluding the variable of interest and the marginal distributions of the variable combinations along the classes of the variable of interest.
#' @details
#' # Example Tables (random numbers)
#' ## Turns a single side stratified dataframe such as this:
#' |age_group |sex | employ_status | counts |
#' |:----:|:-----:|:----:|:---:|
#' | A1 | male | employed | 25 |
#' | A2 | male | employed | 132 |
#' | A3 | male | employed | 122 |
#' | A1 | male | unemployed | 133 |
#' | A2 | male | unemployed | 323 |
#' | A3 | male | unemployed | 360 |
#' | A1 | female | employed | 175 |
#' | A2 | female | employed | 275 |
#' | A3 | female | employed | 394 |
#' | A1 | female | unemployed | 389 |
#' | A2 | female | unemployed | 206 |
#' | A3 | female | unemployed |  25 |
#' | A1 | non-binary | employed | 196 |
#' | A2 | non-binary | employed | 248 |
#' | A3 | non-binary | employed | 137 |
#' | A1 | non-binary | unemployed | 203 |
#' | A2 | non-binary | unemployed | 270 |
#' | A3 | non-binary | unemployed |  34 |
#'
#' ## into a one variable marginal distribution table for a specified variable of interest, and adds a total for the population of the attribute combination
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
#' @md
#' @examples
#' ## generating some example mock data ##
#' # stratified dataframe mock data, can be output of function: crosstabular_to_singleside_df
#' age_group = rep(c("A1", "A2", "A3", "A4"), 6)
#' sex = rep(c("male", "female", "non-binary"), each = 8)
#' employ_status = rep(rep(c("employed", "unemployed"), each = 4),3)
#' counts = sample(1:400,length(age_group))
#' singleside_stratified_df = data.frame(age_group, sex , employ_status, counts)
#'
#' # function application
#' one_var_marginal_df = restructure_one_var_marginal(singleside_stratified_df, "employ_status", "counts")
#' print(one_var_marginal_df)
#'
#' @export
restructure_one_var_marginal = function(df, variable, countsname){
  classes = unique(df[,c(variable)])
  restColumns = colnames(df)[(colnames(df) != variable) & (colnames(df) != countsname)]
  df_new = unique(as.data.frame(df[, c(restColumns)]))
  colnames(df_new) = restColumns
  for(x in classes){
    df_new[, c(x)] = as.numeric(df[df[,c(variable)] == x, c(countsname)])
  }
  df_new[,c("total")] = rowSums(df_new[,c(classes)])
  return(df_new)
}



#' @title Harmonize the classes of a variable across datasets by providing corresponding lists of values and adding the corresponding values to the dataframe
#' @description This function can help the data preparation by harmonising the classes of a variable. For example, one stratified dataframe might have other age_groups then the neighborhood dataframe. In this function you can add a nested list of corresponding values that in that order belong to a second list of replacement values (to match it to the other dataframe). No data is deleted, only one new column with the new corresponding classes of the other dataframe is added.
#' @param df The dataframe of the one dataset where the new column with classes of the other dataset should be added to. This is logically most likely the dataframe that has the more fine classes that can be aggregated into the other dataframe larger classes.
#' @param orig_colname The columnname of df that corresponds to the variable whose classes should be harmonised
#' @param list_other_df_classes This should be a list of unique classes of the other dataframe with which the entered df should be harmonised
#' @param nested_list_corr_values Provide a nested list (list of lists) for the values of df that correspond to the list_other_df_classes, in the same order as list_other_df_classes. There can be multiple values within df that correspond to one of the list_other_df_classes, which is why it is a list of lists.
#' @param new_col_name the name of the new column that will be added with the corresponding other df classes
#'
#' @return returns the provided dataframe with an additional column of the corresponding values provided.
#' @export
#'
#' @examples
#'#' ## generating some example mock data ##
#' # stratified dataframe mock data, can be output of function: crosstabular_to_singleside_df,
#' # but also any other df with the classes of the variable that should be harmonised in one column
#' # for example one can use the function also applied to the agent_df
#' age_group = rep(c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"), 6)
#' sex = rep(c("male", "female", "non-binary"), each = 18)
#' employ_status = rep(rep(c("employed", "unemployed"), each = 9), 3)
#' counts = sample(1:400,length(age_group))
#' singleside_stratified_df = data.frame(age_group, sex , employ_status, counts)
#'
#' # let us say we have another dataframe that we want to harmonize
#' # and their unique classes for age are "0-19", "20-39", "40-59", "60-79","80plus"
#' # we know that in the singleside_stratified_df that corresponds to list(c("A1", "A2"), c("A3", "A4"), c("A5", "A6"), c("A7", "A8"), c("A9))
#' singleside_stratified_df = varclass_harmonization(df = singleside_stratified_df, orig_colname = "age_group", list_other_df_classes = c( "0-19", "20-39", "40-59", "60-79","80plus"), nested_list_corr_values = list(c("A1", "A2"), c("A3", "A4"), c("A5", "A6"), c("A7", "A8"), c("A9")), new_col_name = "age_group_otherdf")
#' print(singleside_stratified_df)
#'
varclass_harmonization = function(df, orig_colname, list_other_df_classes, nested_list_corr_values, new_col_name){
    df[,c(new_col_name)] = NA
    for(i in 1:length(list_other_df_classes)){
      for(x in 1:length(nested_list_corr_values[[i]])){
          df[df[,c(orig_colname)] %in% nested_list_corr_values[[i]][x], c(new_col_name)] = list_other_df_classes[i]
      }
    }
    return(df)
}


#' @title Aggregating a stratified dataset into the newly added harmonised classes
#' @description Once the new harmonised classes of have been added to a stratified dataframe, the marginal distributions have to be aggregated correspondingly. This function helps doing exactly that, resulting in a new dataframe with only the harmonised classes, other stratified variables that were in the df, and the aggregated marginal distributions.
#' @param df The stratified dataframe which contains old classes and a new harmonised class (corresponding to another dataframe) as well as some marginal distributions
#' @param harmon_var_col The name of the column that contains the new harmonised classes (e.g. output of varclass_harmonization function)
#' @param former_var_col The name of the old column that contains the classes that were initially in the dataframe but which we want to get rid off
#' @param marg_dist_collist A list of names of the columns that specify the marginal distributions (numbers/counts). These will have to be aggregated into the new harmonised classes
#'
#' @return new dataframe with only the harmonised classes, other stratified variables that were in the df, and the aggregated marginal distributions.
#' @export
#'
#' @examples
aggreg_stratdata_in_harmonclasses = function(df, harmon_var_col, former_var_col, marg_dist_collist ){
  classes = unique(df[,c(harmon_var_col)])
  columns = colnames(df)[(colnames(df) != former_var_col) & !(colnames(df) %in% marg_dist_collist)]
  df_new = unique(df[,c(columns)])
  df_new[,c(marg_dist_collist)] = NA
  for(i in seq_along(df_new[,c(harmon_var_col)])){
    for( x in marg_dist_collist){
        df_new[i, c(x)] = sum(df[df[,c(harmon_var_col)] == df_new[,c(harmon_var_col)][i], c(x)])
    }
  }
  return(df_new)
}


#' @title Harmonize the classes of a variable across datasets by providing corresponding lists of values and adding the corresponding values to the dataframe
#' @description This function can help the data preparation by harmonising the classes of a variable. For example, one stratified dataframe might have other age_groups then the neighborhood dataframe. In this function you can add a nested list of corresponding values that in that order belong to a second list of replacement values (to match it to the other dataframe). No data is deleted, only one new column with the new corresponding classes of the other dataframe is added.
#' @param df The dataframe of the one dataset where the new column with classes of the other dataset should be added to. This is logically most likely the dataframe that has the more fine classes that can be aggregated into the other dataframe larger classes.
#' @param orig_colname The columnname of df that corresponds to the variable whose classes should be harmonised
#' @param list_other_df_classes This should be a list of unique classes of the other dataframe with which the entered df should be harmonised
#' @param nested_list_corr_values Provide a nested list (list of lists) for the values of df that correspond to the list_other_df_classes, in the same order as list_other_df_classes. There can be multiple values within df that correspond to one of the list_other_df_classes, which is why it is a list of lists.
#' @param new_col_name the name of the new column that will be added with the corresponding other df classes
#'
#' @return returns the provided dataframe with an additional column of the corresponding values provided.
#' @export
#'
#' @examples
#'#' ## generating some example mock data ##
#' # stratified dataframe mock data, can be output of function: crosstabular_to_singleside_df,
#' # but also any other df with the classes of the variable that should be harmonised in one column
#' # for example one can use the function also applied to the agent_df
#' age_group = rep(c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"), 6)
#' sex = rep(c("male", "female", "non-binary"), each = 18)
#' employ_status = rep(rep(c("employed", "unemployed"), each = 9), 3)
#' counts = sample(1:400,length(age_group))
#' singleside_stratified_df = data.frame(age_group, sex , employ_status, counts)
#'
#' # let us say we have another dataframe that we want to harmonize
#' # and their unique classes for age are "0-19", "20-39", "40-59", "60-79","80plus"
#' # we know that in the singleside_stratified_df that corresponds to list(c("A1", "A2"), c("A3", "A4"), c("A5", "A6"), c("A7", "A8"), c("A9))
#' singleside_stratified_df = varclass_harmonization(df = singleside_stratified_df, orig_colname = "age_group", list_other_df_classes = c( "0-19", "20-39", "40-59", "60-79","80plus"), nested_list_corr_values = list(c("A1", "A2"), c("A3", "A4"), c("A5", "A6"), c("A7", "A8"), c("A9")), new_col_name = "age_group_otherdf")
#' print(singleside_stratified_df)
#'
varclass_harmonization = function(df, orig_colname, list_other_df_classes, nested_list_corr_values, new_col_name){
  df[,c(new_col_name)] = NA
  for(i in 1:length(list_other_df_classes)){
    for(x in 1:length(nested_list_corr_values[[i]])){
      df[df[,c(orig_colname)] %in% nested_list_corr_values[[i]][x], c(new_col_name)] = list_other_df_classes[i]
    }
  }
  return(df)
}


#' @title Add a new spatial unit to the agent dataframe based on a unit map
#' @description Some marginal distributions are not available for the spatial units used to initiate/build the agent_df. However, as long as one spatial units fits into the other and hence can be mapped by having multiple of one spatial unit corresponding to one of the other spatial units, it is no problem. If the agent_df contains the smaller spatial units that fit into the larger units of the new marginal distribution dataframe, then use this function to add the corresponding larger units to the agent_df and use the newly added unit variable in the subsequent attribute distribution functions. If it is the other way and the smaller units are in the spatial marginal distributions dataframe then use the... The spatial unit maps can be generated with calculating the centroids of the smaller units and do a point in polygon function to see which centriods are in which larger spatial unit polygons.
#' @param agent_df the agent_df containing minimum the age group and neighborhood id
#' @param spat_unit_map a two column dataframe, in which the first column are the unique spatial units of the agent_df and the second column are the corresponding larger units of the marginal distribution dataframe
#' @param spat_id_colnam the name of the spatial unit id column in the agent_df
#' @param new_spat_id_colnam the name for the new spatial unit id column in the agent_df
#' @return The agent_df with the new spatial unit column, where agents are assigned to the larger spatial units to which their smaller neighborhood belongs
#' @export
#'
#' @examples
#' #agent_df mock data
#' agent_df = as.data.frame(paste("Agent_",1:500, sep=""))
#' colnames(agent_df) = "agent_ID"
#' agent_df$neigh_id = sample(x=1:20, size=10)
#'
#' #Spatial unit conversion map
#' # let us say the new marginal distributions only have 10 units
#' # and you know how they correspond to your 20 agent neighborhood units
#' spat_unit_map = as.data.frame(1:20)
#' colnames(spat_unit_map) = "neigh_id"
#' spat_unit_map$district_id = rep(1:10, each = 2)
#'
#' #apply function
#' agent_df_new = add_spatial_units_to_agent_df(agent_df = agent_df, spat_unit_map = spat_unit_map, spat_id_colnam = "neigh_id", new_spat_id_colnam = "district_id")
#' print(agent_df_new)
#'
add_spatial_units_to_agent_df = function(agent_df, spat_unit_map, spat_id_colnam , new_spat_id_colnam){
  agent_df[,c(new_spat_id_colnam)] = ""
  for(x in 1:nrow(spat_unit_map)){
    agent_df[agent_df[, c(spat_id_colnam)] == spat_unit_map[x, 1],c(new_spat_id_colnam)] = spat_unit_map[x, 2]
  }
  return(agent_df)
}

