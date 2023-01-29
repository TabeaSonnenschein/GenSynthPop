install.packages("devtools")
library(devtools)
install_github("TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/GenSynthPop")
library(GenSynthPop)

#############################################################
# check out the function documentation
?gen_agent_df                        #generating agent dataframe
?distr_agent_neigh_age_group         #populating the agent_df with age_group and neigh_ID attributes
?crosstabular_to_singleside_df       #data preprocessing: crosstabular to single side variable combinations table
?restructure_one_var_marginal        #data preprocessing:  single-sided stratified dataframe to marginal distribution across classes of one column
?varclass_harmonization              #harmonize the classes of a variable across datasets
?aggreg_stratdata_in_harmonclasses   #Aggregating a stratified dataset into the newly added harmonised classes
?calc_propens_agents                 #Calculating the conditional propensity to have an attribute based on conditional variables
?strat_prop_from_sep_cond_var        #creates a stratified propensity table from separate conditional variable joint distributions
?distr_attr_strat_neigh_stats_binary #distribute attributes based on conditional propensities and neighborhood totals (binary attributes)
?distr_attr_strat_neigh_stats_3plus  #Creates a stratified propensity table from separate conditional variable joint distributions (attributes with 3 or more classes)
?distr_attr_cond_prop                #if neighborhood marginal distributions are unavailable use this: to distribute purely based on conditional propensities
?crossvalid                          #crossvalidate that the agent attribute distribution corresponds to neighborhood and stratified distributions
?add_spatial_units_to_agent_df       #Add a new spatial unit to the agent dataframe based on a unit map

##############################################################
# set the directory to your data folder for the population data
setwd()

# read the neighborhood marginal distributions across age_groups
# preferable is when this dataframe already contains the marginal distributions
# for all relevant variables (e.g. sex, migration-background, employment status, ect.)
# your study goal determines what variables are relevant
neigh_df = read.csv()
colnames(neigh_df) # identify the columnnames corresponding to the age groups

# we generate the agent_df dataframe
agent_df = gen_agent_df(sum(neigh_df[, c("A1", "A2", "A3")]))

# we distribute the agents across the age groups and neighborhoods
agent_df = distr_agent_neigh_age_group(neigh_df = neigh_df,
                                       agent_df = agent_df,
                                       neigh_id = "neigh_ID",
                                       age_colnames = c("A1", "A2", "A3"))
print(agent_df)

# load stratified dataframe for sex based on age_groups
sex_age_df = read.csv()

# Compute conditional propensities for sex based on age_groups
agent_df = calc_propens_agents(sex_age_df,
                               "female",
                               "total",
                               agent_df,
                               c("age"))

# Distribute attributes
agent_df = distr_attr_strat_neigh_stats_binary(agent_df = agent_df,
                                            neigh_df = sex_age_df,
                                            neigh_ID = "neighb_code",
                                            variable=  "gender",
                                            list_var_classes_neigh_df = c("gender_male", "gender_female"),
                                            list_agent_propens =  c("prop_female"),
                                            list_class_names = c("female", "male")
)

# Remove extra columns, if you don't want them for backward induction and transparency
agent_df = subset(agent_df, select=-c(prop_female, random_scores))
print(agent_df)

##### You have successfully added age, location and sex based on age-sex co-variance
##### now you can add variables like that for any given number of attributes that your want to add



