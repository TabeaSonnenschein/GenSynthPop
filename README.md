## **Instructions for R-package: GenSynthPop**
#### *Author: Tabea Sonnenschein, Utrecht University*

This package contains a set of functions that help prepare stratified census datasets to generate conditional propensities, combines the conditional propensities with spatial marginal distributions to generate a representative population and validates that the produced agents have a similar distribution as the initial spatial marginal datasets and the stratified datasets. The generated population is  representative for a city or the spatial extent that is fed into the algorithms and can be used for simulation purposes, such as an agent-based model. The smaller the spatial units of the spatial marginal distributions, the more spatially resolved the agents will be too.

### Overview of functions

#### *For Data Preparation*
* **crosstabular_to_singleside_df:** Crosstabular Stratified Table to Single Sided Variable Combination - Counts Table
* **restructure_one_var_marginal:** Restructures a single-sided stratified dataframe so that the classes of one column/variable of interest are seperate columns
* **varclass_harmonization:** Harmonize the classes of a variable across datasets 
* **aggreg_stratdata_in_harmonclasses:** Aggregating a stratified dataset into the newly added harmonised classes
* **add_spatial_units_to_agent_df:** Add a new spatial unit to the agent dataframe based on a unit map

#### *For Initiating the Agent Dataframe*
* **gen_agent_df:** Generating an agent dataframe of the population size and assigning a unique ID
* **distr_agent_neigh_age_group:** Populating the agent_df with age_group and neigh_ID attributes distributed like a given neighborhood marginal distribution

#### *For Conditional Propensity calculation*
* **calc_propens_agents:** Calculating the conditional propensity to have an attribute based on conditional variables
* **strat_prop_from_sep_cond_var:** Creates a stratified propensity table from separate conditional variable joint distributions

#### *For Attribute Assignment based on conditional and marginal distributions*
* **distr_attr_strat_neigh_stats_binary:** Distributing attributes across agent population based on conditional proabilities and neighborhood totals for binary attributes
* **distr_attr_strat_neigh_stats_3plus:** Distributing attributes across agent population based on conditional proabilities and neighborhood totals for attributes with 3 or more classes
* **distr_attr_cond_prop:** Assigning Attributes purely based on conditional probabilities

#### *For Validation*
* **crossvalid:** Cross validation with the neighborhood and stratified marginal distributions


### Installing package in R
	install.packages("devtools")
	library(devtools)
	install_github("TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/GenSynthPop")
	library(GenSynthPop)

### Looking up documentation for a function
#### There is extensive documentation for the functions within R

Example:

	?crosstabular_to_singleside_df
	help(crosstabular_to_singleside_df)

Should there be remaining questions, shoot me an email: t.s.sonnenschein@uu.nl

### Instructions

1. Start by collecting neighborhood marginal distributions of age_groups. It is recommended to go as spatially resolved as you can (smallest spatial unit) but it depends on what you want to use the synthetic agent population for. You theoretically can even use provincial or national administrative areas, if this is your project scope and goal. We go for neighborhoods because we want to  create an urban ABM.

2. apply **gen_agent_df** for the sum of all age groups in all neighborhoods. This will be the population size.

3. use this new agent_df and the neighborhood marginal distribution dataframe in the **distr_agent_neigh_age_group** code to distribute the agents across neighborhoods and age groups.

4. Read the stratified dataframe with the conditional variable and the variable of interest (that you want to add), for example sex by agegroup, since we already added that one. Make sure that the classes of the conditional variables correspond to the ones in the agent_df. For that you can use **varclass_harmonization**. If the stratified dataset has been assigned larger harmosed classes, the marginal distributions have to be aggregated, for which you can use: **aggreg_stratdata_in_harmonclasses**. If instead of the stratified dataset, the agent_df was assigned larger harmonised classes, then no aggregation is necessary, because the new harmonised attribute can be used for calculating the propensities in step 5. Additionally to restructure and prepare the data so that it can be read by **calc_propens_agents**, you can use the data preparation functions: **crosstabular_to_singleside_df** and **restructure_one_var_marginal**

5. Use **calc_propens_agents** to generate propensities to have the attribute of interest based on the co-variance with the conditional variable that is already in the agent_df (e.g. the likelihood to be female based on the agegroup). This function takes the stratified dataframe, generates the propensities for the conditional variables and adds the given propensity for each agent to the agent_df. If you have a non-binary variable (3 or more classes) then calculate the propensities for every class of the variable (e.g. "low education", "middle education", "high education").

6. Depending on if your variable is binary or not, use **distr_attr_strat_neigh_stats_binary** or **distr_attr_strat_neigh_stats_3plus** by reading the neighborhood marginal distributions for the variable of interest, and the propensities calculated in step 5 to distribute the attribute of interest across the agent population accordingly.

7. Use **crossvalid** to validate that the generated distribution corresponds to the neighborhood and stratified distributions.

8. Repeat steps 4,5,6,7 for any new variable that you want to add to the agent dataframe. The more attributes are added, the more conditional variables can be use (e.g. using age, sex, migrationbackground, household size, as conditional variables for being "employed" or not). However, as many might assume the availability of census and stratified data has its limit :), but that depends on the geographic location and the year of interest.


## you can look at the Example_Application_GenSynthPop.R script for an example application of the functions in the package.