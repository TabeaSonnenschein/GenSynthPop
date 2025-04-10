## **Instructions for R-package: GenSynthPop**

This repository contains the implementation of GenSynthPop, a sample-free tool to construct Synthetic Populations from mixed-aggregation contingency tables. 

This package contains a set of functions that help prepare stratified census datasets to generate conditional propensities, combines the conditional propensities with spatial marginal distributions to generate a representative population and validates that the produced agents have a similar distribution as the initial spatial marginal datasets and the stratified datasets. The generated population is  representative for a city or the spatial extent that is fed into the algorithms and can be used for simulation purposes, such as an agent-based model. The smaller the spatial units of the spatial marginal distributions, the more spatially resolved the agents will be too.

## Updates
Changes in Version 2.0.0 of the package GenSynthPop compared to Version 1.0.0
* implements iterative proportionate fitting to fit multi-variable joint distributions to spatial marginal distributions. 
* implements deterministic assignment, instead of probability distribution sampling
* fuses all steps into a single function, for ease of use

## Publication(s)
The work in this repository is described in:
 de Mooij, J., Sonnenschein, T., Pellegrino, M. et al. GenSynthPop: generating a spatially explicit synthetic population of individuals and households from aggregated data. Auton Agent Multi-Agent Syst 38, 48 (2024).
[https://doi.org/10.1007/s10458-024-09680-7](https://doi.org/10.1007/s10458-024-09680-7)

An Python implementation of this library is available [here](https://doi.org/10.5281/zenodo.11474109)


### Main Function
* **Conditional_attribute_adder():** Adds a target attribute to a synthetic population by fitting it to a contingency table, optionally using iterative proportional fitting (IPF) with margins.


### Installing package in R
```r
	install.packages("devtools")
	library(devtools)
	install_github("TabeaSonnenschein/GenSynthPop")
	library(GenSynthPop)
```
### Looking up documentation for a function
#### There is extensive documentation for the functions within R

Example:
```r
	?Conditional_attribute_adder
	help(Conditional_attribute_adder)
```
Should there be remaining questions, shoot me an email: t.s.sonnenschein@uu.nl

### Instructions

1. Start by collecting neighborhood marginal distributions of age_groups. It is recommended to go as spatially resolved as you can (smallest spatial unit) but it depends on what you want to use the synthetic agent population for. You theoretically can even use provincial or national administrative areas, if this is your project scope and goal. We go for neighborhoods because we want to  create an urban ABM.

2. generate a population by generating unique agents for each person living in each neighborhood

```r
# Load the library
library(GenSynthPop)
neigh_df = read.csv("Neighborhood_statistics.csv")

# Initialize the agent_df
agent_neighborhoods = list()
agent_count = 0
for (i in 1:nrow(neigh_df)) {
  neighb_code = neigh_df[i, "neighb_code"] 
  neighb_total = neigh_df[i, "nr_residents"] 
  agent_neighborhoods = c(agent_neighborhoods, rep(neighb_code, neighb_total))
  agent_count = agent_count + neighb_total
}
agent_ids = paste0("Agent_", 0:(agent_count - 1))
agent_df = data.frame(agent_id = unlist(agent_ids), 
                                       neighb_code = unlist(agent_neighborhoods))
```

3. use this new agent_df and the neighborhood marginal distribution dataframe to distribute the agents across neighborhoods and age groups. 

```r
agecols = c("0-15", "15-25", "25-45", "45-65", "65+")
ageneigh_df = neigh_df[unlist(c("neighb_code", agecols))] %>%
  pivot_longer(cols = all_of(agecols), 
               names_to = "age_group", 
               values_to = "count")    # Create a new column for counts

ageneigh_df = as.data.frame(ageneigh_df)

agent_df = Conditional_attribute_adder(df = agent_df, 
                            df_contingency = ageneigh_df, 
                            target_attribute = "age_group", 
                            group_by = c("neighb_code"))

print(head(agent_df))

```

4. Read the stratified dataframe with the conditional variable and the variable of interest (that you want to add), for example sex by agegroup, since we already added that one. Make sure that the classes of the conditional variables correspond to the ones in the agent_df. We can now use additional neighborhood margins that we have. The statement "variable does not statistically match the original distribution" can be ignored and only is the case because distributions have been adjusted to the local neighborhood margins and therefore do not equal the unadjusted distributions.

```r
sex_age_df = read.csv("sex_age_statistics.csv") # columns age_group, sex, counts

sexcols = c("male", "female")

sexneigh_df <- neigh_df[unlist(c("neighb_code", sexcols))] %>%
  pivot_longer(cols = all_of(sexcols), 
               names_to = "sex", 
               values_to = "count")  
sexneigh_df <- as.data.frame(sexneigh_df)
   

agent_df = Conditional_attribute_adder(df = agent_df, 
                            df_contingency = sex_age_df , 
                            target_attribute = "sex", 
                            group_by = c("neighb_code"),
                            margins= list(ageneigh_df, sexneigh_df),
                            margins_names= c("age_group", "sex"))
print(head(agent_df))

```

I would recommend adding the integer age based on sex and age_group statistics without neighborhood margins. This allows regrouping age into the needed age group categorizations (determined by the data) for subsequent variables.


5. Now we can add multi-variable contingency tables and repeat the function for any data and variables we would like to add. For example let us add education level based on age and sex. We can now use the neighborhood margins for age_group, sex, or even as well for education_level. The function can take contingency tables with any number of variables and any number of neighborhood marginal data. The only requirement is that the conditional variables of the contingency table and marginal data are represented in the agent_df. So all variables apart from the target attribute. The algorithm can deal with cases when no neighborhood marginal data is available for some conditional variables or target attributes. 

```r
edu_age_sex_df = read.csv("edu_sex_age_statistics.csv") # columns age_group, sex, education_level counts

educols = c("high", "middle", "low")

eduneigh_df <- neigh_df[unlist(c("neighb_code", educols))] %>%
  pivot_longer(cols = all_of(educols), 
               names_to = "sex", 
               values_to = "count")  
eduneigh_df <- as.data.frame(eduneigh_df)
eduneigh_df <- eduneigh_df[!is.na(eduneigh_df$count), ]

agent_df = Conditional_attribute_adder(df = agent_df, 
                            df_contingency = edu_age_sex_df , 
                            target_attribute = "education_level", 
                            group_by = c("neighb_code"),
                            margins= list(ageneigh_df, sexneigh_df, eduneigh_df),
                            margins_names= c("age_group", "sex", "education_level"))
print(head(agent_df))

# but it also works without the eduneigh_df

```


### you can look at the Example_Application_GenSynthPop.R script for an example application of the functions in the package.

## License
This package is licensed under the MIT License.
