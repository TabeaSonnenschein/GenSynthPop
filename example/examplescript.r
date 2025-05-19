library(devtools)
install_github("TabeaSonnenschein/GenSynthPop")
library(GenSynthPop)
library(tidyr)
library(dplyr)

?Conditional_attribute_adder 

setwd("C:/Users/6513301/OneDrive - Universiteit Utrecht/Documents/GitHub/GenSynthPop/example")

# Load the library
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

agecols = c("age0_15", "age15_25", "age25_45", "age45_65", "age65plus")
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


edu_age_sex_df = read.csv("edu_sex_age_statistics.csv") # columns age_group, sex, education_level counts

educols = c("high", "middle", "low")

eduneigh_df <- neigh_df[unlist(c("neighb_code", educols))] %>%
  pivot_longer(cols = all_of(educols), 
               names_to = "education_level", 
               values_to = "count")  
eduneigh_df <- as.data.frame(eduneigh_df)

neighwithmissingdata = unique(eduneigh_df$neighb_code[is.na(eduneigh_df$count)])
eduneigh_df = eduneigh_df[!eduneigh_df$neighb_code %in% neighwithmissingdata, ]

agent_df = Conditional_attribute_adder(df = agent_df, 
                            df_contingency = edu_age_sex_df , 
                            target_attribute = "education_level", 
                            group_by = c("neighb_code"),
                            margins= list(ageneigh_df, sexneigh_df, eduneigh_df),
                            margins_names= c("age_group", "sex", "education_level"))
print(head(agent_df))

write.csv(agent_df, "agents.csv", row.names = FALSE)

