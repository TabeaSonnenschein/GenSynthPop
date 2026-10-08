## **Instructions for R-package: GenSynthPop**

This repository contains the implementation of GenSynthPop, a sample-free tool to construct Synthetic Populations from mixed-aggregation contingency tables. 

This package contains a set of functions that help prepare stratified census datasets to generate conditional propensities, combines the conditional propensities with spatial marginal distributions to generate a representative population and validates that the produced agents have a similar distribution as the initial spatial marginal datasets and the stratified datasets. The generated population is  representative for a city or the spatial extent that is fed into the algorithms and can be used for simulation purposes, such as an agent-based model. The smaller the spatial units of the spatial marginal distributions, the more spatially resolved the agents will be too.

## Updates
Changes in Version 2.1.0 compared to Version 2.0.0
* partitions the synthetic population into households, without altering any attribute already fitted
* matches partners on an age gap and on any number of optional categorical determinants, such as origin or educational homogamy
* converts published household counts into person-level margins, so household counts constrain the population instead of falling out of it
* widens the spatial level step by step where a neighbourhood cannot supply a partner, and reports each relaxation

Changes in Version 2.0.0 of the package GenSynthPop compared to Version 1.0.0
* implements iterative proportionate fitting to fit multi-variable joint distributions to spatial marginal distributions. 
* implements deterministic assignment, instead of probability distribution sampling
* fuses all steps into a single function, for ease of use

## Publication(s)
The work in this repository is described in:
 de Mooij, J., Sonnenschein, T., Pellegrino, M. et al. GenSynthPop: generating a spatially explicit synthetic population of individuals and households from aggregated data. Auton Agent Multi-Agent Syst 38, 48 (2024).
[https://doi.org/10.1007/s10458-024-09680-7](https://doi.org/10.1007/s10458-024-09680-7)

An Python implementation of this library is available [here](https://doi.org/10.5281/zenodo.11474109)


### Main Functions
* **Conditional_attribute_adder():** Adds a target attribute to a synthetic population by fitting it to a contingency table, optionally using iterative proportional fitting (IPF) with margins.
* **household_person_margins():** Converts published household counts for each spatial unit into a person-level margin, so household counts constrain the population.
* **assign_households():** Partitions the population into households, matching partners and placing children without altering any attribute already fitted.
* **verify_households():** Reports the result against the distributions and household counts it was given.


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

# but it also works without the eduneigh_df

```

#### Contingency tables that cover only part of the population

Education level is only tabulated from age 15 onwards, so `edu_sex_age_statistics.csv`
has no rows for the `age0_15` group at all. The function leaves agents it has no
distribution for as `NA` and reports them, rather than inventing a value, so those
children come out of the step above without an education level. Assign them afterwards
according to whatever the missing category means in your data - here, no completed
education yet:

```r
agent_df$education_level[agent_df$age_group == "age0_15"] = "low"

# check that nobody is left unassigned
sum(is.na(agent_df$education_level))
```

The same applies to any attribute whose source table describes a subpopulation: fit the
group the data covers, then fill in the remainder explicitly. The function will warn that
the margin has categories the contingency table has no cell for, which is exactly what is
happening here and is expected.


### you can look at the examplescript.R script in the example folder for an application of the functions in the package and example data to run it.

## Households

Once enough attributes have been added to the individuals, they can be partitioned into
households. The population is left exactly as it was fitted: household formation decides
only who ends up with whom, never what anyone is.

### Declaring what the roles mean

Every value of the household-position column is declared once, and nothing downstream refers
to a category name directly. Replace the level names to use the package outside the Dutch
context.

```r
structure <- household_structure(
  hh_role("LivingAlone",            kind = "partner", type = "single",        n_adults = 1),
  hh_role("PartnerNoChildren",      kind = "partner", type = "couple",        n_adults = 2),
  hh_role("PartnerWithChildren",    kind = "partner", type = "couple_kids",   n_adults = 2),
  hh_role("SingleParent",           kind = "partner", type = "single_parent", n_adults = 1),
  hh_role("ChildAtHome",            kind = "child",
          type = c("couple_kids", "single_parent")),
  hh_role("OtherHouseholdMember",   kind = "attached"),
  hh_role("InstitutionalHousehold", kind = "excluded")
)
```

A `partner` role forms the adult core of a household, and `n_adults = 2` means it has to be
matched into a couple. A `child` role is grouped into sibling sets and matched to a core. An
`attached` role joins an already-formed household, which is how a published mean household
size is reached. An `excluded` role is left out entirely, for people who do not live in a
private household.

### Turning published household counts into person-level margins

Spatial statistics normally count households while the population counts people, so the two
cannot be compared until one is converted. Given how many people of each role a household of
each type holds, the conversion is exact, and the result is an ordinary margin that
`Conditional_attribute_adder()` can fit the role against. Household counts then constrain the
population, rather than being whatever the population happens to imply.

```r
m <- household_person_margins(
  spatial_households = spatial,     # area, category, count
  category_split     = split_df,    # category to household type, per region
  composition        = composition, # household type, role, members per household
  structure          = structure,
  area = "NeighbCode", region = "municipality",
  area_region = neigh[, c("NeighbCode", "municipality")],
  residents   = data.frame(NeighbCode = neigh$NeighbCode, count = neigh$PopulationTotal)
)

agent_df <- Conditional_attribute_adder(
  df = agent_df, df_contingency = position_contingency,
  target_attribute = "household_role", group_by = c("NeighbCode"),
  margins = list(ageneigh, sexneigh, m$margin),
  margins_names = c("age_group", "sex", "household_role")
)
```

Read the `residual` element rather than discarding it. Household statistics usually cover
private households only, so people living in institutions show up as a shortfall: thinly
across every area, and heavily in the few areas holding an institution. That makes the
residual a usable margin for a role of kind `"excluded"`, and it identifies the areas where
household formation should not be attempted.

### Matching determinants

What governs who ends up with whom is supplied as a list, and every entry is optional. A
population carrying nothing but ages can still be matched.

```r
age_gap <- match_on_gap("age",
  distribution = gap_distribution(
    from  = c(-20, -10,   -5,    -1,    0,     1,     5,   10,   20),
    to    = c(-40, -20,  -10,    -5,    0,     5,    10,   20,   40),
    count = c(133, 496, 2276, 10375, 8023, 29171, 12331, 3438, 1180)),
  orient_by = "sex", orient_reference = "male")

sex_mix <- match_on_pairing("sex", data.frame(
  level_a = c("male", "male", "female"),
  level_b = c("female", "male", "female"),
  count   = c(68354, 793, 994)))

origin_homogamy <- match_on_pairing("migration_background", origin_pairs)
```

Bands carry their sign in `from` and `to`, so a partner five to ten years younger is
`from = -10, to = -5` and there is no label to parse. A pairing determinant takes a table of
level combinations and is adjusted to the people present in each area before it is used, so a
national homogamy table can be applied to a neighbourhood whose composition differs from the
national one. Any number of pairing determinants may be supplied. They are applied by
nesting, and the gap determinant orders the match inside every resulting cell.

### Forming the households

```r
out <- assign_households(
  agent_df, structure, group_by = c("PC4", "district", "municipality"),
  couple_determinants    = list(age_gap, origin_homogamy),
  child_determinants     = list(mother_gap),
  sex_pairing            = sex_mix,
  children_per_household = kids,
  sibling_spacing        = spacing,
  parent_reference       = list(column = "sex", level = "female"),
  target_household_size  = size_target
)

verify_households(out, structure = structure,
                  couple_determinants = list(age_gap, origin_homogamy),
                  child_determinants  = list(mother_gap),
                  target_households   = m$households)
```

`group_by` is a ladder from the finest spatial level to the coarsest. Matching is attempted at
the finest level, and anyone left without a partner is carried up to the next. Each relaxation
is counted and reported, and no role is ever rewritten to force a match, so the distributions
fitted beforehand survive the step intact.

### Bordering areas

Climbing straight from a neighbourhood to a municipality is a long way to look for a partner.
Supplying `neighbours` inserts a rung in between, so a match is first sought across one shared
boundary. On the Amsterdam buurten that narrows the fallback pool from about a million people
to about thirteen thousand:

| pool | median people |
|---|---|
| one buurt | 1,725 |
| buurt plus the buurten it borders | 13,315 |
| whole municipality | 1,078,075 |

The edge list is a data frame whose first two columns are pairs of areas. Reading it from a
file means adjacency can be edited where geometry misleads, for instance where a canal or a
motorway separates two polygons that touch. Derive a starting point from a shapefile with:

```r
tch <- sf::st_touches(sf::st_make_valid(buurten))
nb  <- do.call(rbind, lapply(seq_along(tch), function(i)
  if (length(tch[[i]]))
    data.frame(a = buurten$buurtcd[i], b = buurten$buurtcd[tch[[i]]])))

out <- assign_households(agent_df, structure, group_by = c("NeighbCode", "municipality"),
                         neighbours = nb, ...)
```

Pairs of bordering areas are offered in random order, so no area is systematically served
first, and an agent matched across one boundary is not offered again on the next. Areas missing
from the list, and islands with no neighbour, fall through to the next column as before.

On the same run this moved most relaxations off the municipality rung:

| relaxed at | without an edge list | with one |
|---|---|---|
| bordering areas | not available | 3,867 agents |
| municipality | 4,163 agents | 371 agents |
| households formed across a boundary | 2,016 | 1,232 |

### One household, one place

A household occupies one dwelling, so by default its members move to a single area. The area is
chosen per pool, splitting the households formed across a boundary evenly between its two
sides, so the moves between any two areas cancel rather than being corrected afterwards. On the
run above, 1,503 agents moved out of 1,055,066, the net flow summed to exactly zero, and no
area gained or lost more than 16 people on net.

This is the one place where household formation touches an attribute the population was fitted
to, so the agents moved and the net flow per area are both in the report. Pass
`relocate = FALSE` to keep everyone where they were fitted, at the cost of households whose
members live in two places.

Children are handled child-first: sibling sets are formed to sizes that reproduce the
published distribution of children per household, then matched to adult cores on the
parent-child gap.

A `floor` on a gap determinant is a hard constraint on who may be paired, not only a bound on
the drawn target. It binds the youngest adult of the household, so no adult ends up closer in
age to the children than the floor allows, whichever partner `parent_reference` names. Where
no candidate in a cell satisfies it, the match is refused rather than forced: those agents
rise up the spatial ladder and anything still unplaced is counted in the report.

Matching at a wider level pairs people from different areas. By default the resulting
household is moved into one of them, as described under One household, one place below.

### What to expect from the fit

Each person is given a target partner value drawn from the requested distribution and matched
to the nearest person still free, processed in random order. Pairing the two sides in sorted
order would be cheaper, but it cannot impose a target distribution of differences: where both
sides have similar value distributions the sorted coupling maps each rank onto its own rank
and cancels the drawn gaps.

Agreement is limited by how many people a cell holds, because a target can only be reproduced
as far as the available values allow. Measured on synthetic pools against a realistic partner
age-gap target, as total variation distance between the realised and target distributions:

| people per cell | total variation distance |
|---|---|
| 50 | 0.19 |
| 200 | 0.14 |
| 1000 | 0.11 |
| 5000 | 0.10 |

Every pairing determinant multiplies the number of cells, so homogamy is paid for in matching
precision on the gap. Adding origin homogamy to a neighbourhood of a few hundred couples
splits it into cells of a few dozen. That is a real trade-off, not a defect, and
`verify_households()` reports both sides of it so the choice can be made on evidence.

A run over 1,055,066 agents in 564 neighbourhoods takes about 40 seconds, or about 60 with an
adjacency edge list, and places 99.8 percent of them. The agents left over are those a hard gap floor refused rather than paired
implausibly, and they are itemised in the report.

### Data sources for the Netherlands

| What it supplies | CBS table |
|---|---|
| Household position by sex, at 4-digit postcode | 83504NED |
| Household composition, size and children, per municipality | 71486ned |
| Children per household by age class of the child | 71487ned |
| Household size crossed with position | 82905NED |
| Partner age gap, in signed bands | 60036ned |
| Couple sex composition | 37772ned |
| Mother age at birth by birth order, giving the parent-child gap and sibling spacing | 37744ned |
| Children at home by which parents they live with | 85729NED |
| People in institutional households | 82887NED |
| Partner origin, for homogamy | 85631NED |
| Partner education, for homogamy | 85834NED |

The age-gap and sex-composition tables count marriages contracted in a year rather than the
stock of couples, and they exclude unmarried cohabitation. The shape is right, the level is a
newlywed level.

## License
This package is licensed under the MIT License.
