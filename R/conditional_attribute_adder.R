library(dplyr)

#' Add a Target Attribute to a Synthetic Population Using Contingency Table Fitting
#'
#' This function adds a target attribute to a synthetic population by fitting it to an input contingency table.
#' If margins are provided, the function will apply iterative proportional fitting (IPF) to adjust the target
#' attribute distribution based on these margins. After the attribute is added, the function performs validation checks.
#'
#' @param df A data frame representing the synthetic population, where each row corresponds to an individual agent.
#' @param df_contingency A data frame representing the contingency table that contains the target attribute's distribution.
#' This should include the column for the target attribute and any other grouping variables.
#' @param target_attribute A string specifying the name of the attribute to be added to the synthetic population. 
#' This attribute must be present in the `df_contingency`.
#' @param group_by if no margins are provided, pick a a vector of column names that exist in both `df` and `df_contingency`. These are the attributes 
#' by which the population and contingency table are grouped. If margins are provided pick a column  name 
#' that exists in both the 'df' and all the 'margins' subtables. These are the attributes 
#' by which the population and margins tables are grouped and whose subgroup marginal distributions are used to fit the contingency table using IPF. Group_by often represent spatial or categorical splits.
#' @param margins An optional list of data frames containing marginal distributions to be used for IPF. If NULL, 
#' only the contingency table is used.
#' @param margins_names An optional list of vectors containing the names of the margin columns, corresponding 
#' to the data frames in `margins`. Each vector should list the margin names for the respective data frame.
#'
#' @details This function adds a target attribute (e.g., age, income level, etc.) to a synthetic population, based on 
#' its distribution in a provided contingency table. If marginal distributions are supplied, IPF is applied to adjust the 
#' population contingency distribution according to the provided margins.
#'
#' The function works in the following steps:
#' 1. **Initial Data Validation**: It checks that all `group_by` columns are present in the synthetic population (`df`).
#' 2. **IPF or Contingency Table Fit**: If margins are provided, IPF is used to adjust the contingency table to match the 
#' margin distributions. Otherwise, the contingency table is applied directly.
#' 3. **Attribute Assignment**: For each group defined by `group_by`, and if margins provided also by the additional contingency variables, the target attribute values are assigned to agents in the 
#' synthetic population based on their distribution in the contingency table or the IPF-adjusted margins.
#' 4. **Validation**: After assigning the attribute, the function verifies that the added attribute's distribution matches the 
#' original contingency table.
#'
#' @return A data frame representing the synthetic population, now with the target attribute added.
#'
#' @examples
#' # Example synthetic population data
#' df_synthetic <- data.frame(
#'   neighb_code = c("neigh_123", "neigh_123", "neigh_456"),
#'   sex = c("male", "female", "male"),
#'   age_group = c("25-45", "15-25", "45-65")
#' )
#'
#' # Example contingency table
#' df_contingency <- data.frame(
#'   age_group = c("0-15", "15-25", "25-45", "45-65", "65+"),
#'   sex = c("male", "female", "male", "female", "male"),
#'   migrationbackground = c("Dutch", "Dutch", "Non-Dutch", "Dutch", "Non-Dutch"),
#'   count = c(19, 31, 66, 54, 29)
#' )
#' 
#' margins <- list(
#'   age_group = data.frame(
#'     neighb_code = c("neigh_123", "neigh_123", "neigh_123", "neigh_123", 
#'                    "neigh_123", "neigh_456", "neigh_456", "neigh_456", 
#'                      "neigh_456", "neigh_456"),
#'     age_group = c("0-15", "15-25", "25-45", "45-65", "65+", "0-15", 
#'                    "15-25", "25-45", "45-65", "65+"),
#'     count = c(50, 100, 150, 200, 75, 40, 90, 130, 180, 60)
#'   ),
#'   sex = data.frame(
#'     neighb_code = c("neigh_123", "neigh_123", "neigh_456", "neigh_456"),
#'     sex = c("male", "female", "male", "female"),
#'     count = c(250, 200, 220, 180)
#'   )
#' )
#'
#' # Apply the function without margins
#' df_updated <- Conditional_attribute_adder(
#'   df = df_synthetic, 
#'   df_contingency = df_contingency, 
#'   target_attribute = "migrationbackground", 
#'   group_by = c("neighb_code"),
#'   margins = margins,
#'   margins_names = c("age_group", "sex")
#' )
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom stats aggregate as.formula ave chisq.test complete.cases setNames
#' @export
Conditional_attribute_adder <- function(df, df_contingency, target_attribute, 
                              group_by = NULL, margins = NULL, margins_names = NULL) {
    # Reset and clean the synthetic population dataframe
    if ("index" %in% colnames(df)) {
        df <- df[ , !colnames(df) %in% "index"]
    }
    if ("level_0" %in% colnames(df)) {
        df <- df[ , !colnames(df) %in% "level_0"]
    }

    if (!all(group_by %in% colnames(df))) {
    stop("One or more columns in 'group_by' do not exist in the dataframe.")
    }
    if (!target_attribute %in% colnames(df_contingency)) {
    stop("The target attribute does not exist in df_contingency.")
    }

    # Add a new empty column for the target attribute
    df[[target_attribute]] <- NA
    
    # Determine the margins group if margins are provided
    if (!is.null(margins)) {
        print("margins provided: using IPF")
        contingency_cols <- colnames(df_contingency)
        uncoveredcontingency <- contingency_cols[(!contingency_cols %in% margins_names) & (contingency_cols != "count")]
        if (length(uncoveredcontingency) == 0) {
          uncoveredcontingency <- NULL
        } else{
          print(paste("uncoveredcontingency: ", uncoveredcontingency))
          }
        uncoveredmargins <- margins_names[!margins_names %in% contingency_cols]
        if (length(uncoveredmargins) > 0) {
          print(paste("Warning: Margins", uncoveredmargins, "not found in the contingency table. It will be ignored."))
          margins <- margins[margins_names %in% contingency_cols]
          margins_names <- margins_names[margins_names %in% contingency_cols]
        }

        margins_group <- unique(unlist(lapply(c(margins_names, uncoveredcontingency), function(x) x[x %in% colnames(df)])))
        marginorder <-lapply(margins_names, function(col_name) {
            unique(df_contingency[[col_name]])
        })
    } else {
        print("no margins provided: using only supplied contigency table")
        margins_group <- group_by
        marginorder <- NULL
        uncoveredcontingency <- NULL
    }
    
    print(paste("margins_group: ", margins_group))
    # Main loop over the group-by attributes
    uniquegroups  <- unique(df[[group_by]])
    for (group_name in uniquegroups) {
        print(paste("Processing group", group_name, "which is", which(uniquegroups == group_name), "of", length(uniquegroups)))
        df_contingency_group <- GenSynthPop::ipf_fit_contingency_table(df_contingency = df_contingency, group_name = group_name, 
                                                            group_by = group_by, margins = margins , 
                                                            margins_names = margins_names, 
                                                            marginorder = marginorder, 
                                                            uncoveredcontingency = uncoveredcontingency)
        group_fractions <- GenSynthPop::get_group_fractions( df_contingency = df_contingency_group, group_by = group_by, target_attribute = target_attribute, margins_names = margins_names, margins_group = margins_group )
        if (is.null(margins)) {
          group_values <- GenSynthPop::get_agent_values_from_fractions(group_fractions= group_fractions, group_agent_count= nrow(df[df[group_by] == group_name,]), target_attribute= target_attribute)
          df[df[[group_by]] == group_name, target_attribute] <- group_values
        } else {
          sub_group_by <- margins_group[!(margins_group %in% group_by) & margins_group != target_attribute]
          sub_group_combinations <- as.data.frame(expand.grid(lapply(sub_group_by, function(col) unique(df[[col]]))))
          colnames(sub_group_combinations) <- sub_group_by
          for (sub_group_comb_indx in seq_len(nrow(sub_group_combinations))) {
              current_combination <- as.vector(unlist(sub_group_combinations[sub_group_comb_indx, sub_group_by]))
              mask <- GenSynthPop::get_group_mask(df, group_name, group_by) 
              for (sub_group in sub_group_by) {
                  mask <- mask & GenSynthPop::get_group_mask(df,sub_group_combinations[sub_group_comb_indx,sub_group], sub_group)
              }
              if (!any(mask)) next
              sub_group_fractions <- group_fractions
              for (i in seq_along(sub_group_by)) {
                sub_group_fractions <- sub_group_fractions[sub_group_fractions[[sub_group_by[i]]] == current_combination[i], ]
              }
              sub_group_fractions <- sub_group_fractions[, c(target_attribute,"fraction")]
              group_values <- GenSynthPop::get_agent_values_from_fractions(group_fractions=sub_group_fractions, group_agent_count= nrow(df[mask, ]), target_attribute = target_attribute)
              df[mask, target_attribute] <- group_values
          }
        }
    }
    print("Verifying attribute")
    GenSynthPop::verify_target_attribute(df, df_contingency, target_attribute, margins_group)
    return(df)
}
