#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 1. Search strategy conduct and reporting

#Set up -----------------------------------------
#Install packages
#install.packages(c("tidyverse", "ggplot2", "here"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(here)

#Import data
ccmethods_data <- read.csv(here("../data/CC-methods-data-extraction-final-20230227-clean-recode.csv"), na = "NA")


#Search quality -----------------------------------

##In text results ----

#Percent using boolean operators correctly 
prop.table(table(ccmethods_data$boolean)) * 100
table(ccmethods_data$boolean, useNA = "always")

#Percent using syntax correctly
prop.table(table(ccmethods_data$syntax)) * 100
table(ccmethods_data$syntax, useNA = "always")

#Percent using subject headings correctly
prop.table(table(ccmethods_data$subhead)) * 100
table(ccmethods_data$subhead, useNA = "always")

#Percent using phrase searching correctly
prop.table(table(ccmethods_data$phrase)) * 100
table(ccmethods_data$phrase, useNA = "always")

#Percent using appropriate keywords and truncation
prop.table(table(ccmethods_data$keyword_var)) * 100
table(ccmethods_data$keyword_var, useNA = "always")

#Percent using justifying the use of limits
prop.table(table(ccmethods_data$limit_just)) * 100
table(ccmethods_data$limit_just, useNA = "always") #note this should remove missing values for those not using limits

#Percent using hedges or adapted searches
prop.table(table(ccmethods_data$hedge)) * 100
table(ccmethods_data$hedge, useNA = "always")

prop.table(table(ccmethods_data$adapt)) * 100
table(ccmethods_data$adapt, useNA = "always")

#Percent reported that peer review was conducted
table(ccmethods_data$peer_review, useNA = "always")


#Search reporting -------------------------------------------

##Figure 3. Database searching ----
##Bar chart: Number reporting at least one database search, all database searches, dates of searches, details of gray literature searches, details of free search engine searches 
##Relevant variables: strategy_rep (new), date_searches, gray_search, 

#Create a new variable for how many database searches were reported based on strategy_one and strategy_all
ccmethods_data <- ccmethods_data %>% 
  mutate(strategy_rep = case_when(strategy_one == "no" | strategy_one == "unclear" ~ "None",
                                  strategy_one == "yes" & strategy_all == "no" ~ "Partial",
                                  strategy_one == "yes" & strategy_all == "unclear" ~ "Partial",
                                  strategy_all == "yes" ~ "Full"))

#Create subset of only needed variables
search_subset <- ccmethods_data[, c("strategy_rep", "date_searches", "gray_search")]

#Convert all to factors, recode and set level orders
search_subset$strategy_rep <- factor(search_subset$strategy_rep, levels=ordered(c("Full", "Partial", "None")))

search_subset$date_searches <- recode_factor(search_subset$date_searches, 
                                             "General date range of searches given" = "Partial", 
                                             "No dates given" = "None",
                                             "Specific dates of each search given" = "Full")

search_subset$date_searches <- factor(search_subset$date_searches, levels=ordered(c("Full", "Partial", "None")))

search_subset$gray_search <- recode_factor(search_subset$gray_search, 
                                           "General gray literature approach stated in text only" = "Partial",
                                           "Some exact search strategies reported" = "Partial",
                                           "Other" = "Partial",
                                           "No reporting of how gray literature was searched" = "None",
                                           "All exact search strategies reported" = "Full")

search_subset$gray_search <- factor(search_subset$gray_search, levels=ordered(c("Full", "Partial", "None")))

#Transform to long format on variable name
search_long <- search_subset %>%
  gather(variable_name, value)

#Group by variable name and count
search_counts <- search_long %>%
  group_by(variable_name, value) %>%
  count()

# Define the desired order of the groups
var_order <- c("strategy_rep", "date_searches", "gray_search")

# Convert the groups to a factor with the desired order and recode
search_counts$variable_name_ordered <- factor(search_counts$variable_name, levels = var_order)
search_counts$variable_name_ordered <- recode_factor(search_counts$variable_name_ordered, 
                                                     strategy_rep = "All database search strategies", 
                                                     date_searches = "Exact dates of all searches",
                                                     gray_search = "All gray literature search strategies")

#Define order of values
search_counts$value <- factor(search_counts$value, levels = ordered(c("Full", "Partial", "None")))

#Set colors
my_colors <- c("green", "yellow", "red")

#Plot stacked grouped bar chart
pt_db_search <- ggplot(search_counts, aes(fill=value, y=n, x=variable_name_ordered)) + 
                geom_bar(position="stack", stat="identity") +
                scale_fill_manual(values = my_colors) +
                labs(x = "", y = "Number of Reviews", fill = "") +
                scale_x_discrete(limits = rev(levels(search_counts$variable_name_ordered)), labels = function(x) str_wrap(x, width = 15)) +
                theme_light() +
                theme(axis.text.x = element_text(size = 11)) +
                #theme(text = element_text(size=15)) +
                coord_flip()

ggsave(here("../plots/fig3.png"), plot = pt_supp, width = 6, height = 4, units = "in", dpi = 300) 


##In text results ----
##More detail about database descriptions
table(ccmethods_data$strategy_one, useNA = "always")
prop.table(table(ccmethods_data$strategy_one)) * 100

table(ccmethods_data$strategy_all, useNA = "always")
prop.table(table(ccmethods_data$strategy_all)) * 100

table(ccmethods_data$date_searches, useNA = "always")
prop.table(table(ccmethods_data$date_searches)) * 100

table(ccmethods_data$gray_search, useNA = "always")
prop.table(table(ccmethods_data$gray_search)) * 100

#Percent reporting how free search engines were used
prop.table(table(ccmethods_data$free_search)) * 100
table(ccmethods_data$free_search, useNA = "always")


#Conduct and reporting over time--------------------------------
#For select variables, small multiples line graph based on date of search

#Variables to consider: handsearching, experts, Google, Google Scholar,
#list of databases, complete search strategies, boolean, subhead,
#keyword variation, phrases, syntax, limit justification, 
#list of gray, gray search, backward, review references,
#forward, reference software, deduplication, search update,
#different types of sources

##Figure 4a. Conduct variables over time ----
##Subset of conduct variables along with search year for temporal analysis 
subset_conduct <- ccmethods_data %>%
  select(search_year, free_gs, engine_google, handsearch, experts, boolean, 
         subhead, keyword_var, phrase, syntax, backward, reviews,
         forward)

# Reshape the dataframe from wide to long format
conduct_long <- subset_conduct %>%
  pivot_longer(cols = -search_year, names_to = "variable", values_to = "value")

# Calculate the percentage of 'yes' values for each variable and year
result_conduct_df <- conduct_long %>%
  group_by(search_year, variable) %>%
  summarize(percent = mean(value == "yes", na.rm = TRUE) * 100)

result_conduct_df$percent <- round(result_conduct_df$percent, 1)

#Filter out pre-2015 and post-2020 due to low numbers
time_cd_df_1520 <- result_conduct_df %>% filter(search_year >= 2015 & search_year <= 2020)

#Recode variable names and reorder
time_cd_df_1520 <- time_cd_df_1520 %>% 
  mutate(variable = 
           forcats::fct_recode(variable,
                               "Backward searching" = "backward",
                               "Boolean operators" = "boolean",  
                               "Google searched" = "engine_google",
                               "Experts contacted" = "experts",
                               "Forward searching" = "forward",
                               "Google Scholar searched" = "free_gs",
                               "Handsearching" = "handsearch",
                               "Keyword variation" = "keyword_var",
                               "Phrase searching" = "phrase",
                               "Related reviews"= "reviews",
                               "Database subheadings" = "subhead",
                               "Database syntax"= "syntax"))

time_cd_df_1520 <- time_cd_df_1520 %>% 
  mutate(variable = factor(variable,
                           levels = c("Boolean operators", "Keyword variation", "Database syntax",
                                      "Phrase searching", "Database subheadings", "Google searched",
                                      "Google Scholar searched", "Handsearching", "Backward searching",
                                      "Forward searching", "Related reviews", "Experts contacted")))

#Small multiple plot of conduct variables
pt_time_conduct <- ggplot(time_cd_df_1520, aes(x = search_year, y = percent)) +
                    geom_line() +
                    facet_wrap(~ variable, ncol = 3, scales="fixed") +
                    labs(x = "Year", y = "Percent") +
                    geom_area(fill = "gray87") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                    panel.grid.minor = element_blank())  

ggsave(here("../plots/fig4a.png"), plot = pt_time_report, width = 7, height = 6, units = "in", dpi = 300) 


##Figure 4b. Reporting variables over time ----

#Recode db_list to none (no db given), some (partial--lacking sub-databases), all (all including sub-db)
ccmethods_data$db_list_gp <- factor(ccmethods_data$db_list) #Convert to factor
levels(ccmethods_data$db_list_gp) #Print levels
levels(ccmethods_data$db_list_gp) <- c("some", "some", "all") #Recode to new values
levels(ccmethods_data$db_list_gp) #Print new levels


#Subset of reporting variables with publication year for temporal analysis 
subset_report <- ccmethods_data %>%
  select(pub_year, gray_url, date_searches, db_platform, strategy_all, gray_list, 
         gray_search, forward_method, refman_software, deduplicate, num_records, 
         db_list, update_search)

subset_report$gray_url <- recode_factor(subset_report$gray_url, 
                                        "all" = "yes", 
                                        "none" = "no",
                                        "some" = "partial")

subset_report$db_platform <- recode_factor(subset_report$db_platform, 
                                           "all" = "yes", 
                                           "none" = "no",
                                           "some" = "partial")

subset_report$db_list <- recode_factor(subset_report$db_list, 
                                          "Partially with some umbrella databases given with no detail" = "partial", 
                                          "Partially with some generic platforms given with no detail" = "partial", 
                                          "Yes: All databases including sub-databases of umbrella databases" = "yes")

subset_report$gray_list <- recode_factor(subset_report$gray_list, 
                                         "all" = "yes", 
                                         "none" = "no",
                                         "some" = "partial")

subset_report$date_searches <- recode_factor(subset_report$date_searches, 
                                             "General date range of searches given" = "partial", 
                                             "No dates given" = "no",
                                             "Specific dates of each search given" = "yes")

subset_report$gray_search <- recode_factor(subset_report$gray_search, 
                                           "General gray literature approach stated in text only" = "partial",
                                           "Some exact search strategies reported" = "partial",
                                           "Other" = "partial",
                                           "No reporting of how gray literature was searched" = "no",
                                           "All exact search strategies reported" = "yes")



# Reshape the dataframe from wide to long format
report_long <- subset_report %>%
  pivot_longer(cols = -pub_year, names_to = "variable", values_to = "value")

# Calculate the percentage of 'yes' values for each variable and year
result_report_df <- report_long %>%
  group_by(pub_year, variable) %>%
  summarize(percent = mean(value == "yes", na.rm = TRUE) * 100)

result_report_df$percent <- round(result_report_df$percent, 1)

#Filter out pre-2015 and post-2020 due to low numbers
time_rp_df_1520 <- result_report_df %>% filter(pub_year >= 2015 & pub_year <= 2020)

time_rp_df_1520 <- time_rp_df_1520 %>% 
  mutate(variable = 
           forcats::fct_recode(variable,
                               "Search updated" = "update_search",
                               "All databases" = "db_list",  
                               "Database platform" = "db_platform",
                               "Deduplication method" = "deduplicate",
                               "Forward citation method" = "forward_method",
                               "Gray lit sources" = "gray_list",
                               "Gray lit searches" = "gray_search",
                               "Website URLs" = "gray_url",
                               "Number of results" = "num_records",
                               "Ref Management Software"= "refman_software",
                               "All search strategies" = "strategy_all",
                               "Search date"= "date_searches"))

time_rp_df_1520 <- time_rp_df_1520 %>% 
  mutate(variable = factor(variable,
                           levels = c("All databases", "Database platform", "Gray lit sources",
                                      "Website URLs", "All search strategies", "Gray lit searches",
                                      "Search date", "Number of results", "Search updated", 
                                      "Forward citation method", "Deduplication method", "Ref Management Software")))

#Small multiple plot of conduct variables
pt_time_report <- ggplot(time_rp_df_1520, aes(x = pub_year, y = percent)) +
                    geom_line() +
                    facet_wrap(~ variable, ncol = 3, scales="fixed") +
                    labs(x = "Year", y = "Percent") +
                    geom_area(fill = "gray87") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                    panel.grid.minor = element_blank()) +
                    scale_y_continuous(limits = c(0, 100))

ggsave(here("../plots/fig4b.png"), plot = pt_time_report, width = 7, height = 6, units = "in", dpi = 300) 
