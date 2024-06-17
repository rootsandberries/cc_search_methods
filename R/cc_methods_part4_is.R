#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 4. Involvement of Information Specialists

#Set up -----------------------------------------

#Install packages
#install.packages(c("tidyverse", "ggplot2", "ggpubr", "here"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(RColorBrewer)
library(here)

#Import data
ccmethods_data <- read.csv(here("./data/CC-methods-data-extraction-final-20240508-clean-recode.csv"), na = "NA")


#IS involvement -------------------------------------

#How IS involvement is reported

#First recode db_list to none (no db given), some (partial--lacking sub-databases), all (all including sub-db)
ccmethods_data$db_list_gp <- factor(ccmethods_data$db_list) #Convert to factor
levels(ccmethods_data$db_list_gp) #Print levels
levels(ccmethods_data$db_list_gp) <- c("some", "some", "all") #Recode to new values
levels(ccmethods_data$db_list_gp) #Print new levels

#Add variable to indicate any information specialist involvement

ccmethods_data_is <- ccmethods_data %>% mutate(infospec=if_else(infosp_coauthor == "yes" | infosp_involve == "yes", "yes", "no"))

## In-text information ----

table(ccmethods_data_is$infosp_involve)
prop.table(table(ccmethods_data_is$infosp_involve)) * 100

table(ccmethods_data_is$infosp_methods)
prop.table(table(ccmethods_data_is$infosp_methods)) * 100

table(ccmethods_data_is$infosp_roles)
prop.table(table(ccmethods_data_is$infosp_roles)) * 100

table(ccmethods_data_is$infosp_coauthor)
prop.table(table(ccmethods_data_is$infosp_coauthor)) * 100

table(ccmethods_data_is$infosp_ackn)
prop.table(table(ccmethods_data_is$infosp_ackn)) * 100

table(ccmethods_data_is$kugley)
prop.table(table(ccmethods_data_is$kugley)) * 100


##Figure 6: Number reporting involvement of librarians at different levels (co-authorship, consulted, no IS) ----
##Count bars for number yes for each type of involvement
##Relevant variables: infosp_coauthor, infosp_methods, infosp_ackn, infosp_roles

#Combine roles, acknowledgement and methods (as IS consulted but not co-author)
ccmethods_data_is <- ccmethods_data_is %>%
  mutate(infosp_rolesac = case_when(infosp_roles == "yes" | infosp_ackn == "yes" | infosp_methods == "yes" ~ "yes",
                                    infosp_roles == "no" & infosp_ackn == "no" & infosp_methods == "no" ~ "no"))

ccmethods_data_is <- ccmethods_data_is %>%
  mutate(infosp_consult = case_when(infosp_rolesac == "yes" & infosp_coauthor == "no" ~ "yes",
                                    infosp_rolesac == "yes" & infosp_coauthor == "yes" ~ "no",
                                    infosp_rolesac == "no" & infosp_coauthor == "no" ~ "no", 
                                    infosp_rolesac == "no" & infosp_coauthor == "yes" ~ "no"))

table(ccmethods_data_is$infosp_consult)
prop.table(table(ccmethods_data_is$infosp_consult)) * 100

#Convert the infosp_involve column to infosp_none (so yes = no and vice versa)
ccmethods_data_is <- ccmethods_data_is %>%
  mutate(infosp_none = case_when(infosp_involve == "yes" ~ "no",
                                 infosp_involve == "no" ~ "yes"  ))

#Subset the needed variables
db_subset_infosp <- ccmethods_data_is[, c("infosp_consult", "infosp_coauthor", "infosp_none", "cg")]


#Convert all to factors and set level orders
db_subset_infosp$infosp_consult <- factor(db_subset_infosp$infosp_consult, levels=ordered(c("yes", "no")))
db_subset_infosp$infosp_coauthor <- factor(db_subset_infosp$infosp_coauthor, levels=ordered(c("yes", "no")))
db_subset_infosp$infosp_none <- factor(db_subset_infosp$infosp_none, levels=ordered(c("yes", "no")))

#Transform to long format on variable name
db_long_infosp <- db_subset_infosp %>%
  gather(variable_name, value, -cg)

#Remove second coordinating group name from cg column for cleaner representation of the data
db_long_infosp$cg <- sub(";.*", "", db_long_infosp$cg)

#Group by variable name and count number of yes values
df_counts_infosp <- db_long_infosp %>% 
  filter(value == "yes") %>% 
  group_by(variable_name, cg) %>% 
  summarise(count = n()) 

# Convert the groups to a factor with the desired order and recode
df_counts_infosp$variable_name_ordered <- recode_factor(df_counts_infosp$variable_name, 
                                                        infosp_coauthor = "Co-authorship",
                                                        infosp_consult = "IS consulted", 
                                                        infosp_none = "No IS Involvement")



#Plot stacked grouped bar chart
my_palette <- brewer.pal(9, "Set3")

pt_is <- ggplot(df_counts_infosp, aes(y=count, x=variable_name_ordered, fill=cg)) + 
          geom_bar(stat="identity") +
          labs(x = "", y = "Number of Reviews") +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
          theme_light() +
          theme(axis.text.x = element_text(size = 10)) +
          coord_flip() +
          theme(legend.position = "bottom") +
          guides(fill = guide_legend(nrow=3, byrow=TRUE,title = NULL)) +
          scale_fill_manual(values = my_palette)

ggsave(here("./plots/fig6.png"), plot = pt_is, width = 8, height = 5, units = "in", dpi = 300) 


#IS involvement, conduct and reporting ----

#Subset to needed variables
ccmethods_data_is_sub <- ccmethods_data_is[, c("id", "grey_url", "free_gs", "engine_google", "handsearch",
                                               "experts", "date_searches", "db_platform", "strategy_all", "boolean", "subhead", "keyword_var", "phrase", 
                                               "syntax", "hedge", "adapt", "grey_list", "grey_search", "free_search", "engine_search", "backward", "forward", 
                                               "reviews", "forward_method", "refman_software", "deduplicate", "num_records", "infosp_methods", 
                                               "infosp_rolesac", "infosp_coauthor", "infosp_involve", "kugley", "update_search", "db_list_gp", "infospec")]


#Recode variables to yes, no, some
ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(grey_url = case_when(
    grey_url == "all" ~ "yes",
    grey_url == "some" ~ "some",
    grey_url == "none" ~ "no",
    TRUE ~ as.character(grey_url)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(date_searches = case_when(
    date_searches == "Specific dates of each search given" ~ "yes",
    date_searches == "General date range of searches given" ~ "yes",
    date_searches == "No dates given" ~ "no",
    TRUE ~ as.character(date_searches)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(db_platform = case_when(
    db_platform == "all" ~ "yes",
    db_platform == "some" ~ "some",
    db_platform == "none" ~ "no",
    TRUE ~ as.character(db_platform)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(grey_list = case_when(
    grey_list == "all" ~ "yes",
    grey_list == "some" ~ "some",
    grey_list == "none" ~ "no",
    TRUE ~ as.character(grey_list)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(grey_search = case_when(
    grey_search == "General grey literature approach stated in text only" ~ "some",
    grey_search == "All exact search strategies reported" ~ "yes",
    grey_search == "No reporting of how grey literature was searched" ~ "no",
    grey_search == "Some exact search strategies reported" ~ "yes",
    TRUE ~ as.character(grey_search)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(free_search = case_when(
    free_search == "list of search terms provided but not exact strategy" ~ "some",
    free_search == "list of search terms provided but not exact strategy and number of pages/results screened reported" ~ "some",
    free_search == "exact search strategy/strategies reported" ~ "yes",
    free_search == "No details given" ~ "no",
    free_search == "exact search strategy/strategies reported and number of pages/results screened reported" ~ "yes",
    TRUE ~ as.character(free_search)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(engine_search = case_when(
    engine_search == "list of search terms provided but not exact strategy" ~ "some",
    engine_search == "list of search terms provided but not exact strategy and number of pages/results screened reported" ~ "some",
    engine_search == "exact search strategy/strategies reported" ~ "yes",
    engine_search == "No details given" ~ "no",
    engine_search == "exact search strategy/strategies reported and number of pages/results screened reported" ~ "yes",
    TRUE ~ as.character(free_search)  # Keep other values unchanged
  ))

ccmethods_data_is_sub <- ccmethods_data_is_sub %>%
  mutate(db_list_gp = case_when(
    db_list_gp == "all" ~ "yes",
    db_list_gp == "some" ~ "some",
    db_list_gp == "none" ~ "no",
    TRUE ~ as.character(db_list_gp)  # Keep other values unchanged
  ))

##Table of conduct variables by IS involvement----
##Create tables of percentage compliance by IS involvement. 

#Is a co-author
ccmethods_IS_coauthor <- ccmethods_data_is_sub %>% filter(infosp_coauthor == 'yes')

#Percentages
coauthor_impact_per <- ccmethods_IS_coauthor %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = '{.col}')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "percentage") %>% 
  mutate(percentage = round(percentage, 1))
#write.csv(coauthor_impact_per, "coauthor_impact_per.csv")

#Counts
coauthor_impact_count <- colSums(ccmethods_IS_coauthor == "yes", na.rm = TRUE)
coauthor_impact_count_df <- data.frame(variable = names(coauthor_impact_count), count = coauthor_impact_count)
#write.csv(coauthor_impact_count_df, "coauthor_impact_count.csv")

#Combine percentage and counts tables by variable name
coauthor_impact <- merge(coauthor_impact_count_df, coauthor_impact_per, by = 'variable', all = TRUE)

#Was mentioned in methods or acknowledgment, but not co-author
ccmethods_IS_mention <- ccmethods_data_is_sub %>% filter(infosp_involve == 'yes', infosp_coauthor == 'no')

#Percentages
mention_impact_per <- ccmethods_IS_mention %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = '{.col}')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "percentage") %>% 
  mutate(percentage = round(percentage, 1))
#write.csv(mention_impact, "mention_impact.csv")

#Counts
mention_impact_count <- colSums(ccmethods_IS_mention == "yes", na.rm = TRUE)
mention_impact_count_df <- data.frame(variable = names(mention_impact_count), count = mention_impact_count)
#write.csv(mention_impact_count_df, "mention_impact_count.csv")

#Combine percentage and counts tables by variable name
mention_impact <- merge(mention_impact_count_df, mention_impact_per, by = 'variable', all = TRUE)

#Is involved in some way
ccmethods_IS_involve <- ccmethods_data_is_sub %>% filter(infospec == 'yes')

#Percentage
involve_impact_per <- ccmethods_IS_involve %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = '{.col}')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "percentage") %>% 
  mutate(percentage = round(percentage, 1))
#write.csv(involve_impact, "involve_impact.csv")

#Is not an author
ccmethods_IS_no_author <- ccmethods_data_is_sub %>% filter(infosp_coauthor == 'no')

#Percentages
no_author_impact_per <- ccmethods_IS_no_author %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = '{.col}')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "percentage") %>% 
  mutate(percentage = round(percentage, 1))
#write.csv(no_author_impact, "no_author_impact.csv")

#No involvement at all
ccmethods_IS_none <- ccmethods_data_is_sub %>% filter(infospec == 'no')

#Percentages
none_impact_per <- ccmethods_IS_none %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = '{.col}')) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "percentage") %>% 
  mutate(percentage = round(percentage, 1))
#write.csv(none_impact, "none_impact.csv")

#Count
none_impact_count <- colSums(ccmethods_IS_none == "yes", na.rm = TRUE)
none_impact_count_df <- data.frame(variable = names(none_impact_count), count = none_impact_count)
#write.csv(none_impact_count_df, "none_impact_count.csv")

#Combine percentage and counts tables by variable name
none_impact <- merge(none_impact_count_df, none_impact_per, by = 'variable', all = TRUE)

#Total percent of each variable with yes value
percent_yes <- ccmethods_data_is_sub %>%
  summarise(across(everything(), ~ mean(. == 'yes', na.rm = TRUE) * 100, .names = 'Percent_{.col}')) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Percentage") %>% 
  mutate(Percentage = round(Percentage, 1))
#write.csv(percent_yes, "percent_yes.csv")

#Assemble tables from above outputs

##Supplementary File 5: Table of conduct variables by IS involvement
#Subset conduct columns only for coauthor, mention and no involvement
#Conduct variables for IS coauthor
conduct_coauth <- coauthor_impact %>% filter(variable == "boolean"|
                                               variable == "subhead"|
                                               variable == "keyword_var"|
                                               variable == "phrase"|
                                               variable == "syntax"|
                                               variable == "free_gs"|
                                               variable == "engine_google"|
                                               variable == "handsearch"|
                                               variable == "experts"|
                                               variable == "backward"|
                                               variable == "forward"|
                                               variable == "reviews"|
                                               variable == "update_search"|
                                               variable == "kugley")

#Conduct variables for IS mention
conduct_mention <- mention_impact %>% filter(variable == "boolean"|
                                               variable == "subhead"|
                                               variable == "keyword_var"|
                                               variable == "phrase"|
                                               variable == "syntax"|
                                               variable == "free_gs"|
                                               variable == "engine_google"|
                                               variable == "handsearch"|
                                               variable == "experts"|
                                               variable == "backward"|
                                               variable == "forward"|
                                               variable == "reviews"|
                                               variable == "update_search"|
                                               variable == "kugley")

#Conduct variables for no IS
#Conduct variables for IS coauthor
conduct_nois <- none_impact %>% filter(variable == "boolean"|
                                         variable == "subhead"|
                                         variable == "keyword_var"|
                                         variable == "phrase"|
                                         variable == "syntax"|
                                         variable == "free_gs"|
                                         variable == "engine_google"|
                                         variable == "handsearch"|
                                         variable == "experts"|
                                         variable == "backward"|
                                         variable == "forward"|
                                         variable == "reviews"|
                                         variable == "update_search"|
                                         variable == "kugley")

#Merge all three dataframes
conduct_is_table1 <- merge(conduct_nois, conduct_mention, by = "variable", all = TRUE)
conduct_is_table <- merge(conduct_is_table1, conduct_coauth, by = "variable", all = TRUE)

#Rename variable names for table
conduct_is_table_rename <- conduct_is_table %>%
  mutate(variable = case_when(
    variable == "backward" ~ "Backward citation searching conducted",
    variable == "boolean" ~ "Boolean operators used correctly",
    variable == "engine_google" ~ "Google searched",
    variable == "experts" ~ "Experts contacted",
    variable == "forward" ~ "Forward citation searching conducted",
    variable == "free_gs" ~ "Google Scholar searched",
    variable == "handsearch" ~ "Handsearches conducted",
    variable == "keyword_var" ~ "Keyword variants used",
    variable == "kugley" ~ "Kugley et al. (2017) guidance cited",
    variable == "phrase" ~ "Phrase searching used correctly",
    variable == "reviews" ~ "References of related reviews searched", 
    variable == "subhead" ~ "Database subj heading/thesauri used", 
    variable == "syntax" ~ "Database syntax used correctly", 
    variable == "update_search" ~ "Search updated prior to publication"
  ))

#Reorder with custom order 
conduct_custom_order <- c("Boolean operators used correctly","Database subj heading/thesauri used","Keyword variants used",
                          "Phrase searching used correctly", "Database syntax used correctly", "Google Scholar searched",
                          "Google searched", "Handsearches conducted","Experts contacted",
                          "Backward citation searching conducted", "Forward citation searching conducted",
                          "References of related reviews searched", "Search updated prior to publication", "Kugley et al. (2017) guidance cited")

conduct_is_table_rename <- conduct_is_table_rename %>%
  mutate(variable = factor(variable, levels = conduct_custom_order)) %>%
  arrange(variable)

write_csv(conduct_is_table_rename, here("./data_outputs/ccmethods_isinvolve_conduct_table.csv"))

#Conduct Table
is_table_df <- read.csv(here("./data_outputs/ccmethods_isinvolve_conduct_table.csv"))
is_table <- is_table_df %>% gt() 

gt_tbl_conduct <- 
  gt(is_table_df) |>
  tab_header(
    title = "",
    subtitle = "Conduct of searches"
  ) |>
  tab_spanner(
    label = "No IS Involvement (n=37)",
    columns = c(count.x, percentage.x)
  ) |>
  tab_spanner(
    label = "IS consulted (n=42)",
    columns = c(count.y, percentage.y)
  ) |>
  tab_spanner(
    label = "IS co-author (n=32)",
    columns = c(count, percentage)
  ) |>
  tab_options(
    heading.subtitle.font.size = 16
  ) %>% 
  cols_label(variable="", count.x = "No.", percentage.x="%", count.y = "No.", percentage.y="%", count = "No.", percentage="%")

gt_tbl_conduct

#Save to image file
#Ensure that Google Chrome is running
gt_tbl_conduct |> gtsave(here("./plots/conduct_is_table.png"), expand = 10)

##Figure 7A. Line graph for IS impact on conduct variables----
#Subset only percentage columns from conduct_is_table
conduct_is_chart_df <- conduct_is_table[, c(1,3, 5, 7)]

#Transpose to long format grouping percentage columns by IS involvement
conduct_is_chart_long <- conduct_is_chart_df %>%
  pivot_longer(cols = c(percentage.x, percentage.y, percentage), 
               names_to = "group", 
               values_to = "percentage")

#Rename group names
conduct_is_chart_long <- conduct_is_chart_long %>%
  mutate(group = case_when(
    group == "percentage.x" ~ "No IS",
    group == "percentage.y" ~ "IS consulted",
    group == "percentage" ~ "IS co-author"))

conduct_is_chart_long <- conduct_is_chart_long %>%
  mutate(group = factor(group, levels = c("No IS", "IS consulted", "IS co-author")))

#Rename variable names for legend
conduct_is_chart_renamed <- conduct_is_chart_long %>%
  mutate(variable = case_when(
    variable == "backward" ~ "Backward citation search",
    variable == "boolean" ~ "Boolean operators correct",
    variable == "engine_google" ~ "Google searched",
    variable == "experts" ~ "Experts contacted",
    variable == "forward" ~ "Forward citation search",
    variable == "free_gs" ~ "Google Scholar searched",
    variable == "handsearch" ~ "Handsearches conducted",
    variable == "keyword_var" ~ "Keyword variants used",
    variable == "kugley" ~ "Kugley et al. (2017) guidance cited",
    variable == "phrase" ~ "Phrase searching correct",
    variable == "reviews" ~ "Related reviews searched", 
    variable == "subhead" ~ "Database subj headings used", 
    variable == "syntax" ~ "Database syntax correct", 
    variable == "update_search" ~ "Search updated"
  ))

#Reorder so that the legend list matches the order of values in the No IS column
custom_order_iscd <- c("Backward citation search", "Boolean operators correct", "Keyword variants used", "Phrase searching correct", "Experts contacted", "Related reviews searched", "Database syntax correct", "Google Scholar searched", "Database subj headings used", "Handsearches conducted", "Forward citation search", "Google searched", "Search updated", "Kugley et al. (2017) guidance cited")
conduct_is_chart_renamed$variable <- factor(conduct_is_chart_renamed$variable, levels = custom_order_iscd)


#Plot change in percentage over groups
#Create distinguishable palette for 14 variables
my_palette_14 <- c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1", "orchid1", "deeppink1", "blue1", 
  "darkturquoise", "green1", "yellow3", "brown"
)


pt_conduct_is <- ggplot(conduct_is_chart_renamed, aes(x = group, y = percentage, color = variable, group = variable)) +
                  geom_line() +
                  geom_point() +
                  labs(title = "",
                  x = "",
                  y = "Percent of reviews",
                  color = "Variable") +
                  theme_minimal() +
                  theme(legend.title=element_blank()) +
                  scale_color_manual(values = my_palette_14)

ggsave(here("./plots/fig7a.png"), plot = pt_conduct_is, width = 6, height = 4, units = "in", dpi = 300) 


##Table of reporting variables by IS involvement----
#Subset reporting columns only for coauthor, mention and no involvement
#Reporting variables for IS coauthor
report_coauth <- coauthor_impact %>% filter(variable == "strategy_all"|
                                              variable == "db_list_gp"|
                                              variable == "db_platform"|
                                              variable == "date_searches"|
                                              variable == "grey_list"|
                                              variable == "grey_search"|
                                              variable == "grey_url"|
                                              variable == "forward_method"|
                                              variable == "refman_software"|
                                              variable == "deduplicate"|
                                              variable == "num_records")

#Report variables for IS mention
report_mention <- mention_impact %>% filter(variable == "strategy_all"|
                                              variable == "db_list_gp"|
                                              variable == "db_platform"|
                                              variable == "date_searches"|
                                              variable == "grey_list"|
                                              variable == "grey_search"|
                                              variable == "grey_url"|
                                              variable == "forward_method"|
                                              variable == "refman_software"|
                                              variable == "deduplicate"|
                                              variable == "num_records")
#Report variables for no IS
#Report variables for IS coauthor
report_nois <- none_impact %>% filter(variable == "strategy_all"|
                                        variable == "db_list_gp"|
                                        variable == "db_platform"|
                                        variable == "date_searches"|
                                        variable == "grey_list"|
                                        variable == "grey_search"|
                                        variable == "grey_url"|
                                        variable == "forward_method"|
                                        variable == "refman_software"|
                                        variable == "deduplicate"|
                                        variable == "num_records")

#Merge all three dataframes
report_is_table1 <- merge(report_nois, report_mention, by = "variable", all = TRUE)
report_is_table <- merge(report_is_table1, report_coauth, by = "variable", all = TRUE)

#Recode variable names for table
report_is_table_rename <- report_is_table %>%
  mutate(variable = case_when(
    variable == "date_searches" ~ "Search dates reported",
    variable == "db_list_gp" ~ "All databases and sub-databases listed",
    variable == "db_platform" ~ "Database platform reported",
    variable == "deduplicate" ~ "Deduplication method reported",
    variable == "forward_method" ~ "Method for forward citation searching described",
    variable == "grey_list" ~ "Grey lit sources listed",
    variable == "grey_search" ~ "Grey lit search reported",
    variable == "grey_url" ~ "Grey lit URLs reported",
    variable == "refman_software" ~ "Reference management software reported",
    variable == "strategy_all" ~ "All search strategies reported",
    variable == "num_records" ~ "Number of records per database reported"
  ))

#Reorder with custom order 
report_custom_order <- c("All search strategies reported","All databases and sub-databases listed","Database platform reported",
                         "Search dates reported", "Grey lit sources listed", "Grey lit search reported",
                         "Grey lit URLs reported", "Method for forward citation searching described",
                         "Reference management software reported", "Deduplication method reported",
                         "Number of records per database reported")

report_is_table_rename <- report_is_table_rename %>%
  mutate(variable = factor(variable, levels = report_custom_order)) %>%
  arrange(variable)

write_csv(report_is_table_rename, here("./data_outputs/ccmethods_isinvolve_report_table.csv"))

#Reporting Table 
is_table_df_report <- read.csv(here("./data_outputs/ccmethods_isinvolve_report_table.csv"))
is_table_report <- is_table_df_report %>% gt() 

gt_tbl_report <- 
  gt(is_table_df_report) |>
  tab_header(
    title = "",
    subtitle = "Reporting of searches"
  ) |>
  tab_spanner(
    label = "No IS Involvement (n=37)",
    columns = c(count.x, percentage.x)
  ) |>
  tab_spanner(
    label = "IS consulted (n=42)",
    columns = c(count.y, percentage.y)
  ) |>
  tab_spanner(
    label = "IS co-author (n=32)",
    columns = c(count, percentage)
  ) |>
  tab_options(
    heading.subtitle.font.size = 16
  ) %>% 
  cols_label(variable="", count.x = "No.", percentage.x="%", count.y = "No.", percentage.y="%", count = "No.", percentage="%")

gt_tbl_report

#Save to image file
gt_tbl_report |> gtsave(here("./plots/report_is_table.png"), expand = 10)


##Figure 7B. Line graph for IS impact on reporting variables----
#Subset only percentage columns from conduct_is_table
report_is_chart_df <- report_is_table[, c(1,3, 5, 7)]

#Transpose to long format grouping percentage columns by IS involvement
report_is_chart_long <- report_is_chart_df %>%
  pivot_longer(cols = c(percentage.x, percentage.y, percentage), 
               names_to = "group", 
               values_to = "percentage")

#Rename group names
report_is_chart_long <- report_is_chart_long %>%
  mutate(group = case_when(
    group == "percentage.x" ~ "No IS",
    group == "percentage.y" ~ "IS consulted",
    group == "percentage" ~ "IS co-author"))

report_is_chart_long <- report_is_chart_long %>%
  mutate(group = factor(group, levels = c("No IS", "IS consulted", "IS co-author")))

#Recode variable names for chart legend
report_is_chart_renamed <- report_is_chart_long %>%
  mutate(variable = case_when(
    variable == "date_searches" ~ "Search dates",
    variable == "db_list_gp" ~ "Database names",
    variable == "db_platform" ~ "Database platform",
    variable == "deduplicate" ~ "Dedup method",
    variable == "forward_method" ~ "Forward citation method",
    variable == "grey_list" ~ "Grey lit sources",
    variable == "grey_search" ~ "Grey lit search",
    variable == "grey_url" ~ "Grey lit URLs",
    variable == "refman_software" ~ "Ref mgmt software",
    variable == "strategy_all" ~ "All search strategies",
    variable == "num_records" ~ "Number of database results"
  ))

#Reorder so that the legend list matches the order of values in the No IS column
custom_order_isrp <- c("Grey lit sources", "Search dates", "Forward citation method", "Database names", "Database platform", "Ref mgmt software", "All search strategies", "Grey lit search", "Number of database results", "Grey lit URLs", "Dedup method")
report_is_chart_renamed$variable <- factor(report_is_chart_renamed$variable, levels = custom_order_isrp)

#Create distinguishable palette for 11 variables
my_palette_11 <- c25 <- c(
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1", "deeppink1", "blue1", 
  "darkturquoise", "green1", "brown"
)

#Plot change in percentage over groups
pt_report_is <- ggplot(report_is_chart_renamed, aes(x = group, y = percentage, color = variable, group = variable)) +
                geom_line() +
                geom_point() +
                guides(fill = guide_legend(title = NULL)) +
                labs(title = "",
                x = "",
                y = "Percent of reviews",
                color = "Variable") +
                theme_minimal() +
                theme(legend.title=element_blank()) +
                scale_color_manual(values = my_palette_14) #+
                #theme(text = element_text(size=13))

ggsave(here("./plots/fig7b.png"), plot = pt_report_is, width = 6, height = 4, units = "in", dpi = 300) 

