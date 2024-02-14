#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 5. Other considerations



#Set up -----------------------------------------

#Install packages
#install.packages(c("tidyverse", "ggplot2", "ggpubr", "here"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(lubridate)
library(here)

#Import data
ccmethods_data <- read.csv(here("./data/CC-methods-data-extraction-final-20230227-clean-recode.csv"), na = "NA")


#Time lag between search and publication --------------------
##Figure 8a. A histogram and box plot of number of months between last search and publication ----

#Create new columns with proper date formats from year and month
ccmethods_data$pub_date <- make_date(ccmethods_data$pub_year, ccmethods_data$pub_mo)
ccmethods_data$search_date <- make_date(ccmethods_data$search_year, ccmethods_data$search_mo)

#calculate the number of days elapsed between publication date and last search date and convert to number of months
ccmethods_data$search_lag <- difftime(ccmethods_data$pub_date, ccmethods_data$search_date, units = "days") %>% 
  as.numeric() / 30.44 

#round to integer
ccmethods_data$search_lag <- round(ccmethods_data$search_lag)

#Simple histogram plot of time lags
pt_hist_timelag <- ggplot(ccmethods_data, aes(x = search_lag)) +
                    geom_histogram(color="black", fill="lightgray", binwidth = 3) + theme_light() +
                    labs(x = str_wrap("Number of months between search date and publication date", width=50), y="Number of reviews") +
                    scale_y_continuous(breaks = seq(from = 0, to = 12, by = 2), limits = c(0, 12)) +
                    scale_x_continuous(breaks = seq(from = 0, to = 100, by = 25), limits = c(0, 100)) #+
                    #theme(text = element_text(size=15))

ggsave(here("../plots/fig8a.png"), plot = pt_hist_timelag, width = 6, height = 4, units = "in", dpi = 300) 

##Figure 8b. Box plot comparing Coordinating groups----

#Separate rows by coordinating group
mydata_sep_lag <- separate_rows(ccmethods_data, cg, sep=";")

#Remove groups with very few reviews
mydata_sep_lag <- mydata_sep_lag %>% subset(cg != "Nutrition") %>% 
  subset(cg != "Methods") %>% subset(cg != "Knowledge Translation and Implementation")

#Plot boxplot by coordinating group and label outliers
pt_box_timelag <- ggplot(mydata_sep_lag, aes(x = cg, y = search_lag)) +
                  geom_boxplot() + 
                  geom_text(data = mydata_sep_lag %>% 
                  group_by(cg) %>%
                  filter(search_lag < quantile(search_lag, 0.25) - 1.5*IQR(search_lag) | search_lag > quantile(search_lag, 0.75) + 1.5*IQR(search_lag)),
                  aes(x = cg, y = search_lag, label = round(search_lag, 2)),
                  hjust = 1.5, vjust = 0.5, size = 4) +
                  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
                  labs(x="Coordinating Group", y=("Number of months"))

ggsave(here("../plots/fig8b.png"), plot = pt_box_timelag, width = 6, height = 4, units = "in", dpi = 300) 


##ANOVA: Differences between coordinating groups using one-way ANOVA----
myaov <- aov(search_lag ~ cg, data = mydata_sep_lag)
summary(myaov)
anova(myaov)

##In text results----
#Get median, mean, min and max of time lag
summary(ccmethods_data$search_lag)

#Number of reviews mentioning that search was updated
table(ccmethods_data$update_search)
prop.table(table(ccmethods_data$update_search)) * 100


#Information management ------------------------------------

##In text results----
#Percent reporting deduplication methods 
prop.table(table(ccmethods_data$deduplicate)) * 100
table(ccmethods_data$deduplicate, useNA = "always")

prop.table(table(ccmethods_data$refman_software)) * 100
table(ccmethods_data$refman_software, useNA = "always")

#Most common citation manager or tool reported
#Generate csv file of just the rows that indicate the use of a software to manually find the most common
just_ref_software <- ccmethods_data %>% subset(refman_software == "yes") %>% 
  select(refman_software_text)

write.csv(just_ref_software, here("../data_outputs/refman_software.csv"))

#Import csv file after manually identifying tool used
ref_tools <- read.csv(here("../data_outputs/refman_software_clean.csv"))
nrow(ref_tools)

ref_tools %>%
  group_by(tool) %>%
  summarize(count = n())

#Supplementary Table 2: Adherence to AMSTAR --------------------------------------------

#At least 2 databases searched
twodb_num_yes <- ccmethods_data %>%
  summarize(twodb_num_yes = sum(two_db == 'yes', na.rm = TRUE)) %>%
  pull(twodb_num_yes)

twodb_perc_yes <- ccmethods_data %>%
  summarize(twodb_perc_yes = mean(two_db == 'yes', na.rm = TRUE) * 100) %>%
  pull(twodb_perc_yes) |> round(digits=1)

#Provided search strategy
stratone_num_yes <- ccmethods_data %>%
  summarize(stratone_num_yes = sum(strategy_one == 'yes', na.rm = TRUE)) %>%
  pull(stratone_num_yes)

stratone_perc_yes <- ccmethods_data %>%
  summarize(stratone_perc_yes = mean(strategy_one == 'yes', na.rm = TRUE) * 100) %>%
  pull(stratone_perc_yes) |> round(digits=1)

#Justified restrictions
#Create new justification column where NA is yes (for use below)
ccmethods_data$limit_just_amstar <- ifelse(is.na(ccmethods_data$limit_just), "yes", ccmethods_data$limit_just)

limjust_num_yes <- ccmethods_data %>%
  summarize(limjust_num_yes = sum(limit_just_amstar == 'yes', na.rm = TRUE)) %>%
  pull(limjust_num_yes)

limjust_perc_yes <- ccmethods_data %>%
  summarize(limjust_perc_yes = mean(limit_just_amstar == 'yes', na.rm = TRUE) * 100) %>%
  pull(limjust_perc_yes) |> round(digits=1)

#Conducted backward citation searching

bw_num_yes <- ccmethods_data %>%
  summarize(bw_num_yes = sum(backward == 'yes', na.rm = TRUE)) %>%
  pull(bw_num_yes)

bw_perc_yes <- ccmethods_data %>%
  summarize(bw_perc_yes = mean(backward == 'yes', na.rm = TRUE) * 100) %>%
  pull(bw_perc_yes) |> round(digits=1)

#Searched trial and study registries
trreg_yes <- sum(ccmethods_data$trials == "yes" | ccmethods_data$registries == "yes")
trreg_percent <- trreg_yes/nrow(ccmethods_data)*100 |> round(digits=2)


#Create new column combining trials and registries (for use below)
ccmethods_data$trial_reg <- ifelse(ccmethods_data$trials == "yes" | ccmethods_data$registries == "yes", "yes", 
                       ifelse(ccmethods_data$trials != "yes" & ccmethods_data$registries != "yes", "no", "unclear"))

#Consulted experts
exp_num_yes <- ccmethods_data %>%
  summarize(exp_num_yes = sum(experts == 'yes', na.rm = TRUE)) %>%
  pull(exp_num_yes)

exp_perc_yes <- ccmethods_data %>%
  summarize(exp_perc_yes = mean(experts == 'yes', na.rm = TRUE) * 100) %>%
  pull(exp_perc_yes) |> round(digits=1)

#Searched gray literature
grey_yes <- sum(ccmethods_data$conf_proc == "yes" | ccmethods_data$theses == "yes" | ccmethods_data$govt == "yes" | ccmethods_data$ngo == "yes")
grey_percent <- grey_yes/nrow(ccmethods_data)*100 |> round(digits=1)


#Create new column combining all gray lit columns (for use below)
ccmethods_data$all_grey <- ifelse(ccmethods_data$conf_proc == "yes" | ccmethods_data$theses == "yes" | ccmethods_data$govt == "yes" | ccmethods_data$ngo == "yes", "yes", 
                                   ifelse(ccmethods_data$conf_proc != "yes" & ccmethods_data$theses != "yes" & ccmethods_data$govt != "yes" & ccmethods_data$ngo != "yes", "no", "unclear"))

#Conducted search within 24 months of completion
#Create new columns with proper date formats from year and month
ccmethods_data$pub_date <- make_date(ccmethods_data$pub_year, ccmethods_data$pub_mo)
ccmethods_data$search_date <- make_date(ccmethods_data$search_year, ccmethods_data$search_mo)

#calculate the number of days elapsed between publication date and last search date and convert to month
ccmethods_data$search_lag <- difftime(ccmethods_data$pub_date, ccmethods_data$search_date, units = "days") %>% 
  as.numeric() / 30.44 

#round to integer
ccmethods_data$search_lag <- round(ccmethods_data$search_lag)

#number of reviews where search lag is less than or equal to 24 months
lag_24 <- sum(ccmethods_data$search_lag <= 24)
lag_24_percent <- lag_24/nrow(ccmethods_data)*100 |> round(digits=1)

#Full Yes adherence: all partial plus backward citation searching, searched trials/registries, consulted experts, searched grey literature, conducted search within 24 months
ccmethods_data$amstar_full_yes <- ifelse(ccmethods_data$amstar_partial_yes == "yes" & ccmethods_data$backward == "yes" & ccmethods_data$experts == "yes" & ccmethods_data$all_grey == "yes" & ccmethods_data$search_lag <= 24, "yes", "no") 
full_yes <- sum(ccmethods_data$amstar_full_yes == "yes")
full_yes_perc <- ((full_yes/nrow(ccmethods_data)) * 100) |> round(digits=1)


#Partial Yes adherence: at least 2 databases, provided at least one search strategy, justified restrictions
ccmethods_data$amstar_partial_yes <- ifelse(ccmethods_data$two_db == "yes" & ccmethods_data$strategy_one == "yes" & ccmethods_data$limit_just_amstar == "yes", "yes", "no") 
partial_yes <- sum(ccmethods_data$amstar_partial_yes == "yes")
partial_yes_only <- partial_yes - full_yes
partial_yes_perc <- ((partial_yes_only/nrow(ccmethods_data)) * 100) |> round(digits=1)

#Calculate number and percent of No ratings (total number of reviews minus number of Partial Yes ratings)
no_amst_num <- nrow(ccmethods_data) - partial_yes
no_amst_perc <- ((no_amst_num/nrow(ccmethods_data)) * 100) |> round(digits=1)


#AMSTAR table
#Create dataframe from values above
number <- c(twodb_num_yes, stratone_num_yes, limjust_num_yes, bw_num_yes, trreg_yes, exp_num_yes, grey_yes, lag_24, full_yes, partial_yes_only, no_amst_num)
percent <- c(twodb_perc_yes, stratone_perc_yes, limjust_perc_yes, bw_perc_yes, trreg_percent, exp_perc_yes, grey_percent, lag_24_percent, full_yes_perc, partial_yes_perc, no_amst_perc)
criteria <- c("Searched at least 2 databases", "Provided search strategy", "Justified publication restrictions", "Searched the reference lists of included studies", "Searched trial/study registries", "Included/consulted content experts", "Searched for grey literature", "Conducted search within 24 months", "Yes", "Partial Yes", "No")
is_table_amstar <- data.frame(criteria = criteria, number = number, percent = percent)
is_table_amstar$percent <- round(is_table_amstar$percent, 1)

#Prepare dataframe as table for gt
is_table <- is_table_amstar %>% gt() 

gt_tbl_amstar <- 
  gt(is_table_amstar) |>
  tab_spanner(
    label = "Meets AMSTAR 2 criteria",
    columns = c(number, percent)
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = criteria == "Yes"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = criteria == "No"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = criteria == "Partial Yes"
    )
  ) |>
  cols_align(
    align = "center",
    columns = number
  ) |>
  cols_align(
    align = "center",
    columns = percent
  ) |>
  tab_options(
    heading.subtitle.font.size = 16
  ) %>% 
  cols_label(criteria="", number = "Number of reviews", percent ="Percent of reviews (%)")

gt_tbl_amstar

#Save to image file (note: may need to restart RStudio to get this to work)
gt_tbl_amstar |> gtsave(here("./plots/amstar_table.png"), expand = 10)

