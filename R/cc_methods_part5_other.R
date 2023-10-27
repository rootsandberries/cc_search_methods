#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 5. Other considerations



#Set up -----------------------------------------

#Install packages
#install.packages(c("tidyverse", "ggplot2", "ggpubr", "here"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(here)

#Import data
ccmethods_data <- read.csv(here("../data/CC-methods-data-extraction-final-20230227-clean-recode.csv"), na = "NA")


#Time lag between search and publication --------------------
##Figure 8a. A histogram and box plot of number of months between last search and publication ----

#Create new columns with proper date formats from year and month
ccmethods_data$pub_date <- make_date(ccmethods_data$pub_year, ccmethods_data$pub_mo)
ccmethods_data$search_date <- make_date(ccmethods_data$search_year, ccmethods_data$search_mo)

#calculate the number of months elapsed between publication date and last search date
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
