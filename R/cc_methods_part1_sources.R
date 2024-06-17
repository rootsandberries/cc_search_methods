#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 1. Source selection and reporting

#Set up -----------------------------------------
#Install packages
#install.packages(c("tidyverse", "ggplot2", "ggpubr", "here"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(here)

#Import data
ccmethods_data <- read.csv(here("./data/CC-methods-data-extraction-final-20240508-clean-recode.csv"), na = "NA")

#Source reporting ---------------------------------

##Figure 1. Source reporting ----
##Bar chart showing number reporting database names, database platforms, database date range
##Count bars in red, yellow and green for number with none, partial or full reporting on each
##Relevant variables: db_list, db_platform, date_range 

#Recode db_list to none (no db given), some (partial--lacking sub-databases), all (all including sub-db)
ccmethods_data$db_list_gp <- factor(ccmethods_data$db_list) #Convert to factor
levels(ccmethods_data$db_list_gp) #Print levels
levels(ccmethods_data$db_list_gp) <- c("some", "some", "all") #Recode to new values
levels(ccmethods_data$db_list_gp) #Print new levels

#Create subset of needed variables only
db1_subset <- ccmethods_data[, c("db_list_gp", "db_platform", "date_range", "grey_url", "grey_list")]

#Convert all to factors and set level orders
db1_subset$db_platform <- factor(db1_subset$db_platform, levels=ordered(c("all", "some", "none")))
db1_subset$date_range <- factor(db1_subset$date_range, levels=ordered(c("all", "some", "none")))
db1_subset$db_list_gp <- factor(db1_subset$db_list_gp, levels=ordered(c("all", "some", "none")))
db1_subset$grey_list <- factor(db1_subset$grey_list, levels=ordered(c("all", "some", "none", NA)))
db1_subset$grey_url <- factor(db1_subset$grey_url, levels=ordered(c("all", "some", "none", NA)))

#Recode to Full, Partial and None
db1_subset$db_platform <- db1_subset$db_platform %>% 
  factor(levels=ordered(c("all", "some", "none"))) %>% 
  recode_factor( 
    all = "Full", 
    some = "Partial",
    none = "None")

db1_subset$date_range <- db1_subset$date_range %>% 
  factor(levels=ordered(c("all", "some", "none"))) %>% 
  recode_factor( 
    all = "Full", 
    some = "Partial",
    none = "None")

db1_subset$db_list_gp <- db1_subset$db_list_gp %>% 
  factor(levels=ordered(c("all", "some", "none"))) %>% 
  recode_factor( 
    all = "Full", 
    some = "Partial",
    none = "None")

db1_subset$grey_list <- db1_subset$grey_list %>% 
  factor(levels=ordered(c("all", "some", "none"))) %>% 
  recode_factor( 
    all = "Full", 
    some = "Partial",
    none = "None")

#How many reviews provided a full list of grey literature sources?
summary(db1_subset$grey_list) #91

db1_subset$grey_url <- db1_subset$grey_url %>% 
  factor(levels=ordered(c("all", "some", "none"))) %>% 
  recode_factor( 
    all = "Full", 
    some = "Partial",
    none = "None")

#Transform to long format on variable name
db_long <- db1_subset %>%
  gather(variable_name, value)

#Group by variable name and count
db_counts <- db_long %>%
  group_by(variable_name, value) %>%
  count()

# Define the desired order of the groups
var_order <- c("db_list_gp", "db_platform", "date_range", "grey_list", "grey_url")

# Convert the groups to a factor with the desired order and recode
db_counts$variable_name_ordered <- factor(db_counts$variable_name, levels = var_order)
db_counts$variable_name_ordered <- recode_factor(db_counts$variable_name_ordered, 
                                                 db_list_gp = "Databases and sub-databases", 
                                                 db_platform = "Database platforms",
                                                 date_range = "Database date ranges",
                                                 grey_list = "Grey literature sources",
                                                 grey_url = "Website URLs")

#Define order of values
db_counts$value <- factor(db_counts$value, levels = ordered(c("Full", "Partial", "None", NA)))

#Set colors
my_colors <- c("green", "yellow", "red")

#Plot stacked grouped bar chart
pt_source_rp <- ggplot(db_counts, aes(fill=value, y=n, x=variable_name_ordered)) + 
                  geom_bar(position="stack", stat="identity") +
                  scale_fill_manual(values = my_colors, na.value = "grey80", labels = c(levels(db_counts$value), "Not applicable")) +
                  labs(x = "", y = "Number of Reviews", fill = "") +
                  scale_x_discrete(limits = rev(levels(db_counts$variable_name_ordered)), labels = function(x) str_wrap(x, width = 15)) +
                  theme_light() +
                  theme(axis.text.x = element_text(size = 11)) +
                  coord_flip()

ggsave(here("./plots/fig1.png"), plot = pt_source_rp, width = 6, height = 4, units = "in", dpi = 300) 

#For in text results, more detail about database descriptions
table(ccmethods_data$db_list, useNA = "always")
prop.table(table(ccmethods_data$db_list)) * 100

table(ccmethods_data$db_platform, useNA = "always")
prop.table(table(ccmethods_data$db_platform)) * 100

table(ccmethods_data$date_range, useNA = "always")
prop.table(table(ccmethods_data$date_range)) * 100

table(ccmethods_data$db_list_gp, useNA = "always")
prop.table(table(ccmethods_data$db_list_gp)) * 100

#Number and percent reporting website urls
table(ccmethods_data$grey_url, useNA = "always")
prop.table(table(ccmethods_data$grey_url)) * 100

#Number and percent reporting grey lit sources
table(ccmethods_data$grey_list, useNA = "always")
prop.table(table(ccmethods_data$grey_list)) * 100


#Source selection --------------------------------

##Figure 2. Grey lit sources ----
##The proportion of reviews by coordinating group that intentionally searched sources for different types of grey literature 
##Multiple versions of this figure were generated and the heat map was used in the final manuscript

#Separate rows with more than one coordinating group
mydata_sep <- separate_rows(ccmethods_data, cg, sep=";")
table(mydata_sep$cg)

#Create new variables that combine govt/ngo and trials/registers
mydata_sep <- mydata_sep %>% 
  mutate(govt_ngo = case_when(govt == "yes" | ngo == "yes" ~ "yes",
                              govt == "no" & ngo == "no" ~ "no",
                              TRUE ~ "unclear"))

mydata_sep <- mydata_sep %>% 
  mutate(trials_reg = case_when(trials == "yes" | registries == "yes" ~ "yes",
                                trials == "no" & registries == "no" ~ "no",
                                TRUE ~ "unclear"))

#Remove Nutrition since its a subgroup of International Development (and all of the Nutrition reviews are also ID reviews)
mydata_sep <- subset(mydata_sep, cg != "Nutrition")

##Figure 2. Option 1 (used in manuscript) --Heat map option

#Subset only needed variables
db2_subset <- mydata_sep[, c("cg", "conf_proc", "theses", "trials_reg", "govt_ngo")]

#Summarize percent by coordinating group for each type of grey lit
df_summary <- db2_subset %>%
  gather(variable, value, -cg) %>%
  group_by(cg, variable) %>%
  summarize(percent = mean(value == "yes") * 100)

#Transpose to wide format and then long
df_summary_wide <- df_summary %>%
  spread(variable, percent)

df_summary_long <- df_summary_wide %>%
  gather(variable, value, -cg)

#Make variable factor and recode
df_summary_long$variable <- factor(df_summary_long$variable)
levels(df_summary_long$variable)
levels(df_summary_long$variable) <- c("Conference Proceedings", "Govt, NGO, INGO Websites", 
                                      "Theses & Dissertations", "Trials & Registries")

#Reorganize so there is some order to percentages: govt, trials, theses, conf.
df_summary_long$variable <- factor(df_summary_long$variable, levels = c("Govt, NGO, INGO Websites", "Trials & Registries", 
                                                                        "Theses & Dissertations", "Conference Proceedings"))

#Reorganize so there is some order to coordinating group (number of reviews): International Development, Education, Social Welfare, Crime and Justice, Disability, Ageing, Business and Management, Knowledge Translation, Methods
df_summary_long$cg <- factor(df_summary_long$cg, levels = c("Methods", "Knowledge Translation and Implementation", "Business and Management", 
                                                            "Ageing", "Disability", "Crime and Justice", "Social Welfare", 
                                                            "Education", "International Development"))


#Plot heatmap
pt_heat_grey <- ggplot(df_summary_long, aes(variable, cg, fill = value)) +
                geom_tile() +
                scale_fill_gradient(low = "white", high = "steelblue") +
                labs(title = "", x = "", y = "", fill = "% 'A'") +
                geom_text(aes(label = paste0(round(value), "%")), size = 3, color = "black") +
                theme_minimal() +  theme(legend.position = "none", axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 11)) + 
                scale_x_discrete(position="top") +
                scale_y_discrete(labels = function(x) str_wrap(x, width = 25))

ggsave(here("./plots/fig2.png"), plot = pt_heat_grey, width = 9.5, height = 6, units = "in", dpi = 300) 


#Additional in text results ----

#Percent searching at least 2 relevant databases
prop.table(table(ccmethods_data$two_db)) * 100
table(ccmethods_data$two_db)

#Percent searching regional databases
prop.table(table(ccmethods_data$geog_db)) * 100

#Percent searching Google Scholar and other free scholarly search engines
prop.table(table(ccmethods_data$free_gs)) * 100
table(ccmethods_data$free_gs)

#Percent searching Microsoft Academic and other free scholarly search engines
prop.table(table(ccmethods_data$free_ma)) * 100
table(ccmethods_data$free_ma)

#Percent searching Dimensions and other free scholarly search engines
prop.table(table(ccmethods_data$free_dim)) * 100
table(ccmethods_data$free_dim)

#Percent searching Google and other free search engines
prop.table(table(ccmethods_data$engine_google)) * 100
table(ccmethods_data$engine_google)

#Percent reference Kugley guidance
prop.table(table(ccmethods_data$kugley)) * 100
table(ccmethods_data$kugley)

#How many reviews did no web searching?
no_web <- ccmethods_data %>%
  filter(govt == "no", ngo == "no", free_gs == "no", engine_google == "no")
nrow(no_web)

#Figures not used in manuscript -----
#These are alternative versions of Figure 2 not used in the manuscript

##Figure 2. Option 2 (not used)--Side-by-side bar charts

#Group by coordinating groups and plot each source type

#Conference proceedings
confproc_prop <-mydata_sep %>% 
  group_by(cg, conf_proc) %>%
  summarize(n=n()) %>% 
  mutate(prop=n/sum(n)*100)

confproc_prop$conf_proc <- factor(confproc_prop$conf_proc, levels = c("yes", "no", "unclear"))

p1 <- ggplot(confproc_prop, aes(x = cg, y = prop, fill = conf_proc)) +
  geom_bar(stat = "identity") +
  labs(title = "Conference Proceedings",
       x = "",
       y = "",
       fill = "") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=10), axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 11), legend.position = "none") + 
  scale_fill_grey(start = .3, end = .9) + 
  coord_flip() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))


#Theses and Dissertations
theses_prop <-mydata_sep %>% 
  group_by(cg, theses) %>%
  summarize(n=n()) %>% 
  mutate(prop=n/sum(n)*100)

theses_prop$theses <- factor(theses_prop$theses, levels = c("yes", "no", "unclear"))

p2 <- ggplot(theses_prop, aes(x = cg, y = prop, fill = theses)) +
  geom_bar(stat = "identity") +
  labs(title = "Theses & Dissertations",
       x = "",
       y = "",
       fill = "") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=10), axis.text.y = element_blank(), axis.text.x = element_text(size = 11), legend.position = "none") + 
  scale_fill_grey(start = .3, end = .69) + 
  coord_flip() 

#Trials and registries
trials_prop <-mydata_sep %>% 
  group_by(cg, trials_reg) %>%
  summarize(n=n()) %>% 
  mutate(prop=n/sum(n)*100)

trials_prop$trials_reg <- factor(trials_prop$trials_reg, levels = c("yes", "no", "unclear"))

p3 <- ggplot(trials_prop, aes(x = cg, y = prop, fill = trials_reg)) +
  geom_bar(stat = "identity") +
  labs(title = "Trials & Registries",
       x = "",
       y = "",
       fill = "") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=10), axis.text.y = element_blank(), axis.text.x = element_text(size = 11), legend.position = "none") + 
  scale_fill_grey(start = .3, end = .9) + 
  coord_flip() 

#Government, NGO and INGO websites
govt_prop <-mydata_sep %>% 
  group_by(cg, govt_ngo) %>%
  summarize(n=n()) %>% 
  mutate(prop=n/sum(n)*100)

govt_prop$govt_ngo <- factor(govt_prop$govt_ngo, levels = c("yes", "no", "unclear"))

p4 <- ggplot(govt_prop, aes(x = cg, y = prop, fill = govt_ngo)) +
  geom_bar(stat = "identity") +
  labs(title = "Government, NGO, & INGO Websites",
       x = "",
       y = "",
       fill = "") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=10), axis.text.y = element_blank(), axis.text.x = element_text(size = 11), legend.position = "none") + 
  scale_fill_grey(start = .3, end = .69) + 
  coord_flip()

#Arrange plots in side by side grid and add legend at bottom

prow <- ggarrange(p1, p2, p3, p4, ncol = 4, widths = c(1, 0.6, 0.6, 0.6),
                  common.legend = TRUE, legend="bottom") ##save as image 1200 X 500 or so

prow

#Figure 2 Option 3 (not used). Bubble plot option
ggplot(df_summary_long, aes(variable, cg, colour = variable, size = value)) +
  geom_point() +
  geom_text(aes(label = paste0(round(value), "%")), size = 3, color = "white") +
  scale_x_discrete(position="top") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_size_continuous(range = c(5, 30)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none", 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())


