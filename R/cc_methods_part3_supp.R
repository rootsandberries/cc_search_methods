#Searching and reporting in Campbell Collaboration systematic reviews: An assessment of current methods
#Part 3. Supplemental search methods



#Set up -----------------------------------------

#Install packages
#install.packages(c("tidyverse", "ggplot2"))

#Load libraries
library(tidyverse)
library(ggplot2)
library(here)

#Import data
ccmethods_data <- read.csv(here("../data/CC-methods-data-extraction-final-20230227-clean-recode.csv"), na = "NA")


#Supplementary searching -------------------------------------

##Figure 5: Supplementary search reporting ----
##Bar chart: Number reporting handsearching, forward and backward citation searching, contacting experts, related reviews
##Count bars in shades of gray for number yes, no and unclear reporting on each
##Relevant variables: handsearch, experts, backward, forward, reviews

table(ccmethods_data$handsearch)
table(ccmethods_data$backward)
table(ccmethods_data$forward)
table(ccmethods_data$reviews)
table(ccmethods_data$experts)


#Subset the needed variables
db_subset_supp <- ccmethods_data[, c("handsearch", "experts", "backward", "forward", "reviews")]

#Convert all to factors and set level orders
db_subset_supp$handsearch <- factor(db_subset_supp$handsearch, levels=ordered(c("yes", "no", "unclear")))
db_subset_supp$experts <- factor(db_subset_supp$experts, levels=ordered(c("yes", "no", "unclear")))
db_subset_supp$backward <- factor(db_subset_supp$backward, levels=ordered(c("yes", "no", "unclear")))
db_subset_supp$forward <- factor(db_subset_supp$forward, levels=ordered(c("yes", "no", "unclear")))
db_subset_supp$reviews <- factor(db_subset_supp$reviews, levels=ordered(c("yes", "no", "unclear")))


#Transform to long format on variable name
db_long_supp <- db_subset_supp %>%
  gather(variable_name, value)

#Group by variable name and count
db_counts_supp <- db_long_supp %>%
  group_by(variable_name, value) %>%
  count()

# Define the desired order of the groups
var_order <- c("forward", "handsearch", "experts", "reviews", "backward")

# Convert the groups to a factor with the desired order and recode
db_counts_supp$variable_name_ordered <- factor(db_counts_supp$variable_name, levels = var_order)
db_counts_supp$variable_name_ordered <- recode_factor(db_counts_supp$variable_name_ordered, 
                                                      forward = "Forward citation chasing",
                                                      handsearch = "Handsearching of journals or proceedings", 
                                                      reviews = "References of related reviews",
                                                      experts = "Experts contacted",
                                                      backward = "Backward citation chasing")

#Define order of values
db_counts_supp$value <- factor(db_counts_supp$value, levels = ordered(c("yes", "no", "unclear")))

#Set colors
my_colors <- c("gray45", "gray80", "gray95")

#Plot stacked grouped bar chart
pt_supp <- ggplot(db_counts_supp, aes(fill=value, y=n, x=variable_name_ordered)) + 
            geom_bar(position="stack", stat="identity", colour="black") +
            scale_fill_manual(values = my_colors) +
            labs(x = "", y = "Number of Reviews", fill = "") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
            theme_light() +
            theme(axis.text.x = element_text(size = 10), legend.box = "horizontal") +
            coord_flip() 

ggsave(here("../plots/fig5.png"), plot = pt_supp, width = 6, height = 4, units = "in", dpi = 300) 
