# -------------------------------------------------------------------------------------------------
#
#                                        MULTIPLE COMPARISONS
#                                               2024
#                                    EPSY 8251, Andrew Zieffler
#                      © GNU GENERAL PUBLIC LICENSE (Version 3, 29 June 2007)
#                                        
# -------------------------------------------------------------------------------------------------


##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(emmeans)
library(ggridges)
library(tidyverse)



##################################################
### Import data
##################################################

# Read in data and create dummy variables
pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/pew.csv") |>
  mutate(
    none    = if_else(news_source == "None", 1, 0),
    con     = if_else(news_source == "Conservative", 1, 0),
    com     = if_else(news_source == "Comedy", 1, 0),
    lib     = if_else(news_source == "Liberal", 1, 0)
  )


# View data
pew



##################################################
### Fit model
##################################################

# Fit model (None is reference group)
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + 
                 com + con + lib, data = pew)



##################################################
### Dunn-Bonferroni approach to adjusting p-values
##################################################

c(
  0.01142, #Comedy vs. Conservative                     
  0.54808, #Comedy vs. Liberal                          
  0.00977, #Comedy vs. None                             
  0.000582, #Conservative vs. Liberal                    
  0.955204, #Conservative vs. None                       
  0.000385 #Liberal vs. None                            
) * 6



##################################################
### Obtain Dunn-Bonferroni p-values directly
##################################################

# Fit model using categorical predictor
lm.news_source = lm(knowledge ~ 1 + age + education + news + engagement + news_source, data = pew)


# Coefficient-level output
tidy(lm.news_source)


# Obtain the unadjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "none")


# Obtain the Dunn-Bonferroni adjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "bonferroni")



##################################################
### Controlling p-value for False Discovery Rate
##################################################

# Obtain the Benjamini-Hochberg adjusted p-values for pairwise differences
emmeans(lm.news_source, specs = pairwise ~news_source, adjust = "BH")


