# -------------------------------------------------------------------------------------------------
#
#                                 POLYCHOTOMOUS CATEGORICAL PREDICTORS
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
library(ggridges)
library(patchwork)
library(tidyverse)



##################################################
### Import data
##################################################

# Read in data
pew = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/main/data/pew.csv")
pew



##################################################
### Exploration
##################################################

# Density plot of news knowledge
p1 = ggplot(data = pew, aes(x = knowledge)) +
  stat_density(geom = "line", color = "#c62f4b") +
  #theme_bw() +
  xlab("News knowledge") +
  ylab("Probability density")


# Bar plot of news source
p2 = ggplot(data = pew, aes(x = news_source)) +
  geom_bar(fill = "#c62f4b") +
  #theme_bw() +
  xlab("News source") +
  ylab("Frequency")


# Layout plots
p1 | p2


# Scatterplot
ggplot(data = pew, aes(x = news_source, y = knowledge)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_bw() +
  xlab("News source") +
  ylab("News knowledge")


# Compute summary statistics
pew |>
  group_by(news_source) |>
  summarize(
    M  = mean(knowledge),
    SD = sd(knowledge),
    N = n()
  ) |>
  arrange(desc(M))



##################################################
### Create dummy-coded variables
##################################################

# Create all four dummy variables
pew = pew |>
  mutate(
    none    = if_else(news_source == "None", 1, 0),
    con     = if_else(news_source == "Conservative", 1, 0),
    com     = if_else(news_source == "Comedy", 1, 0),
    lib     = if_else(news_source == "Liberal", 1, 0)
  )


# Examine data
pew |>
  print(width = Inf)


# Get the categories
pew |>
  select(news_source) |>
  unique()



##################################################
### Fit model
##################################################

# News source = None is reference group
lm.none = lm(knowledge ~ 1 + con + com + lib, data = pew)


# Model-level info
glance(lm.none) |>
  print(width = Inf)


# Which news sources show differences in the average news knowledge?
# Coefficient-level info
tidy(lm.none)



##################################################
### Pairwise Comparisons for Americans who get their News from Conservative Sources
##################################################

# Conservative news sources is reference group
lm.con = lm(knowledge ~ 1 + com + lib + none, data = pew)


# Model-level info
glance(lm.con) |>
  print(width = Inf)


# Coefficient-level info
tidy(lm.con)



##################################################
### Pairwise Comparisons for Americans who get their News from Comedy Sources
##################################################

# Reference group = Comedy News
lm.com = lm(knowledge ~ 1 + con + lib + none, data = pew)


# Coefficient-level output
tidy(lm.com)



##################################################
### Pairwise Comparisons for Americans who get their News from Liberal Sources
##################################################

# Reference group = Liberal News
lm.lib = lm(knowledge ~ 1 + con + com + none, data = pew)


# Coefficient-level output
tidy(lm.lib)



##################################################
### ANCOVA Model
##################################################

# News source = None is reference group
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + 
                 com + con + lib, data = pew)


# Model-level info
glance(lm.none.2) |>
  print(width = Inf)


# Coefficient-level info
tidy(lm.none.2)



##################################################
### Adjusted mean differences
##################################################

####################################
# Mean age = 50.81
# Mean education = 13.92
# Mean news consumption = 41.91
# Mean political engagement = 73.18
####################################


# Compute adjusted mean for comedy source
-27.8 + 0.186*50.81 + 3.24*13.92 + 0.230*41.91 + 0.233*73.18 +
  0.0825*1 + 7.87*0 +  5.96*0


# Compute adjusted mean for conservative source
-27.8 + 0.186*50.81 + 3.24*13.92 + 0.230*41.91 + 0.233*73.18 +
  0.0825*0 + 7.87*1 +  5.96*0


# Compute adjusted mean for liberal source
-27.8 + 0.186*50.81 + 3.24*13.92 + 0.230*41.91 + 0.233*73.18 +
  0.0825*0 + 7.87*0 +  5.96*1 


# Compute adjusted mean for none source
-27.8 + 0.186*50.81 + 3.24*13.92 + 0.230*41.91 + 0.233*73.18 +
  0.0825*0 + 7.87*0 +  5.96*0 



##################################################
### Obtaining the Other Adjusted Pairwise Comparisons
##################################################

# News source = Comedy is reference group
lm.com.2 = lm(knowledge ~ 1 + age + education + news + engagement + 
                con + lib + none, data = pew)


# Coefficient-level output
tidy(lm.com.2)


# News source = conservative is reference group
lm.con.2 = lm(knowledge ~ 1 + age + education + news + engagement + 
                com + lib + none, data = pew)


# Coefficient-level output
tidy(lm.con.2)



##################################################
### Another visualization: Confidence Intervals for Adjusted Means
##################################################

# News source = None is reference group
lm.none.2 = lm(knowledge ~ 1 + age + education + news + engagement + 
                 com + con + lib, data = pew)


d = data.frame(
  age        = 50.81,
  education  = 13.92,
  news       = 41.91,
  engagement = 73.18,
  com        = c(0, 1, 0, 0),
  con        = c(0, 0, 1, 0),
  lib        = c(0, 0, 0, 1)
)


# View d
d


# Obtain adjusted means and standard errors for each row in d
predict(lm.none.2, newdata = d, se = TRUE)


# Create data frame
plot_data = data.frame(
  source = c("None", "Comedy", "Conservative", "Liberal"),
  m = c(55.51970, 63.38794, 55.60224, 61.47512),
  se = c(1.0169867, 2.8982189, 0.9788414, 1.3693665)
)


# View data
plot_data


# Compute t-star
qt(.975, df = 1086)


# Compute CI limits
plot_data = plot_data |>
  mutate(
    lower = m - 1.962151*se,
    upper = m + 1.962151*se
  )

# View data
plot_data


# Create plot
ggplot(data = plot_data, aes(x = m, y = source)) +
  #Create CI
  geom_segment(
    aes(x = lower, y = source, xend = upper, yend = source),
    color = "#ff2f92",
    linewidth = 1.5
  ) +
  #Add adjusted mean
  geom_point(
    size = 3,
    color = "#ff2f92"
  ) +
  #theme_bw() +
  xlab("Adjusted mean news knowledge score") +
  ylab("News source(s)") +
  xlim(50, 80)








