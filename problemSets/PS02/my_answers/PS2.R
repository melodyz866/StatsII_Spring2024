#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
model <- glm(choice ~ countries + sanctions, data=climateSupport, family=binomial(link="logit"))

#glm model
model <- glm(choice ~ countries + sanctions, data=climateSupport, family=binomial(link="logit"))

# summary of the model
model_summary <- summary(model)

# conduct the ANOVA test
model_anova <- anova(model, test="Chisq")

# save the summary
summary_file <- "model_summary.txt"
sink(summary_file)
print(model_summary)
sink()  

# Save the ANOVA results 
anova_file <- "model_anova.txt"
sink(anova_file)
print(model_anova)
sink()  

## Problem 2b
intercept <- -0.005665
coef_L <- 0.458452
coef_Q <- -0.009950


# proportional to the number of countries 
L_value <- 80 / 192
Q_value <- L_value^2

log_odds = intercept + (coef_L * L_value) + (coef_Q * Q_value)

# Convert log-odds to probability
probability = exp(log_odds) / (1 + exp(log_odds))
print(probability)

## Problem 3b
# Fit model with interaction
model_with_interaction <- glm(choice ~ countries * sanctions, family=binomial(link="logit"), data=climateSupport)

# Compare models
anova(model, model_with_interaction, test="Chisq")
