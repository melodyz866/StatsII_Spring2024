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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Create a categorical variable for GDP difference
gdp_data$GDPWdiff_Cat <- cut(gdp_data$GDPWdiff,
                            breaks = c(-Inf, -0.01, 0.01, Inf),
                            labels = c("negative", "no change", "positive"))

#construct the multinomial logit model 
multinom_model <- multinom(GDPWdiff_Cat ~ REG + OIL, data = gdp_data, ref = "no change")

# Display the summary of the model
summary(multinom_model)
summary_output <- capture.output(summary(multinom_model))

# Write the output to a text file
writeLines(summary_output, "model_summary.txt")
# 2. order multinomial logit model 
ordered_model <- polr(GDPWdiff_Cat ~ REG + OIL, data = gdp_data, Hess=TRUE)

summary(ordered_model)
summary_output <- capture.output(summary(ordered_model))


writeLines(summary_output, "ordered_summary.txt")
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Constructing the Poisson regression model
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
             family = poisson(link = "log"), 
             data = mexico_elections)

summary(poisson_model)
coefficients_summary <- summary(poisson_model)$coefficients

write.table(coefficients_summary, file = "coefficients_summary.txt", sep = "\t", col.names = NA, quote = FALSE)
# 3 the estimate log count 
log_count <- predict(poisson_model, newdata = data.frame(competitive.district=1, marginality.06=0, PAN.governor.06=1), type = "link")
#Convert  to the original count 
count_predicted <- exp(log_count)

print(count_predicted)
