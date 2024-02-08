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
# Function to calculate the p-value using the fix formula
ks_p_value <- function(D) {
  # fist part of the formula
  pi_sqrt_2 <- sqrt(2 / pi)  # This is √(2/π)
  
  # accumulate the sum of k
  sum_val <- 0
  k_max <- 500
  for (k in 1:k_max) {
    sum_val <- sum_val + exp(-2 * (2 * k - 1)^2 * pi^2 / (8 * D^2))
  }
  
  # Calculate the p-value using the accumulated sum
  p_value <- pi_sqrt_2 * sum_val / D  
  
  # Ensure the p-value is within [0, 1]
  p_value <- min(max(p_value, 0), 1)
  
  return(p_value)
}

# Function to perform the Kolmogorov-Smirnov test
ks_test <- function(data) {
  
  # Create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Generate test statistic D
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  # Calculate the p-value using the  formula function in last step and D
  p_value <- ks_p_value(D)  
  
  # Return the test statistic and p-value
  return(list(D = D, p_value = p_value))
}

set.seed (123)
# Generate 1,000 cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)

# Perform the Kolmogorov-Smirnov test
ks_test_result <- ks_test(data)

# Print the results
print(ks_test_result)

# valid with ks function
ks_result <- ks.test(data, "pnorm")

# Print the results
print(ks_result)

#####################
# Problem 2
#####################
# data set generated
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# step 1: function to calculate sum of square residual (ssr)
#data$x - Predictor, data$y - Response

ssr <- function(params) {
  predicted_values <- params[1] + params[2] * data$x
  residuals <- data$y - predicted_values
  sum(residuals^2)
}

# step2: use the optim() function with the BFGS method to
#find the parameter estimates that minimize the sum of squared residuals

estimate_params <- c(0, 0) #  guesses for intercept and slope
optim_results <- optim(par = estimate_params, fn = ssr, method = "BFGS")
bfgs_params <- optim_results$par

#For comparison, fit an OLS model using the lm() function
#Estimate Parameters using lm()
lm_model <- lm(y ~ x, data = data)
lm_params <- coef(lm_model)

print(bfgs_params) 
print(lm_params) 
