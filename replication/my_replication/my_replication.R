
# Replication File


# Load Libraries

library(stargazer)
library(plm)
library(lmtest)
library(Amelia)

# Load data
data <- read.csv('election_timing_economic_manipulation_data.csv',header=T)
num_variables <- ncol(data)
num_entries <- nrow(data)


# Impute data

set.seed(1234)

data_imp <- amelia(x = data, m=10, ts= "Year",
                   cs="country",ords=c('execrlc','eu','single_party','dem_age',
                                       'BBR','DR','legislative_type'), polytime = 3)


# Make function that takes reg equation as argument, outputs results

imp_fun <- function(form=y~x, mod='within',eff='time',data){ # note dat is changed
  
  b_out1 <- NULL # Ests
  se_out1 <- NULL # SEs
  r_square1 <- NULL # R2
  adj_r_square1 <-NULL # Adj R2

  for(i in 1:data$m){
    imp_data <- data$imputations[[i]]
    # Cubic polynomial to account for trend
    imp_data$t <- imp_data$Year - min(as.numeric(paste(imp_data$Year)))
    imp_data$t2 <- imp_data$t^2 
    imp_data$t3 <- imp_data$t^3
    
    
    mod1 <- plm(form,model=mod,effect = eff,index=c('country','Year'),data=imp_data) 
    
    se1 <-coeftest(mod1, vcov=vcovHC(mod1,cluster="group"))
    b_out1 <- rbind(b_out1, mod1$coef)
    se_out1 <- rbind(se_out1, se1[,2])
    r_square1 <- rbind(r_square1, summary(mod1)$r.square[1])
    adj_r_square1 <- rbind(adj_r_square1, summary(mod1)$r.square[2])
  }
  
  
  # Combine results from imputations
  combined_results1 <- mi.meld(q=b_out1, se=se_out1)
  # Get z scores
  mi_z1 <- combined_results1$q.mi/combined_results1$se.mi
  # Get p-values
  mi_p1 <- 2*(1 - pnorm(abs(mi_z1)))
  # Combine the above (minus z-score)
  combined_final1 <- cbind(t(round(combined_results1$q.mi,3)), t(round(combined_results1$se.mi,3)), round(t(mi_p1),4))
  colnames(combined_final1) <- c("EST", "SE", "P")
  
  r2 <- sum(r_square1)/length(r_square1)
  adj_r2 <- sum(adj_r_square1)/length(adj_r_square1)
  
  
  return(list(combined_final1,r2,adj_r2, nrow(imp_data)))
  
}


# Table 1, MODELS 1 - 8

mod1_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+log_gdp_dol+inflation+revenue+gross_debt+
                      vote_share+govfrac, 
                    mod="within",eff="time",data=data_imp)

mod1_imp

mod2_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+log_gdp_dol+inflation+revenue+gross_debt+
                      vote_share+govfrac+t+t2+t3,
                    mod="pooling",data=data_imp)


mod3_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+log_gdp_dol+inflation+revenue+gross_debt+
                      vote_share+govfrac+t+t2+t3, 
                    mod="within", eff="individual",data=data_imp)

mod4_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+log_gdp_dol+inflation+revenue+gross_debt+
                      vote_share+govfrac, mod="within",eff="twoways",data=data_imp)

mod5_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP,
                    mod="within",eff="time",data=data_imp)

mod6_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+t+t2+t3, mod="pooling",data=data_imp)

mod7_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP+t+t2+t3, mod="within",eff="individual",data=data_imp)

mod8_imp <- imp_fun(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                      change_GDP, mod="within",eff="twoways",data=data_imp)


# Make baseline, non imputed, models, replace coefficients THIS IS JUST FOR STARGAZER TO PRINT OUT imputed data

# Add cubic polynomial to baseline data
data2 <- data
data2$t <- data2$Year - min(as.numeric(paste(data2$Year)))
data2$t2 <- data2$t^2 
data2$t3 <- data2$t^3

mod1_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+log_gdp_dol+inflation+revenue+gross_debt+vote_share+govfrac,index=c("country","Year"), model="within",effect="time", data=data2)
mod2_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+log_gdp_dol+inflation+revenue+gross_debt+vote_share+govfrac+t+t2+t3,index=c("country","Year"), model="pooling", data=data2)
mod3_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+log_gdp_dol+inflation+revenue+gross_debt+vote_share+govfrac+t+t2+t3, model="within",effect="individual",index=c("country","Year"), data=data2)
mod4_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+log_gdp_dol+inflation+revenue+gross_debt+vote_share+govfrac, model="within",effect="twoways",index=c("country","Year"), data=data2)
mod5_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP, model="within",effect="time",index=c("country","Year"), data=data2)
mod6_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+t+t2+t3, model="pooling",index=c("country","Year"), data=data2)
mod7_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP+t+t2+t3, model="within",effect="individual",index=c("country","Year"), data=data2)
mod8_base <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+change_GDP, model="within",effect="twoways",index=c("country","Year"), data=data2)


# Step 1: Extract Necessary Components from plm models
extract_plm_for_stargazer <- function(model) {
  coef_df <- summary(model)$coef  # Extracting the model's summary coefficients table
  coef <- coef_df[, 1]  # Coefficients
  se <- coef_df[, 2]  # Standard errors
  return(list(coef = coef, se = se))
}

# Step 2: Prepare  data for stargazer
models_list <- list(mod1_base, mod2_base, mod3_base, mod4_base, mod5_base, mod6_base, mod7_base, mod8_base)
extracted_summaries <- lapply(models_list, extract_plm_for_stargazer)

coefs <- lapply(extracted_summaries, function(x) x$coef)
ses <- lapply(extracted_summaries, function(x) x$se)

# Prepare R^2 and Adjusted R^2 values
r_squared <- sapply(list(mod1_imp, mod2_imp, mod3_imp, mod4_imp, mod5_imp, mod6_imp, mod7_imp, mod8_imp), function(model) round(model[[2]], 3))
adj_r_squared <- sapply(list(mod1_imp, mod2_imp, mod3_imp, mod4_imp, mod5_imp, mod6_imp, mod7_imp, mod8_imp), function(model) round(model[[3]], 3))

# Use stargazer with the manually extracted coefficients, standard errors, and add R^2 and Adjusted R^2
stargazer(models_list, type = "latex",
          coef=list(coefs[[1]], coefs[[2]], coefs[[3]], coefs[[4]], coefs[[5]], coefs[[6]], coefs[[7]], coefs[[8]]),
          se=list(ses[[1]], ses[[2]], ses[[3]], ses[[4]], ses[[5]], ses[[6]], ses[[7]], ses[[8]]),
          covariate.labels = c("Lagged Consumption to Investment (Log)", "Electoral Cycle", "Dissolution Powers",
                               "GDP Growth", "GDP (Log)", "Inflation", "Revenue", "Debt to GDP", "Vote Share", "Government Fraction",
                               "Time", "Time Squared", "Time Cubed", "Electoral Cycle times Dissolution Powers"),
          add.lines = list(c("R-squared", r_squared), c("Adjusted R-squared", adj_r_squared)),
          omit.stat = c("rsq", "f", "ser"), # Since we are manually adding R^2 and Adjusted R^2
          out = "model_summary2_table.tex")



# Figure 1

dy_de_coef <- NULL
dy_dp_coef <- NULL
dy_de_se <- NULL
dy_dp_se <- NULL

for(i in 1:data_imp$m){
  dat <- data_imp$imputations[[i]]
  #  dat <- pdata.frame(dat, index=c("country", "Year"))
  
  mod1_pool <- plm(log_con_inv~lag(log_con_inv)+prop_elapsed*max_pm_gov_leg+
                     change_GDP+log_gdp_dol+inflation+revenue+gross_debt+
                     vote_share,index=c("country","Year"), 
                   model="within",effect='time', data=dat)
  ests <- coef(mod1_pool)
  cov_mat <- vcovHC(mod1_pool,cluster="group") # For FE
  DP <- seq(0,10)
  E <- seq(0,10)/10
  dy_de_coef <- rbind(dy_de_coef, ests["prop_elapsed"] + ests["prop_elapsed:max_pm_gov_leg"]*DP)
  dy_dp_coef <- rbind(dy_dp_coef, ests["max_pm_gov_leg"] + ests["prop_elapsed:max_pm_gov_leg"]*E)
  
  dy_de_se <- rbind(dy_de_se, sqrt(cov_mat["prop_elapsed","prop_elapsed"] +DP^2*cov_mat["prop_elapsed:max_pm_gov_leg","prop_elapsed:max_pm_gov_leg"] + 2*DP*cov_mat["prop_elapsed","prop_elapsed:max_pm_gov_leg"]))
  dy_dp_se <- rbind(dy_dp_se, sqrt(cov_mat["max_pm_gov_leg","max_pm_gov_leg"] +E^2*cov_mat["prop_elapsed:max_pm_gov_leg","prop_elapsed:max_pm_gov_leg"] + 2*E*cov_mat["max_pm_gov_leg","prop_elapsed:max_pm_gov_leg"]))
}

combined_1 <- mi.meld(q=dy_de_coef, se=dy_de_se)
upper1 <- combined_1$q.mi + 1.96*combined_1$se.mi
lower1 <- combined_1$q.mi - 1.96*combined_1$se.mi

combined_2 <- mi.meld(q=dy_dp_coef, se=dy_dp_se)
upper2 <- combined_2$q.mi + 1.96*combined_2$se.mi
lower2 <- combined_2$q.mi - 1.96*combined_2$se.mi

# Figure 1a

par(mar=c(4,4.1,4.1,3.2)) # more space on right, less on left

# histogram
hist(data2$max_pm_gov_leg, xlab=NULL, ylab=NULL, col = "light gray"
     ,lty=0, axes = F, main=NULL)
axis(4) # Put labels on right of plot
mtext("Observations", side=4, line=2)
rug(jitter(data2$max_pm_gov_leg, amount=1)) #rugplot
par(new=TRUE) # To overlap ME plot on histogram

plot(c(min(DP), max(DP)), c(min(lower1), max(upper1)), xlab="Dissolution Powers",
     ylab="", main="Marginal Effect of \n Proportion of Term Elapsed", type="n") # type="n" removes points
lines(DP, combined_1$q.mi, lwd=2)
lines(DP, lower1, lty = 3, lwd=2) # lwd makes lines thicker
lines(DP, upper1, lty = 3, lwd=2)
mtext("ME of Term Elapsed", side=2, line=2)
abline(h=0)


# Figure 1b
par(mar=c(4,4.1,4.1,3.2)) # more space on right, less on left

# histogram
hist(data2$prop_elapsed, xlab=NULL, ylab=NULL, col = "light gray"
     ,lty=0, axes = F, main=NULL)
axis(4) # Put labels on right of plot
mtext("Observations", side=4, line=2)
rug(jitter(data2$prop_elapsed, amount=1)) #rugplot
par(new=TRUE) # To overlap ME plot on histogram

plot(c(min(E), max(E)), c(min(lower2), max(upper2)), xlab="Term Elapsed",
     ylab="", main="Marginal Effect of \n Dissolution Powers", type="n") # type="n" removes points
lines(E, combined_2$q.mi, lwd=2)
lines(E, lower2, lty = 3, lwd=2) # lwd makes lines thicker
lines(E, upper2, lty = 3, lwd=2)
mtext("ME of Diss. Powers", side=2, line=2)
abline(h=0)


#### extended model########
library(ggplot2)
library(dplyr)
library(coefplot)
library(broom)
library(dotwhisker)
library(pROC)

# Calculate year-over-year change in log_con_inv
data <- data %>% 
  group_by(country) %>%
  mutate(delta_log_con_inv = log_con_inv - lag(log_con_inv))

# Define significant economic manipulation as top quartile of changes
threshold <- quantile(data$delta_log_con_inv, 0.75, na.rm = TRUE)

# Create a binary variable for significant economic manipulation
data$Economic_Manipulation <- ifelse(data$delta_log_con_inv >= threshold, 1, 0)


# Fit the logistic regression model to the entire dataset
model_logi <- glm(Economic_Manipulation ~ change_GDP + single_party + Year + inflation + revenue, 
                  data = data, family = "binomial")
summary(model_logi)
# draw a dot-and-whisker plot
dwplot(model_logi)

# This extracts the dataset that the model was fit to, which handles rows with NAs
fitted_data <- model_logi$model
fitted_data$std_residuals_logi <- residuals(model_logi, type = "pearson")
# Create the residual plot
ggplot(fitted_data, aes(x = change_GDP, y = std_residuals_logi)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Logistic Regression", x = "Change in GDP", y = "Standardized Residuals") +
  theme_minimal()



# Display the model summary 
summary(model_logi)

# create a new data frame with the values to predict probabilities
newdata <- data.frame(change_GDP = seq(min(data$change_GDP), max(data$change_GDP), length.out = 100),
                      single_party = mean(data$single_party),
                      Year = mean(data$Year),
                      inflation = mean(data$inflation),
                      revenue = mean(data$revenue))

# predict probabilities
newdata$predicted_prob <- predict(model_logi, newdata = newdata, type = "response")

# create the plot
ggplot(newdata, aes(x = change_GDP, y = predicted_prob)) +
  geom_line() +
  labs(x = "Change in GDP", y = "Predicted Probability of Economic Manipulation")

ggplot() +
  geom_line(data = newdata, aes(x = change_GDP, y = predicted_prob), size = 1) +  # Plot the predicted probabilities
  geom_point(data = data, aes(x = change_GDP, y = Economic_Manipulation, color = factor(eu)), alpha = 0.5) +  # Add the actual data points
  scale_color_manual(values = c("blue", "red"), labels = c("Non-EU Country", "EU Country")) +
  labs(x = "Change in GDP", y = "Predicted Probability of Economic Manipulation", color = "Country Type") +
  theme_minimal()

ggplot() +
  geom_line(data = newdata, aes(x = change_GDP, y = predicted_prob), size = 1) +  # Plot the predicted probabilities
  geom_point(data = data, aes(x = change_GDP, y = Economic_Manipulation, color = factor(single_party)), alpha = 0.5) +  # Add the actual data points
  scale_color_manual(values = c("blue", "red"), labels = c("single party", "non singel party")) +
  labs(x = "Change in GDP", y = "Predicted Probability of Economic Manipulation", color = "Country Type") +
  theme_minimal()


plot <- ggplot() +
  geom_line(data = newdata, aes(x = change_GDP, y = predicted_prob), size = 1) +  # Plot the predicted probabilities
  geom_point(data = data, aes(x = change_GDP, y = Economic_Manipulation, color = as.factor(Economic_Manipulation)), alpha = 0.5) +  # Add the actual data points
  scale_color_manual(values = c("green", "red"), labels = c("non-occurrences", "occurrences of manipulation ")) +
  labs(x = "Change in GDP", y = "Predicted Probability Manipulation", color = "Outcome") +
  theme_minimal()

print(plot)

predicted_probs <- predict(model_logi, newdata = data, type = "response")

# Generate the ROC curve and calculate the AUC
roc_curve <- roc(data$Economic_Manipulation, predicted_probs)
auc_value <- auc(roc_curve)

# Plot the ROC curve
plot(roc_curve, main="ROC Curve", col="yellow", lwd=2)

# Add the AUC to the plot
auc_value <- auc(roc_curve)
legend("bottomright", legend=paste("AUC =", round(auc_value, 4)), box.lty=1, col="yellow", lwd=2)
# Print the AUC value
print(auc_value)

# Fit the Poisson regression model to the entire dataset

manipulation_count <- data %>%
   group_by(country, Year) %>%
  summarise(manipulation_count = sum(Economic_Manipulation, na.rm = TRUE)) %>%
  ungroup()  
data <- merge(data, manipulation_count, by = c("country", "Year"))

model_pois <- glm(manipulation_count ~ change_GDP + single_party + Year + inflation + revenue, 
                  data = data, family = "poisson")

summary(model_pois)

# Calculate the standardized residuals
data$std_residuals <- rstandard(model_pois)

# Create the residual plot
ggplot(data, aes(x = change_GDP, y = std_residuals)) +
  geom_point(alpha = 0.5, color="red") +  
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Poisson", x = "Change in GDP", y = "Standardized Residuals") +
  theme_minimal()
