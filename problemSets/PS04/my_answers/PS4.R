# load libaries
library(eha)
library(survival)

#load data
data(child)
head(child)

# Fit the Cox Proportional Hazards model
cox_model <- coxph(Surv(enter,exit,event) ~ m.age + sex, data = child)

# Display the model summary
summary(cox_model)

plot_cox<-coxreg(Surv(enter,exit,event) ~ m.age + sex, data = child)

plot(plot_cox)