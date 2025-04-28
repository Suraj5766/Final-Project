
data=read.csv(file.choose(),header = T)
data
head(data)
##DATA 

Surname=data$Surname
Surname
CreditScore=data$CreditScore
CreditScore
Geography=data$Geography
Geography
Gender=data$Gender
Gender
Age=data$Age
Age
Tenure=data$Tenure
Tenure
Balance=data$Balance
Balance
Num.Of.accounts=data$Num.Of.accounts
Num.Of.accounts
Cr.Card=data$Cr.Card
Cr.Card
active.member=data$active.member
active.member
monthly.Salary=data$monthly.Salary
monthly.Salary
Exited=data$Exited
Exited


install.packages("survminer")
library(survival)
library(survminer)

# Create a survival object 
surv_obj = Surv(Tenure, Exited)
surv_obj

# 1) Fit survival curves for Male vs. FeMale
fit_Gen = survfit(surv_obj ~ Gender, data = data)
fit_Gen

# Plot the survival curves Male and Female
plot(
  fit_Gen ,
  col = c("green", "red"), 
  lty = 1:2,                  # Line types for the groups
  xlab = "time(Year)", 
  ylab = "Survival Probability Exised",
  main = "Survival Curves: Male vs Female "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" Male", "Female"),
  col = c("green", "red"),
  lty = 1:2
)


# 2) Fit survival curves for Male vs. FeMale
fit_Geo = survfit(surv_obj ~ Geography, data = data)
fit_Geo
# Plot the survival curves Geography
plot(
  fit_Geo ,
  col = c("green", "red","blue"), 
  lty = 1:2,                  # Line types for the groups
  xlab = "time(days)", 
  ylab = "Survival Probability Exised",
  main = "Survival Curves: Geography "
)

# Add a legend
legend(
  "bottomleft",
  legend = c(" France", "Spain","Germany"),
  col = c("green", "red","blue"),
  lty = 1:2
)



# Perform the log-rank test for Gender
logrank_Gen = survdiff(surv_obj ~ Gender, data = data)
logrank_Gen

# Perform the log-rank test for Geography
logrank_Geo = survdiff(surv_obj ~ Geography, data = data)
logrank_Geo

# schofield residual
ph_test=cox.zph(cox_model)
ph_test




# Test proportional hazards assumption
cox_model = coxph(surv_obj ~ CreditScore+Geography+Gender+Age+Balance+Num.Of.accounts+Cr.Card+active.member+monthly.Salary, data = data)
cox_model
summary(cox_model)


