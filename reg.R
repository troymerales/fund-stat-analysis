model1 <- lm(
  freturn_2025 ~ investment_style + DAP + er + fr_load + fund_flow + flow_vol + btm + mom + fund_age,
  data = data
)


# 1. Original Regression 
summary(model1)

# 2. Linearity Test (Ramsey RESET Test)
resettest(model1)

# 3. Normality Test
shapiro.test(residuals(model1))

# 4. Homoscedasticity Test (Breusch-Pagan)
bptest(model1)

# 5. Multicollinearity Test (VIF)
vif(model1)

# 6. Independence of Errors (Durbin-Watson)
dwtest(model1)

# 7. ENDOGENEITY TEST (Durbin-Wu-Hausman
first_stage <- lm(fund_flow ~ investment_style + DAP + er + fr_load +
                    flow_vol + btm + mom + fund_age, data = data, na.action = na.exclude)

res_flow <- residuals(first_stage)

hausman_model <- lm(freturn_2025 ~ investment_style + DAP + er + fr_load +
                      fund_flow + flow_vol + btm + mom + fund_age + res_flow,
                    data = data)

summary(hausman_model)


# 8. Diagnostic Plot
par(mfrow=c(2,2))
plot(model1)

# 9. Cook's Distance 
cooks <- cooks.distance(model1)

plot(model1, which = 4)

# Identify influential observations
outliers <- which(cooks > 4/nrow(data))

print(outliers)

# ------------------------------------------
# 10. Remove Outliers
# ------------------------------------------
new_data <- data[-outliers, ]


# 11. Run Regression Again
new_model <- lm(freturn_2025 ~ investment_style + DAP + er + fr_load + fund_flow + flow_vol + btm + mom + fund_age, data = new_data)

# 12. Final Results
summary(new_model)




#MODEL 2
model2 <- lm(
  freturn_2025 ~ investment_style + DAP, data = data)

# 1. Original Regression 
summary(model2)

# 2. Linearity Test (Ramsey RESET Test)
resettest(model2)

# 3. Normality Test
shapiro.test(residuals(model2))

# 4. Homoscedasticity Test (Breusch-Pagan)
bptest(model2)

# 5. Multicollinearity Test (VIF)
vif(model2)

# 6. Independence of Errors (Durbin-Watson)
dwtest(model2)

# 7. ENDOGENEITY TEST (Durbin-Wu-Hausman
first_stage <- lm(DAP ~ investment_style, data = data)

res_DAP <- residuals(first_stage)

hausman_model <- lm(
  freturn_2025 ~ investment_style + DAP + res_DAP,
  data = data
)

summary(hausman_model)

# 9. Cook's Distance 
cooks <- cooks.distance(model1)

plot(model1, which = 4)

# Identify influential observations
outliers <- which(cooks > 4/nrow(data))

print(outliers)

# ------------------------------------------
# 10. Remove Outliers
# ------------------------------------------
new_data <- data[-outliers, ]

new_model2 <- lm(
  freturn_2025 ~ investment_style + DAP, data = new_data)

summary(new_model2)
