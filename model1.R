# ==========================================
# Regression Analysis with Diagnostics
# ==========================================

install.packages("readxl")
install.packages("olsrr")
install.packages("car")
install.packages("lmtest")

library(readxl)
library(olsrr)
library(car)
library(lmtest)

data <- read_excel("data.xlsx")
summary(data)

data$post <- 1

fund_type <- data$"Fund Type"
post <- data$post
investment_style <- ifelse(data$"Investment Style" == "Quantitative", 1, 0)
freturn_ytd <- data$"Fund Return (YTD)"
freturn_2025 <- data$"Fund Return (2025)"
br <- data$"Benchmark Return"
actual <- data$"Actual (1-YR)"
predicted <- data$"Predicted"
fsize_2025 <- data$"Fund Size 2025"
fsize_2026 <- data$"Fund Size 2026"
er <- data$"Expense Ratio (%)"
fr_load <- as.numeric(sub("%/.*", "", data$`Front / Rear Load (%)`))
fund_flow <- data$"Fund Flow"
flow_vol <- data$"Flow Volatility (annualized)"
btm <- data$"Boook to Market"
mom <- data$"Momentum (Y2025 return)"
market_cap <- data$"Market Cap"
fund_age <- as.numeric(gsub(" years", "", data$"Fund Age (Years)"))

model1 <- lm(freturn_2025 ~ investment_style + fsize_2025 + fsize_2026 + er + fr_load + flow_vol + btm + mom + fund_age)
summary(model1)
