#install.packages("Hmisc")

library(Hmisc)
library(readxl)

# Load dataset
data <- read_excel("data.xlsx")

# Convert Fund Age to numeric
data$Fund_Age <- as.numeric(gsub(" years", "", data$`Fund Age (Years)`))

# Create Difference of Actual and Predicted Return
data$Diff_AP <- data$`Actual (1-YR)` - data$Predicted

# Clean Front / Rear Load
data$fr_load <- sapply(
  strsplit(gsub("%", "", data$`Front / Rear Load (%)`), "/"),
  function(x) {
    nums <- suppressWarnings(as.numeric(x))
    
    if (length(nums) < 2 || any(is.na(nums))) return(NA)
    if (nums[2] == 0) return(NA)
    
    nums[1] / nums[2]
  }
)

# Select numeric variables for Pearson correlation
vars <- data[, c(
  "Fund Return (YTD)",
  "Diff_AP",
  "Fund Flow",
  "Flow Volatility (annualized)",
  "Expense Ratio (%)",
  "Boook to Market",
  "Momentum (Y2025 return)",
  "Fund_Age"
)]

# Add cleaned load variable
vars$fr_load <- data$fr_load

# Pearson correlation with p-values
pearson_results <- rcorr(as.matrix(vars), type = "pearson")

pearson_results
