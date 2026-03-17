# ==========================================
# LOAD PACKAGES
# ==========================================

packages <- c("readxl", "olsrr", "car", "lmtest", "psych")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# ==========================================
# LOAD DATA
# ==========================================

data <- read_excel("data.xlsx")

# Remove problematic row
data <- data[-17, ]

# ==========================================
# DATA CLEANING
# ==========================================

# Convert fund sizes to numeric
data$`Fund Size 2025` <- as.numeric(data$`Fund Size 2025`)
data$`Fund Size 2026` <- as.numeric(data$`Fund Size 2026`)

# Replace NA fund sizes with 0
data$`Fund Size 2025`[is.na(data$`Fund Size 2025`)] <- 0
data$`Fund Size 2026`[is.na(data$`Fund Size 2026`)] <- 0

# Clean Front / Rear Load and compute ratio
data$fr_load <- sapply(
  strsplit(gsub("%", "", data$`Front / Rear Load (%)`), "/"),
  function(x) {
    nums <- suppressWarnings(as.numeric(x))
    
    if (length(nums) < 2 || any(is.na(nums))) return(0)
    if (nums[2] == 0) return(0)
    
    nums[1] / nums[2]
  }
)

# Clean fund age
data$fund_age <- as.numeric(gsub(" years", "", data$`Fund Age (Years)`))

# ==========================================
# CREATE REGRESSION VARIABLES
# ==========================================

data$investment_style <- ifelse(data$`Investment Style` == "Quantitative", 1, 0)

data$freturn_ytd  <- data$`Fund Return (YTD)`
data$freturn_2025 <- data$`Fund Return (2025)`

data$fsize_2025 <- data$`Fund Size 2025`
data$fsize_2026 <- data$`Fund Size 2026`

data$er <- data$`Expense Ratio (%)`
data$fund_flow <- data$`Fund Flow`
data$flow_vol <- data$`Flow Volatility (annualized)`
data$btm <- data$`Boook to Market`

# Momentum cleaning
data$mom <- data$`Momentum (Y2025 return)`
data$mom[is.na(data$mom)] <- 0

data$actual <- data$`Actual (1-YR)`
data$predicted <- data$`Predicted`

# Dependent variable
data$DAP <- data$actual - data$predicted

# ==========================================
# DESCRIPTIVE STATISTICS
# ==========================================

#describe(data[, sapply(data, is.numeric)])[, c("mean","sd","min","max")]
summary(data)
