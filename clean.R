# ==========================================
# LOAD PACKAGES
# ==========================================

packages <- c("readxl", "olsrr", "car", "lmtest")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ==========================================
# LOAD DATA
# ==========================================

data <- readxl::read_excel("data.xlsx")
summary(data)

# Remove problematic row
data <- data[-17, ]

# ==========================================
# DATA CLEANING
# ==========================================

# Convert fund sizes to numeric
data$"Fund Size 2025" <- as.numeric(data$"Fund Size 2025")
data$"Fund Size 2026" <- as.numeric(data$"Fund Size 2026")

# Replace NA fund sizes with 0
data$"Fund Size 2025"[is.na(data$"Fund Size 2025")] <- 0
data$"Fund Size 2026"[is.na(data$"Fund Size 2026")] <- 0

# Clean Front / Rear Load and compute ratio
data$fr_load <- sapply(
  strsplit(gsub("%", "", data$"Front / Rear Load (%)"), "/"),
  function(x) {
    nums <- suppressWarnings(as.numeric(x))
    
    if (length(nums) < 2 || any(is.na(nums))) return(0)
    if (nums[2] == 0) return(0)
    
    nums[1] / nums[2]
  }
)

# Clean fund age
data$fund_age <- as.numeric(gsub(" years", "", data$"Fund Age (Years)"))

# ==========================================
# CREATE REGRESSION VARIABLES
# ==========================================

investment_style <- ifelse(data$"Investment Style" == "Quantitative", 1, 0)

freturn_ytd  <- data$"Fund Return (YTD)"
freturn_2025 <- data$"Fund Return (2025)"

fsize_2025 <- data$"Fund Size 2025"
fsize_2026 <- data$"Fund Size 2026"

er <- data$"Expense Ratio (%)"
fund_flow <- data$"Fund Flow"
flow_vol <- data$"Flow Volatility (annualized)"
btm <- data$"Boook to Market"

data$"Momentum (Y2025 return)"[is.na(data$"Momentum (Y2025 return)")] <- 0
mom <- data$"Momentum (Y2025 return)"

fund_age <- data$fund_age

model1 <- lm(freturn_2025 ~ investment_style + fsize_2025 + fsize_2026 + er + fr_load + flow_vol + btm + mom + fund_age)
summary(model1)