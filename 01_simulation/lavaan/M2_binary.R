# -----------------------------------------------------
# Binary Yes/No Model: Fixed loading design for attributes
# Uses WLSMV estimator and theta parameterization
# -----------------------------------------------------

library(lavaan)

# Load binary data: 4 products, each with binary response
dta <- read.table('../data/binary_data.txt', header = FALSE)
names(dta) <- c('B1', 'B2', 'B3', 'B4')

# Specify model: 3 latent factors (f0: baseline, f1–f2: attributes)
model_binary <- '
  # Structured part-worth factors with fixed loading patterns
  f0 =~  1*B1 +  1*B2 +  1*B3 +  1*B4      # baseline (all +1)
  f1 =~ -1*B1 +  1*B2 + -1*B3 +  1*B4      # attribute 1
  f2 =~ -1*B1 + -1*B2 +  1*B3 +  1*B4      # attribute 2

  # Thresholds: all binary indicators share common threshold "c"
  B1 | c*t1
  B2 | c*t1
  B3 | c*t1
  B4 | c*t1

  # Latent means
  f0 ~ 0*1      # fixed at 0 for identification
  f1 ~ 1        # freely estimated
  f2 ~ 1        # freely estimated

  # Variance identification (at least one factor fixed)
  f1 ~~ 1*f1
'

# Fit model using WLSMV with theta parameterization
fit <- cfa(
  model = model_binary,
  data = dta,
  ordered = c("B1", "B2", "B3", "B4"),
  parameterization = "theta",
  estimator = "WLSMV",
  meanstructure = TRUE
)

# Summary with model fit and standardized estimates
summary(fit, fit.measures = TRUE)
