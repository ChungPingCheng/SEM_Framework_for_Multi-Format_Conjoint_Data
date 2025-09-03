# -----------------------------------------------------
# Rating Model: Fixed loading design for conjoint items
# Part-worths are defined via structured CFA model
# -----------------------------------------------------

library(lavaan)

# Load rating data: 4 products, each rated on a fixed scale
dta <- read.table('../data/rating_data.txt', header = FALSE)
names(dta) <- c('RT1', 'RT2', 'RT3', 'RT4')

# Specify model: 3 latent factors (f0: baseline, f1–f2: attributes)
model_rating <- '
  # First-order structured factors (fixed-loading design)
  f0 =~  1*RT1 +  1*RT2 +  1*RT3 +  1*RT4      # baseline (all +1)
  f1 =~ -1*RT1 +  1*RT2 + -1*RT3 +  1*RT4      # attribute 1
  f2 =~ -1*RT1 + -1*RT2 +  1*RT3 +  1*RT4      # attribute 2

  # Intercepts: all items share same intercept "c" (identified here)
  RT1 ~ c*1
  RT2 ~ c*1
  RT3 ~ c*1
  RT4 ~ c*1

  # Latent means
  f0 ~ 0*1      # fixed at 0 for identification
  f1 ~ 1        # freely estimated
  f2 ~ 1        # freely estimated
'

# Estimate model using CFA framework (meanstructure = TRUE)
fit <- cfa(
  model = model_rating,
  data = dta,
  meanstructure = TRUE
)

# Summary with fit indices and parameter estimates
summary(fit, fit.measures = TRUE)
