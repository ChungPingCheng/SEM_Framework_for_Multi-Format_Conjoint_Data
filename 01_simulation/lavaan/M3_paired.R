# ---------------------------------------------------------
# Paired Comparison Model: SEM specification of preferences
# ---------------------------------------------------------

library(lavaan)

# Load pairwise comparison data (6 binary indicators for 4 products)
dta <- read.table('../data/pairwise_data.txt', header = FALSE)
names(dta) <- c('P1_2', 'P1_3', 'P1_4', 'P2_3', 'P2_4', 'P3_4')

# Specify model
model_pair <- '
  # ---------------------------------------------------------
  # Structured preferences (Y1–Y4): each represents one product
  # Each binary comparison loads +1 on winner, -1 on loser
  # ---------------------------------------------------------
  Y1 =~  1*P1_2 +  1*P1_3 +  1*P1_4
  Y2 =~ -1*P1_2 +  1*P2_3 +  1*P2_4
  Y3 =~ -1*P1_3 + -1*P2_3 +  1*P3_4
  Y4 =~ -1*P1_4 + -1*P2_4 + -1*P3_4

  # Identification: fix means and variances of structured latent variables
  Y1 ~ 0*1
  Y2 ~ 0*1
  Y3 ~ 0*1
  Y4 ~ 0*1

  Y1 ~~ 0*Y1
  Y2 ~~ 0*Y2
  Y3 ~~ 0*Y3
  Y4 ~~ 0*Y4

  # ---------------------------------------------------------
  # Part-worth factors (f1–f2): interpret attribute structure
  # ---------------------------------------------------------
  f1 =~ -1*Y1 +  1*Y2 + -1*Y3 +  1*Y4      # attribute 1
  f2 =~ -1*Y1 + -1*Y2 +  1*Y3 +  1*Y4      # attribute 2

  # Free latent means
  f1 ~ 1
  f2 ~ 1

  # Variances and covariance
  f1 ~~ 1*f1
  f2 ~~ f2
  f1 ~~ f2

  # ---------------------------------------------------------
  # Binary thresholds fixed at zero (probit link)
  # ---------------------------------------------------------
  P1_2 | 0*t1
  P1_3 | 0*t1
  P1_4 | 0*t1
  P2_3 | 0*t1
  P2_4 | 0*t1
  P3_4 | 0*t1

  # Free residual variances (required for theta parametrization)
  P1_2 ~~ P1_2
  P1_3 ~~ P1_3
  P1_4 ~~ P1_4
  P2_3 ~~ P2_3
  P2_4 ~~ P2_4
  P3_4 ~~ P3_4
'

# Fit model using WLSMV and theta parameterization
fit <- cfa(
  model = model_pair,
  data = dta,
  ordered = c("P1_2", "P1_3", "P1_4", "P2_3", "P2_4", "P3_4"),
  estimator = "WLSMV",
  parameterization = "theta",
  meanstructure = TRUE
)

# Model summary
summary(fit, fit.measures = TRUE)
