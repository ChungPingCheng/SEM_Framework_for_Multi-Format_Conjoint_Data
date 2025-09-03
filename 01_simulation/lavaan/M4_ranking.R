# ------------------------------------------------------------------
# Ranking Model: SEM Specification Using Transformed Pairwise Data
# ------------------------------------------------------------------
# Note: Degrees of freedom (and associated fit indices) need adjustment when using ranking-derived pairwise comparisons.

library(lavaan)

# Load ranking-based pairwise comparison data (6 indicators for 4 products)
dta <- read.table('../data/ranking_data.txt', header = FALSE)
names(dta) <- c('RK1','RK2','RK3','RK4', 'P1_2','P1_3','P1_4','P2_3','P2_4','P3_4')

# Model specification
model_pair <- '
  # ---------------------------------------------------------
  # Structured latent preferences (Y1–Y4): 4 products
  # Each pairwise indicator loads +1 on winner, -1 on loser
  # ---------------------------------------------------------
  Y1 =~  1*P1_2 +  1*P1_3 +  1*P1_4
  Y2 =~ -1*P1_2 +  1*P2_3 +  1*P2_4
  Y3 =~ -1*P1_3 + -1*P2_3 +  1*P3_4
  Y4 =~ -1*P1_4 + -1*P2_4 + -1*P3_4

  # Structured layer identification: fix means and residuals to 0
  Y1 ~ 0*1
  Y2 ~ 0*1
  Y3 ~ 0*1
  Y4 ~ 0*1

  Y1 ~~ 0*Y1
  Y2 ~~ 0*Y2
  Y3 ~~ 0*Y3
  Y4 ~~ 0*Y4

  # ---------------------------------------------------------
  # Attribute-level latent factors (f1, f2) based on contrast patterns
  # ---------------------------------------------------------
  f1 =~ -1*Y1 +  1*Y2 + -1*Y3 +  1*Y4      # attribute 1
  f2 =~ -1*Y1 + -1*Y2 +  1*Y3 +  1*Y4      # attribute 2

  # Factor means free to estimate
  f1 ~ 1
  f2 ~ 1

  # Variance and covariance
  f1 ~~ 1*f1
  f2 ~~ f2
  f1 ~~ f2

  # ---------------------------------------------------------
  # Binary thresholds fixed at zero (probit link model)
  # ---------------------------------------------------------
  P1_2 | 0*t1
  P1_3 | 0*t1
  P1_4 | 0*t1
  P2_3 | 0*t1
  P2_4 | 0*t1
  P3_4 | 0*t1

  # Residual variances at item level (required for theta parameterization)
  P1_2 ~~ P1_2
  P1_3 ~~ P1_3
  P1_4 ~~ P1_4
  P2_3 ~~ P2_3
  P2_4 ~~ P2_4
  P3_4 ~~ P3_4
'

# Fit model using WLSMV estimator and theta parameterization
fit <- cfa(
  model = model_pair,
  data = dta,
  ordered = c("P1_2", "P1_3", "P1_4", "P2_3", "P2_4", "P3_4"),
  estimator = "WLSMV",
  parameterization = "theta",
  meanstructure = TRUE
)

# Output model summary
summary(fit, fit.measures = TRUE)


