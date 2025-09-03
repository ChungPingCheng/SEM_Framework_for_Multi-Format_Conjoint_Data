# ------------------------------------------------------------------
# Mixed-format Model (Binary yes/no + Paired Comparisons)
# ------------------------------------------------------------------

library(lavaan)

# Load binary + pairwise comparison data for 4 products (10 indicators)
dta <- read.table('../data/binary_pairwise_data.txt', header = FALSE)
names(dta) <- c('B1','B2','B3','B4','P1_2','P1_3','P1_4','P2_3','P2_4','P3_4')

# Model specification
model_mix_bin_pair <- '
  # ---------------------------------------------------------
  # Structured latent preferences (Y1–Y4): 4 products
  # Binary items load directly; pairwise = win-loss structure
  # ---------------------------------------------------------
  Y1 =~ 1*B1 +  1*P1_2 +  1*P1_3 +  1*P1_4
  Y2 =~ 1*B2 + -1*P1_2 +  1*P2_3 +  1*P2_4
  Y3 =~ 1*B3 + -1*P1_3 + -1*P2_3 +  1*P3_4
  Y4 =~ 1*B4 + -1*P1_4 + -1*P2_4 + -1*P3_4

  # Identification: means and residuals of Y fixed to zero
  Y1 ~ 0*1
  Y2 ~ 0*1
  Y3 ~ 0*1
  Y4 ~ 0*1

  Y1 ~~ 0*Y1
  Y2 ~~ 0*Y2
  Y3 ~~ 0*Y3
  Y4 ~~ 0*Y4

  # ---------------------------------------------------------
  # Attribute-level latent factors (f0, f1, f2)
  # f0 = baseline; f1/f2 = part-worth contrasts
  # ---------------------------------------------------------
  f0 =~  1*Y1 +  1*Y2 +  1*Y3 +  1*Y4
  f1 =~ -1*Y1 +  1*Y2 + -1*Y3 +  1*Y4
  f2 =~ -1*Y1 + -1*Y2 +  1*Y3 +  1*Y4

  # Identification: f0 mean = 0, f1 variance = 1
  f0 ~ 0*1
  f1 ~ 1
  f2 ~ 1

  f0 ~~ f0
  f1 ~~ 1*f1
  f2 ~~ f2
  f1 ~~ f2
  f0 ~~ f1
  f0 ~~ f2

  # ---------------------------------------------------------
  # Thresholds
  # Binary items: common threshold freely estimated (c*t1)
  # Pairwise items: thresholds fixed at zero (probit model)
  # ---------------------------------------------------------
  B1 | c*t1
  B2 | c*t1
  B3 | c*t1
  B4 | c*t1

  P1_2 | 0*t1
  P1_3 | 0*t1
  P1_4 | 0*t1
  P2_3 | 0*t1
  P2_4 | 0*t1
  P3_4 | 0*t1

  # Residual variances (required for theta parameterization)
  B1 ~~ B1
  B2 ~~ B2
  B3 ~~ B3
  B4 ~~ B4

  P1_2 ~~ P1_2
  P1_3 ~~ P1_3
  P1_4 ~~ P1_4
  P2_3 ~~ P2_3
  P2_4 ~~ P2_4
  P3_4 ~~ P3_4
'

# Fit model using WLSMV estimator and theta parameterization
fit <- cfa(
  model = model_mix_bin_pair,
  data = dta,
  ordered = c("B1", "B2", "B3", "B4", "P1_2", "P1_3", "P1_4", "P2_3", "P2_4", "P3_4"),
  estimator = "WLSMV",
  parameterization = "theta",
  meanstructure = TRUE
)

# Output model summary
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
