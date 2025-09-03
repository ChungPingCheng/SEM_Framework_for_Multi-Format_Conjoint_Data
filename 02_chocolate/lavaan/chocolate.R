# ==========================================================
# SEM for Pairwise Comparisons (Chocolate data)
# Format: full ranking -> 28 implied pairwise comparisons
# Estimation: WLSMV with theta parameterization in lavaan
# Goal: Replicate the Mplus setup used in the manuscript
# ==========================================================

library(lavaan)

# ----------------------------------------------------------
# 1) Data import
#    The raw file contains 28 binary indicators (0/1) for
#    the implied pairwise comparisons among 8 products.
#    No header row in the text file; we assign names below.
# ----------------------------------------------------------
dta <- read.table("../data/chocolate_pairwise.txt", header = FALSE)

# ----------------------------------------------------------
# 2) Declare ordered (categorical) indicators
#    Each name encodes a pairwise comparison, e.g., i1_i7 is
#    the binary outcome for product 1 preferred over product 7.
# ----------------------------------------------------------
ord_vars <- c(
  "i1_i7","i1_i8","i1_i10","i1_i12","i1_i14","i1_i15","i1_i16",
  "i7_i8","i7_i10","i7_i12","i7_i14","i7_i15","i7_i16",
  "i8_i10","i8_i12","i8_i14","i8_i15","i8_i16",
  "i10_i12","i10_i14","i10_i15","i10_i16",
  "i12_i14","i12_i15","i12_i16",
  "i14_i15","i14_i16",
  "i15_i16"
)
names(dta) <- ord_vars

# ----------------------------------------------------------
# 3) Model specification
#    Three-layer perspective (per manuscript):
#    - Part-worth factors (not explicitly listed here);
#    - Structured preferences for 8 products (i1, i7, ..., i16);
#    - Binary observed responses as thresholded differences.
#
#    Here, we operationalize the middle layer by specifying
#    8 first-order latent “structured preferences” (i1,...,i16),
#    each measured by the pairwise indicators with fixed ±1
#    loadings to encode the differencing.
#    Higher-order conceptual factors (k1, k2, k3, pr, w, c)
#    summarize structure across products (for exposition).
# ----------------------------------------------------------
model <- '
  # --------------------------------------------------------
  # A) First-order structured preferences (8 products)
  #    Fixed ±1 loadings implement pairwise differencing:
  #    y(i,j) ~ 1*pref(i) + (-1)*pref(j), probit link via thresholds
  # --------------------------------------------------------
  i1  =~  1*i1_i7 + 1*i1_i8 + 1*i1_i10 + 1*i1_i12 + 1*i1_i14 + 1*i1_i15 + 1*i1_i16
  i7  =~ -1*i1_i7 + 1*i7_i8 + 1*i7_i10 + 1*i7_i12 + 1*i7_i14 + 1*i7_i15 + 1*i7_i16
  i8  =~ -1*i1_i8 + -1*i7_i8 + 1*i8_i10 + 1*i8_i12 + 1*i8_i14 + 1*i8_i15 + 1*i8_i16
  i10 =~ -1*i1_i10 + -1*i7_i10 + -1*i8_i10 + 1*i10_i12 + 1*i10_i14 + 1*i10_i15 + 1*i10_i16
  i12 =~ -1*i1_i12 + -1*i7_i12 + -1*i8_i12 + -1*i10_i12 + 1*i12_i14 + 1*i12_i15 + 1*i12_i16
  i14 =~ -1*i1_i14 + -1*i7_i14 + -1*i8_i14 + -1*i10_i14 + -1*i12_i14 + 1*i14_i15 + 1*i14_i16
  i15 =~ -1*i1_i15 + -1*i7_i15 + -1*i8_i15 + -1*i10_i15 + -1*i12_i15 + -1*i14_i15 + 1*i15_i16
  i16 =~ -1*i1_i16 + -1*i7_i16 + -1*i8_i16 + -1*i10_i16 + -1*i12_i16 + -1*i14_i16 + -1*i15_i16

  # Identification choice for first-order layer:
  # In the paired-comparison/Thurstonian setup, we set
  # the variances and means of these preference scores to zero.
  # Stochasticity is captured at the comparison (indicator) level.
  i1 ~~ 0*i1;  i7 ~~ 0*i7;  i8 ~~ 0*i8;  i10 ~~ 0*i10;
  i12~~ 0*i12; i14~~ 0*i14; i15~~ 0*i15; i16~~ 0*i16
  i1 ~ 0*1;  i7 ~ 0*1;  i8 ~ 0*1;  i10 ~ 0*1;
  i12~ 0*1;  i14~ 0*1;  i15~ 0*1;  i16~ 0*1

  # --------------------------------------------------------
  # B) Higher-order conceptual factors (expository grouping)
  #    These summarize structure across product-level prefs;
  #    coefficients fixed at 1 for a transparent mapping.
  # --------------------------------------------------------
  k1  =~ 1*i8  + 1*i14
  k2  =~ 1*i12 + 1*i16
  k3  =~ 1*i1  + 1*i10

  pr  =~ 1*i10 + 1*i14 + 1*i15 + 1*i16
  w   =~ 1*i7  + 1*i10 + 1*i12 + 1*i14
  c   =~ 1*i7  + 1*i8  + 1*i10 + 1*i14

  # Identification for the higher-order layer:
  # Fix one variance to 1; the rest (variances/covariances) remain free.
  w ~~ 1*w

  # --------------------------------------------------------
  # C) Threshold constraints for binary indicators
  #    To replicate the Mplus setting, fix the first threshold
  #    of every binary comparison to 0 (probit link).
  #    If robustness is a concern, allow thresholds to be free.
  # --------------------------------------------------------
  i1_i7  | 0*t1 
  i1_i8  | 0*t1
  i1_i10 | 0*t1
  i1_i12 | 0*t1
  i1_i14 | 0*t1
  i1_i15 | 0*t1
  i1_i16 | 0*t1

  i7_i8  | 0*t1
  i7_i10 | 0*t1
  i7_i12 | 0*t1
  i7_i14 | 0*t1
  i7_i15 | 0*t1
  i7_i16 | 0*t1

  i8_i10 | 0*t1
  i8_i12 | 0*t1
  i8_i14 | 0*t1
  i8_i15 | 0*t1
  i8_i16 | 0*t1

  i10_i12 | 0*t1
  i10_i14 | 0*t1
  i10_i15 | 0*t1
  i10_i16 | 0*t1

  i12_i14 | 0*t1
  i12_i15 | 0*t1
  i12_i16 | 0*t1

  i14_i15 | 0*t1
  i14_i16 | 0*t1

  i15_i16 | 0*t1

  # --------------------------------------------------------
  # D) Indicator residual variances (optional explicit form)
  #    With ordered=WLSMV+theta, lavaan already treats these
  #    as free by default; listing them is not required.
  #    We keep them explicit here for parity with the Mplus
  #    "free residuals" line.
  # --------------------------------------------------------
  i1_i7  ~~ i1_i7
  i1_i8  ~~ i1_i8
  i1_i10 ~~ i1_i10
  i1_i12 ~~ i1_i12
  i1_i14 ~~ i1_i14
  i1_i15 ~~ i1_i15
  i1_i16 ~~ i1_i16

  i7_i8  ~~ i7_i8
  i7_i10 ~~ i7_i10
  i7_i12 ~~ i7_i12
  i7_i14 ~~ i7_i14
  i7_i15 ~~ i7_i15
  i7_i16 ~~ i7_i16

  i8_i10 ~~ i8_i10
  i8_i12 ~~ i8_i12
  i8_i14 ~~ i8_i14
  i8_i15 ~~ i8_i15
  i8_i16 ~~ i8_i16

  i10_i12 ~~ i10_i12
  i10_i14 ~~ i10_i14
  i10_i15 ~~ i10_i15
  i10_i16 ~~ i10_i16

  i12_i14 ~~ i12_i14
  i12_i15 ~~ i12_i15
  i12_i16 ~~ i12_i16

  i14_i15 ~~ i14_i15
  i14_i16 ~~ i14_i16

  i15_i16 ~~ i15_i16

  # --------------------------------------------------------
  # E) higher-order means/variances/covariances explicitly below.
  # --------------------------------------------------------
   k1~1; k2~1; k3~1; pr~1; w~1; c~1; k1~~k1; k2~~k2; k3~~k3; pr~~pr; c~~c; 
   k1~~k2; k1~~k3; k1~~pr; k1~~w; k1~~c; 
   k2~~k3; k2~~pr; k2~~w; k2~~c; 
   k3~~pr; k3~~w; k3~~c; 
   pr~~w; pr~~c; 
   w~~c;
'

# ----------------------------------------------------------
# 4) Estimation settings
#    ordered = ... declares categorical indicators;
#    estimator/parameterization match the Mplus setup.
# ----------------------------------------------------------
fit <- lavaan(
  model = model,
  data = dta,
  ordered = ord_vars,
  estimator = "WLSMV",
  parameterization = "theta",
  meanstructure = TRUE
)

# ----------------------------------------------------------
# 5) Reporting
#    Include global fit, standardized loadings, thresholds,
#    and residuals to compare against Mplus output.
# ----------------------------------------------------------
summary(fit, fit.measures = TRUE, standardized = TRUE)
