library(pacman)
p_load(conjoint, lavaan)
data(chocolate)

# Use respondent-by-item ranking matrix as input
dta <- cprefm

# Generate all pairwise combinations of the 16 items
colnames(dta) <- paste0("i", 1:16)
items <- colnames(dta)
pairwise_comparisons <- combn(items, 2, simplify = FALSE)

# Create matrix to store pairwise preference responses
dtanew <- matrix(NA, nrow = nrow(dta), ncol = length(pairwise_comparisons))
colnames(dtanew) <- sapply(pairwise_comparisons, function(x) paste(x, collapse = "_"))

# Compute pairwise responses: 1 if item1 is preferred (i.e., ranked lower), else 0
for (i in seq_along(pairwise_comparisons)) {
  pair <- pairwise_comparisons[[i]]
  item1 <- pair[1]
  item2 <- pair[2]
  dtanew[, i] <- ifelse(dta[[item1]] < dta[[item2]], 1, 0)
}

# Convert to data frame for lavaan
dtanew <- as.data.frame(dtanew)

# --- SEMWISE MODEL 3 ---
model3 <- "
  # First-order latent factors (items: i1, i7, i8, i10, i12, i14, i15, i16)
  i1  =~  1*i1_i7  + 1*i1_i8  + 1*i1_i10 + 1*i1_i12 + 1*i1_i14 + 1*i1_i15 + 1*i1_i16
  i7  =~ -1*i1_i7  + 1*i7_i8  + 1*i7_i10 + 1*i7_i12 + 1*i7_i14 + 1*i7_i15 + 1*i7_i16
  i8  =~ -1*i1_i8  + -1*i7_i8 + 1*i8_i10 + 1*i8_i12 + 1*i8_i14 + 1*i8_i15 + 1*i8_i16
  i10 =~ -1*i1_i10 + -1*i7_i10 + -1*i8_i10 + 1*i10_i12 + 1*i10_i14 + 1*i10_i15 + 1*i10_i16
  i12 =~ -1*i1_i12 + -1*i7_i12 + -1*i8_i12 + -1*i10_i12 + 1*i12_i14 + 1*i12_i15 + 1*i12_i16
  i14 =~ -1*i1_i14 + -1*i7_i14 + -1*i8_i14 + -1*i10_i14 + -1*i12_i14 + 1*i14_i15 + 1*i14_i16
  i15 =~ -1*i1_i15 + -1*i7_i15 + -1*i8_i15 + -1*i10_i15 + -1*i12_i15 + -1*i14_i15 + 1*i15_i16
  i16 =~ -1*i1_i16 + -1*i7_i16 + -1*i8_i16 + -1*i10_i16 + -1*i12_i16 + -1*i14_i16 + -1*i15_i16

  # Higher-order or domain-level factors (examples)
  fk1  =~ 1*i8  + 1*i14
  fk2  =~ 1*i12 + 1*i16
  fk3  =~ 1*i1  + 1*i10

  fpr1 =~ 1*i10 + 1*i14 + 1*i15 + 1*i16
  fpr2 =~ 1*i8  + 1*i12

  fw1  =~ 1*i7  + 1*i10 + 1*i12 + 1*i14
  fw2  =~ 1*i1  + 1*i16

  fc   =~ 1*i1  + 1*i8 + 1*i10 + 1*i14

  # Thresholds (fixing probit thresholds at 0)
  "

model3 <- paste0(model3, paste0(colnames(dtanew), " | 0*t1\n", collapse = ""))

model3 <- paste0(model3, "
  # Intercepts for first-order latent factors
  i1  ~ a*1
  i7  ~ a*1
  i8  ~ a*1
  i10 ~ a*1
  i12 ~ a*1
  i14 ~ a*1
  i15 ~ a*1
  i16 ~ a*1

  # Intercepts for higher-order factors
  fk1  ~ 1
  fk2  ~ 1
  fk3  ~ 1
  fpr1 ~ 1
  fpr2 ~ 1
  fw1  ~ 1
  fw2  ~ 1
  fc   ~ 1

  # Example variance constraint
  fk1 ~~ 1*fk1

  # Residuals for binary indicators (fixed to 0 here)
  ")

model3 <- paste0(model3, paste0(colnames(dtanew), " ~~ 0*", colnames(dtanew), "\n", collapse = ""))

fit3 <- lavaan::sem(model3, data = dtanew, ordered = colnames(dtanew), parameterization = "theta")
summary(fit3, fit.measures = TRUE, standardized = TRUE)

# MODEL 31 has same factor structure with explicit variances added
model31 <- model3
model31 <- paste0(model31, "
  fk2 ~~ fk2
  fk3 ~~ fk3
  fpr1 ~~ fpr1
  fpr2 ~~ fpr2
  fw1 ~~ fw1
  fw2 ~~ fw2
  fc  ~~ fc
")

fit31 <- lavaan::sem(model31, data = dtanew, ordered = colnames(dtanew), parameterization = "theta")
summary(fit31, fit.measures = TRUE, standardized = TRUE)
