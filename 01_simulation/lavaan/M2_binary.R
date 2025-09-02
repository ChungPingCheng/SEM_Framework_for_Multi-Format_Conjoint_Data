library(lavaan)

dta <- read.table('binary_data.txt',header=F)
names(dta) <- c('B1','B2','B3','B4')

model_rating <- '
  f0 =~  1*B1 +  1*B2 +  1*B3 +  1*B4
  f1 =~ -1*B1 +  1*B2 + -1*B3 +  1*B4
  f2 =~ -1*B1 + -1*B2 +  1*B3 +  1*B4

  B1 | c*t1
  B2 | c*t1
  B3 | c*t1
  B4 | c*t1

  f0 ~ 0*1
  f1 ~ 1
  f2 ~ 1
  f1 ~~ 1*f1
'

fit <- cfa(model_rating, data = dta, meanstructure = TRUE,
                  ordered = c("B1","B2","B3","B4"), parameterization="theta", estimator = "WLSMV")

summary(fit, fit.measures = TRUE)
