library(lavaan)

dta <- read.table('pairwise_data.txt',header=F)
names(dta) <- c('P1_2','P1_3','P1_4','P2_3','P2_4','P3_4')

model_pair <- '
  Y1 =~  1*P1_2 +  1*P1_3 +  1*P1_4
  Y2 =~ -1*P1_2 +  1*P2_3 +  1*P2_4
  Y3 =~ -1*P1_3 + -1*P2_3 +  1*P3_4
  Y4 =~ -1*P1_4 + -1*P2_4 + -1*P3_4

  Y1 ~ 0*1
  Y2 ~ 0*1
  Y3 ~ 0*1
  Y4 ~ 0*1

  Y1 ~~ 0*Y1
  Y2 ~~ 0*Y2
  Y3 ~~ 0*Y3
  Y4 ~~ 0*Y4

  f1 =~ -1*Y1 +  1*Y2 + -1*Y3 +  1*Y4
  f2 =~ -1*Y1 + -1*Y2 +  1*Y3 +  1*Y4

  f1~1
  f2~1

  f1 ~~ 1*f1
  f2 ~~ f2
  f1 ~~ f2

  P1_2 ~~ P1_2
  P1_3 ~~ P1_3
  P1_4 ~~ P1_4
  P2_3 ~~ P2_3
  P2_4 ~~ P2_4
  P3_4 ~~ P3_4

  P1_2 | 0*t1
  P1_3 | 0*t1
  P1_4 | 0*t1
  P2_3 | 0*t1
  P2_4 | 0*t1
  P3_4 | 0*t1
'

fit <- cfa(
  model_pair,
  data = dta,
  ordered = c("P1_2","P1_3","P1_4","P2_3","P2_4","P3_4"),
  estimator = "WLSMV", parameterization="theta",
  meanstructure = TRUE
)

summary(fit, fit.measures = TRUE)
