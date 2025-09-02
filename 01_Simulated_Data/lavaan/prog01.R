library(lavaan)

dta <- read.table('rating_data.txt',header=F)
names(dta) <- c('RT1','RT2','RT3','RT4')

model_rating <- '
  f0 =~  1*RT1 +  1*RT2 +  1*RT3 +  1*RT4
  f1 =~ -1*RT1 +  1*RT2 + -1*RT3 +  1*RT4
  f2 =~ -1*RT1 + -1*RT2 +  1*RT3 +  1*RT4

  RT1 ~ c*1
  RT2 ~ c*1
  RT3 ~ c*1
  RT4 ~ c*1

  f0 ~ 0*1
  f1 ~ 1
  f2 ~ 1
'

fit <- cfa(model_rating, data = dta, meanstructure = TRUE)
summary(fit, fit.measures = TRUE)
