library(MRFcov)

birds_binomial <- Bird.parasites[, 1:4]
MRFcov(birds_binomial, family = "binomial")
