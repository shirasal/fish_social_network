library(MRFcov)

birds_binomial <- Bird.parasites[, 1:4]
MRFcov(birds_binomial, family = "binomial")

### next stage will be to understand the output and it's components and then create a matrix from my data that looks like the sample data and run