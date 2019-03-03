
library(MRFcov)
# Citation: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221
library("testthat")
library("plotly")
library("tidyverse")

example_data <- Bird.parasites
MRFcov(data = example_data, family = "binomial")

