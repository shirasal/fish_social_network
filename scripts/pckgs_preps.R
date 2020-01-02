library(parallel)
library(igraph)
library(tidyverse)
library(MRFcov)

parallel::detectCores() # in case I'd like to speed up MRFcov by spreading processing over >1 core