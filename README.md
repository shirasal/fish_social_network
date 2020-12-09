# fish_social_network

Exploring changes in fish co-occurence patterns along environmental gradients using [MRFcov model](https://github.com/nicholasjclark/MRFcov), by [nicholasjclark](https://github.com/nicholasjclark).
Data is described in [medata](https://shirasal.github.io/medata/index.html).

<!--<a href="introduction.html">Introduction</a> (NA yet)
<a href="methods.html">Methods</a> (NA yet)
<a href="results.html">*Results*</a>-->

**WORK IN PROGRESS**

## Review this study via the following workflow

1. [Packages required](#R/packages.R)
2. Objects and data loaded from [here](#data/data_and_objects.RData). Functions loaded via the [functions script](#R/functions.R); More information on data processing in the [Data processing script](#R/data_processing.R).
3. *optional*: Data distribution and exploration in [Data Overview](#Data_Overview.Rmd) or in the [HTML](#Data_Overview.html)
4. Run models and explore relative importance of covariates (RI) with [this script](#R/models.R). Only the nonspatial Poisson model (`grps_pois` and `dip_pois`) is relevant for my thesis results.
5. [Predict and visualise](#Prediction_visualisations.Rmd) species pairs abundances - predictions + raw data

Extra: [Species Maps](#figures/species_maps)

[Results for viewing](#Prediction_visualisations.html) (not final)
