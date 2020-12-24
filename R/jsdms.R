# https://cran.r-project.org/web/packages/jSDM/vignettes/jSDM.html
# https://ecology.ghislainv.fr/jSDM/articles/jSDM.html

library(jSDM)

data(frogs, package="jSDM")
head(frogs)

PA_frogs <- frogs[,4:12]
Env_frogs <- cbind(scale(frogs[,1]),frogs[,2],scale(frogs[,3]))
colnames(Env_frogs) <- colnames(frogs[,1:3])

mod_jSDM_block_frogs <- jSDM::jSDM_probit_block(
  # Response variable 
  presence_site_sp = as.matrix(PA_frogs), 
  # Explanatory variables 
  site_suitability = ~.,   
  site_data = as.data.frame(Env_frogs), n_latent=2,
  # Chains
  burnin=20000, mcmc=5000, thin=5,
  # Starting values
  alpha_start=0, beta_start=0,
  lambda_start=0, W_start=0,
  V_alpha_start=1, 
  # Priors
  shape=0.5, rate=0.0005,
  mu_beta=0, V_beta=1.0E6,
  mu_lambda=0, V_lambda=10,
  # Various 
  seed=1234, verbose=1)

plot(coda::as.mcmc(mod_jSDM_block_frogs$mcmc.alpha[,1:2]))

plot_residual_cor(mod_jSDM_block_frogs)



grps_obs <- grps_mat %>% select(c(groupers)) %>% as.matrix()
grps_obs[grps_obs > 0] <- 1 
grps_vars <- grps_mat %>% select(!(c(groupers))) %>% mutate(mpa = as.numeric(mpa))
head(grps_obs)
head(grps_vars)

mod_jSDM_grps <- jSDM_probit_block(presence_site_sp = grps_obs, 
                                          site_data = grps_vars, site_suitability = ~.)

plot_residual_cor(mod_jSDM_grps)


dip_obs <- dip_mat %>% select(all_of(diplodus)) %>% as.matrix()
dip_obs[dip_obs > 0] <- 1 
dip_vars <- dip_mat %>% select(!(c(diplodus))) %>% mutate(mpa = as.numeric(mpa))
head(dip_obs)
head(dip_vars)

mod_jSDM_dip <- jSDM_probit_block(presence_site_sp = dip_obs, 
                                   site_data = dip_vars, site_suitability = ~.)

plot_residual_cor(mod_jSDM_dip)


# -------------------------------------------------------------------------

# https://www.helsinki.fi/en/researchgroups/statistical-ecology/hmsc
# https://github.com/hmsc-r/HMSC

library(Hmsc)

# vignette(package = "Hmsc")
# vignette("vignette_3_multivariate_high")

