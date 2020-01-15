# load packages:
source("scripts/pckgs_preps.R")

# load data and regularise the temperature covariate:
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  mutate(tmean_reg = scale(tmean)) # regularise temperature covariate
str(med_raw)

# Subset the data to western and eastern basins:
west <- med_raw %>% filter(country %in% c("France", "Italy", "Spain"))
east <- med_raw %>% filter(country %in% c("Croatia", "Greece", "Israel", "Malta", "Turkey"))

##################################################################
########################### RUN MODELS ###########################
##################################################################



# Groupers
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

Serranidae_w <- west %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg, enforcement)
str(Serranidae_w)

Serranidae_e <- east %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg, enforcement)
str(Serranidae_e)

# Temperature

# Enforcement

# Depth

# Combined covariates