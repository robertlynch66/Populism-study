#### Final  models for OSR project
data <- readRDS("full_pop_data.rds")

# scale social capital index
data$soc_cap_indx_14 <- data$soc_cap_indx_14 -min(data$soc_cap_indx_14,na.rm=T)
data$soc_cap_indx_14 <- data$soc_cap_indx_14/(max(data$soc_cap_indx_14,na.rm=T))
data$sk05 <- data$sk05 - min(data$sk05, na.rm=T)
data$sk05 <- data$sk05/max(data$sk05, na.rm=T)
data$sk2014_over_2005 <- data$soc_cap_indx_14-data$sk05

# log population and median hh income differences
data$pop_change_16_to_8 <- log(data$pop_2014) -log(data$pop_2010)
data$median_hh_income_08 <- data$median_hh_income_16_to_8 + data$median_hh_income
data$median_hh_income_16_to_8 <- log(data$median_hh_income) -log(data$median_hh_income_08)


# add weights
data$total_votes_16_gen <- data$trump_16+data$clinton_16
data$total_votes_16_08_gen <- data$trump_16+data$mccain_08
# for the primaries scale by population not votes
#data$pop_2014

library(dplyr)
library(emmeans)
library(lme4)
library(betareg)


# Model 1 Trump vs Clinton
#check correls amongst predictors
mod1 <- data %>% select(trump_perc_16, pop_density_2014,drug_16_scaled,alcohol_16_scaled,
                        suicides_16_scaled,perc_bachelors,
                        diversity_idx_2016,soc_cap_indx_14,perc_white,              
                        perc_hisp,pop_density_2014,median_hh_income,
                        total_votes_16_gen,total_votes_16_08_gen,pop_2014)      

summary(mod1)
cor(mod1,use = "complete.obs")


### beta regression with weights using betareg - 4 models each

# model 1 trump percent 16- fixed factors in 2016

model_trump_16 = betareg(trump_perc_16 ~ soc_cap_indx_14 +
                           suicides_16_scaled  +drug_16_scaled+
                           log(pop_density_2014)+log(median_hh_income)+perc_white+perc_hisp+diversity_idx_2016,
                         weights=total_votes_16_gen,
                         data = mod1)
options(digits = 6)
options(scipen=999)
summary(model_trump_16)

# model 2 sanders percent 16 - fixed factors in 2016
mod2 <- data %>% select(sanders_perc_16, pop_density_2014,drug_16_scaled,alcohol_16_scaled,
                        suicides_16_scaled,perc_bachelors,pop_2014,
                        diversity_idx_2016,soc_cap_indx_14,perc_white,              
                        perc_hisp,pop_density_2014,median_hh_income)      

summary(mod2)
cor(mod2,use = "complete.obs")

model_sanders_16 = betareg(sanders_perc_16 ~ soc_cap_indx_14 +
                             suicides_16_scaled  +drug_16_scaled+
                             log(pop_density_2014)+log(median_hh_income)+perc_white+perc_hisp+diversity_idx_2016,
                           weights=pop_2014,
                           data = mod2)

summary(model_sanders_16)

#model 3 mccain vs trump longitudnal
mod3 <- data %>% select(trump_perc_16_vs_08,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                        suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                        sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                        percent_hispanic_16_to_8,median_hh_income_16_to_8,total_votes_16_08_gen)      

summary(mod3)
cor(mod3,use = "complete.obs")

model_trump_vs_mccain = betareg(trump_perc_16_vs_08 ~ drug_16_over_08+alcohol_16_over_08+
                                  suicides_16_over_08+
                                  sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                  percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                  log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                weights=total_votes_16_08_gen,
                                data = mod3)

summary(model_trump_vs_mccain)

# model 4 sanders vs obama longitudnal

mod4 <- data %>% select(sanders_perc_16_vs_08,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                        suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                        sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                        percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod4)
cor(mod4,use = "complete.obs")

model_sanders_vs_obama = betareg(sanders_perc_16_vs_08 ~ drug_16_over_08+alcohol_16_over_08+
                                   suicides_16_over_08+
                                   sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                   percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                   log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                 weights=pop_2014,
                                 data = mod4)

summary(model_sanders_vs_obama)


# model 5 longitudnal changes but sanders vs hillary
mod5 <- data %>% select(sanders_perc_16,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                        suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                        sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                        percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod5)
cor(mod5,use = "complete.obs")

model_sanders_vs_clinton_long = betareg(sanders_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                                          suicides_16_over_08+
                                          sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                          percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                          log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                        weights=pop_2014,
                                        data = mod5)

summary(model_sanders_vs_clinton_long)

# model 6 longitudinal changes but trump vs clinton
mod6 <- data %>% select(trump_perc_16,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                        suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                        sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                        percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod6)
trump_clinton <- trump_clinton[complete.cases(trump_clinton),]

cor(mod6,use = "complete.obs")

model_trump_vs_clinton_long = betareg(trump_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                                        suicides_16_over_08+
                                        sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                        percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                        log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                      weights=pop_2014,
                                      data = mod6)

summary(model_trump_vs_clinton_long)




# aggregated binomials in rethinking
#####################################################################################
## Rethinking Bayes code for model 1 #################################
##################################################################
# Model 1 percent trump 16
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds"
p <- readRDS(paste0(path, file))

# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08

p <- p %>% select (state_id,trump_16, soc_cap_indx_14, suicides_16_scaled, drug_16_scaled,
                      pop_density_2014,median_hh_income,perc_white,perc_hisp,diversity_idx_2016,
                      total_votes_16_gen)

p<- p[complete.cases(p),]

# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$total_votes_16_gen <- as.integer(p$total_votes_16_gen)


data_list <- list(
  trump_16 = p$trump_16,
  soc_cap_indx_14 = p$soc_cap_indx_14,
  suicides_16_scaled = p$suicides_16_scaled,
  drug_16_scaled = p$drug_16_scaled,
  pop_density_2014 = p$pop_density_2014,
  median_hh_income = p$median_hh_income,
  perc_white = p$perc_white,
  perc_hisp = p$perc_hisp,
  diversity_idx_2016 =p$diversity_idx_2016,
  total_votes_16_gen = p$total_votes_16_gen,
  state_id_seq = p$state_id_seq)





model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_16 ~ dbinom(total_votes_16_gen, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap*soc_cap_indx_14 +
      bdrug*drug_16_scaled +
      bsuic*suicides_16_scaled +
      bdens*pop_density_2014 +
      bmedinc*median_hh_income +
      bwhite*perc_white +
      bhisp*perc_hisp +
      bdiversity*diversity_idx_2016,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap ~ dnorm(0,1),
    bdrug ~ dnorm(0,1),
    bsuic ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bwhite ~ dnorm(0,1),
    bhisp ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap=0,bdrug=0,bsuic=0,bdens=0,bmedinc=0,bwhite=0,bhisp=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "Trump_16_model.rds"

saveRDS(model, paste0(path, filename))



#####################################################################################
## Rethinking Bayes code for model 2 #################################
##################################################################
# Model 2  sanders 16
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds"
p <- readRDS(paste0(path, file))

# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08
p$total_votes_16_primary <- p$sanders_primary + p$clinton_primary

p <- p %>% select (state_id,sanders_primary, soc_cap_indx_14, suicides_16_scaled, drug_16_scaled,
                   pop_density_2014,median_hh_income,perc_white,perc_hisp,diversity_idx_2016,
                   total_votes_16_primary)

p<- p[complete.cases(p),]

# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_primary<- as.integer(p$sanders_primary)
p$total_votes_16_primary <- as.integer(p$total_votes_16_primary)


data_list <- list(
  sanders_primary = p$sanders_primary,
  soc_cap_indx_14 = p$soc_cap_indx_14,
  suicides_16_scaled = p$suicides_16_scaled,
  drug_16_scaled = p$drug_16_scaled,
  pop_density_2014 = p$pop_density_2014,
  median_hh_income = p$median_hh_income,
  perc_white = p$perc_white,
  perc_hisp = p$perc_hisp,
  diversity_idx_2016 =p$diversity_idx_2016,
  total_votes_16_primary = p$total_votes_16_primary,
  state_id_seq = p$state_id_seq)





model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    sanders_primary ~ dbinom(total_votes_16_primary, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap*soc_cap_indx_14 +
      bdrug*drug_16_scaled +
      bsuic*suicides_16_scaled +
      bdens*pop_density_2014 +
      bmedinc*median_hh_income +
      bwhite*perc_white +
      bhisp*perc_hisp +
      bdiversity*diversity_idx_2016,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap ~ dnorm(0,1),
    bdrug ~ dnorm(0,1),
    bsuic ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bwhite ~ dnorm(0,1),
    bhisp ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap=0,bdrug=0,bsuic=0,bdens=0,bmedinc=0,bwhite=0,bhisp=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "Sanders_16_model.rds"

saveRDS(model, paste0(path, filename))


## Rethinking Bayes code for model 3 #################################
##################################################################
# Model 3 Trump vs mccain
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds"
p <- readRDS(paste0(path, file))

# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08
p$total_votes_16_primary <- p$sanders_primary + p$clinton_primary

p <- p %>% select (state_id,trump_16, sk2014_over_2005, suicides_16_over_08, drug_16_over_08,alcohol_16_over_08,
                   pop_density_2014,perc_bachelors, diversity_idx_2016,
                   percent_white_16_to_8,percent_hispanic_16_to_8,pop_2014,median_hh_income,
                   total_votes_16_08_gen)

p<- p[complete.cases(p),]
#2,420 cases - we lose another 1500 if we use change in hh income and chnage in population
# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$total_votes_16_08_gen <- as.integer(p$total_votes_16_08_gen)
p$pop_2014 <- log(p$pop_2014)
p$median_hh_income <- log(p$median_hh_income)

data_list <- list(
  trump_16 = p$trump_16,
  sk_change = p$sk2014_over_2005,
  suicides_change = p$suicides_16_over_08,
  drugs_change = p$drug_16_over_08,
  alcohol_change = p$alcohol_16_over_08,
  pop_density = p$pop_density_2014,
  perc_bachelors = p$perc_bachelors,
  diversity_idx =p$diversity_idx_2016,
  perc_hisp_change = p$percent_hispanic_16_to_8,
  perc_white_change = p$percent_white_16_to_8,
  pop = p$pop_2014,
  median_hh_income = p$median_hh_income,
  total_votes_16_08_gen = p$total_votes_16_08_gen,
  state_id_seq = p$state_id_seq)





model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_16 ~ dbinom(total_votes_16_08_gen, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap_c*sk_change +
      bdrug_c*drugs_change +
      bsuic_c*suicides_change +
      bdens*pop_density +
      bmedinc*median_hh_income +
      bwhite_c*perc_white_change +
      bhisp_c*perc_hisp_change +
      bdiversity*diversity_idx,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap_c ~ dnorm(0,1),
    bdrug_c ~ dnorm(0,1),
    bsuic_c ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bwhite_c ~ dnorm(0,1),
    bhisp_c ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bdrug_c=0,bsuic_c=0,bdens=0,bmedinc=0,bwhite_c=0,bhisp_c=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "Trmump_vs_mccain_model.rds"

saveRDS(model, paste0(path, filename))


## Rethinking Bayes code for model 4 #################################
##################################################################
# Model 4 sanders vs obama
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds"
p <- readRDS(paste0(path, file))

# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08
p$total_votes_16_primary <- p$sanders_primary + p$clinton_primary
p$total_votes_16_08_primary <- p$sanders_primary+p$obama_primary_08

p <- p %>% select (state_id,sanders_primary, sk2014_over_2005, suicides_16_over_08, drug_16_over_08,alcohol_16_over_08,
                   pop_density_2014,perc_bachelors, diversity_idx_2016,
                   percent_white_16_to_8,percent_hispanic_16_to_8,pop_2014,median_hh_income,
                   total_votes_16_08_primary)

p<- p[complete.cases(p),]
#2,420 cases - we lose another 1500 if we use change in hh income and chnage in population
# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_primary<- as.integer(p$sanders_primary)
p$pop_2014 <- log(p$pop_2014)
p$median_hh_income <- log(p$median_hh_income)
p$total_votes_16_08_primary <- as.integer(p$total_votes_16_08_primary)


data_list <- list(
  sanders_primary = p$sanders_primary,
  sk_change = p$sk2014_over_2005,
  suicides_change = p$suicides_16_over_08,
  drugs_change = p$drug_16_over_08,
  alcohol_change = p$alcohol_16_over_08,
  pop_density = p$pop_density_2014,
  perc_bachelors = p$perc_bachelors,
  diversity_idx =p$diversity_idx_2016,
  perc_hisp_change = p$percent_hispanic_16_to_8,
  perc_white_change = p$percent_white_16_to_8,
  pop = p$pop_2014,
  median_hh_income = p$median_hh_income,
  total_votes_16_08_primary = p$total_votes_16_08_primary,
  state_id_seq = p$state_id_seq)





model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    sanders_primary ~ dbinom(total_votes_16_08_primary, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap_c*sk_change +
      bdrug_c*drugs_change +
      bsuic_c*suicides_change +
      bdens*pop_density +
      bmedinc*median_hh_income +
      bwhite_c*perc_white_change +
      bhisp_c*perc_hisp_change +
      bdiversity*diversity_idx,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap_c ~ dnorm(0,1),
    bdrug_c ~ dnorm(0,1),
    bsuic_c ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bwhite_c ~ dnorm(0,1),
    bhisp_c ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bdrug_c=0,bsuic_c=0,bdens=0,bmedinc=0,bwhite_c=0,bhisp_c=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "sanders_vs_obama_model.rds"

saveRDS(model, paste0(path, filename))