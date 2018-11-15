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