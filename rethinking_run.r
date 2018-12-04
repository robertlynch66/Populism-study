### Trump vs mccain  model 7 longitudnal
##################################################################
# Model 7 trump vs vs mccain long
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds"
p <- readRDS(paste0(path, file))

# scale social capital index
p$soc_cap_indx_14 <- p$soc_cap_indx_14 -min(p$soc_cap_indx_14,na.rm=T)
p$soc_cap_indx_14 <- p$soc_cap_indx_14/(max(p$soc_cap_indx_14,na.rm=T))
p$sk05 <- p$sk05 - min(p$sk05, na.rm=T)
p$sk05 <- p$sk05/max(p$sk05, na.rm=T)
p$sk2014_over_2005 <- p$soc_cap_indx_14-p$sk05

# log population and median hh income differences
p$pop_change_16_to_8 <- log(p$pop_2014) -log(p$pop_2010)
p$median_hh_income_08 <- p$median_hh_income_16_to_8 + p$median_hh_income
p$median_hh_income_16_to_8 <- log(p$median_hh_income) -log(p$median_hh_income_08)



# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08
p$total_votes_16_primary <- p$sanders_primary + p$clinton_primary
p$total_votes_16_08_primary <- p$sanders_primary+p$obama_primary_08


p <- p %>% select(state_id,trump_16, sk2014_over_2005, suicides_16_over_08, drug_16_over_08,
                  alcohol_16_over_08,
                  pop_density_2014,perc_bachelors, diversity_idx_2016,pop_change_16_to_8,
                  percent_white_16_to_8,percent_hispanic_16_to_8,pop_2014,median_hh_income_16_to_8,
                  pop_2014,total_votes_16_08_gen)

p<- p[complete.cases(p),]
#2,420 cases - we lose another 1500 if we use change in hh income and chnage in population
# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$total_votes_16_08_gen<- as.integer(p$total_votes_16_08_gen)



data_list <- list(
  trump_16= p$trump_16,
  sk_change = p$sk2014_over_2005,
  suicides_change = p$suicides_16_over_08,
  drugs_change = p$drug_16_over_08,
  alcohol_change = p$alcohol_16_over_08,
  pop_density = log(p$pop_density_2014),
  perc_bachelors = p$perc_bachelors,
  diversity_idx =p$diversity_idx_2016,
  perc_hisp_change = p$percent_hispanic_16_to_8,
  perc_white_change = p$percent_white_16_to_8,
  pop_change = p$pop_change_16_to_8,
  total_votes_16_08_gen = p$total_votes_16_08_gen,
  median_hh_income_change = p$median_hh_income_16_to_8,
  state_id_seq = p$state_id_seq)

model <- map2stan(
  alist(
    trump_16 ~ dbinom(total_votes_16_08_gen, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap_c*sk_change +
      balcho_c*alcohol_change +
      bdrug_c*drugs_change +
      bsuic_c*suicides_change +
      bdens*pop_density +
      bpop_c*pop_change +
      bpercbach*perc_bachelors +
      bmedinc_c*median_hh_income_change +
      bwhite_c*perc_white_change +
      bhisp_c*perc_hisp_change +
      bdiversity*diversity_idx,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap_c ~ dnorm(0,1),
    bdrug_c ~ dnorm(0,1),
    balcho_c ~ dnorm(0,1),
    bsuic_c ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bpop_c ~ dnorm(0,1),
    bpercbach ~ dnorm(0,1),
    bmedinc_c ~ dnorm(0,1),
    bwhite_c ~ dnorm(0,1),
    bhisp_c ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bdrug_c=0,balcho_c=0,bsuic_c=0,bdens=0,bpercbach=0,bpop_c=0,
             bmedinc_c=0,bwhite_c=0,bhisp_c=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_mccain_long_model.rds"

saveRDS(model, paste0(path, filename))