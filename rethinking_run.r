library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes_16 <- p$trump_16+p$clinton_16

# get change in social capital index - do not scale (1 unit equals 1 sd)
p$sk2014_over_2005 <- p$sk14-p$sk05

# census data differences
p$pop_change_16_to_10 <- log(p$pop16E) -log(p$pop_2010)
p$median_hh_income_16_to_10 <- log(p$hh_income16E) - log(p$hh_income10E)
p$white_16_to_10 <- (p$white16E/p$pop16E)-(p$white10E/p$pop_2010)
p$hispanic_16_to_10 <- (p$hispanic16E/p$pop16E)-(p$hispanic10E/p$pop_2010)
p$bachelors_16_to_10 <- (p$bachelors16E/p$pop16E)-(p$bachelors10E/p$pop_2010)
p$male_unemplmt_16_to_10 <- (p$male_unemployed16E/p$pop16E)-(p$male_unemployed10E/p$pop_2010)
p$female_unemplmt_16_to_10 <- (p$female_unemployed16E/p$pop16E)-(p$female_unemployed10E/p$pop_2010)
p$for_born_16_to_10 <- (p$foreign_born16E/p$pop16E)-(p$foreign_born10E/p$pop_2010)

# cdc data differences
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e5-(p$alcohol_08/p$pop10E)*10e5
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e5-(p$drug_08/p$pop10E)*10e5
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e5-(p$suicides_08/p$pop10E)*10e5


# fixed rates and percentage 2016
p$sk2014 <- p$sk14
p$pop_16 <- log(p$pop16E)
p$median_hh_income_16<- log(p$hh_income16E)
p$white_16 <- p$white16E/p$pop16E

p$hispanic_16 <- p$hispanic16E/p$pop16E
p$bachelors_16 <- p$bachelors16E/p$pop16E
p$male_unemplmt_16 <- p$male_unemployed16E/p$pop16E
p$female_unemplmt_16 <- p$female_unemployed16E/p$pop16E
p$for_born_16 <- p$foreign_born16E/p$pop16E
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e5
p$drugs16 <- (p$drug_16/p$pop16E)*10e5
p$suicides16 <- (p$suicides_16/p$pop16E)*10e5
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","total_votes_16",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16")
p <- p[complete.cases(p), ]


p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_votes<- as.integer(p$trump_16)
p$total_votes_16 <- as.integer(p$total_votes_16)


#model3<-glm(cbind(trump_primary_votes,(trump_primary_votes+cruz_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model3)    
### rethinking model
data_list <- list(
  trump_votes= p$trump_votes,
  total_votes_16 = p$total_votes_16,
  sk_change = p$sk2014_over_2005,
  pop_change = p$pop_change_16_to_10,
  median_hh_income_change = p$median_hh_income_16_to_10,
  perc_bachelors_change = p$bachelors_16_to_10,
  male_unemplmt_change = p$male_unemplmt_16_to_10,
  female_unemplmt_change = p$female_unemplmt_16_to_10,
  for_born_change= p$for_born_16_to_10,
  alcohol_change = p$alcohol_16_to_8,
  drugs_change = p$drugs_16_to_8,
  suicides_change = p$suicides_16_to_8,
  sk2014 = p$sk2014,
  diversity16 =p$diversity16,
  pop16 = p$pop_16,
  median_hh_income16 = p$median_hh_income_16,
  white16 = p$white_16,
  bachelors16 = p$bachelors_16,
  male_unemplmt16 = p$male_unemplmt_16,
  female_unemplmt16 = p$female_unemplmt_16,
  state_id_seq = p$state_id_seq)

# trump clinton model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_votes ~ dbinom(total_votes_16, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap_c*sk_change +
      bpop_c*pop_change +
      bmedinc_c*median_hh_income_change +
      bpercbach_c*perc_bachelors_change +
      bmaleunemploy_c*male_unemplmt_change +
      bfemaleunemploy_c*female_unemplmt_change +
      bforborn_c*for_born_change +
      balcohol_c*alcohol_change +
      bdrugs_c*drugs_change +
      bsuicides_c*suicides_change +
      bscap*sk2014 +
      bdiversity*diversity16 +
      bpop*pop16 +
      bmedinc*median_hh_income16 +
      bperwhite*white16 +
      bpercbach*bachelors16 +
      bmaleunemploy*male_unemplmt16 +
      bfemaleunemploy*female_unemplmt16,
    a ~ dnorm(0,1),
    #a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap_c ~ dnorm(0,1),
    bpop_c ~ dnorm(0,1),
    bmedinc_c ~ dnorm(0,1),
    bpercbach_c ~ dnorm(0,1),
    bmaleunemploy_c ~ dnorm(0,1),
    bfemaleunemploy_c ~ dnorm(0,1),
    bforborn_c ~ dnorm(0,1),
    balcohol_c ~ dnorm(0,1),
    bdrugs_c ~ dnorm(0,1), 
    bsuicides_c ~ dnorm(0,1),
    bscap ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1),
    bpop ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bperwhite ~ dnorm(0,1),
    bpercbach ~ dnorm(0,1),
    bmaleunemploy ~ dnorm(0,1),
    bfemaleunemploy ~ dnorm(0,1)
    
  ), data=data_list, iter=5000, warmup=1500, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_clinton_no_states_model.rds"

saveRDS(model, paste0(path, filename))