
#############################################
#############################################
#############################################
#############################################
#############################################
#########SKIP FOR CLUSTER - RETHINKING SCRIPT
##############################################
### Make a correlation matrix - choose variables from p
library("Hmisc")
pc <- p %>% select(4,55:80)
res2 <- rcorr(as.matrix(pc))
res2
#flatten matrix function
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(Hmisc)
# get correlation matrix for predictors
f <-flattenCorrMatrix(res2$r, res2$P)

# variance infaltion factor of the model
# first get ML model
pc <- pc[complete.cases(pc), ]
options(scipen=999)

### read in data
p<- readRDS("../data files/populism_data_new.rds")

#####Trump vs Clinton ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
#path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes <- p$trump_16+p$clinton_16



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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","clinton_16","total_votes",
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
p$trump_16<- as.integer(p$trump_16)
p$kasich_votes <- as.integer(p$kasich_votes)
library(lme4)
# Final model with all key variables with VIF's below 5
model<-glm(cbind(trump_16,clinton_16) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
             bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
             pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)    
# trump HRC
model2<-glmer(cbind(trump_16,clinton_16) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
             male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8 + bachelors_16_to_10 + white_16_to_10 +(1|state),family=binomial, data=p)    
summary(model2)
# Sanders HRC
model3<-glmer(cbind(sanders_primary1,clinton_primary1) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
                male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
                drugs_16_to_8+suicides_16_to_8 + bachelors_16_to_10 + white_16_to_10 +(1|state),family=binomial, data=p)    
summary(model3)
# Trump vs Cruz
model4<-glmer(cbind(trump_primary_votes,cruz_votes) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
                male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
                drugs_16_to_8+suicides_16_to_8 + bachelors_16_to_10 + white_16_to_10 +(1|state),family=binomial, data=p)    
summary(model4)
# trump vs kasich
model5<-glmer(cbind(trump_primary_votes,kasich_votes) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
                male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
                drugs_16_to_8+suicides_16_to_8 + bachelors_16_to_10 + white_16_to_10 +(1|state),family=binomial, data=p)    
summary(model5)
# check variance inflation factors
library(car)
vif(model)      

# variance inflation factor higher than  5 you may not want to use the predictor - a 5.23 inducates that the SE of the precitor is
# about 2.3 X as high as it would be if it wasn't correlated with any of the other predictors.
                         

########################### 
########################### 
########################### 
##########PASTE TO RETHINKING RUN############
##############################################            
# for convenience - comment out
p<- readRDS("../data files/populism_data_new.rds")

#####Trump vs Clinton ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
#path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes <- p$trump_16+p$clinton_16



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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","clinton_16","total_votes",
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
p$trump_16<- as.integer(p$trump_16)
p$kasich_votes <- as.integer(p$kasich_votes)


#model4<-glm(cbind(trump_16,total_votes) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model4)    
### rethinking model
data_list <- list(
  trump_votes= p$trump_16,
  total_votes = p$total_votes,
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
    trump_votes ~ dbinom(total_votes, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_clinton_full_model.rds"

saveRDS(model, paste0(path, filename))


# GLM model trump vs HRC
model<-glm(cbind(trump_16,clinton_16+trump_16) ~ sk2014_over_2005 + pop_change_16_to_10+median_hh_income_16_to_10+
             bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
             pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)
summary(model)
####sanders clinton model#############
######################################
########################################
###########################################
# for convenience - comment out
#p<- readRDS("../data files/populism_data_new.rds")

#####Sanders vs Clinton ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes_16 <- p$sanders_primary1+p$clinton_primary1

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","sanders_primary1","clinton_primary1","total_votes_16",
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
p$sanders_primary1<- as.integer(p$sanders_primary1)
p$total_votes_16 <- as.integer(p$total_votes_16)




#model2<-glm(cbind(sanders_primary1,(sanders_primary1+clinton_primary1)) ~ sk2014_over_2005 + pop_change_16_to_10+
            #  median_hh_income_16_to_10+
            # bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
            #  drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
            # pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)




# check variance inflation factors
#library(car)
#vif(model2)    
### rethinking model
data_list <- list(
  sanders= p$sanders_primary1,
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

# sanders clinton model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    sanders ~ dbinom(total_votes_16, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "sanders_vs_clinton_full_model.rds"

saveRDS(model, paste0(path, filename))



################################################################
#################################################################
###############################################################
##################################################
####trump vs cruz model#############
######################################
########################################
###########################################
# for convenience - comment out
#p<- readRDS("../data files/populism_data_new.rds")

#####Trump vs Cruz ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes_16 <- p$trump_primary_votes+p$cruz_votes

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_primary_votes","cruz_votes","total_votes_16",
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
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
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
  trump_primary_votes= p$trump_primary_votes,
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

# trump cruz model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_primary_votes ~ dbinom(total_votes_16, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_cruz_full_model.rds"

saveRDS(model, paste0(path, filename))

####trump vs mccain model#############
######################################
########################################
###########################################
# for convenience - comment out
#p<- readRDS("../data files/populism_data_new.rds")

#####Trump vs mccain ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes <- p$trump_16+p$mccain_08

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","mccain_08","total_votes",
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
p$trump_16<- as.integer(p$trump_16)
p$total_votes <- as.integer(p$total_votes)


#model4<-glm(cbind(trump_16,total_votes) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model4)    
### rethinking model
data_list <- list(
  trump_votes= p$trump_16,
  total_votes = p$total_votes,
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

# trump mccain model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_votes ~ dbinom(total_votes, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_mccain_full_model.rds"

saveRDS(model, paste0(path, filename))




######################################
########################################
###########################################
# for convenience - comment out
#p<- readRDS("../data files/populism_data_new.rds")

#####Sanders vs Obama ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in primaries
p$total_votes <- p$sanders_primary1+p$obama_08

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","sanders_primary1","total_votes",
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
p$sanders_primary1<- as.integer(p$sanders_primary1)
p$total_votes <- as.integer(p$total_votes)


model5<-glm(cbind(sanders_primary1,total_votes) ~ sk2014_over_2005 + pop_change_16_to_10+
median_hh_income_16_to_10+
bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
 drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model5)    
### rethinking model
data_list <- list(
  sanders_primary1= p$sanders_primary1,
  total_votes = p$total_votes,
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

# sanders model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    sanders_primary1 ~ dbinom(total_votes, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "sanders_vs_obama_full_model.rds"

saveRDS(model, paste0(path, filename))


################################################################
#################################################################
###############################################################
##################################################
####trump vs kasich model#############
######################################
########################################
###########################################
# for convenience - comment out
#p<- readRDS("../data files/populism_data_new.rds")

#####Trump vs Kasich ####################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

p$kasich_votes <- as.numeric(p$kasich_votes)
# get DV (n) total votes in general
p$total_votes_16 <- p$trump_primary_votes+p$kasich_votes

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_primary_votes","kasich_votes","total_votes_16",
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
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$total_votes_16 <- as.integer(p$total_votes_16)


#model6<-glm(cbind(trump_primary_votes,(trump_primary_votes+kasich_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
#drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model6)    
### rethinking model
data_list <- list(
  trump_primary_votes= p$trump_primary_votes,
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

# trump kasich model
model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_primary_votes ~ dbinom(total_votes_16, p),
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
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
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
    
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap_c=0,bpop_c=0,bmedinc_c=0,bpercbach_c=0,bmaleunemploy_c=0,bfemaleunemploy_c=0,
             bforborn_c=0,balcohol_c=0,bdrugs_c=0,bsuicides_c=0,bscap=0,
             bdiversity=0,bpop=0,bmedinc=0,bperwhite=0,bpercbach=0,bmaleunemploy=0,bfemaleunemploy=0), chains=4, cores=4)


path<- (paste0("results/"))
filename <- "trump_vs_kasich_full_model.rds"

saveRDS(model, paste0(path, filename))

# rstan arm aggregated binomial
library(dplyr)
library(rstanrm)
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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
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
data_list <- data.frame(
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
# 
model <- stan_glmer(formula = cbind(trump_votes, total_votes_16) ~ 
                      sk_change + 
                      pop_change + 
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      sk2014 +
                      diversity16 +
                      pop16 +
                      median_hh_income16 +
                      white16 +
                      bachelors16 +
                      male_unemplmt16 +
                      female_unemplmt16 +
                      (1 | state_id_seq), 
                    family = binomial, data = data_list,chains = 4, iter = 5000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_clinton_full_rstanarm.rds"

saveRDS(model, paste0(path, filename))



#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#The new rstanarm models
#
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new.rds")
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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","clinton_16",
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
p$trump_16 <- as.integer(p$trump_16 )
p$clinton_16 <- as.integer(p$clinton_16)


#model3<-glm(cbind(trump_primary_votes,(trump_primary_votes+cruz_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model3)    
### rethinking model
data_list <- data.frame(
  trump_16 = p$sanders_primary1,
  clinton_16 = p$clinton_primary1,
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
# 
model <- stan_glmer(formula = cbind(trump_16 , clinton_16) ~ 
                      sk_change + 
                      pop_change + 
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      sk2014 +
                      diversity16 +
                      pop16 +
                      median_hh_income16 +
                      white16 +
                      bachelors16 +
                      male_unemplmt16 +
                      female_unemplmt16 +
                      (1 | state_id_seq), 
                    family = binomial, data = data_list,chains = 4, iter = 5000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_clinton_full_rstanarm3.rds"

saveRDS(model, paste0(path, filename))
############################################################################
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new.rds")
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes_16 <- p$sanders_primary1+p$clinton_primary1 

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","sanders_primary1","clinton_primary1",
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
p$sanders_primary1<- as.integer(p$sanders_primary1)
p$clinton_primary1 <- as.integer(p$clinton_primary1)


#model3<-glm(cbind(trump_primary_votes,(trump_primary_votes+cruz_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model3)    
### rethinking model
data_list <- data.frame(
  sanders_votes= p$sanders_primary1,
  clinton_votes = p$clinton_primary1,
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
# 
model <- stan_glmer(formula = cbind(sanders_votes, clinton_votes) ~ 
                      sk_change + 
                      pop_change + 
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      sk2014 +
                      diversity16 +
                      pop16 +
                      median_hh_income16 +
                      white16 +
                      bachelors16 +
                      male_unemplmt16 +
                      female_unemplmt16 +
                      (1 | state_id_seq), 
                    family = binomial, data = data_list,chains = 4, iter = 5000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "sanders_vs_clinton_full_rstanarm3.rds"

saveRDS(model, paste0(path, filename))
#############################################################################
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new.rds")
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# get DV (n) total votes in general
p$total_votes_16 <- p$trump_primary_votes+p$cruz_votes 

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


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
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_primary_votes","cruz_votes",
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
p$trump<- as.integer(p$trump_primary_votes)
p$cruz_votes <- as.integer(p$cruz_votes)


#model3<-glm(cbind(trump_primary_votes,(trump_primary_votes+cruz_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model3)    
### rethinking model
data_list <- data.frame(
  trump= p$trump,
  cruz = p$cruz_votes,
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
# 
model <- stan_glmer(formula = cbind(trump, cruz) ~ 
                      sk_change + 
                      pop_change + 
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      sk2014 +
                      diversity16 +
                      pop16 +
                      median_hh_income16 +
                      white16 +
                      bachelors16 +
                      male_unemplmt16 +
                      female_unemplmt16 +
                      (1 | state_id_seq), 
                    family = binomial, data = data_list,chains = 4, iter = 5000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_cruz_full_rstanarm3.rds"

saveRDS(model, paste0(path, filename))


####################################
####################################
####################################
####################################
####Only change variables -
####################################
####################################
####################################library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new.rds")
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))



# get change in social capital index - do not scale (1 unit equals 1 sd)
p$sk2014_over_2005 <- p$sk14-p$sk05

# census data differences
p$pop_change_16_to_10 <- log(p$pop16E) -log(p$pop_2010)
p$median_hh_income_16_to_10 <- log(p$hh_income16E) - log(p$hh_income10E)
p$white_16_to_10 <- (p$white16E/p$pop16E)-(p$white10E/p$pop_2010)
p$bachelors_16_to_10 <- (p$bachelors16E/p$pop16E)-(p$bachelors10E/p$pop_2010)
p$male_unemplmt_16_to_10 <- (p$male_unemployed16E/p$pop16E)-(p$male_unemployed10E/p$pop_2010)
p$female_unemplmt_16_to_10 <- (p$female_unemployed16E/p$pop16E)-(p$female_unemployed10E/p$pop_2010)
p$for_born_16_to_10 <- (p$foreign_born16E/p$pop16E)-(p$foreign_born10E/p$pop_2010)

# cdc data differences
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4





p <- p %>% select ("county","state","trump_primary_votes","cruz_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]


#p$state_id <- coerce_index(p$state)
#p <- p %>% arrange(state_id)
#p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$cruz_votes <- as.integer(p$cruz_votes)


#model3<-glm(cbind(trump_primary_votes,(trump_primary_votes+cruz_votes)) ~ sk2014_over_2005 + pop_change_16_to_10+
# median_hh_income_16_to_10+
#bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
# drugs_16_to_8+suicides_16_to_8 + sk2014+ diversity16+                  
#pop_16+median_hh_income_16+white_16+bachelors_16+male_unemplmt_16+female_unemplmt_16,family=binomial, data=p)


# check variance inflation factors
#library(car)
#vif(model3)    
### rethinking model
data_list <- data.frame(
  trump_votes= p$trump_primary_votes,
  cruz_votes = p$cruz_votes,
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
  state_id = p$state)

# trump clinton model
# 
model <- stan_glmer(formula = cbind(trump_votes, cruz_votes) ~ 
                      sk_change + 
                      pop_change + 
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      (1 | state_id), 
                    family = binomial, data = data_list,chains = 4, iter = 5000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_cruz_change_only_rstanarm3.rds"

saveRDS(model, paste0(path, filename))