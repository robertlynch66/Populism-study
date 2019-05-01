# trump clinton

library(dplyr)
p<- readRDS("../data files/populism_data_new2.rds")

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
p$pop_16 <- p$pop16E



p <- p %>% select ("county","state","trump_16","clinton_16","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$clinton_16 <- as.integer(p$clinton_16)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm5.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm4.rds")
#OR

#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm4.rds")

#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm4.rds")

pr <- c( 0.25, 0.5, 0.75)

quantile(p$bachelors_16_to_10 ,  probs = pr) # Extremes & Quartiles by default
#25%          50%          75% 
#0.006032245 0.015262186 0.024444660
us_sk_change <- sum(p$sk2014_over_2005*p$pop_16)/mean(p$pop_16)/nrow(p)
us_pop_change <- sum(p$pop_change_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_hhi_change <- sum(p$median_hh_income_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_bach_change <- sum(p$bachelors_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_male_u_change <- sum(p$male_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_female_u_change <- sum(p$female_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_fb_change <- sum(p$for_born_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_alcho_change <- sum(p$alcohol_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_drug_change <- sum(p$drugs_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_suicide_change <- sum(p$suicides_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)  
us_white_change <- sum(p$white_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p) 

ps <- data.frame(
  trump_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  white_change =rep(us_white_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(0.024444660,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change ,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
# get means and intervals
mean(s_mu$mean)
mean(s_mu$lower)
mean(s_mu$upper)



### sanders clinton#################################################################################
### 
### 
### 
### ################################################################################################
library(dplyr)

p<- readRDS("../data files/populism_data_new2.rds")
p$total_votes <- p$sanders_primary1+p$clinton_primary1

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
p$pop_16 <- p$pop16E
p$log_pop_16 <- log(p$pop16E)
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



p <- p %>% select ("county","state","sanders_primary1","clinton_primary1","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_primary1<- as.integer(p$sanders_primary1)
p$clinton_primary1 <- as.integer(p$clinton_primary1)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm4.rds")
#25%          50%          75% 
#0.006032245 0.015262186 0.024444660
ps <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  white_change = rep (us_white_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep( 0.024444660,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change ,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_sanders_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
# get means and intervals
mean(s_mu$predicted_sanders_percent_mean)
mean(s_mu$lower)
mean(s_mu$upper)

##### Trumo kasich#################################################################################3
p<- readRDS("../data files/populism_data_new2.rds")
p$kasich_votes <- as.numeric(p$kasich_votes)
p$total_votes <- p$trump_primary_votes+p$kasich_votes

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
p$pop_16 <- p$pop16E
p$log_pop_16 <- log(p$pop16E)
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


p$total_votes <- p$trump_primary_votes+p$kasich_votes
p <- p %>% select ("county","state","trump_primary_votes","kasich_votes","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$kasich_votes <- as.integer(p$kasich_votes)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm4.rds")
#us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)  
#25%          50%          75% 
#0.006032245 0.015262186 0.024444660
ps <- data.frame(
  trump_votes = rep(0,41),
  kasich_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(0.024444660 ,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_trump_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
mean(s_mu$predicted_trump_percent_mean)
mean(s_mu$lower)
mean(s_mu$upper)




#### Trump CRuz#####################################################################################
library(dplyr)

p<- readRDS("../data files/populism_data_new2.rds")
p$total_votes <- p$trump_primary_votes+p$cruz_votes

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
p$pop_16 <- p$pop16E
p$log_pop_16 <- log(p$pop16E)
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



p <- p %>% select ("county","state","trump_primary_votes","cruz_votes","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$cruz_votes <- as.integer(p$cruz_votes)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm4.rds")
us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)  
ps <- data.frame(
  trump_votes = rep(0,41),
  cruz_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(0.024339096 ,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_trump_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
mean(s_mu$predicted_trump_percent_mean)
mean(s_mu$lower)
mean(s_mu$upper)

##########################################
############################################
##############################################
# get the R squared out of rstanarm
stan_model <- stan_glm(mpg ~ wt, data = mtcars)

ss_res <- var(residuals(m1))
ss_total <- var(fitted(m1)) + var(residuals(m1))
1 - (ss_res / ss_total)