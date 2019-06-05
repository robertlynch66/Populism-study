# simple simulation example

install.packages('dplyr')
install.packages('rstanarm')
library(dplyr)
library(rstanarm)



# path to the folder with the R data files

# read in the data 
# PUT in CORRECTpath in place of '...' below to read in the sample data frame 'sample' which should give you 2 of the smallest counties - (loving, texas and clark, idaho and greenlee, arizona)
p<- readRDS(".../sample.rds")

#make dependent variable integers
p$trump_votes<- as.integer(p$trump_16)
p$clinton_votes <- as.integer(p$clinton_16)

# run in rstanarm
data_list <- data.frame(
  trump_votes= p$trump_votes,
  clinton_votes = p$clinton_votes,
  sk_change = p$sk2014_over_2005,
  white_16_to_10 = p$white_16_to_10,
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
model <- stan_glmer(formula = cbind(trump_votes, clinton_votes) ~ 
                      sk_change + 
                      pop_change + 
                      white_16_to_10 +
                      median_hh_income_change + 
                      perc_bachelors_change +
                      male_unemplmt_change +
                      female_unemplmt_change +
                      for_born_change +
                      alcohol_change +
                      drugs_change +
                      suicides_change +
                      (1 | state_id), 
                    family = binomial, data = data_list,chains = 1, iter = 2000, warmup = 500,control=list(max_treedepth=20))





# get parameter estimates
summary(model)
