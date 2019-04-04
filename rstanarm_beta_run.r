library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# #p<- readRDS("../data files/populism_data_new.rds")
path<- (paste0("~/r_files/"))
file<- "populism_data_new.rds"
p <- readRDS(paste0(path, file))

# add weights
p$total_votes_16 <- p$trump_16+p$clinton_16
# get dv - percent trump votes
p$trump_perc_16 <- p$trump_16/(p$trump_16+p$clinton_16)
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


p <- p %>% select ("county","state","trump_16","total_votes_16","trump_perc_16",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16")
p <- p[complete.cases(p), ]



#make dv integers
#p$trump_votes<- as.integer(p$trump_16)
#p$total_votes_16 <- as.integer(p$total_votes_16)


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
  trump_perc_16= p$trump_perc_16,
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
  state = p$state)

# trump clinton model
# 
model <- stan_betareg(trump_perc_16 ~ 
                      sk_change+ 
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
                      female_unemplmt16, weights=as.integer(total_votes_16),
                     data = data_list, chains = 1, iter = 100, warmup = 100,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_clinton_full_rstanarm_beta.rds"

saveRDS(model, paste0(path, filename))