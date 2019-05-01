
########Trump vs Kasich#######################################
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new2.rds")
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



# make dv's integers
p$trump_primary_votes <- as.integer(p$trump_primary_votes)+1
p$kasich_votes <- as.integer(p$kasich_votes)+1


p <- p %>% select ("county","state","trump_primary_votes","kasich_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]


### stan glmer model
data_list <- data.frame(
  trump_votes= p$trump_primary_votes,
  cruz_votes = p$cruz_votes,
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


model <- stan_glmer(formula = cbind(trump_votes, cruz_votes) ~ 
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
                    family = binomial, data = data_list,chains = 4, iter = 4000, warmup = 1500,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_cruz_change_only_rstanarm7.rds"

saveRDS(model, paste0(path, filename))