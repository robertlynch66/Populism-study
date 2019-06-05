####Only change variables -
##################################################
####trump vs clinton model#############
######################################
########################################
###########################################
library(dplyr)
library(rstanarm)
# for convenience - comment out
p<- readRDS("../data files/populism_data_new2.rds")
####################################

####################################
####################################
### trump vs clinton model ########################
### ###############################################
### 

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





p <- p %>% select ("county","state","trump_16","clinton_16",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]


#p$state_id <- coerce_index(p$state)
#p <- p %>% arrange(state_id)
#p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_votes<- as.integer(p$trump_16)
p$clinton_votes <- as.integer(p$clinton_16)



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
                    family = binomial, data = data_list,chains = 1, iter = 50, warmup = 25,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_clinton_change_only_rstanarm7.rds"
saveRDS(model, paste0(path, filename))
###############################################################
###############################################################
###############################################################
####################################
####################################
### sanders vs clinton model ########################
### ###############################################
### 
p<- readRDS("../data files/populism_data_new2.rds")

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



p <- p %>% select ("county","state","clinton_primary1","sanders_primary1",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]


#p$state_id <- coerce_index(p$state)
#p <- p %>% arrange(state_id)
#p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_votes<- as.integer(p$sanders_primary1)
p$clinton_votes <- as.integer(p$clinton_primary1)


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
  sanders_votes= p$sanders_votes,
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

# sanders clinton model
# 
model <- stan_glmer(formula = cbind(sanders_votes, clinton_votes) ~ 
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
                    family = binomial, data = data_list,chains = 1, iter = 50, warmup = 25,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "sanders_vs_clinton_change_only_rstanarm7.rds"
saveRDS(model, paste0(path, filename))
###############################################################
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



# make dv's numeric first
p$trump_primary_votes <- as.integer(p$trump_primary_votes)+1
p$kasich_votes <- as.integer(p$kasich_votes)+1
#make dv integers
#p$trump_primary_votes<- as.integer(p$trump_primary_votes)
#p$kasich_votes <- as.integer(p$kasich_votes)

p <- p %>% select ("county","state","trump_primary_votes","kasich_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]


#p$state_id <- coerce_index(p$state)
#p <- p %>% arrange(state_id)
#p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))



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
  kasich_votes = p$kasich_votes,
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

# trump kasich model
# 
model <- stan_glmer(formula = cbind(trump_votes, kasich_votes) ~ 
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
                    family = binomial, data = data_list,chains = 1, iter = 50, warmup = 25,control=list(max_treedepth=20))



path<- (paste0("results/"))
filename <- "trump_vs_kasich_change_only_rstanarm7.rds"

saveRDS(model, paste0(path, filename))



# test for homogeneity of variance of predictors
library(dplyr)
p<- readRDS("../data files/populism_data_new2.rds")
p <- p%>% select(3:6,55:66)
p <- p[complete.cases(p),]

p$trump_percent<- p$trump_16/(p$clinton_16+p$trump_16)
bartlett.test(trump_percent ~ interaction(sk2014_over_2005,pop_change_16_to_10,median_hh_income_16_to_10,white_16_to_10,
bachelors_16_to_10,male_unemplmt_16_to_10,female_unemplmt_16_to_10,for_born_16_to_10,alcohol_16_to_8,drugs_16_to_8,             suicides_16_to_8), data=p)

bartlett.test(trump_percent ~ interaction(sk2014_over_2005,pop_change_16_to_10), data=p)


#### get the relationship between cancer and populist votes
#### 
library(dplyr)
p<- readRDS("../data files/populism_data_new2.rds")
p <- p%>% select(1:6,17,20,37,47,55:66)
p <- p[complete.cases(p),]
# read in new cancer data
c <- read.csv("../data files/cancer_deaths.csv", na.strings=c("Suppressed","Missing"))

# change all counties to characters
c$County <- as.character(c$County)    
c$State <- as.character(c$State) 
c$county<-gsub(" ","",social_cap$county, fixed=TRUE)

## # get counties seperated and spelled right
library(openintro)
library(tidyr)
c <- c %>% separate(County, 
                c("county", "name","state"))
c$state <- tolower(c$State)
c$county<-tolower(c$county)
c$cancer_2008 <- as.numeric(c$cancer_2008)
c$cancer_2016 <- as.numeric(c$cancer_2016)
c$State <- NULL
c$name <- NULL


# join dfs
data<-left_join(p, c, by=c("state", "county"))

data$cancer_16_to_8 <- (data$cancer_2016/data$pop16E)*10e4-(data$cancer_2008/data$pop10E)*10e4

# run models (all 3) against cancer

model<-glm(cbind(trump_16,clinton_16) ~ sk2014_over_2005 + pop_change_16_to_10+
median_hh_income_16_to_10+
bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
drugs_16_to_8+suicides_16_to_8,family=binomial, data=data)
summary(model)

model2<-glm(cbind(trump_16,clinton_16) ~ sk2014_over_2005 + pop_change_16_to_10+
             median_hh_income_16_to_10+
             bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8+cancer_16_to_8,family=binomial, data=data)
summary(model2)

# Sanders
model<-glm(cbind(sanders_primary1,clinton_primary1) ~ sk2014_over_2005 + pop_change_16_to_10+
             median_hh_income_16_to_10+
             bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8,family=binomial, data=data)
summary(model)

model2<-glm(cbind(sanders_primary1,clinton_primary1) ~ sk2014_over_2005 + pop_change_16_to_10+
              median_hh_income_16_to_10+
              bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
              drugs_16_to_8+suicides_16_to_8+cancer_16_to_8,family=binomial, data=data)
summary(model2)

# kasich
model<-glm(cbind(trump_primary_votes,kasich_votes ) ~ sk2014_over_2005 + pop_change_16_to_10+
             median_hh_income_16_to_10+
             bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
             drugs_16_to_8+suicides_16_to_8,family=binomial, data=data)
summary(model)

model2<-glm(cbind(trump_primary_votes,kasich_votes) ~ sk2014_over_2005 + pop_change_16_to_10+
              median_hh_income_16_to_10+
              bachelors_16_to_10+male_unemplmt_16_to_10+female_unemplmt_16_to_10+for_born_16_to_10+alcohol_16_to_8+
              drugs_16_to_8+suicides_16_to_8+cancer_16_to_8,family=binomial, data=data)
summary(model2)


###########################Structural equation models
# Load the lavaan library
library(lavaan)

# Look at the dataset
data(p)
head(p)

# Define your model specification
model <- 'latent=~suicides16+for_born_16+suicides_16_to_8+log_pop_16'
