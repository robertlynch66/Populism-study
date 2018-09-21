library(lme4)
library(dplyr)
#   Dredge code for determining key variables
data <- readRDS("C:/Users/rofrly/Dropbox/Github/Populism_ms/all_data.rds")

#  format variables




##  Rename variables
names(data) = c("county", "state", "clinton_16","trump_16", "obama_08", "mccain_08", "obama_12", "romney_12",
                   "clinton_primary","sanders_primary","miss_data", "alcohol_08", "alcohol_12", "alcohol_16","drug_08", 
                "drug_12","drug_16","suicides_08", "suicides_12", "suicides_16","pop_2014",
                "pop_2010","perc_under_18","perc_over_65","perc_females","perc_white","perc_black",
                "perc_asian","perc_hisp","perc_immigrants","perc_high_school","perc_bachelors",
                "perc_vets","commuting_time","perc_own_homes","per_cap_income","median_hh_income",
                "perc_below_poverty","land_area","pop_density_2010","pop_density_2014","religious_14",
                "civic_14","business_14","political_14","professsional_14","labor_14","bowling_14",
                "recreation_14","golf_14","sports_14","pop_14","soc_cap_agg_14","voter_to_12",
                "census_resp_10","non_profits_14","soc_cap_indx_14","hh_inc_change_p25","hh_inc_change_p75",
                "diversity_idx_2016")

data$clinton_16 <- as.numeric(gsub(",","",data$clinton_16))
data$trump_16 <- as.numeric(gsub(",","",data$trump_16))
data$obama_08 <- as.numeric(gsub(",","",data$obama_08))
data$mccain_08 <- as.numeric(gsub(",","",data$mccain_08))
data$clinton_primary <- as.numeric(gsub(",","",data$clinton_primary))
data$sanders_primary <- as.numeric(gsub(",","",data$sanders_primary))

data$total_votes_16 <- data$clinton_16+data$trump_16
data$total_votes_08 <- data$obama_08+data$mccain_08
data$total_votes_12 <- data$obama_12+data$romney_12
data$primary_total <- data$clinton_primary+data$sanders_primary
#scale raw data counts for population - maybe not a good idea
## maybe unscale the 
data$suicides_16_scaled <- data$suicides_16/data$pop_2014*1e5
data$alcohol_08_scaled <- data$alcohol_08/data$pop_2014*1e5
data$alcohol_12_scaled  <- data$alcohol_12/data$pop_2014*1e5
data$alcohol_16_scaled  <- data$alcohol_16/data$pop_2014*1e5
data$drug_08_scaled    <-  data$drug_08/data$pop_2014*1e5
data$drug_12_scaled   <- data$drug_12  /data$pop_2014*1e5
data$drug_16_scaled  <- data$drug_16 /data$pop_2014*1e5
data$suicides_08_scaled  <- data$suicides_08 /data$pop_2014*1e5
data$suicides_12_scaled  <- data$suicides_12 /data$pop_2014*1e5
data$religious_14_scaled <- data$religious_14/data$pop_2014*1e5
data$civic_14_scaled <- data$civic_14/data$pop_2014*1e5
data$business_14_scaled  <- data$business_14 /data$pop_2014*1e5
data$political_14_scaled <- data$political_14/data$pop_2014*1e5
data$professsional_14_scaled <- data$professsional_14/data$pop_2014*1e5
data$labor_14_scaled <- data$labor_14/data$pop_2014*1e5
data$bowling_14_scaled  <- data$bowling_14 /data$pop_2014*1e5
data$recreation_14_scaled  <- data$recreation_14 /data$pop_2014*1e5
data$golf_14_scaled  <- data$golf_14 /data$pop_2014*1e5
data$sports_14_scaled   <- data$sports_14  /data$pop_2014*1e5
data$non_profits_14_scaled  <- data$non_profits_14 /data$pop_2014*1e5



#  get surplus votes
data$surplus_hillary <- data$clinton_16-data$trump_16
data$surplus_hillary <- data$surplus_hillary - min(data$surplus_hillary)

data$surplus_hillary2 <- data$clinton_primary-data$sanders_primary
data$surplus_hillary2 <- data$surplus_hillary2 - min(data$surplus_hillary2, na.rm=T)


# rescale and structure variables
data$county_id <- as.factor(data$county_id)
data$county_id <- coerce_index(data$county_id)
data_cc <- data[complete.cases(data),]

#memodel <- glmer(cbind(clinton_16, trump_16) ~ log(pop_2014)+suicides_16 + (1|state), family = binomial, data=data,
#               control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

model <- glm(cbind(clinton_16, trump_16) ~ log(pop_2014)+suicides_16, family = binomial, data=data)

 ### offset can be log of population size

#Recall that an offset is just a predictor variable whose coefficient is fixed at 1
#we've converted the LHS of the model equation to be a rate of surplus Hillary vootes per unit population
model <- glm(surplus_hillary ~ suicides_16 +offset(I(log(pop_2014))), family=poisson, data=data)
summary(model)

data2 <- data[, sapply(data, is.numeric)]
cor(data2, use = "complete.obs", method = "pearson")

cor(data2$pop_2014,data2,use = "complete.obs", method = "pearson")

#  run offset model in rethinking
#Compute the offset
library(rethinking)
data$log_pop<-log(data$pop_2014)
# fit the model
offset_model<-map2stan(
  alist(
    surplus_hillary~dpois(lambda),
    log(lambda)<- log_pop+a+b*suicides_16,
    a ~ dnorm (0,100),
    b ~ dnorm (0,1)
  ), data=d)
# save the files as csv and rds
saveRDS(data, "C:/Users/robert/Dropbox/Github/Populism_ms/all_data.rds")
# save as .csv file
write.csv(data, file = "C:/Users/robert/Dropbox/Github/Populism_ms/all_data.csv")

## the aggregated binomial with varying intercepts
data_cc$state_id <- coerce_index(data_cc$state)
model<- map(
  alist(
    # again the n value or number of trials is the number of applications
    trump_16~ dbinom(total_votes_16, p),
    # indexed intercepts for all 50 states
    logit(p) <- a[state_id] + bpb*perc_black,
    # the prior for the intercepts mus be indexed too since we are doing them one at a time
    a[state_id] ~ dnorm (0,10), 
    bpb~ dnorm (0,10)
  ), data=data_cc,start=list(a=0,bpb=0))