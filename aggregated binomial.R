library(rethinking)
library(dplyr)
library(lme4)
full_data <- readRDS("full_pop_data.rds")
#OR
data <- readRDS("subset_pop_data.rds")

data <- readRDS("final_dataset.rds")

# read in obama_clinton data 2008

clinton_obama_2008 <- read_csv("clinton_obama_2008.csv")
data <- clinton_obama_2008 %>% select (1,2,7,8,9) %>% as.data.frame()
# make column names lower case
names(data) <- tolower(names(data))
library(openintro)
data$state <- abbr2state(data$state)
# add clinton obama to full data
full_data <- full_data %>% left_join(data, by=c("county","state"))
colnames(full_data$clinton) <- "clinton_primary_08"
colnames(full_data$obama) <- "obama_primary_08"
names(full_data)[names(full_data) == 'clinton'] <- 'clinton_primary_08'
names(full_data)[names(full_data) == 'obama'] <- 'obama_primary_08'
# compare sanders to obama
full_data$sanders_over_obama <- full_data$sanders_primary-full_data$obama_primary_08
full_data$sanders16_over_clinton8 <- full_data$sanders_primary-full_data$clinton_primary_08


## then skip to line 106
## 
## 
# read in new data sheets
fb_2016 <- read.csv("Foreign_born_percent_2016.csv")
fb_2008 <- read.csv("Foreign_born_percent_2008.csv")
econ_2016 <- read.csv("economic_data_2016.csv")
econ_2008 <- read.csv("economic_data_2008.csv")
census_2008 <- read.csv("Census_data_2008.csv")
census_2016 <- read.csv("census data_2016.csv")
# fix df's, join and get diffs fb
fb16 <- separate(data = fb_2016, col = Geography, into = c("county", "state"), sep = "\\,")
fb16$state <- gsub(" ","",fb16$state, fixed=TRUE)
fb08 <- separate(data = fb_2008, col = Geography, into = c("county", "state"), sep = "\\,")
fb08$state <- gsub(" ","",fb08$state, fixed=TRUE)
fb <- fb08 %>% left_join(fb16, by=c("state"="state","county"="county"))
fb$pop_change_16_to_8 <- fb$total_2016-fb$total_2008
fb$foreign_born_16_to_8 <-fb$percent_foreign_born_2016-fb$precent_foreign_born_2008
fb$out_of_state_born_16_to_8 <-fb$percent_born_out_of_state_2016-fb$percent_born_out_of_state_2008
fb <- fb %>% select ("county","state","pop_change_16_to_8","foreign_born_16_to_8","out_of_state_born_16_to_8")
fb$county <- gsub(" County","",fb$county, fixed=TRUE)
# census
c16 <- separate(data = census_2016, col = Geography, into = c("county", "state"), sep = "\\,")
c16$state <- gsub(" ","",c16$state, fixed=TRUE)
c08 <- separate(data = census_2008, col = Geography, into = c("county", "state"), sep = "\\,")
c08$state <- gsub(" ","",c08$state, fixed=TRUE)
c <- c08 %>% left_join(c16, by=c("state"="state","county"="county"))
c$percent_white_16_to_8 <- c$Percent_white_2016-c$percent_white_2008
c$percent_black_16_to_8 <- c$percent_black_2016-c$percent_black_2008
c$percent_hispanic_16_to_8 <- c$percent_hispanic_2016-c$percent_hispanic_2008
c <- c %>% select ("county","state","percent_hispanic_16_to_8","percent_black_16_to_8","percent_white_16_to_8")
c$county <- gsub(" County","",c$county, fixed=TRUE)
# income
e16 <- separate(data = econ_2016, col = Geography, into = c("county", "state"), sep = "\\,")
e16$state <- gsub(" ","",e16$state, fixed=TRUE)
e08 <- separate(data = econ_2008, col = Geography, into = c("county", "state"), sep = "\\,")
e08$state <- gsub(" ","",e08$state, fixed=TRUE)
e <- e08 %>% left_join(e16, by=c("state"="state","county"="county"))
e$median_hh_income_16_to_8 <- e$median_hh_income_2016-e$Median_hh_income_2008
e <- e %>% select ("county","state","median_hh_income_16_to_8")
e$county <- gsub(" County","",e$county, fixed=TRUE)
#rejoin to data
data <- data %>% left_join (c, by =c("county","state"))
data <- data %>% left_join (e, by =c("county","state"))
data <- data %>% left_join (fb, by =c("county","state"))
# make full data and subset data
saveRDS(data, "full_pop_data.rds")
data <- data %>% select(1:10,25:32,37,38,41,92,94:96,99:106)
### skip this stuff through lines 34
saveRDS(data, "subset_pop_data.rds")
# get covariates you want and complete cases
 data$trump_over_romney <- data$trump_16-data$romney_12
 data$trump_over_mccain <- data$trump_16-data$mccain_08
 data$trump_over_hillary <- data$trump_16-data$clinton_16
 
 #drugs over time
 data$drug_16_over_08 <-(data$drug_16-data$drug_08)/(data$pop_2010)*1e5
 #alcohol over time
 data$alcohol_16_over_08 <-(data$alcohol_16-data$alcohol_08)/(data$pop_2010)*1e5
 #suicides over time
 data$suicides_16_over_08 <-(data$suicides_16-data$suicides_08)/(data$pop_2010)*1e5
 
 
# correaltion matrix function
 flattenCorrMatrix <- function(cormat, pmat) {
   ut <- upper.tri(cormat)
   data.frame(
     row = rownames(cormat)[row(cormat)[ut]],
     column = rownames(cormat)[col(cormat)[ut]],
     cor  =(cormat)[ut],
     p = pmat[ut]
   )
 }
 
 
 # select variables


c("trump_16","mccain_08","perc_white","pop_density_2014",
                             "median_hh_income","perc_immigrants",
                             "alcohol_16_over_08",
                             "suicides_16_over_08","sk2014_over_2005",
                             "drug_16_over_08","percent_hispanic_16_to_8",  
                               "percent_black_16_to_8",     
                              "percent_white_16_to_8",     
                               "median_hh_income_16_to_8",  
                               "pop_change_16_to_8",        
                               "foreign_born_16_to_8",
                               "out_of_state_born_16_to_8")
 data_cc <- data_cc[complete.cases(data_cc),]
 
 
 data_cc <- data %>% select ("county","state","trump_16","mccain_08","perc_white", "pop_density_2014",
  "median_hh_income", "perc_immigrants","alcohol_16_over_08",
  "suicides_16_over_08","sk2014_over_2005",
  "drug_16_over_08", "percent_hispanic_16_to_8", "percent_black_16_to_8",   
  "percent_white_16_to_8")
 data_cc <- data_cc[complete.cases(data_cc),]
 # one variable against all others
 COR <- cor(as.matrix(df[,1]), as.matrix(df[,-1]))

# get correaltions between matrix and a single variable
 apply(data_cc,2, function(col)cor(col, data_cc$drug_16_over_08))
 
 

 
 # Question - what is the probability of voting for Trump across
 #all counties? But within states (varying intercepts)
# index states to make it a varying intercept in the model
  d$state_id <- coerce_index(d$state)
  
  cor.test(data$sanders_perc_16_vs_08,data$trump_perc_16_vs_08, use="complete.obs")
  
  
 # First just do it with standard glm (aggregated binomial)
  data_cc$county <- coerce_index(data_cc$county)
  data_cc$state <- coerce_index(data_cc$state)
  
  data_cc$county_id <- seq.int(nrow(data_cc))
  
  model_glm<-glmer(cbind(trump_16,mccain_08) ~ perc_white + pop_density_2014+
                   median_hh_income + perc_immigrants+alcohol_16_over_08 +
  suicides_16_over_08  +sk2014_over_2005+
  drug_16_over_08 + percent_hispanic_16_to_8 + percent_black_16_to_8 +   
  percent_white_16_to_8 + (1|county_id),
  data=data_cc, family=binomial,control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 10)
  
  summary(model_glm)
  
  m <- glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
               (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)
  # linear model using offset
  
  # make new variables
  data_cc$trump_over_mccain <- data_cc$trump_16-data_cc$mccain_08
  data_cc$trump_over_romney <- data_cc$trump_16-data_cc$romney_12
  
  # use log of trump_16 for offset
  model <- glm (trump_over_mccain~ log(trump_16) + perc_white +pop_density_2014+
                median_hh_income + perc_immigrants+
                alcohol_16_over_08+
                suicides_16_over_08+sk2014_over_2005+
                drug_16_over_08+percent_hispanic_16_to_8+  
                percent_black_16_to_8+     
                percent_white_16_to_8+     
                median_hh_income_16_to_8+  
                pop_change_16_to_8+        
                foreign_born_16_to_8+
                out_of_state_born_16_to_8, data=data_cc, family=gaussian)
  summary(model)
  
  
votes<-map(
  alist(
    #Trump votes is a binomial ditribution based on the number of trials (total votes) 
    #for each row of aggregated data with a probability of a Trump vote given as p 
    trump_16 ~ dbinom(total_votes, p),
 
    # the model is the logistic function of p equals the intercept plus some slope times
    #the per-capita suicides in each county
    logit(p) <- a[state_id] + bs*suicides_per_capita,
    
    a ~ dnorm (0,10),
    bs ~ dnorm (0,10)),data=d, start=list(a=0,bm=0))

#Should I use varying intercepts for counties (i.e. let counties vary)? 
#like depts in UC Berkely data
#I think I want counties to be entered as varying intercept--
#data structure of d
#  county suicides_per_capita trump_votes hillary_votes total_votes 
1    1            1                 512          313          825  
2    2            2                  89           19          108    
3    3            2                 353          207          560    
4    4           20                  17            8           25    
5    5           19                 120          205          325    
6    6            4                 202          391          593    

# varying intercepts model
d$county_id <- coerce_index(d$county)
vote_vi<- map2stan(
  alist(
    # again the n value or number of trials is the number of applications
    admit~ dbinom(applications, p),
    # indexed intercepts for all 6 depts
    logit(p) <- a[county_id] + bs*suicides_per_capita,
    # the prior for the intercepts must be indexed too since we are doing them one at a time
    a[dept_id] ~ dnorm (0,10), 
    bm~ dnorm (0,10)
  ), data=d)

# to check if the varying intercept model allowing county to vary is the best model- use the
# comare funtion and see if this model beats the single intercept model
compare(votes, votes_vi)

#Also do a postcheck to see if the single intercept retordicts the data
# make a model with varying slopes and intercepts
model <- map2stan(
  alist(
    trump_vote ~ dbinom(total_votes, p)
  )
)

m10.9<- map(
  alist(
    # again the n value or number of trials is the number of applications
    admit~ dbinom(applications, p),
    # indexed intercepts for all 6 depts
    logit(p) <- a[dept_id] + bm*male,
    # the prior for the intercepts mus be indexed too since we are doing them one at a time
    a[dept_id] ~ dnorm (0,10), 
    bm~ dnorm (0,10)
  ), data=d)


saveRDS(full_data, "full_pop_data.rds")
write.csv(new_data, file = "C:/Users/robert/Dropbox/pop_data.csv")
install.packages("xlsx")
library("xlsx")
write.xlsx(data, "C:/Users/robert/Dropbox/pop_data.xlsx", sheetName="Sheet1")
res <- write.xlsx(data, "C:/Users/robert/Dropbox/pop_data.csv")  


### here are some examples from R data camp
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("tidycensus")
install.packages("tigris")
install.packages("maps")
z <- data(df_county_demographics)
library(choroplethr)
left_join(df_county_demographics, uspres_county, by = "county.fips")


library(choroplethr)
#  a free course:  www.CensusMappingCourse.com
library(choroplethrMaps)
library(tidycensus)
library(tigris)
library(maps)
data(df_county_demographics)
data(df_president)
data(df_president_ts)


#### census data from decennial census or american community survey done every 10 years
## census api key
filename <- "api_key.txt"
my_api <- readChar(filename, file.info(filename)$size)
my_api <- gsub("\r\n","",my_api)
#my census key is 47fc5f7f57c9d8f649d6b2920096acfe57e9f589
# examine the 2013, 5-year county percent hispanic estimates as a boxplot and choropleth


library(tidycensus)

census_api_key(my_api, install = TRUE)
readRenviron("~/.Renviron")
# the boxplot shows the distribution
# 
 state_pop <- get_decennial(geography = "state", 
variables = "P001001")
 
 state_income <- get_acs(geography = "state", 
                         variables = "B19013_001")
 
 # note moe = margin of error
 
 
boxplot(df_county_demographics$percent_hispanic)

# the choropleth map shows the location of the values
# first set the 'value' column to be the column we want to render
df_county_demographics$value = df_county_demographics$percent_hispanic
full_data$value <- full_data$sanders_over_obama
# this makes a beautiful map
county_choropleth(df_county_demographics)
county_choropleth(data)
# basic graphs
ggplot(county_merged, aes(x=percent_white,y=Dem.pct)) +
  geom_point() +
  geom_smooth(method="lm")


uspres_results.slim <- uspres_results %>%
  select(-c(is.national.winner, national.count, national.party.percent))


# link county fips to full data by county and state
library(tidyr)
library(dplyr)
fips <- separate(data = county.fips, col = polyname, into = c("state", "county"), sep = "\\,")
# link fips to full data by stata and county to get fips back



full_data$county <- tolower(full_data$county)
full_data$state <- tolower(full_data$state)
full_data <- full_data %>% left_join (fips, by =c("county","state"))

full_data$region <- full_data$fips




# get rid of all duplicated region numbers in data frame column region
data <- subset(full_data, !duplicated(full_data[,114]) )



# choose the variable you want to map
data$value <- data$suicides_16_scaled
#Then map it!
map_suicides_16 <- county_choropleth(data)
map_trump_percent <- county_choropleth(data)

# choose another variable
data$percent_trump <- data$trump_16/(data$trump_16+data$clinton_16)
data$value <- data$percent_trump
# map it
map_sanders16_beats_clinton8 <- county_choropleth(data)

# choose another variable
data$value <- data$sanders_over_obama
# map it
map_sanders_over_obama <- county_choropleth(data)

# choose another variable
data$value <- data$trump_over_mccain
# map it
map_map_trump_over_mccain <- county_choropleth(data)

# choose another variable
data$value <- data$trump_over_hillary
# map it
map_trump_over_hillary <- county_choropleth(data)

# choose another variable
data$value <- data$sanders_over_hillary
# map it
map_sanders_over_hillary <- county_choropleth(data)

# put maps side by side
library(gridExtra)

# Graph the two maps (democratic_map and white_map) from the previous exercises side-by-side
grid.arrange(map_trump_over_hillary, map_sanders_over_hillary)

## rename the column 'region' to county.fips
df_county_demographics <- df_county_demographics %>%
  rename("county.fips" = region)

## other more cusomizable mapping tools in R
#ggplot + geom_sf(): fast, customizability, need for data
#leaflet: interactive, customizable, steep learning curve, need for data
#
#clorplehtr - simple - must name county.fips region and value as what you want mapped
county_map <- county_merged %>%
  dplyr::rename("region" = county.fips,
                "value" = Dem.pct)


# the way they model county dem support in R course
fit <- lm(Dem.pct ~ percent_white + per_capita_income, data=county_merged)
summary(fit)


# correlations between DV's
data:  data$sanders_over_obama and data$trump_over_mccain
t = 10.517, df = 1313, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
  0.2281144 0.3278526
sample estimates:
  cor 
0.2787349 

data$sanders_over_hillary_16 <- data$sanders_primary-data$clinton_primary

#Pearson's product-moment correlation

data:  data$trump_over_hillary and data$sanders_over_hillary_16
t = 44.42, df = 2622, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
0.6328873 0.6765832
sample estimates:
cor 
0.6552831 


#  New models discussed with samuli
# 2 newd DV's are proportion of votes for trump in 2016 and proportion of votes for sanders in 2016
data$sanders_percent <- data$sanders_primary/(data$clinton_primary+data$sanders_primary)
data$trump_percent <- data$trump_16/(data$trump_16+data$clinton_16)

# put in an offset with beta distribution 
library(lmtest)

lrtest(model.beta)

headTail(Data)
library(emmeans)

library(lme4)
library(betareg)
# first make sure that DV it between 0 and 1 and subtract 0.0001 from the DV if necessary (e.g. for caucus states)

#with offset
model = betareg(trump_percent ~ soc_cap_indx_14 + drug_16 + alcohol_16 +
                  median_hh_income+perc_white+offset(log(pop_2014)),
                data = data)

summary(model)
# normal
model_norm = glm(trump_percent ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
                  median_hh_income+perc_white+offset(log(pop_2014)),
                data = data, family=gaussian)

summary(model_norm)

## aggregated binomial again
# offset
library(rethinking)
data$county_id <- coerce_index(data$county)
agg_bin<-glm(cbind(trump_16,clinton_16) ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
              median_hh_income+perc_white,
                 data=data, family=binomial)

summary(agg_bin)


fit <- glm(trump_percent ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
             median_hh_income+perc_white+offset(log(pop_2014)), data=data)
summary(fit)


# Beta distribution with weights - recovers the intercept percent votes for Trump
#with offset
##################################################
##################################################
##################################################
d$sanders_percent <- d$sanders_primary/(d$clinton_primary+d$sanders_primary)
#reduce to less than 1 fro caucus counties
d$sanders_percent <- d$sanders_percent-0.00001
d$trump_percent <- d$trump_16/(d$trump_16+d$clinton_16)

# get weights
d$total_votes<-d$clinton_16+d$trump_16
#recover intercept
model <- betareg(trump_percent ~ 1,weights=total_votes, data = d)
#run trump model
model = betareg(trump_percent ~ soc_cap_indx_14 + suicides_16_scaled + drug_16_scaled + alcohol_16_scaled +drug_16_scaled+
                  perc_bachelors+pop_density_2014+
                  median_hh_income+perc_white,weights=total_votes,
                data = d)

summary(model)
# get weights
d$total_votes<-d$sanders_primary+d$clinton_primary
# recover intercept
model <- betareg(sanders_percent ~ 1,weights=total_votes, data = d)
#run sanders model
model = betareg(sanders_percent ~ soc_cap_indx_14 + suicides_16_scaled + drug_16_scaled + alcohol_16_scaled +drug_16_scaled+
                  perc_bachelors+pop_density_2014+
                  median_hh_income+perc_white,weights=total_votes,
                data = d)
summary(model)







#############################################################################################################################
# normal
model_norm = glm(trump_percent ~ soc_cap_indx_14  + suicides_16_scaled+ drug_16_scaled + alcohol_16_scaled +drug_16_scaled+perc_bachelors+pop_density_2014+
                   median_hh_income+perc_white+offset(log(pop_2014)),
                 data = d, family=gaussian)

summary(model_norm)

## aggregated binomial again
# offset
library(rethinking)
data$state_id <- coerce_index(data$county)
agg_bin<-glm(cbind(sanders_primary,clinton_primary) ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
               median_hh_income+perc_white,
             data=data, family=binomial)

summary(agg_bin)


fit <- lm(sanders_percent ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
            median_hh_income+perc_white+offset(log(pop_2014)), data=data)
summary(fit)

d <- full_data
## quick and dirty repated measures
## obama_08 mccain_08
d$mccain_percent <- d$mccain_08/(d$mccain_08+d$obama_08)
d$trump_percent <- d$trump_16/(d$trump_16+d$clinton_16)
d$clinton_percent <- d$clinton_16/(d$trump_16+d$clinton_16)
d$gop_popsurge <- d$trump_percent-d$mccain_percent


d <- d %>% select (gop_popsurge,alcohol_16_over_08,suicides_16_over_08,drug_16_over_08,sk2014_over_2005,percent_white_16_to_8,
                     median_hh_income_16_to_8,pop_2014,drug_16_scaled,alcohol_16_scaled,suicides_16_scaled,percent_hispanic_16_to_8)

cor(d,use = "complete.obs")
model <- glm(gop_popsurge~alcohol_16_over_08+suicides_16_over_08+drug_16_over_08+sk2014_over_2005+percent_white_16_to_8+
               median_hh_income_16_to_8+offset(log(pop_2014)), data=d)

summary(model)

# use glimmer to see what it lloks like in map2stan
glimmer(percent_trump~as.factor(actor) +
          prosoc_left*condition-condition, data=data, family=binomial)


glimmer(cbind(trump_16,clinton_16) ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
               median_hh_income+perc_white + (1|state_id),
             data=data, family=binomial)


glimmer (glm(cbind(sanders_primary,clinton_primary) ~ soc_cap_indx_14 +  drug_16 + alcohol_16 +
      median_hh_income+perc_white,
    data=data, family=binomial))
### Now do repeated measures
# use full_data
# 
library(lme4)
library(lmerTest)
demo1.aov <- aov(vote_percent ~ group * year + Error(id), data = demo1)
summary(demo1.aov)

library(psycho)
library(tidyverse)

df <- psycho::emotion %>% 
  select(Participant_ID, 
         Participant_Sex, 
         Emotion_Condition, 
         Subjective_Valence,
         Recall)

summary(df)

# in rethinking treatment or group is Africa
#m7.5 <- map( ????????????
alist(
  log_gdp ~ dnorm( mu , sigma ) ,
  mu <- a + gamma*rugged + bA*cont_africa ,
  gamma <- bR + bAR*cont_africa ,
  a ~ dnorm( 8 , 100 ) ,
  bA ~ dnorm( 0 , 1 ) ,
  bR ~ dnorm( 0 , 1 ) ,
  bAR ~ dnorm( 0 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
) ,
data=dd )


########### repeated measure model template####
library(boot)
betareg()

model <- glmer(gop_percent ~ year*population + year*sucides + year*income + (1|county_id/state_id)+
  weights=total_votes, data=d)
  
#########################################
  model = betareg(clinton_percent ~ binary , data = d)
  
  summary(model)
  inv.logit(-0.867)
  inv.logit(-0.867+0.745)
  -inv.logit(-0.867)+inv.logit(-0.867+0.745)
  -inv.logit (-9.392) + inv.logit(-9.392+0.5857)
  exp(0.656)/(1+exp(0.656))
  mean(d$clinton_percent)
  
  mean(d$perc_asian, na.rm=T)
  d$binary <- ifelse(d$perc_asian<1.4,'0','1')
  
  aggregate(d$clinton_percent~d$binary,FUN=mean, na.rm=TRUE)
  
  model_norm = glm(trump_percent ~ 1+weights=(clinton_16+trump_16)),
                   data = d, family=gaussian)
  
  summary(model_norm)
  
  model_norm = glm(trump_percent ~ 1,
                   data = d, family=gaussian)
  
  summary(model_norm)
  
  d$tot<-d$clinton_16+d$trump_16
  model <- betareg(trump_percent ~ 1,weights=tot, data = d)
  
  library(glmmTMB)
  # beta with random effects
  glmmTMB(trump_percent ~ 1 + (1|state), d, family=list(family="beta",link="logit"))
  d$state_id <- coerce_index(d$state)
  
  z<- glmmTMB(trump_percent ~ soc_cap_indx_14 + suicides_16_scaled + drug_16_scaled + alcohol_16_scaled +drug_16_scaled+
    perc_bachelors+pop_density_2014+
    median_hh_income+perc_white+ (1|state_id),weights=total_votes,data=d,family=list(family="beta",link="logit"))
    
  summary(z)
  s<-supps
  
  s$state_id <- coerce_index(s$state)
  s$sanders_perc_16_vs_08<- s$sanders_primary/(s$sanders_primary+s$clinton_primary)
  s$sanders_perc_16_vs_08 <-s$sanders_perc_16_vs_08 -0.000001
  s$trump_perc_16_vs_08<- s$trump_16/(s$trump_16+s$mccain_08)
  s$sanders_perc_16<- s$sanders_primary/(s$sanders_primary+s$clinton_primary)  
  s$sanders_perc_16 <-  s$sanders_perc_16-0.00000001
  s$trump_perc_16<- s$trump_16/(s$trump_16+s$clinton_16)
    
    
  ## select and correlate variables
  d2 <- d %>% select (state_id,sanders_perc_16_vs_08,trump_perc_16_vs_08,sanders_perc_16,trump_perc_16,
                      clinton_16,trump_16,obama_08,mccain_08,clinton_primary,sanders_primary,obama_primary_08,
                      alcohol_16_over_08,suicides_16_over_08,drug_16_over_08,sk2014_over_2005,percent_white_16_to_8,
                     median_hh_income_16_to_8,pop_2014,drug_16_scaled,alcohol_16_scaled,suicides_16_scaled,
                     percent_hispanic_16_to_8,diversity_idx_2016,percent_black_16_to_8,
                     soc_cap_indx_14,sk05,perc_white,perc_black,perc_hisp,pop_density_2014,
                     median_hh_income,perc_below_poverty)
  
  cor(d,use = "complete.obs")
  library(weights)
  wtd.cor(data$sanders_perc_16, y=data$trump_perc_16, weight=data$pop_2014, mean1=FALSE, collapse=TRUE, bootse=FALSE,
          bootp=FALSE, bootn=1000)
  
  wtd.cor(data$sanders_perc_16_vs_08, y=data$trump_perc_16_vs_08, weight=data$pop_2014, mean1=FALSE, collapse=TRUE, bootse=FALSE,
          bootp=FALSE, bootn=1000)
  
  data$sanders_perc_16,data$trump_perc_16, use="complete.obs"
  saveRDS(s, "C:/Users/robert/Dropbox/Github/Populism_ms/full_pop_data.rds")
  ##### F
  2,649 to Hillary Clinton’s 503
### aggregated bonomila in map2stan