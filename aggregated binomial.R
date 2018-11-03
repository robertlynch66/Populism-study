library(rethinking)
library(dplyr)
library(lme4)
full_data <- readRDS("full_pop_data.rds")
#OR
data <- readRDS("subset_pop_data.rds")


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
  pop$state_id <- coerce_index(pop$state)
  
 # First just do it with standard glm (aggreegated binomial)
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