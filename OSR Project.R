#Predicting supressed rows

#STEPS:
#sum of all populations of counties with supressed rows grouped by state (missing pop)
#divide each missing county population by total missing (ratio)
#add state suicide totals to each county row (create new column)
#sum suicide counts for all complete rows grouped by state 
#subtract complete sum from state total (missing total)
#multiply missing total times ratio (only for missing rows)


#### First impute suicides 2016
#read in files from Nicolas computer
suicide_county_2016<-read.csv("/Users/nick/Dropbox/Nicolas shared folder/Mortality by county/suicide_county_data_2016.csv")
suicide_state_2016<-read.csv("/Users/nick/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2016.csv")

# from U Turku windows desktop
suicide_county_2016<-read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Mortality by county/suicide_county_data_2016.csv")
suicide_state_2016<-read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2016.csv")

# from my laptop
suicide_county_2016<-read.csv("/home/robert/Dropbox/Nicolas shared folder/Mortality by county/suicide_county_data_2016.csv")
suicide_state_2016<-read.csv("/home/robert/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2016.csv")
# new commit



#load package dplyr
library(dplyr)
#install.packages("openintro")
library(openintro)
# complete cases - get rid of NA's in population
complete <- suicide_county_2016[complete.cases(suicide_county_2016), ]

#change 'population' from a factor to a numeric

complete$Population <-as.numeric(complete$Population)
complete$State <- as.character(complete$State)
# convert state to character
suicide_state_2016$State <- as.character(suicide_state_2016$State)
colnames(suicide_state_2016)[2] <- "suicides_state_total"
# filter the missing counties and sum their populations by each state
miss_pop_2016 <- complete %>% filter(Suicides.16<0) 
miss_pop_2016 <- aggregate(miss_pop_2016$Population, by=list(State=miss_pop_2016$State),
                           FUN=sum)
# rename the column
colnames(miss_pop_2016)[2] <- "missing_pop"


#sum suicide counts for all complete rows grouped by state 

complete_suicides <- complete %>% filter(Suicides.16>=0) 
complete_suicides <- aggregate(complete_suicides$Suicides.16, by=list(State=complete_suicides$State), FUN=sum)
# rename the column
colnames(complete_suicides)[2] <- "complete_deaths"


#left_join miss_pop and suicide_county_2016
complete<-left_join(complete,miss_pop_2016,by="State")

#divide each missing county population by total missing (ratio)
proportion <-complete %>% filter(Suicides.16<0) %>% mutate(prop_of_missing=Population/missing_pop)

#convert state abbreviations to full state names in order to left_join with state level data

complete$State<-abbr2state(complete$State)


#left_join suicide_county_2016 and suicide_state_2016
complete<- left_join(complete,suicide_state_2016,by="State")



# spell out state names from proportions and complete suicides tables
proportion$State<-abbr2state(proportion$State)
complete_suicides$State<-abbr2state(complete_suicides$State)

#left_join complete_suicides_2016 and suicide_county_2016
complete<-left_join(complete, complete_suicides, by="State")


#left_join ratio and suicide_county_2016
complete <-left_join(complete, proportion, by=c("County","State"))

# rename columns to sensibe names


#subtract known county deaths from state total deaths to get the missing deaths
complete$missing <- complete$suicides_state_total - complete$complete_deaths

#multiply missing total times ratio (only for missing rows)
complete$predicted_deaths <- complete$missing * complete$prop_of_missing

# replace NA's with zeros for next step to work
complete$Suicides.16.y[is.na(complete$Suicides.16.y)] <- 0
#merged predicted and known deaths into one column
complete$final_suicides_2016<-ifelse(complete$Suicides.16.y == -1, 
                                                complete$predicted_deaths, 
                                                complete$Suicides.16.x)
# rename and delete unnecessary columns

complete<-complete %>% select(County,State,Population.x,Suicides.16.y,final_suicides_2016)

#rename columns
#library(plyr) 

suicides_16 <- complete
names(suicides_16) <- c("county", "state","population","missing","suicides_2016")
# delete old temporary data frames
complete<- NULL
complete_suicides <- NULL
miss_pop_2016 <- NULL
proportion <- NULL
suicide_county_2016 <- NULL
suicide_state_2016 <- NULL

##########################################################################################################################################################################
###########################################################################################################################
#### Next impute suicides 2012
#read in files from Nicolas computer
#suicide_county_2016<-read.csv("/Users/nick/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2016.csv")
#suicide_state_2016<-read.csv("/Users/nick/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2016.csv")

# from U Turku windows desktop
suicide_county_2012<-read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Mortality by county/suicide_county_data_2012.csv")
suicide_state_2012<-read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Mortality by county/suicide_state_data_2012.csv")


#load package dplyr
library(dplyr)
#install.packages("openintro")
library(openintro)
# complete cases - get rid of NA's in population
complete <- suicide_county_2012[complete.cases(suicide_county_2012), ]

#change 'population' from a factor to a numeric

complete$Population <-as.numeric(as.character(complete$Population))
complete$State <- as.character(complete$State)
# convert state to character
suicide_state_2012$State <- as.character(suicide_state_2012$State)
colnames(suicide_state_2012)[2] <- "suicides_state_total"
# filter the missing counties and sum their populations by each state
miss_pop_2012 <- complete %>% filter(Suicides.12<0) 
miss_pop_2012 <- aggregate(miss_pop_2012$Population, by=list(State=miss_pop_2012$State), FUN=sum)
# rename the column
colnames(miss_pop_2012)[2] <- "missing_pop"


#sum suicide counts for all complete rows grouped by state 

complete_suicides <- complete %>% filter(Suicides.12>=0) 
complete_suicides <- aggregate(complete_suicides$Suicides.12, by=list(State=complete_suicides$State), FUN=sum)
# rename the column
colnames(complete_suicides)[2] <- "complete_deaths"


#left_join miss_pop and suicide_county_2016
complete<-left_join(complete,miss_pop_2012,by="State")

#divide each missing county population by total missing (ratio)
proportion <-complete %>% filter(Suicides.12<0) %>% mutate(prop_of_missing=Population/missing_pop)

#convert state abbreviations to full state names in order to left_join with state level data

complete$State<-abbr2state(complete$State)


#left_join suicide_county_2016 and suicide_state_2016
complete<- left_join(complete,suicide_state_2012,by="State")



# spell out state names from proportions and complete suicides tables
proportion$State<-abbr2state(proportion$State)
complete_suicides$State<-abbr2state(complete_suicides$State)

#left_join complete_suicides_2016 and suicide_county_2016
complete<-left_join(complete, complete_suicides, by="State")


#left_join ratio and suicide_county_2016
complete <-left_join(complete, proportion, by=c("County","State"))

# rename columns to sensibe names


#subtract known county deaths from state total deaths to get the missing deaths
complete$missing <- complete$suicides_state_total - complete$complete_deaths

#multiply missing total times ratio (only for missing rows)
complete$predicted_deaths <- complete$missing * complete$prop_of_missing

# replace NA's woith zeros for next step to work
complete$Suicides.12.y[is.na(complete$Suicides.12.y)] <- 0
#merged predicted and known deaths into one column
complete$final_suicides_2012<-ifelse(complete$Suicides.12.y == -1, 
                                     complete$predicted_deaths, 
                                     complete$Suicides.12.x)
# rename and delete unnecessary columns

complete<-complete %>% select(County,State,Population.x,Suicides.12.y,final_suicides_2012)

#rename columns
#library(plyr) 
#library(dplyr)
suicides_12<-rename(complete,c("County"="county","State"="state","Population.x"="population",
                               "Suicides.12.y"="missing", "final_suicides_2012"="suicides_2012"))


# temporary fix - make all negative rows 0

#suicides_12$suicides_2012[suicides_12$suicides_2012<0] <- 0
# delete old temporary data frames
complete<- NULL
complete_suicides <- NULL
miss_pop_2012 <- NULL
proportion <- NULL
suicide_county_2012 <- NULL
suicide_state_2012 <- NULL

########################################################################################
#######################################################################################
### load and fix data frames number 1 through 8
# unique rows for motality datasets
#1
general_election <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Election Data/general_election_data.csv")
#general_election <- read.csv("C:/Users/rofrly/Dropbox/Nicolas shared folder/Election Data/general_election_data.csv")
general_election$county <- as.character(general_election$county)
general_election$state <- abbr2state(general_election$state)
general_election <-general_election[row.names(unique(general_election[,c("county", "state")])),]

#2
obama_romney <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Election Data/Obama_Romney.csv")
obama_romney$county <- as.character(obama_romney$county)
obama_romney$state <- abbr2state(obama_romney$state)
obama_romney$Obama <- obama_romney$Obama/100
obama_romney$Romney <- obama_romney$Romney/100
obama_romney <-obama_romney[row.names(unique(obama_romney[,c("county", "state")])),]

#3
dnc_primary <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Election Data/democratic_primary_data.csv")
dnc_primary$county <- as.character(dnc_primary$county)
dnc_primary$state <- abbr2state(dnc_primary$state)
dnc_primary <-dnc_primary[row.names(unique(dnc_primary[,c("county", "state")])),]

# 4
county_demographics <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/County Demographics/all_county_demographics_2.csv")
county_demographics$state <- abbr2state(county_demographics$state)
county_demographics$county <- as.character(county_demographics$county)
# get rid of the space after the county name
county_demographics$county <- gsub(" ", "", county_demographics$county, fixed = TRUE)
county_demographics <-county_demographics[row.names(unique(county_demographics[,c("county", "state")])),]

#5 nnn
diversity_index <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/County Demographics/diversity_index.csv")
diversity_index$state <- abbr2state(diversity_index$state)
diversity_index$county <- as.character(diversity_index$county)
diversity_index <-diversity_index[row.names(unique(diversity_index[,c("county", "state")])),]

#6
social_mobility <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/County Demographics/social_mobility.csv")
social_mobility$state <- abbr2state(social_mobility$state)
social_mobility$county <- as.character(social_mobility$county)
social_mobility <-social_mobility[row.names(unique(social_mobility[,c("county", "state")])),]

#7
social_capital <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/County Demographics/social_capital.csv")
social_capital$county <- as.character(social_capital$county)
social_capital$county <- gsub(" ", "", social_capital$county, fixed = TRUE)
social_capital$state <- as.character(social_capital$state)
# get rid of the space before the state abbreviation
social_capital$state <- gsub(" ", "", social_capital$state, fixed = TRUE)
social_capital$state <- abbr2state(social_capital$state)
social_capital <-social_capital[row.names(unique(social_capital[,c("county", "state")])),]
#8
gini_index <- read.csv("/Users/rofrly/Dropbox/Nicolas shared folder/Mortality by county/gini_index.csv")

gini_index$county <- as.character(gini_index$county)
gini_index$state <- abbr2state(gini_index$state)
gini_index <-gini_index[row.names(unique(gini_index[,c("county", "state")])),]

#9
suicides_16$county <- as.character(suicides_16$county)
suicides_16 <-suicides_16[row.names(unique(suicides_16[,c("county", "state")])),]

### Get suicides 2012 from nicolas!!!



# then join the tables and run a basic correaltion matrix and dredge model
# Join tables
# 1 and #2

full_data <- general_election %>% left_join (obama_romney, by=c("state"="state","county"="county"))
#3
full_data <- full_data %>% left_join (dnc_primary, by=c("state"="state","county"="county"))
#4
full_data <- full_data %>% left_join (county_demographics, by=c("state"="state","county"="county"))
#5
full_data <- full_data %>% left_join (diversity_index, by=c("state"="state","county"="county"))
#6
full_data <- full_data %>% left_join (social_mobility, by=c("state"="state","county"="county"))
#7
full_data <- full_data %>% left_join (social_capital, by=c("state"="state","county"="county"))
#8
full_data <- full_data %>% left_join (gini_index, by=c("state"="state","county"="county"))
#9
full_data <- full_data %>% left_join (suicides_16, by=c("state"="state","county"="county"))

## need to add rest of mortality data


# per capita suicides
full_data$per_capita_suicides_2016<- full_data$suicides_2016/full_data$population
full_data$trump_over_romney <- full_data$Trump-full_data$Romney
# make DV's (candidates) proportional to the population
#full_data$Trump_votes <- round(full_data$Trump*full_data$population, digits=0)
#full_data$Clinton_votes <- round(full_data$Clinton*full_data$population, digits=0)
#full_data$Romney_votes <- round(full_data$Romney*full_data$Population..2010., digits=0)
#full_data$sanders_votes <- round(full_data$sanders*full_data$population)

# Get the three key DV's now
#full_data$Trump_votes_over_clinton <- round(full_data$Trump_votes-full_data$Clinton_votes, digits=0)
#full_data$Trump_votes_over_romney <- round(full_data$Trump_votes-full_data$Romney_votes, digits=0)
#full_data$Sanders_votes_over_clinton <- round(full_data$sanders_votes-full_data$Clinton_votes, digits =0)
### run some models
#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
test <- full_data %>% select("")
test <- full_data %>% select (-c(county, state))

# variable names
"Clinton"                                                             
 "Trump"                                                               
 "Obama"                                                               
 "Romney"                                                              
"clinton" 
"sanders"                                                             
 "Population..2014."                                                   
 "Population..2010."                                                   
 "X..people.under.18..2014."                                           
 "X..people.over.65..2014."                                            
 "X..female..2014."                                                    
 "X..white..2014."                                                     
 "X..black..2014."                                                     
 "X..asian..2014."                                                     
 "X..hispanic..2014."                                                  
 "X..immigrants..2009.2013."                                           
 "X..high.school.graduate..people.over.25..2009.2013."                 
 "X..bachelors.degree..people.over.25..2009.2013."                     
 "X..of.veterans.2009.2013."                                           
 "mean.travel.time.to.work..mins..2009.2013."                          
 "homeownership.rate..2009.2013."                                      
 "Per.capita.money.income.in.past.12.months..2009.2013."   
 "median.household.income..2009.2013."                                 
 "X..people.below.poverty.line..2009.2013."                            
 "land.area.in.square.miles..2010."                                    
 "population.per.square.mile..2010."                                   
 "Population..per.square.mile.using.2014.population.and.2010.land.area"
 "diversity_index"                                                     
 "Household_income_change_p25"                                         
 "Household_income_change_p75"                                         
 "Religious2014"                                                       
 "Civic2014"                                                           
 "Business2014"                                                        
 "Political2014"                                                       
 "Professional2014"                                                    
 "Labor2014"                                                           
 "Bowling2014"                                                         
 "Recreational2014"                                                    
 "Golf2014"                                                            
"Sports2014"                                                          
 "pop2014"                                                             
 "assn2014"                                                            
 "pvote2012"                                                           
 "respn2010"                                                           
 "nccs2014"                                                            
 "sk2014"                                                              
 "gini_index"                                                          
 "population"                                                          
 "missing"
 "suicides_2016"                                                       
 "trump_over_romney"  
 
 # run models###################################################################################
#  Married and had kids after the war
################## MODEL RANK ##################################################################
options(na.action = "na.fail") 
#model<-glm(HostOut ~ M1F2+RFA96+RFA2006+RelDev+IntRB+ExtRB+Christian+Church+baptize, data=RL_Data, family = binomial)
model<-glm(kids ~  birthyear+ lotta +martta+birthyear*lotta +birthyear*martta+    
             education + agriculture , data=maw,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE) #subset = (!RelDev | !IntRB))# find out from Dylan how to exclude more correlated  variables from being entered into the same model
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")

##########################################################################
full_data$per_capita_suicides_2016 <- full_data$per_capita_suicides_2016*.000000001
trump1<-glm(Trump ~  per_capita_suicides_2016  , data=full_data,
           family = gaussian )
summary(trump1)
clinton_primary<-glm(clinton ~  per_capita_suicides_2016  , data=full_data,
           family = gaussian )
summary(clinton_primary)
clinton_general<-glm(Clinton ~  per_capita_suicides_2016  , data=full_data,
           family = gaussian )
summary(clinton_general)
sanders<-glm(sanders ~  per_capita_suicides_2016  , data=full_data,
           family = gaussian )
summary(sanders)
####Make some plots
library(ggplot2)

full_data$per_capita_suicides<- full_data$per_capita_suicides_2016 *100000

# save RDS file to Nicolas folder
saveRDS(full_data, "C:/Users/rofrly/Dropbox/Nicolas shared folder/populism_data.rds")

# remake dataframe with 'Clinton in primary', 'Clinton in general', 'Trump in general'
#and 'Sanders in primary'
library(dplyr)
library(tidyr)
data <- full_data %>% select ("per_capita_suicides","population","sanders",
                              "clinton","Trump","Clinton")
# tidy data - gather up the columns
data <- gather(data, key = Candidate, value = percent,
       sanders, clinton, Trump, Clinton)


ggplot(data, aes(x=per_capita_suicides, y= percent, color=Candidate))+
  stat_sum(alpha=.5)+ scale_x_continuous(limit=c(0,30)) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels=c(0,.25,.5,0.75,1))+
  geom_smooth(method='lm',formula=y~x) + labs(x = "Suicides per capita", 
                                              y="Percent of vote")+ 
  scale_colour_manual(values=c("#6A5ACD", "#191970", "#FF4500","#800000"), 
                       name="Candidate",
                       breaks=c("clinton", "Clinton", "sanders","Trump"),
                       labels=c("Clinton primary", "Clinton general", "Sanders primary",
                                "Trump general"))