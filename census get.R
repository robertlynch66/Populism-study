# get census data again
library(dplyr)
library(tidyverse)

#### Final  models for OSR project
data <- readRDS("full_pop_data.rds")


###############################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
### get the 2016 primary data (all of it)
p <- read.csv("all_primary_data_2016.csv")
p$state <- as.character(p$state)
p$county <- as.character(p$county)
p$candidate <- as.character(p$candidate)
p$state_abbreviation<- NULL
p$party <- as.character(p$party)


# make a df for dems
dems <- p %>% filter(party=="Democrat")
dems$party <-NULL
# make a df for gop
reps <- p %>% filter(party=="Republican")
reps$party<-NULL

library(tidyr)

# do the DNC 2016 primary
dems <- unite_(data=dems, "votes", c("votes","fraction_votes"))

dems <- spread(data=dems, key=candidate, value=votes)

dems <- dems %>%
  separate('Bernie Sanders', c("sanders_votes", "sanders_fraction"), "_") %>%
  separate('Hillary Clinton', c("clinton_primary_votes","clinton_primary_faction"), "_")


# do the GOP but unite the columns first so they go together in spread
reps <- unite_(data=reps,"votes", c("votes","fraction_votes"))
reps<- spread(data=reps, key=candidate, value=votes)
# seperate the variables again
reps <- reps %>% 
  separate('Ben Carson', c("carson_votes", "carson_fraction"), "_") %>%
  separate('Donald Trump', c("trump_primary_votes","trump_primary_fraction"), "_") %>%
  separate('Jeb Bush', c("bush_votes","bush_fraction"), "_") %>%
  separate('Ted Cruz', c("cruz_votes","cruz_fraction"), "_") %>%
  separate('Marco Rubio', c("rubio_votes","rubio_fraction"), "_") %>%
  separate('John Kasich', c("kasich_votes","kasich_fraction"), "_") %>%
  separate('Carly Fiorina', c("fiorina_votes","fiorina_fraction"), "_") %>%
  separate('Chris Christie', c("christie_votes","christie_fraction"), "_") %>%
  separate('Mike Huckabee', c("huckabee_votes","huckabee_fraction"), "_") %>%
  separate('Rand Paul', c("paul_votes","paul_fraction"), "_") %>%
  separate('Rick Santorum', c("santorum_votes","santorum_fraction"), "_") 

reps$trump_primary_votes <- as.numeric(reps$trump_primary_votes)
reps$trump_primary_fraction <- as.numeric(reps$trump_primary_fraction)
reps$cruz_votes <- as.numeric(reps$cruz_votes)
reps$cruz_fraction <- as.numeric(reps$cruz_fraction)
reps<- setNames(reps, tolower(names(reps)))
reps$state <- tolower(reps$state)
reps$county <- tolower(reps$county)

data$county <- gsub(" ","",data$county, fixed=TRUE)
reps$county <- gsub(" ","",reps$county, fixed=TRUE)
reps$county <- gsub("-*", "", reps$county)
data$county <- gsub("-*","",data$county)
big <- data %>% left_join(reps, by=c("state","county"))
# read in the hamner county demographic data
#d <- read.csv ("hamner_county_dem_data.csv")
#library(data.table)
#setnames(d, tolower(names(d)))
#d$county <- d$area_name
#d$county <- tolower(d$county)
#d <- as.data.frame(sapply(d,gsub,pattern=" county",replacement=""))
#d$county <- as.character(d$county)
#d$county <- gsub(" ", "", d$county) 
# make states full
#library(openintro)
#d$state <- abbr2state(d$state_abbreviation)
#d$state <- tolower(d$state)
#d$county <- gsub(" ","",d$county, fixed=TRUE)
#d$county <- gsub("-*","",d$county)
# big <- big %>% left_join(d,  by=c("state","county"))


# link the new datsets to the big data then clean it up
# join data to dems
dems <- setNames(dems, tolower(names(dems)))
dems$state <- tolower(dems$state)
dems$county <- tolower(dems$county)
dems$sanders_votes <- as.numeric(dems$sanders_votes)
dems$sanders_fraction <- as.numeric(dems$sanders_fraction)
dems$clinton_primary_votes <- as.numeric(dems$clinton_primary_votes)
dems$clinton_primary_faction <- as.numeric(dems$clinton_primary_faction)
dems$county <- gsub(" ","",dems$county, fixed=TRUE)
dems$county <- gsub("-*","",dems$county)
big <- big %>% left_join(dems, by=c("state","county"))
saveRDS(big, "big_data.rds")


data <- big_data %>% select (1,2,3,4,9,10,12,14,15,17,18,20,60,121,123,125,127,128,129,131,133,135,137,139,141,146,147,148) %>% 
  filter(state!="alaska")
# make sure data coutny and state are formatted correctly
data$state <- gsub(" ","",data$state, fixed=TRUE)
data$state <- gsub(" ","",data$state, fixed=TRUE)
data$county <- gsub("-*","",data$county)
# remove duplicate columns
data <- data [!duplicated(data[c(1,2)]),]
# reget mccain
elect_2008 <- read_csv("2008_election_data.csv")
elect_2008$county <- tolower(elect_2008$county)
elect_2008$state <- tolower(elect_2008$state)
elect_2008$county <- gsub(" ","",elect_2008$county, fixed=TRUE)
elect_2008$county <- gsub("-*","",elect_2008$county)
elect_2008 <- elect_2008 [!duplicated(elect_2008[c(1,2)]),]


newdata <- data%>% left_join(elect_2008, by=c("county"="county","state"="state"))
### primary data missing some key states like minnesota and kansas - not sure why

# reread in social capital data get rid of spaces in county names
 library(openintro)
sk9 <- read_csv("SK_2009_2005.csv") %>% as.data.frame()
sk9 <- sk9 %>%
  separate(county_state, c("county", "state"), ", ")
sk9$state<-abbr2state(sk9$state)
sk9[290,2]<- "districtofcolumbia"
sk9$county <- tolower(sk9$county)
sk9$state <- tolower(sk9$state)
sk9$county <- gsub(" ","",sk9$county, fixed=TRUE)
sk9$state <- gsub(" ","",sk9$state, fixed=TRUE)
sk9$county <- gsub("-*","",sk9$county)
sk9 <- sk9 [!duplicated(sk9[c(1,2)]),]

newdata <- newdata %>% left_join (sk9, by=c("county"="county","state"="state"))

# read in sk14
sk14 <- read_csv("social_capital.csv") %>% as.data.frame()
sk14 <- sk14 %>% select ("county","state","sk2014")
sk14$state<-abbr2state(sk14$state)
sk14$county <- tolower(sk14$county)
sk14$state <- tolower(sk14$state)
sk14$county <- gsub(" ","",sk14$county, fixed=TRUE)
sk14$state <- gsub(" ","",sk14$state, fixed=TRUE)
sk14$county <- gsub("-*","",sk14$county)
# remove "city" and "parish from county
sk14$county <- gsub("city","",sk14$county)
sk14$county <- gsub("parish","",sk14$county)
sk14 <- sk14 [!duplicated(sk14[c(1,2)]),]

newdata <- newdata %>% left_join (sk14, by=c("county"="county","state"="state"))


## add all the census data inclusing population and pop density

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
# population from the 2010 decennial census to avoid error ascoiated with estimates
county_pop_16 <- get_decennial(geography = "county", 
                           variables = "P001001")

# link pop to data
county_pop_16 <- county_pop_16 %>% select ("NAME", "value") %>% as.data.frame()
county_pop_16<- county_pop_16 %>%
  separate(NAME, c("county", "state"), ", ")
county_pop_16$county <- tolower(county_pop_16$county)
county_pop_16$state <- tolower(county_pop_16$state)
county_pop_16$county <- gsub(" ","",county_pop_16$county, fixed=TRUE)
county_pop_16$state <- gsub(" ","",county_pop_16$state, fixed=TRUE)
county_pop_16$county <- gsub("-*","",county_pop_16$county)
# remove "city" and "parish from county
county_pop_16$county <- gsub("city","",county_pop_16$county)
county_pop_16$county <- gsub("parish","",county_pop_16$county)
county_pop_16$county <- gsub("county","",county_pop_16$county)
county_pop_16 <- county_pop_16 [!duplicated(county_pop_16[c(1,2)]),]
county_pop_16$pop_2010 <- county_pop_16$value
county_pop_16$value <- NULL


newdata <- newdata %>% left_join (county_pop_16, by=c("county"="county","state"="state"))


# add 2010 census variables
# # variable codes - table - I think the part after the underscore is the column number
#employed is number employes who are 16 and older
#columns for native and foreign born are  'native' and 'foreign born'
new_vars <- c(white16 = "B03002_003", 
              hispanic16 = "B03002_012",
              hh_income16="B19013_001",
              bachelors16="B16010_041",
              pop16="B01003_001",
              male_unemployed16="C23002A_008",
              female_unemployed16="C23002B_021",
              native16= "B05012_002",
              foreign_born16="B05012_003")


new_data10 <- get_acs(geography = "county", 
                      variables = new_vars,
                      survey="acs5",
                      year=2010,
                      cache_table = TRUE,
                      keep_geo_vars=TRUE,
                      output="wide") %>% as.data.frame()



new_data16 <- get_acs(geography = "county", 
                      #state = "CA",
                      variables = new_vars,
                      survey="acs5",
                      year=2016,
                      cache_table = TRUE,
                      keep_geo_vars=TRUE,
                      output="wide") %>% as.data.frame()












#ACS vars
v10 <- load_variables(year = 2010,
                      dataset = "acs5",
                      cache = TRUE)

view(v10)

# chooose a variable
## foreign born
#var <- filter(v16, str_detect(name, "B05012"))

# variable codes - table - I think the part after the underscore is the column number
#employed is number employes who are 16 and older
#columns for native and foreign born are  'native' and 'foreign born'
new_vars <- c(white16 = "B03002_003", 
             hispanic16 = "B03002_012",
              hh_income16="B19013_001",
               bachelors16="B16010_041",
              pop16="B01003_001",
              male_unemployed16="C23002A_008",
             female_unemployed16="C23002B_021",
              native16= "B05012_002",
              foreign_born16="B05012_003")


# this defaults to collect data from the 5 year ACS 2012-2016
# Request a summary variable from the ACS

new_data16 <- get_acs(geography = "county", 
                      #state = "CA",
                      variables = new_vars,
                      survey="acs5",
                      year=2016,
                      cache_table = TRUE,
                      keep_geo_vars=TRUE,
                      output="wide") %>% as.data.frame()

new_data10 <- get_acs(geography = "county", 
                      #state = "CA",
                      variables = new_vars,
                      survey="acs5",
                      year=2010,
                      cache_table = TRUE,
                      keep_geo_vars=TRUE,
                      output="wide") %>% as.data.frame()

# read in big_data
big_data <- readRDS("big_data.rds")
 data <- big_data %>% select(1:4,6,9,10,121,123,125,127,128,129,131,133,135,137,139,141,57,98,40,41,60,66,68,69,71,72,65)

 
 

#Clean new_data16 and newdata_10 for joining

new_data16

# join data from data to census data
data2 <- data%>% left_join(data16, by=c("county"="county","state"="state"))
# examples
# years 2010 - 2016 are available, survey= "acs1", "acs3", and "acs5".
get_acs(geography = "county", year=2016, 
        cache_table = TRUE,
                       variables = "B23025_01")





tarrant_county <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)


#### seraching for variables

## to find census variable codes
#https://censusreporter.org/
## ACS 3 million people a year
## searching census data for variables



# this 'acs5' gets the 5 year acs
#acs1 get the 1 year acs data
v16 <- load_variables(year = 2016,
                      dataset = "acs5",
                      cache = TRUE)

v16

# chooose a variable
library(tidyverse)

b <- filter(v16, str_detect(name, "B99151_001"))

b

# anatomy ov a cebsus scs variable
#B19001_002E  - the B means it comes from a base table
#c - collapsed table, s- subject tables, dp - I forget
# 19001 is thta table
# _002 refers to the varibale code within this table
# E retunrs estimate - but is optional fpr tidy census

# make a useful chart of census data in ggplot 2
library(tidycensus)
library(tidyverse)

# Assign Census variables vector to race_vars  these are the race cats - counts
race_vars <- c(White = "B03002_003", Black = "B03002_004",  
               Asian = "B03002_006", Hispanic = "B03002_012")





# transpose rows to columns (i.e. wide format) with 'spread' in tidyr package
# spread takes the key variable which is multiple variables in a row and puts it in columns, the value is all the 
#variable values asscoiate with the key
library(tidyr)

new <- spread(data=race_percentage, key=variable, value=pct)

# wrangle data with tidy census
biggest <- mi_cities %>% group_by(year) %>%
  filter(totalpopE == max(totalpopE)) %>%
  select (NAME,totalpopE, year)

# get changes in ACS data across time using purrr package
library(purrr)
mi_cities <- map_df(2012:2016, function(x) {
  get_acs(geography = "county", output="wide",
          variables = c(totalpop = "B01003_001"), 
          #state = "MI", 
          survey = "acs1", 
          year = x) %>%
    mutate(year = x) %>% as.data.frame()
})


######### THIS IS WHERE YOU ARE!!!!!!!!!
# use tigris to get counties
library(tigris)

all_counties <- counties()
plot(all_counties)
# Tigris is for mapping county borders etc..
#  Data available from ACS in Tigris map files for 1990, 2000, 2010, and 2011-2017
# ggplot hh income
ne_income <- get_acs(geography = "state", 
                     variables = "B19013_001", 
                     survey = "acs1", 
                     state = c("ME", "NH", "VT", "MA", 
                               "RI", "CT", "NY"))

# plot it
ggplot(ne_income, 
       aes(x = estimate, 
           # reorder puts the Y axis names in order of their estimate mean I think
           y = reorder(NAME, estimate))) + 
  geom_point(color = "navy", size = 4) +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 14) +
  labs(x = "2016 ACS estimate", 
       y = "", 
       title = "Median household income by state")
# Obtain and view state median household income from the 2012-2016 American Community Survey
state_income <- get_acs(geography = "state", variables = "B19013_001")

head(state_income)

# this replaces the space and the word county and wyoming with nothing in the variable 'NAME'
NAME = str_replace(NAME, " County, Wyoming", "")

# in  ###########################
### data standardization etc...


# scale social capital index
data$soc_cap_indx_14 <- data$soc_cap_indx_14 -min(data$soc_cap_indx_14,na.rm=T)
data$soc_cap_indx_14 <- data$soc_cap_indx_14/(max(data$soc_cap_indx_14,na.rm=T))
data$sk05 <- data$sk05 - min(data$sk05, na.rm=T)
data$sk05 <- data$sk05/max(data$sk05, na.rm=T)
data$sk2014_over_2005 <- data$soc_cap_indx_14-data$sk05

# log population and median hh income differences
data$pop_change_16_to_8 <- log(data$pop_2014) -log(data$pop_2010)
data$median_hh_income_08 <- data$median_hh_income_16_to_8 + data$median_hh_income
data$median_hh_income_16_to_8 <- log(data$median_hh_income) -log(data$median_hh_income_08)


# add weights
data$total_votes_16_gen <- data$trump_16+data$clinton_16
data$total_votes_16_08_gen <- data$trump_16+data$mccain_08
# for the primaries scale by population not votes
#data$pop_2014
