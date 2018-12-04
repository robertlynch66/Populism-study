library(dplyr)
library(openintro)

# read in mortality data
a08<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_alcohol_deaths_2008.csv")
a12<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_alcohol_deaths_2012.csv")
a16<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_alcohol_deaths_2016.csv")
d08<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_drug_od_2008.csv")
d12<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_drug_od_2012.csv")
d16<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_drug_od_2016.csv")
s08<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_suicides_2008.csv")
s12<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_suicides_2012.csv")
s16<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/predicted_suicides_2016.csv")



# remove supefluous columns
a12<-a12 %>% select("county", "state", "final_alcohol_deaths_2012")
a16<-a16 %>% select("county", "state", "final_alcohol_deaths_2016")
d08<-d08 %>% select("county", "state", "final_drug_od_2008")
d12<-d12 %>% select("county", "state", "final_drug_od_2012")
d16<-d16 %>% select("county", "state", "final_drug_od_2016")
s08<-s08 %>% select("county", "state", "final_suicides_2008")
s12<-s12 %>% select("county", "state", "final_suicides_2012")
s16<-s16 %>% select("county", "state", "suicides_2016")

# read in voting data
ed_2016<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/trump_clinton.csv")
ed_2016$state<-abbr2state(ed_2016$state)

ed_2012<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/2012_election_data.csv")
ed_2012$state<-abbr2state(ed_2012$state)

ed_2008<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/2008_election_data.csv")

primary_2016<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/sanders_clinton.csv")
primary_2016$state<-abbr2state(primary_2016$state)


demographics<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/all_county_demographics_2.csv")
demographics$state<-abbr2state(demographics$state)
demographics$county<-as.character(demographics$county)
demographics$county<-gsub(" ", "", demographics$county, fixed = TRUE)
social_cap<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/social_capital.csv")
# remove space before the state#################################
social_cap$state<-gsub(" ", "", social_cap$state, fixed = TRUE)
social_cap$county<-gsub(" ","",social_cap$county, fixed=TRUE)
social_cap$state<-abbr2state(social_cap$state)

social_mob<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data/social_mobility.csv")
social_mob$state<-abbr2state(social_mob$state)

diversity<-read.csv("/Users/robert/Dropbox/Nicolas shared folder/Final Data//diversity_index.csv")
diversity$state<-abbr2state(diversity$state)

# change all counties to characters
ed_2016$county <- as.character(ed_2016$county)
ed_2012$county <- as.character(ed_2012$county)
ed_2008$county <- as.character(ed_2008$county)
ed_2008$state <- as.character(ed_2008$state)
primary_2016$county <- as.character(primary_2016$county)

# select rows for mortality data and change vectors to characters
a08$county <- as.character(a08$county)
a08$state <- as.character(a08$state)

a08 <- a08 %>% select ("county","state","alcohol_deaths_2008.y","final_alcohol_deaths_2008" )



# find duplicates
ed_2008[duplicated(ed_2008[,1:2]),]
ed_2012[duplicated(ed_2012[,1:2]),]
primary_2016[duplicated(primary_2016[,1:2]),]
a08[duplicated(a08[,1:2]),]
a12[duplicated(a12[,1:2]),]
a16[duplicated(a16[,1:2]),]
d08[duplicated(d08[,1:2]),]
d12[duplicated(d12[,1:2]),]
d16[duplicated(d16[,1:2]),]
s16[duplicated(s16[,1:2]),]
s08[duplicated(s08[,1:2]),]
s12[duplicated(s12[,1:2]),]
s16[duplicated(s16[,1:2]),]
demographics[duplicated(demographics[,1:2]),]
diversity[duplicated(diversity[,1:2]),]
social_cap[duplicated(social_cap[,1:2]),]
social_mob[duplicated(social_mob[,1:2]),]

# make complete df
all_data<- ed_2016 %>% left_join (ed_2008, by=c("state", "county"))
all_data<- all_data %>% left_join(ed_2012, by=c("state", "county"))
all_data<-all_data %>% left_join(primary_2016, by=c("state", "county"))

all_data<-left_join(all_data, a08,by=c("state", "county"))
all_data<-left_join(all_data, a12,by=c("state", "county"))
all_data<-left_join(all_data, a16,by=c("state", "county"))
all_data<-left_join(all_data, d08,by=c("state", "county"))
all_data<-left_join(all_data, d12,by=c("state", "county"))
all_data<-left_join(all_data, d16,by=c("state", "county"))
all_data<-left_join(all_data, s08,by=c("state", "county"))
all_data<-left_join(all_data, s12,by=c("state", "county"))
all_data<-left_join(all_data, s16,by=c("state", "county"))



all_data<-left_join(all_data, demographics, by=c("state", "county"))
all_data<-left_join(all_data, social_cap, by=c("state", "county"))
all_data<-left_join(all_data, social_mob, by=c("state", "county"))
all_data<-left_join(all_data, diversity, by=c("state", "county"))
# at end save .rds file in dropbox folder
saveRDS(all_data, "/Users/robert/Dropbox/Nicolas shared folder/Final Data/all_data.rds")
# save as .csv file
write.csv(all_data, file = "/Users/robert/Dropbox/Nicolas shared folder/Final Data/all_data.csv")





















