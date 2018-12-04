#### Final  models for OSR project
data <- readRDS("full_pop_data.rds")

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

library(dplyr)
library(emmeans)
library(lme4)
library(betareg)


# Model 1 Trump vs Clinton
#check correls amongst predictors
mod1 <- data %>% select(trump_perc_16, pop_density_2014,drug_16_scaled,alcohol_16_scaled,
                     suicides_16_scaled,perc_bachelors,
                     diversity_idx_2016,soc_cap_indx_14,perc_white,              
                     perc_hisp,pop_density_2014,median_hh_income,
                     total_votes_16_gen,total_votes_16_08_gen,pop_2014)      

summary(mod1)
cor(mod1,use = "complete.obs")


### beta regression with weights using betareg - 4 models each

# model 1 trump percent 16

model_trump_16 = betareg(trump_perc_16 ~ soc_cap_indx_14 +
                           suicides_16_scaled  +drug_16_scaled+
                           log(pop_density_2014)+log(median_hh_income)+perc_white+perc_hisp+diversity_idx_2016,
                         weights=total_votes_16_gen,
                         data = mod1)
options(digits = 6)
options(scipen=999)
summary(model_trump_16)

# model 2 sanders percent 16
mod2 <- data %>% select(sanders_perc_16, pop_density_2014,drug_16_scaled,alcohol_16_scaled,
                     suicides_16_scaled,perc_bachelors,pop_2014,
                     diversity_idx_2016,soc_cap_indx_14,perc_white,              
                     perc_hisp,pop_density_2014,median_hh_income)      

summary(mod2)
cor(mod2,use = "complete.obs")

model_sanders_16 = betareg(sanders_perc_16 ~ soc_cap_indx_14 +
                             suicides_16_scaled  +drug_16_scaled+
                             log(pop_density_2014)+log(median_hh_income)+perc_white+perc_hisp+diversity_idx_2016,
                           weights=pop_2014,
                           data = mod2)

summary(model_sanders_16)

#model 3 mccain vs trump
mod3 <- data %>% select(trump_perc_16_vs_08,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                     suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                     sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                     percent_hispanic_16_to_8,median_hh_income_16_to_8,total_votes_16_08_gen)      

summary(mod3)
cor(mod3,use = "complete.obs")

model_trump_vs_mccain = betareg(trump_perc_16_vs_08 ~ drug_16_over_08+alcohol_16_over_08+
                                  suicides_16_over_08+
                                  sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                  percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                  log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                weights=total_votes_16_08_gen,
                                data = mod3)

summary(model_trump_vs_mccain)

# model 4 sanders vs obama

mod4 <- data %>% select(sanders_perc_16_vs_08,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                     suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                     sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                     percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod4)
cor(mod4,use = "complete.obs")

model_sanders_vs_obama = betareg(sanders_perc_16_vs_08 ~ drug_16_over_08+alcohol_16_over_08+
                                   suicides_16_over_08+
                                   sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                   percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                   log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                 weights=pop_2014,
                                 data = mod4)

summary(model_sanders_vs_obama)


# model longitudnal changes but sanders vs hillary
mod5 <- data %>% select(sanders_perc_16,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                     suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                     sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                     percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod5)
cor(mod5,use = "complete.obs")

model_sanders_vs_clinton_long = betareg(sanders_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                                   suicides_16_over_08+
                                   sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                   percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                   log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                 weights=pop_2014,
                                 data = mod5)

summary(model_sanders_vs_clinton_long)

# model longitudinal changes but trump vs clinton
mod6 <- data %>% select(trump_perc_16,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                     suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                     sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                     percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      

summary(mod6)
trump_clinton <- trump_clinton[complete.cases(trump_clinton),]

cor(mod6,use = "complete.obs")

model_trump_vs_clinton_long = betareg(trump_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                                          suicides_16_over_08+
                                          sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                          percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                          log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                        weights=pop_2014,
                                        data = mod6)

summary(model_trump_vs_clinton_long)

#### make figure for longitudnal predcitors and clinton vs sanders
library(ggplot2)
# plot the points (actual observations), regression line, and confidence interval

# run model

m1 <- betareg(trump_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                suicides_16_over_08+
                sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                percent_hispanic_16_to_8+median_hh_income_16_to_8+
                log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
              weights=pop_2014,
              data = trump_clinton)

summary(m1)
# make a df with the same number fo rows as the model used
# make a new df with the actual values of suicides or a sequence and the mean values of all the other variables

x1 <- data.frame("suicides_16_over_08" = trump_clinton$suicides_16_over_08,
  #"suicides_16_over_08" = seq(from =-36, to =47, length.out =1381),
                "trump_perc_16" = trump_clinton$trump_perc_16,
                "drug_16_over_08" = rep(mean(trump_clinton$drug_16_over_08),1381),
                "alcohol_16_over_08" = rep(mean(trump_clinton$alcohol_16_over_08),1381),
                "sk2014_over_2005" = rep(mean(trump_clinton$sk2014_over_2005),1381),
                "percent_white_16_to_8" = rep(mean(trump_clinton$percent_white_16_to_8),1381),
                "pop_change_16_to_8" = rep(mean(trump_clinton$pop_change_16_to_8),1381),
                "percent_hispanic_16_to_8" = rep(mean(trump_clinton$percent_hispanic_16_to_8),1381),
                "median_hh_income_16_to_8" = rep(mean(trump_clinton$median_hh_income_16_to_8),1381),
                "pop_2014" = trump_clinton$pop_2014,
                "pop_density_2014" = rep(mean(trump_clinton$pop_density_2014),1381),
                "diversity_idx_2016" = rep(mean(trump_clinton$diversity_idx_2016),1381),
                "perc_bachelors" = rep(mean(trump_clinton$perc_bachelors),1381))

 # get the model predictions and variance from your new df (like link in rethinking)
predicted.suicides1 <- cbind(x1[,1],x1[,2],x1[,10],
  predict(m1,newdata=x1,type="response"),
  predict(m1, newdata=x1, type = "variance")
) %>% as.data.frame()
predicted.suicides1$cat <- "suicides"
# name variables
 colnames(predicted.suicides1) <- c("pred_obs","trump_obs" ,"pop", "trump_predicted","variance","cat")
 # scale suicides 'pred_obs' to a value between 0 and 1
 predicted.suicides1$pred_obs = (predicted.suicides1$pred_obs-min(predicted.suicides1$pred_obs))/
   (max(predicted.suicides1$pred_obs)-min(predicted.suicides1$pred_obs))
# make a dataframe predicting social capital change
 x2 <- data.frame("suicides_16_over_08" = rep(mean(trump_clinton$suicides_16_over_08),1381),
                  #"suicides_16_over_08" = seq(from =-36, to =47, length.out =1381),
                  "trump_perc_16" = trump_clinton$trump_perc_16,
                  "drug_16_over_08" = rep(mean(trump_clinton$drug_16_over_08),1381),
                  "alcohol_16_over_08" = rep(mean(trump_clinton$alcohol_16_over_08),1381),
                  "sk2014_over_2005" = trump_clinton$sk2014_over_2005,
                  "percent_white_16_to_8" = rep(mean(trump_clinton$percent_white_16_to_8),1381),
                  "pop_change_16_to_8" = rep(mean(trump_clinton$pop_change_16_to_8),1381),
                  "percent_hispanic_16_to_8" = rep(mean(trump_clinton$percent_hispanic_16_to_8),1381),
                  "median_hh_income_16_to_8" = rep(mean(trump_clinton$median_hh_income_16_to_8),1381),
                  "pop_2014" = trump_clinton$pop_2014,
                  "pop_density_2014" = rep(mean(trump_clinton$pop_density_2014),1381),
                  "diversity_idx_2016" = rep(mean(trump_clinton$diversity_idx_2016),1381),
                  "perc_bachelors" = rep(mean(trump_clinton$perc_bachelors),1381))
 
 # get the model predictions and variance from your new df (like link in rethinking)
 predicted.suicides2 <- cbind(x2[,5],x2[,2],x2[,10],
                              predict(m1,newdata=x2,type="response"),
                              predict(m1, newdata=x2, type = "variance")
 ) %>% as.data.frame()
 predicted.suicides2$cat <- "social capital"
 # name variables
 colnames(predicted.suicides2) <- c("pred_obs","trump_obs" ,"pop", "trump_predicted","variance","cat")
 #predicted.suicides2$pred_obs <-scalar1(predicted.suicides2$pred_obs)
 predicted.suicides2$pred_obs = (predicted.suicides2$pred_obs-min(predicted.suicides2$pred_obs))/
   (max(predicted.suicides2$pred_obs)-min(predicted.suicides2$pred_obs))
 # row bind the 2 df's
 d <- rbind(predicted.suicides1,predicted.suicides2)
 
 
#### make figure 3 for longitudnal predictors and trump vs clinton
 

cols <-c("#0066CC", "#FF9900")
 p <- ggplot(d, aes(pred_obs,trump_obs,colour = cat))#, size=pop
 p <- p + geom_point(aes(size = pop))+
   scale_size(name   = "County population",
              breaks = c(10000, 100000, 250000, 500000,1000000,2000000),
              labels = c("10k", "100k", "250k", "500k", "1 mil","2 mil"))
 p <- p + geom_line(aes(pred_obs, trump_predicted))
 p <- p + geom_ribbon(aes(ymin=trump_predicted-variance,ymax=trump_predicted+variance), alpha=0.3)+
   scale_color_manual(values=cols) +
   #theme_Publication()+
   scale_y_continuous(name="Percentage voting for\nTrump vs Clinton in 2016",breaks=c(0.25,0.5,0.75),
                      labels=c('25%','50%','75%'))+
   scale_colour_manual(
     name="Key",
     values = cols,
     breaks = c("suicides", "social capital","pop"),
     labels = c("Suicides", "Social Capital","Population")
   )+
   
   scale_x_continuous(name="Change in social capital and per capita suicides\nbetween 2008 and 2016 (standardized)",
                      breaks=c(0.00,1.00),labels=c("Maximum decrease","Maximum increase"))+

  
   ggtitle("A decline in social capital and an increase in per capita suicides between\n2008 and 2016 predicts support for Trump")+  
   

 
 # Label appearance
   theme_bw()+
theme(legend.text = element_text(size = 8, face = "bold"))+
     theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
     theme(axis.title.x =element_text (size=8,face="bold"))+
      theme(axis.title.y = element_text (size=8,face="bold"))+
      theme(legend.title = element_text(size=9,face = "bold"))+
   
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      
      p
      
      
      
      
      
 ggsave(p, filename = "Figure 3.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")

 
 ### make figure 4 change in suicides and social capita vs support for sanders in 2016
 library(ggplot2)
 # plot the points (actual observations), regression line, and confidence interval
 
 # run model
 mod5 <- data %>% select(sanders_perc_16,drug_16_over_08,alcohol_16_over_08,pop_density_2014,
                         suicides_16_over_08,diversity_idx_2016,perc_bachelors,
                         sk2014_over_2005,percent_white_16_to_8, pop_change_16_to_8,             
                         percent_hispanic_16_to_8,median_hh_income_16_to_8,pop_2014)      
 
 summary(mod5)
 cor(mod5,use = "complete.obs")
 mod5 <- mod5[complete.cases(mod5),]
m2 <- betareg(sanders_perc_16 ~ drug_16_over_08+alcohol_16_over_08+
                                           suicides_16_over_08+
                                           sk2014_over_2005+percent_white_16_to_8+ pop_change_16_to_8+             
                                           percent_hispanic_16_to_8+median_hh_income_16_to_8+
                                           log(pop_density_2014)+diversity_idx_2016+perc_bachelors,
                                         weights=pop_2014,
                                         data = mod5)
 
 summary(m2)
 
# mod5 has 1298 cases
 
 # make a df with the same number fo rows as the model used
 # make a new df with the actual values of suicides or a sequence and the mean values of all the other variables
 
 x2 <- data.frame("suicides_16_over_08" = mod5$suicides_16_over_08,
                  #"suicides_16_over_08" = seq(from =-36, to =47, length.out =1381),
                  "sanders_perc_16" = mod5$sanders_perc_16,
                  "drug_16_over_08" = rep(mean(mod5$drug_16_over_08),1298),
                  "alcohol_16_over_08" = rep(mean(mod5$alcohol_16_over_08),1298),
                  "sk2014_over_2005" = rep(mean(mod5$sk2014_over_2005),1298),
                  "percent_white_16_to_8" = rep(mean(mod5$percent_white_16_to_8),1298),
                  "pop_change_16_to_8" = rep(mean(mod5$pop_change_16_to_8),1298),
                  "percent_hispanic_16_to_8" = rep(mean(mod5$percent_hispanic_16_to_8),1298),
                  "median_hh_income_16_to_8" = rep(mean(mod5$median_hh_income_16_to_8),1298),
                  "pop_2014" = mod5$pop_2014,
                  "pop_density_2014" = rep(mean(mod5$pop_density_2014),1298),
                  "diversity_idx_2016" = rep(mean(mod5$diversity_idx_2016),1298),
                  "perc_bachelors" = rep(mean(mod5$perc_bachelors),1298))
 
 # get the model predictions and variance from your new df (like link in rethinking)
 predicted.suicides1 <- cbind(x2[,1],x2[,2],x2[,10],
                              predict(m2,newdata=x2,type="response"),
                              predict(m2, newdata=x2, type = "variance")
 ) %>% as.data.frame()
 predicted.suicides1$cat <- "suicides"
 # name variables
 colnames(predicted.suicides1) <- c("pred_obs","sanders_obs" ,"pop", "sanders_predicted","variance","cat")
 # scale suicides 'pred_obs' to a value between 0 and 1
 predicted.suicides1$pred_obs = (predicted.suicides1$pred_obs-min(predicted.suicides1$pred_obs))/
   (max(predicted.suicides1$pred_obs)-min(predicted.suicides1$pred_obs))
 # make a dataframe predicting social capital change
 x3 <- data.frame("suicides_16_over_08" = rep(mean(mod5$suicides_16_over_08),1298),
                  #"suicides_16_over_08" = seq(from =-36, to =47, length.out =1381),
                  "sanders_perc_16" = mod5$sanders_perc_16,
                  "drug_16_over_08" = rep(mean(mod5$drug_16_over_08),1298),
                  "alcohol_16_over_08" = rep(mean(mod5$alcohol_16_over_08),1298),
                  "sk2014_over_2005" = mod5$sk2014_over_2005,
                  "percent_white_16_to_8" = rep(mean(mod5$percent_white_16_to_8),1298),
                  "pop_change_16_to_8" = rep(mean(mod5$pop_change_16_to_8),1298),
                  "percent_hispanic_16_to_8" = rep(mean(mod5$percent_hispanic_16_to_8),1298),
                  "median_hh_income_16_to_8" = rep(mean(mod5$median_hh_income_16_to_8),1298),
                  "pop_2014" = mod5$pop_2014,
                  "pop_density_2014" = rep(mean(mod5$pop_density_2014),1298),
                  "diversity_idx_2016" = rep(mean(mod5$diversity_idx_2016),1298),
                  "perc_bachelors" = rep(mean(mod5$perc_bachelors),1298))
 
 # get the model predictions and variance from your new df (like link in rethinking)
 predicted.suicides2 <- cbind(x3[,5],x3[,2],x3[,10],
                              predict(m2,newdata=x3,type="response"),
                              predict(m2, newdata=x3, type = "variance")
 ) %>% as.data.frame()
 predicted.suicides2$cat <- "social capital"
 # name variables
 colnames(predicted.suicides2) <- c("pred_obs","sanders_obs" ,"pop", "sanders_predicted","variance","cat")
 #predicted.suicides2$pred_obs <-scalar1(predicted.suicides2$pred_obs)
 predicted.suicides2$pred_obs = (predicted.suicides2$pred_obs-min(predicted.suicides2$pred_obs))/
   (max(predicted.suicides2$pred_obs)-min(predicted.suicides2$pred_obs))
 # row bind the 2 df's
 d2 <- rbind(predicted.suicides1,predicted.suicides2)
 
 
 #### make figure 3 for longitudnal predictors and trump vs clinton
 
 
 cols <-c("#0066CC", "#FF9900")
 p2 <- ggplot(d2, aes(pred_obs,sanders_obs,colour = cat))#, size=pop
 p2 <- p2 + geom_point(aes(size = pop))+
   scale_size(name   = "County population",
              breaks = c(10000, 100000, 250000, 500000,1000000,2000000),
              labels = c("10k", "100k", "250k", "500k", "1 mil","2 mil"))
 p2 <- p2 + geom_line(aes(pred_obs, sanders_predicted))
 p2 <- p2 + geom_ribbon(aes(ymin=sanders_predicted-variance,ymax=sanders_predicted+variance), alpha=0.3)+
   scale_color_manual(values=cols) +
   #theme_Publication()+
   scale_y_continuous(name="Percentage voting for\nSanders vs Clinton in 2016 Primary",breaks=c(0.25,0.5,0.75),
                      labels=c('25%','50%','75%'))+
   scale_colour_manual(
     name="Key",
     values = cols,
     breaks = c("suicides", "social capital","pop"),
     labels = c("Suicides", "Social Capital","Population")
   )+
   
   scale_x_continuous(name="Change in social capital and per capita suicides\nbetween 2008 and 2016 (standardized)",
                      breaks=c(0.00,1.00),labels=c("Maximum decrease","Maximum increase"))+
   
   
   ggtitle("A decline in social capital and an increase in per capita suicides between\n2008 and 2016 predicts support for Sanders")+  
   
   
   
   # Label appearance
   theme_bw()+
   theme(legend.text = element_text(size = 8, face = "bold"))+
   theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
   theme(axis.title.x =element_text (size=8,face="bold"))+
   theme(axis.title.y = element_text (size=8,face="bold"))+
   theme(legend.title = element_text(size=9,face = "bold"))+
   
   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
 
 p2
 
 
 
 
 
 ggsave(p2, filename = "Figure 4.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
 
################################################
 #################################################
 ##################################################
 ### maps
 #### Final  models for OSR project
 data <- readRDS("full_pop_data.rds")
 
 library(choroplethr)
 library(choroplethrMaps)
 library(tidycensus)
 library(tigris)
 library(maps)
 data$percent_trump <- data$trump_16/(data$trump_16+data$clinton_16)
 
 data$value <- data$percent_trump*100
 #remove duplicate fips in region variable
 
 data <- subset(data, !duplicated(data[,114]) )
 
 
 
 
 
 trump16 <- county_choropleth(data, 
                              title      = "  US General Election Results 2016", 
                              legend     = "Percent Trump", 
                              num_colors = 0, 
                              state_zoom = c("arizona","alabama", "arkansas","california","colorado","connecticut","delaware",
                                             "florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland",
                                             "massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire",
                                             "new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
                                             "rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington",
                                             "west virginia","wisconsin","wyoming")) 
 
 
 
 
 data$percent_sanders <- data$sanders_primary/(data$sanders_primary+data$clinton_primary)
 data$value <- data$percent_sanders*100
 
 data$value <- ifelse(data$state=='kansas'|data$state=='maine'| data$state=='minnesota'|data$state=='north dakota',
                      data$percent_trump*100,data$value)
 sanders16 <- county_choropleth(data, 
                                title      = "     Democratic Primary Results 2016", 
                                legend     = "Percent Sanders", 
                                num_colors = 0, 
                                state_zoom = c("arizona","alabama", "arkansas","california","colorado","connecticut","delaware",
                                               "florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland",
                                               "massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire",
                                               "new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
                                               "rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington",
                                               "west virginia","wisconsin","wyoming")) 
 
 
 ### put trump and sanders map next to each other
 library(gridExtra)
 
 # Graph the two maps (democratic_map and white_map) from the previous exercises side-by-side
 emaps <- grid.arrange(trump16, sanders16)
 ggsave(emaps, filename = "Figure 1.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
 
 
 
 ## make suicides map and social capital maps
 #social capital
 data$sk_index <- data$sk2014_over_2005-min(data$sk2014_over_2005, na.rm=T)
 data$sk_index <- data$sk_index/max(data$sk_index,na.rm=T)  
 data$sk_index <- data$sk_index*100  
 data$value <- (data$sk_index-50)*2
 
 data$value <- ifelse(data$state=='louisiana',
                      12,data$value)
 sk_map <- county_choropleth(data,
                             title = "Change in Social Capital between 2008 and 2016",
                             legend = "Change in\nsocial capital\n%",
                             num_colors = 0,
                             state_zoom = c("arizona","alabama", "arkansas","california","colorado","connecticut","delaware",
                                            "florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland",
                                            "massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire",
                                            "new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
                                            "rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington",
                                            "west virginia","wisconsin","wyoming")) 
 
 
 # suicides
 data$value <- data$suicides_16_over_08
 data$value <- ifelse(data$state=='louisiana',
                      38,data$value)
 suicides <- county_choropleth(data,
                               title = "Change in Suicides per 100,000 individuals\nbetween 2008 and 2016",
                               legend = "Change in\nper capita\nSuicides",
                               num_colors = 0,
                               state_zoom = c("arizona","alabama", "arkansas","california","colorado","connecticut","delaware",
                                              "florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland",
                                              "massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire",
                                              "new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
                                              "rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington",
                                              "west virginia","wisconsin","wyoming")) 
 
 bmaps <- grid.arrange(sk_map, suicides)
 ggsave(bmaps, filename = "Figure 2.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
 
 library(wCorr)
 weightedCorr(data$sk2014_over_2005,data$suicides_16_over_08,weights=data$pop_2014, na.rm=T)
 wtd.cors(data$sk2014_over_2005,data$suicides_16_over_08,weight=data$pop_2014)
 