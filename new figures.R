### new bayesian plots for populism paper
# read in the data
p<- readRDS("../data files/populism_data_new.rds")
p$total_votes <- p$trump_16+p$clinton_16



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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


# fixed rates and percentage 2016
p$sk2014 <- p$sk14
p$pop_16 <- p$pop16E
p$log_pop_16 <- log(p$pop16E)
p$median_hh_income_16<- log(p$hh_income16E)
p$white_16 <- p$white16E/p$pop16E

p$hispanic_16 <- p$hispanic16E/p$pop16E
p$bachelors_16 <- p$bachelors16E/p$pop16E
p$male_unemplmt_16 <- p$male_unemployed16E/p$pop16E
p$female_unemplmt_16 <- p$female_unemployed16E/p$pop16E
p$for_born_16 <- p$foreign_born16E/p$pop16E
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","trump_16","clinton_16","total_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16","log_pop_16","pop_16")
p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$total_votes <- as.integer(p$total_votes)
# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_full_rstanarm.rds")
M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
 # get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
                   value = "mort",alcohol_16_to_8:suicides_16_to_8)


get_variables(M1)
z <- tidy(M1)


##model_intercept
model_intercept <-sum(p1$trump_16)/sum(p1$total_votes)
model_se <- z$std.error[1]
#get_slopes
s_slope <- z$estimate[11]*10
a_slope <- z$estimate[9]*10
d_slope <- z$estimate[10]*10
s_se <-z$std.error[11]
a_se <-z$std.error[9]
d_se <-z$std.error[10]
#suicides_slope <- coef_hrc_trump$estimate[11]*10
# suicide draws
s_draws <- spread_draws(M1, `(Intercept)`, suicides_change)
s_draws$mort_change <- s_draws$suicides_change*10
s_draws$suicides_change <- NULL
s_draws <- as.data.frame(s_draws)
s_draws$mort_cat <- "suicides_16_to_8"
# drug draws
d_draws <- spread_draws(M1, `(Intercept)`, drugs_change)
d_draws$mort_change <- d_draws$drugs_change*10
d_draws$drugs_change <- NULL
d_draws <- as.data.frame(d_draws)
d_draws$mort_cat <- "drugs_16_to_8"
#alcohol draws
a_draws <- spread_draws(M1, `(Intercept)`, alcohol_change)
a_draws$mort_change <- a_draws$alcohol_change*10
a_draws$alcohol_change <- NULL
a_draws <- as.data.frame(a_draws)
a_draws$mort_cat <- "alcohol_16_to_8"

# make the posterior draws df
newdf <- rbind(a_draws,d_draws,s_draws)

# join p1 to slope and intrecept data
p1$slope <- ifelse(p1$mort_cat=="alcohol_16_to_8",a_slope,ifelse(p1$mort_cat=="drugs_16_to_8",d_slope,
                                                                 ifelse(p1$mort_cat=="suicides_16_to_8",s_slope, NA)))
p1$se <- ifelse(p1$mort_cat=="alcohol_16_to_8",a_se,ifelse(p1$mort_cat=="drugs_16_to_8",d_se,
                                                                 ifelse(p1$mort_cat=="suicides_16_to_8",s_se, NA)))
p1$intercept <- model_intercept
p1$mortlb = p1$slope -p1$se
p1$mortub = p1$slope +p1$se

# get the county predictions
library(rstanarm)

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_votes", "county_id")
p3 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# get the correct intercept
newdf$intercept<- sum(p1$trump_16)/sum(p1$total_votes)-exp(newdf$`(Intercept)`)
# jitter the intercepts so ablines don't overlap completely
newdf$intercept <- ifelse(newdf$mort_cat == "drugs_16_to_8" , newdf$intercept+0.01, 
                      ifelse(newdf$mort_cat == "alcohol_16_to_8" , newdf$intercept-0.01,newdf$intercept))
                            
# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c(0.4880792, 0.4880792, 0.4880792) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf2 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
# use p3 for predicted values

  
######################################################
####### facet grid plot ###### Looks better I think
# make facet group a factor
p3$mort_cat<- as.factor(p3$mort_cat)
# label facets
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot1 <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 500000,1000000),
             labels = c("10k", "100k",  "500k", "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.3,option="magma",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  
  
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-100,100),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  
  ggtitle("An increase in per capita deaths from suicides, 
          alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

facet_plot1
ggsave(facet_plot1, filename = "Figure 1.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
############################################
############################################
# alternative using 'predictions' hoding all other variables constant model' instead of draws
suicides_seq <- seq (from = -2000L, to = 2000L, by=100)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
ps <- data.frame(
  trump_votes = rep(0,41),
  total_votes_16 = rep(100,41), # total votes here?
  sk_change=rep(mean(p$sk2014_over_2005),41),
  pop_change=rep(mean(p$pop_change_16_to_10),41),
  median_hh_income_change = rep(mean(p$median_hh_income_16_to_10),41),
  perc_bachelors_change = rep(mean(p$bachelors_16_to_10),41),
  male_unemplmt_change = rep(mean(p$male_unemplmt_16_to_10),41),
  female_unemplmt_change = rep(mean(p$female_unemplmt_16_to_10),41),
  for_born_change = rep(mean(p$for_born_16_to_10),41),
  alcohol_change = rep(mean(p$alcohol_16_to_8),41),
  drugs_change = rep(mean(p$drugs_16_to_8),41),
  suicides_change = suicides_seq,
  sk2014 = rep(mean(p$sk2014),41),
  diversity16 = rep(mean(p$diversity16),41),
  pop16 = rep(mean(log(p$pop_16)),41),
  median_hh_income16=rep(mean(p$median_hh_income_16),41),
  white16=rep(mean(p$white_16),41),
  bachelors16=rep(mean(p$bachelors_16),41),
  male_unemplmt16=rep(mean(p$male_unemplmt_16),41),
  female_unemplmt16=rep(mean(p$female_unemplmt_16),41))%>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- d_pi[1,]
s_upper <- d_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_trump_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
# then drugs
drugs_seq <- seq (from = -2000L, to = 2000L, by=100)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pd <- data.frame(
  trump_votes = rep(0,41),
  total_votes_16 = rep(100,41), # total votes here?
  sk_change=rep(mean(p$sk2014_over_2005),41),
  pop_change=rep(mean(p$pop_change_16_to_10),41),
  median_hh_income_change = rep(mean(p$median_hh_income_16_to_10),41),
  perc_bachelors_change = rep(mean(p$bachelors_16_to_10),41),
  male_unemplmt_change = rep(mean(p$male_unemplmt_16_to_10),41),
  female_unemplmt_change = rep(mean(p$female_unemplmt_16_to_10),41),
  for_born_change = rep(mean(p$for_born_16_to_10),41),
  alcohol_change = rep(mean(p$alcohol_16_to_8),41),
  drugs_change = drugs_seq,
  suicides_change = rep(mean(p$suicides_16_to_8),41),
  sk2014 = rep(mean(p$sk2014),41),
  diversity16 = rep(mean(p$diversity16),41),
  pop16 = rep(mean(log(p$pop_16)),41),
  median_hh_income16=rep(mean(p$median_hh_income_16),41),
  white16=rep(mean(p$white_16),41),
  bachelors16=rep(mean(p$bachelors_16),41),
  male_unemplmt16=rep(mean(p$male_unemplmt_16),41),
  female_unemplmt16=rep(mean(p$female_unemplmt_16),41))%>% as.data.frame()

d <-posterior_predict(M1, newdata=pd,re.form=~0) 
d <- as.data.frame(d)
d_mu <- apply(d,2,mean) %>% as.data.frame()
d_pi <- apply(as.matrix(d), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
d_lower <- d_pi[1,]
d_upper <- d_pi[2,]
d_mu$drugs <- rownames(d_mu)
names(d_mu) <- c("predicted_trump_percent_mean", "mort")
d_mu$lower <- d_lower
d_mu$upper <- d_upper
d_mu$mort_cat <- "drugs_16_to_8"
# then alcohol
alcohol_seq <- seq (from = -2000L, to = 2000L, by=100)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pa <- data.frame(
  trump_votes = rep(0,41),
  total_votes_16 = rep(100,41), # total votes here?
  sk_change=rep(mean(p$sk2014_over_2005),41),
  pop_change=rep(mean(p$pop_change_16_to_10),41),
  median_hh_income_change = rep(mean(p$median_hh_income_16_to_10),41),
  perc_bachelors_change = rep(mean(p$bachelors_16_to_10),41),
  male_unemplmt_change = rep(mean(p$male_unemplmt_16_to_10),41),
  female_unemplmt_change = rep(mean(p$female_unemplmt_16_to_10),41),
  for_born_change = rep(mean(p$for_born_16_to_10),41),
  alcohol_change = alcohol_seq,
  drugs_change = rep(mean(p$drugs_16_to_8),41),
  suicides_change = rep(mean(p$suicides_16_to_8),41),
  sk2014 = rep(mean(p$sk2014),41),
  diversity16 = rep(mean(p$diversity16),41),
  pop16 = rep(mean(log(p$pop_16)),41),
  median_hh_income16=rep(mean(p$median_hh_income_16),41),
  white16=rep(mean(p$white_16),41),
  bachelors16=rep(mean(p$bachelors_16),41),
  male_unemplmt16=rep(mean(p$male_unemplmt_16),41),
  female_unemplmt16=rep(mean(p$female_unemplmt_16),41))%>% as.data.frame()

a <-posterior_predict(M1, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$drugs <- rownames(a_mu)
names(a_mu) <- c("predicted_trump_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu)
pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),3)
pred_df$trump_percent <- (pred_df$predicted_trump_percent_mean)/100 +0.12
pred_df$lower <- (pred_df$lower)/100 + 0.12
pred_df$upper <- (pred_df$upper)/100 + 0.12
# replot with the predicted df rather than the draws 
p3$mort_cat<- as.factor(p3$mort_cat)
p3$trump_percent <- p3$predicted_trump_votes/p3$total_votes
# label facets
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot1a <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 500000,1000000),
             labels = c("10k", "100k",  "500k", "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, alpha = 0.1),show.legend=FALSE)+

  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, 
          #                        colour=mort_cat),size=0.2,alpha=0.8)+
  geom_line(data = pred_df, mapping=aes(y = trump_percent, x = mort2, colour=mort_cat),
            size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+


  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",
                     breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-200,200),
                     breaks=c(-150,-75,0,75,150),labels=c("-150","-75","0","75","150"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
     #     alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

facet_plot1a
ggsave(facet_plot1a, filename = "Figure 1.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
 
#########################
#########################
#########################Read in Sanders model

library(dplyr)
library(rstanarm)
# path to the folder with the R data files
# p<- readRDS("../data files/populism_data_new.rds")

p<- readRDS("../data files/populism_data_new.rds")
# get DV (n) total votes in general
p$total_votes_16 <- p$sanders_primary1+p$clinton_primary1 

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
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4


# fixed rates and percentage 2016
p$sk2014 <- p$sk14
p$pop_16 <- p$pop16E
p$log_pop_16 <- log(p$pop16E)
p$median_hh_income_16<- log(p$hh_income16E)
p$white_16 <- p$white16E/p$pop16E

p$hispanic_16 <- p$hispanic16E/p$pop16E
p$bachelors_16 <- p$bachelors16E/p$pop16E
p$male_unemplmt_16 <- p$male_unemployed16E/p$pop16E
p$female_unemplmt_16 <- p$female_unemployed16E/p$pop16E
p$for_born_16 <- p$foreign_born16E/p$pop16E
p$alcohol16 <- (p$alcohol_16/p$pop16E)*10e4
p$drugs16 <- (p$drug_16/p$pop16E)*10e4
p$suicides16 <- (p$suicides_16/p$pop16E)*10e4
p$diversity16 <- p$diversity_idx_2016 # goes from 0 to 1


p <- p %>% select ("county","state","sanders_primary1","clinton_primary1",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","log_pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16")
p <- p[complete.cases(p), ]

library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_primary1<- as.integer(p$sanders_primary1)
p$clinton_primary1 <- as.integer(p$clinton_primary1)

# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(rstanarm)
# read in the rstanarm object
M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_full_rstanarm3.rds")
summary(M2, digits=6)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",alcohol_16_to_8:suicides_16_to_8)

##
get_variables(M2)
z <- tidy(M2)

##model_intercept
model_intercept <-sum(p1$sanders_primary1)/sum(p1$clinton_primary1+p1$sanders_primary1)
model_se <- z$std.error[1]
#get_slopes
s_slope <- z$estimate[11]
a_slope <- z$estimate[9]
d_slope <- z$estimate[10]
s_se <-z$std.error[11]
a_se <-z$std.error[9]
d_se <-z$std.error[10]

# suicide draws
s_draws <- spread_draws(M2, `(Intercept)`, suicides_change)
s_draws$mort_change <- s_draws$suicides_change
s_draws$suicides_change <- NULL
s_draws <- as.data.frame(s_draws)
s_draws$mort_cat <- "suicides_16_to_8"
# drug draws
d_draws <- spread_draws(M2, `(Intercept)`, drugs_change)
d_draws$mort_change <- d_draws$drugs_change
d_draws$drugs_change <- NULL
d_draws <- as.data.frame(d_draws)
d_draws$mort_cat <- "drugs_16_to_8"
#alcohol draws
a_draws <- spread_draws(M2, `(Intercept)`, alcohol_change)
a_draws$mort_change <- a_draws$alcohol_change
a_draws$alcohol_change <- NULL
a_draws <- as.data.frame(a_draws)
a_draws$mort_cat <- "alcohol_16_to_8"

# make the posterior draws df
newdf3 <- rbind(a_draws,d_draws,s_draws)

# join p1 to slope and intrecept data
p1$slope <- ifelse(p1$mort_cat=="alcohol_16_to_8",a_slope,ifelse(p1$mort_cat=="drugs_16_to_8",d_slope,
                                                                 ifelse(p1$mort_cat=="suicides_16_to_8",s_slope, NA)))
p1$se <- ifelse(p1$mort_cat=="alcohol_16_to_8",a_se,ifelse(p1$mort_cat=="drugs_16_to_8",d_se,
                                                           ifelse(p1$mort_cat=="suicides_16_to_8",s_se, NA)))
p1$intercept <- model_intercept
p1$mortlb = p1$slope -p1$se
p1$mortub = p1$slope +p1$se

# get the county predictions
library(rstanarm)

z <-posterior_predict(M2) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_sanders_votes", "county_id")
p4 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# get the correct intercept
newdf3$intercept<- exp(newdf3$`(Intercept)`)
##model_intercept
newdf3$intercept2 <-sum(p1$sanders_primary1)/sum(p1$clinton_primary1+p1$sanders_primary1)
# jitter the intercepts so ablines don't overlap completely


# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c( 39.00712,  39.00712,  39.00712) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf4 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
# use p3 for predicted values

######################################################
####### facet grid plot ###### Looks better I think
# make facet group a factor
p4$mort_cat<- as.factor(p4$mort_cat)
# label facets
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot2 <- ggplot(p4, aes(x=mort,y=predicted_sanders_votes/(sanders_primary1+clinton_primary1), colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 500000,1000000),
             labels = c("10k", "100k",  "500k", "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf3, mapping=aes(intercept = intercept2, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_abline(data = newdf4, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.3,option="magma",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  
  
  scale_y_continuous(name="Percentage of votes for\nSanders vs Clinton in 2016 Primary",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-100,100),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  
  ggtitle("An increase in per capita deaths from suicides and 
          alcohol\nbetween 2008 and 2016 predicts support for Sanders") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

facet_plot2
ggsave(facet_plot2, filename = "Figure 2.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")






  ##########################Extra shit#################################
  ##########################
  ##########################
tidy(M1)
posterior_interval(M1)
posterior_interval(M1, prob = 0.5)
posterior_interval(M1, prob = 0.95)
#age_seq <- seq (from = 0, to = 1, length.out=32)

p6 <- gather(p, key = "suicides", value = "predict")
posterior <- spread_draws(M1, suicides_change)


posterior$suicides_change <- posterior$suicides_change*10
# doesnt work
#mean(between(posterior, 0.00013, 0.00015))

# Visualizing a Bayesian model
library(broom)
tidy(M1)
# extract the coeffcients from the model

#coef_hrc_trump <- tidy(M1)
#coef_hrc_trump
# rescale estimates and stand errors

#model_intercept <- coef_hrc_trump$estimate[1]
#model_intercept

#suicides_slope <- coef_hrc_trump$estimate[11]*10
# make predictions for new data 
# trump votes and total votes need to be specified as number of trials
suicides_seq <- seq (from = -100000, to = 100000, by=1000)
library(dplyr)
attach(p)
predictions <- tidyr::crossing(
  sanders_votes = 0,
  clinton_votes = 100,
  sk_change=mean(sk2014_over_2005),
  pop_change=mean(pop_change_16_to_10),
  median_hh_income_change = mean(median_hh_income_16_to_10),
  perc_bachelors_change = mean(bachelors_16_to_10),
  male_unemplmt_change = mean(male_unemplmt_16_to_10),
  female_unemplmt_change = mean(female_unemplmt_16_to_10),
  for_born_change = mean(for_born_16_to_10),
  alcohol_change = mean(alcohol_16_to_8),
  drugs_change = mean(drugs_16_to_8),
  suicides_change = suicides_seq,
  sk2014 = mean(sk2014),
  diversity16 = mean(diversity16),
  pop16 = mean(pop_16),
  median_hh_income16=mean(median_hh_income_16),
  white16=mean(white_16),
  bachelors16=mean(bachelors_16),
  male_unemplmt16=mean(male_unemplmt_16),
  female_unemplmt16=mean(female_unemplmt_16))%>% as.data.frame()
detach(p)

# gather in the next step


# make matrix for random effect
# re.form = ~0 zeroes out the random effects for the predictions
preds <- posterior_predict(M2, newdata = predictions, re.form=~0)

preds <- as.data.frame(preds)
summary(sp[, 1])
summary(sp[, 11])
suicide_predict[1:10,]

# get the means and PI for suicides
pred.p <- apply(sp, 2, mean)
pred.p.PI <- apply(sp, 2, PI)
df <- rbind(pred.p,seq(1:11))
df <- t(df)
colnames(df) <- c("mean.predicted","suicides") 
df <- as.data.frame(df)
ggplot(data=df, aes(x=suicides, y=mean.predicted)) +
  geom_line()+
  geom_point()

### start here
### 
### 
### 
# gather and spread are in the tidyr package
library(tidyr)

#names(suicide_predict) <- paste("suicides_",seq (from = -50, to = 50, by=2), sep = "")
plot_posterior <- gather(sp, key = "suicides", value = "predict")
# order the suicides
plot_posterior$suicides <- as.integer(plot_posterior$suicides)


cols <- c("0" = "#7d1f1f", "1" = "#1f7d1f")

plot1 <- ggplot() +
  stat_lineribbon(data=plot_posterior, aes(x=suicides, y=predict),.width = c(.8,0.95 ),
                  show.legend=T) 

plot1 <- ggplot() +
  stat_lineribbon(data=plot_posterior, aes(x=suicides, y=predict, group=factor(lotta),color=factor(lotta) ),.width = c(.8,0.95 ),
                  show.legend=T) 

ggplot(plot_posterior, aes(x = predict)) +
  facet_wrap(~ suicides, ncol = 1) +
  geom_density()
