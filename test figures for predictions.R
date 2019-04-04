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
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",alcohol_16_to_8:suicides_16_to_8)


# get predictions
p$county_id <- rownames(p)
#p<- p %>% filter (county_id==1)
suicides_seq <- seq (from = -1000L, to = 1000L, by=100)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
ps <- data.frame(
  trump_votes = 0,
  total_votes_16 = 100, # total votes here?
  sk_change=rep(mean(p$sk2014_over_2005),21),
  pop_change=rep(mean(p$pop_change_16_to_10),21),
  median_hh_income_change = rep(mean(p$median_hh_income_16_to_10),21),
  perc_bachelors_change = rep(mean(p$bachelors_16_to_10),21),
  male_unemplmt_change = rep(mean(p$male_unemplmt_16_to_10),21),
  female_unemplmt_change = rep(mean(p$female_unemplmt_16_to_10),21),
  for_born_change = rep(mean(p$for_born_16_to_10),21),
  alcohol_change = rep(mean(p$alcohol_16_to_8),21),
  drugs_change = rep(mean(p$drugs_16_to_8),21),
  suicides_change = suicides_seq,
  sk2014 = rep(mean(p$sk2014),21),
  diversity16 = rep(mean(p$diversity16),21),
  pop16 = rep(mean(log(p$pop_16)),21),
  median_hh_income16=rep(mean(p$median_hh_income_16),21),
  white16=rep(mean(p$white_16),21),
  bachelors16=rep(mean(p$bachelors_16),21),
  male_unemplmt16=rep(mean(p$male_unemplmt_16),21),
  female_unemplmt16=rep(mean(p$female_unemplmt_16),21))%>% as.data.frame()

#note: in newdata we want agree and disagree to sum to the number of people we
# want to predict for. the values of agree and disagree don't matter so long as
# their sum is the desired number of trials. we need to explicitly imply the
# number of trials like this because our original data are aggregate. if we had
# bernoulli data then it would be a given we wanted to predict for single
# individuals.
library(rstanarm)

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_votes", "county_id")
p3 <- p %>% left_join(z,by="county_id")
# make matrix for random effect



# re.form = ~0 zeroes out the random effects for the predictions
s <- posterior_predict(M1,  newdata=ps, re.form=~0)
s <- apply(s,2,mean) %>% as.data.frame()
s  <- 


d$county_id <- rep(1:3091, each=21)
d$suicides <- rep(seq(1:21),3091)
names(d) <- c("prediction", "county","suicide_change")

# group 

sp <- as.data.frame(sp)
get_variables(M1)
z <- tidy(M1)

# practice df with posterior_predict using agg binomial
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"), 
                              prior = student_t(df = 7), 
                              prior_intercept = student_t(df = 7),
                              chains = 1, cores = 1)
womensrole_bglm_1
newdata <- data.frame(agree = c(0,0), disagree = c(100,100), education = c(12,16),
                      gender = factor("Female", levels = c("Male", "Female")))

y_rep <- posterior_predict(womensrole_bglm_1, newdata)
summary(apply(y_rep, 1, diff))
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
# I think spread_draws is like extract samples in rethinking
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
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# get the correct intercept
newdf$intercept<- sum(p1$trump_16)/sum(p1$total_votes)-exp(newdf$`(Intercept)`)
# jitter the intercepts so ablines don't overlap completely
newdf$intercept <- ifelse(newdf$mort_cat == "drugs_16_to_8" , newdf$intercept+0.01, 
                          ifelse(newdf$mort_cat == "alcohol_16_to_8" , newdf$intercept-0.01,newdf$intercept))

# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c(0.4780792, 0.4880792, 0.4980792) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf2 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
general_plot <- ggplot(p1, aes(mort,trump_16/total_votes,colour = mort_cat)) + 
  geom_point(size=0.2,position=position_jitter(h=0.0, w=0.2))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  #geom_point(aes(size = pop_16))+
  #scale_size(name   = "County population",
  #      breaks = c(10000, 100000, 250000, 500000,1000000,2000000),
  #    labels = c("10k", "100k", "250k", "500k", "1 mil","2 mil"))+
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.3,option="magma",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  
  
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",breaks=c(0.25,0.5,0.75),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-50,75),
                     breaks=c(-25,0,25,50),labels=c("-25","0","25","50"))+
  
  
  ggtitle("An increase in per capita deaths from suicides, 
          alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=8,face="bold"))+
  theme(axis.title.y = element_text (size=8,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

general_plot


# make facet group a factor
p3$mort_cat<- as.factor(p3$mort_cat)
# label facets
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
general_plot <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
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
                     breaks=c(-50,-25,0,25,50),labels=c("-50","-25","0","25","50"))+
  
  
  ggtitle("An increase in per capita deaths from suicides, 
          alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=8,face="bold"))+
  theme(axis.title.y = element_text (size=8,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

general_plot