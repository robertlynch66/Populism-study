# clinton trump figure 1 main plot
library(dplyr)
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
M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_full_rstanarm3.rds")
M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",alcohol_16_to_8:suicides_16_to_8)

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


# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c(0.4880792, 0.4880792, 0.4880792) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf2 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
# use p3 for predicted values
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
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
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

######################################################
####### facet grid plot ######
# label facets
library(cowplot)

### my plots ###########################  Clinton Trump#######
### First one is based on predictions
### Based on predictions
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
 
  geom_point(aes(size = pop_16), alpha=0.4)+

  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, fill = mort_cat,alpha = 0.5),show.legend=FALSE)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, 
  #                        colour=mort_cat),size=0.2,alpha=0.8)+
  #geom_line(data = pred_df, mapping=aes(y = trump_percent, x = mort2, colour=mort_cat),
          #  size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",
                     breaks=c(0.48,0.49,0.50),limits=c(0.48,0.50),
                     labels=c('48%','49%','50%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
  #     alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1

# next do inset
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
  geom_point(size=0.1, alpha=0.1)+
  
  #geom_point(aes(size = pop_16), alpha=0.1, size=0.1)+
  #scale_size(name   = "County population",
       #      breaks = c(10000, 100000, 1000000),
      #       labels = c("10k", "100k",  "1 mil"))+
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
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-200,200),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
  #     alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.position = "none")

facet_plot_inset_1

# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.64, y = .00, width = .35, height = .30)
inset_1
## save it
ggsave(filename = "trump-clinton_predictions.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)

### draws inset plot based on draws

labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), alpha=1/10)+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k","1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
 
  
  
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",
                     breaks=c(0.45,0.50,0.55),limits=c(0.4,0.6),
                     labels=c('45%','50%','55%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#facet_plot_main_1
# inset
# 
# #######################################################
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), size=0.1,alpha=0.1)+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution\n(all samples)"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  scale_y_continuous(name="",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-190,190),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

facet_plot_inset_1

inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.77, y = .00, width = .23, height = .28)
inset_1
## save it
ggsave(filename = "trump_clinton_posterior.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)
#####################################################################
###################################################################
###################################################################
### Next do Sanders and use M2 - recycle all the file names
################################################################
library(dplyr)
p<- readRDS("../data files/populism_data_new.rds")
p$total_votes <- p$clinton_primary1+p$sanders_primary1

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


p <- p %>% select ("county","clinton_primary1","sanders_primary1","clinton_16","total_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16","log_pop_16","pop_16","state")
p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$sanders_primary1<- as.integer(p$sanders_primary1)
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
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_full_rstanarm.rds")
M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_full_rstanarm3.rds")
#M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",alcohol_16_to_8:suicides_16_to_8)

z <- tidy(M2)


##model_intercept
model_intercept <-sum(p1$sanders_primary1)/sum(p1$total_votes)
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

z <-posterior_predict(M2) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_sanders_votes", "county_id")
p3 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# get the correct intercept
newdf$intercept<- sum(p1$sanders_primary1)/sum(p1$total_votes)

# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c(0.4393462, 0.4393462, 0.4393462) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf2 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws
suicides_seq <- seq (from = -200L, to = 200L, by=10)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
ps <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), 
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

s <-posterior_predict(M2, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_sanders_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
# then drugs
drugs_seq <- seq (from = -200L, to = 200L, by=10)


pd <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
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

d <-posterior_predict(M2, newdata=pd,re.form=~0) 
d <- as.data.frame(d)
d_mu <- apply(d,2,mean) %>% as.data.frame()
d_pi <- apply(as.matrix(d), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
d_lower <- d_pi[1,]
d_upper <- d_pi[2,]
d_mu$drugs <- rownames(d_mu)
names(d_mu) <- c("predicted_sanders_percent_mean", "mort")
d_mu$lower <- d_lower
d_mu$upper <- d_upper
d_mu$mort_cat <- "drugs_16_to_8"
# then alcohol
alcohol_seq <- seq (from = -200L, to = 200L, by=10)


pa <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), 
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

a <-posterior_predict(M2, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$drugs <- rownames(a_mu)
names(a_mu) <- c("predicted_sanders_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu)
pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),3)
pred_df$sanders_percent <- (pred_df$predicted_sanders_percent_mean)/100 -0.035
pred_df$lower <- (pred_df$lower)/100 -0.035
pred_df$upper <- (pred_df$upper)/100 -0.035
# replot with the predicted df rather than the draws 
p3$mort_cat<- as.factor(p3$mort_cat)
p3$sanders_percent <- p3$predicted_sanders_votes/p3$total_votes

######################################################
####### facet grid plot ######
# label facets
library(cowplot)

### First one is based on predictions
### Based on predictions
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=sanders_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.4)+
  
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, fill = mort_cat,alpha = 0.5),show.legend=FALSE)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, 
  #                        colour=mort_cat),size=0.2,alpha=0.8)+
  #geom_line(data = pred_df, mapping=aes(y = trump_percent, x = mort2, colour=mort_cat),
  #  size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="Percentage of votes for Sanders vs Clinton in 2016 Primary",
                     breaks=c(0.42,0.44,0.46),limits=c(0.41,0.47),
                     labels=c('42%','44%','46%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  

  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1

# next do inset
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=sanders_percent, colour=mort_cat)) + 

  geom_point(size=0.1, alpha=0.1)+
  #geom_point(aes(size = pop_16), alpha=0.4, size=0.1)+
  #scale_size(name   = "County population",
         #    breaks = c(10000, 100000, 1000000),
        #     labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+

  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, alpha = 0.1),show.legend=FALSE)+
  
  geom_line(data = pred_df, mapping=aes(y = sanders_percent, x = mort2, colour=mort_cat),
            size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-200,200),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.position = "none")

facet_plot_inset_1

# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.66, y = .00, width = .35, height = .30)
inset_1
## save it
ggsave(filename = "sanders_clinton_predictions.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)

### draws inset plot based on draws - posteriors

facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=predicted_sanders_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), alpha=0.4)+
  scale_size(name = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k","1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  
  
  scale_y_continuous(name="Percentage of votes for Sanders vs Clinton in 2016 Primary",
                     breaks=c(0.35,0.40,0.45,0.50,0.55),limits=c(0.35,0.55),
                     labels=c('35%','40%','45%','50%','55%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#facet_plot_main_1
# inset
# 
# #######################################################
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=predicted_sanders_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), alpha=0.1, size =0.1)+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution\n(all samples)"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  scale_y_continuous(name="",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-190,190),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

facet_plot_inset_1

inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.77, y = .00, width = .23, height = .28)
#inset_1

## save it
ggsave(filename = "sanders_clinton_posterior.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)


##################################################################
###### Next do Trump Cruz
###################################################################
library(dplyr)
p<- readRDS("../data files/populism_data_new.rds")
p$total_votes <- p$cruz_votes+p$trump_primary_votes

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


p <- p %>% select ("county","cruz_votes","total_votes","trump_primary_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014", 
                   "diversity16","pop_16","median_hh_income_16", "white_16","bachelors_16",
                   "male_unemplmt_16","female_unemplmt_16","log_pop_16","pop_16","state")
p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
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
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_full_rstanarm.rds")
#M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_full_rstanarm3.rds")
M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",alcohol_16_to_8:suicides_16_to_8)

z <- tidy(M3)


##model_intercept
model_intercept <-sum(p1$trump_primary_votes)/sum(p1$total_votes)
model_se <- z$std.error[1]
#get_slopes
s_slope <- z$estimate[11]
a_slope <- z$estimate[9]
d_slope <- z$estimate[10]
s_se <-z$std.error[11]
a_se <-z$std.error[9]
d_se <-z$std.error[10]

# suicide draws
s_draws <- spread_draws(M3, `(Intercept)`, suicides_change)
s_draws$mort_change <- s_draws$suicides_change
s_draws$suicides_change <- NULL
s_draws <- as.data.frame(s_draws)
s_draws$mort_cat <- "suicides_16_to_8"
# drug draws
d_draws <- spread_draws(M3, `(Intercept)`, drugs_change)
d_draws$mort_change <- d_draws$drugs_change
d_draws$drugs_change <- NULL
d_draws <- as.data.frame(d_draws)
d_draws$mort_cat <- "drugs_16_to_8"
#alcohol draws
a_draws <- spread_draws(M3, `(Intercept)`, alcohol_change)
a_draws$mort_change <- a_draws$alcohol_change
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

z <-posterior_predict(M3) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_primary_votes", "county_id")
p3 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# get the correct intercept
newdf$intercept<- sum(p1$trump_primary_votes)/sum(p1$total_votes)

# make a new df for the actual mean lines
mort_cat = c("alcohol_16_to_8","suicides_16_to_8","drugs_16_to_8")
model_intercept = c(0.6315331, 0.6315331, 0.6315331) 
model_slope = c(a_slope, s_slope, d_slope) 

newdf2 = data.frame(mort_cat, model_slope, model_intercept)      
# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws
suicides_seq <- seq (from = -200L, to = 200L, by=10)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
ps <- data.frame(
  trump = rep(0,41),
  cruz = rep(100,41), 
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

s <-posterior_predict(M3, newdata=ps,re.form=~0) 
s <- as.data.frame(s)
s_mu <- apply(s,2,mean) %>% as.data.frame()
s_pi <- apply(as.matrix(s), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
s_lower <- s_pi[1,]
s_upper <- s_pi[2,]
s_mu$suicides <- rownames(s_mu)
names(s_mu) <- c("predicted_trump_primary_percent_mean", "mort")
s_mu$lower <- s_lower
s_mu$upper <- s_upper
s_mu$mort_cat <- "suicides_16_to_8"
# then drugs
drugs_seq <- seq (from = -200L, to = 200L, by=10)


pd <- data.frame(
  trump = rep(0,41),
  cruz = rep(100,41), # total votes here?
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

d <-posterior_predict(M3, newdata=pd,re.form=~0) 
d <- as.data.frame(d)
d_mu <- apply(d,2,mean) %>% as.data.frame()
d_pi <- apply(as.matrix(d), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
d_lower <- d_pi[1,]
d_upper <- d_pi[2,]
d_mu$drugs <- rownames(d_mu)
names(d_mu) <- c("predicted_trump_primary_percent_mean", "mort")
d_mu$lower <- d_lower
d_mu$upper <- d_upper
d_mu$mort_cat <- "drugs_16_to_8"
# then alcohol
alcohol_seq <- seq (from = -200L, to = 200L, by=10)


pa <- data.frame(
  trump = rep(0,41),
  cruz = rep(100,41), 
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

a <-posterior_predict(M3, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$drugs <- rownames(a_mu)
names(a_mu) <- c("predicted_trump_primary_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu)
pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),3)
pred_df$trump_percent <- (pred_df$predicted_trump_primary_percent_mean)/100 -0.035
pred_df$lower <- (pred_df$lower)/100 -0.035
pred_df$upper <- (pred_df$upper)/100 -0.035
# replot with the predicted df rather than the draws 
p3$mort_cat<- as.factor(p3$mort_cat)
p3$trump_percent <- p3$predicted_trump_primary_votes/p3$total_votes

### First one is based on predictions
### Based on predictions
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.4)+
  
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, fill = mort_cat,alpha = 0.5),show.legend=FALSE)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, 
  #                        colour=mort_cat),size=0.2,alpha=0.8)+
  #geom_line(data = pred_df, mapping=aes(y = trump_percent, x = mort2, colour=mort_cat),
  #  size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="Percentage of votes for Trump vs Cruz in 2016 Primary",
                     breaks=c(0.55,0.60,0.65,0.70),limits=c(0.55,0.70),
                     labels=c('55%','60%','65%','70%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1

# next do inset
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
  
  
  geom_point(size=0.1, alpha=0.1)+
  #scale_size(name   = "County population",
        #     breaks = c(10000, 100000, 1000000),
          #   labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  
  geom_ribbon(data=pred_df, mapping=aes(ymin=lower, ymax=upper, x=mort2,
                                        colour=mort_cat, alpha = 0.1),show.legend=FALSE)+
  
  geom_line(data = pred_df, mapping=aes(y = trump_percent, x = mort2, colour=mort_cat),
            size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Mortality 95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-200,200),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.position = "none")

facet_plot_inset_1

# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.66, y = .00, width = .35, height = .30)
inset_1
## save it
ggsave(filename = "trump_cruz_predictions.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)


############################################################3

### now do posteriors
facet_plot_main_1 <- ggplot(p3, aes(x=mort,y=predicted_trump_primary_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), alpha=0.4)+
  scale_size(name = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k","1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  
  
  scale_y_continuous(name="Percentage of votes for Trump vs Cruz in 2016 Primary",
                     breaks=c(0.50,0.55,0.60,0.65,0.70,0.75),limits=c(0.45,0.80),
                     labels=c('50%','55%','60%','65%','70%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"))+
  theme(plot.title = element_text(size=10,face = "bold", hjust = 0.5))+
  theme(axis.title.x =element_text (size=10,face="bold"))+
  theme(axis.title.y = element_text (size=10,face="bold"))+
  theme(legend.title = element_text(size=9,face = "bold"))+
  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

facet_plot_main_1
# inset
# 
# #######################################################
facet_plot_inset_1 <- ggplot(p3, aes(x=mort,y=predicted_trump_primary_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16), alpha=0.1, size =0.1)+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  #geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution\n(all samples)"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  scale_y_continuous(name="",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-190,190),
                     breaks=c(-150,0,150),labels=c("-150","0","150"))+
  
  
  #ggtitle("An increase in per capita deaths from suicides, 
  # alcohol and opiate overdoses\nbetween 2008 and 2016 predicts support for Trump") +
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=10,face="bold"),
        axis.title.y = element_text (size=10,face="bold"),
        legend.title = element_text(size=9,face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=6, face="bold"),
        axis.text.y = element_text(size=6, face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

facet_plot_inset_1

inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.72, y = .00, width = .26, height = .26)
inset_1

## save it
ggsave(filename = "trump_cruz_posterior.with.inset.png", 
       plot = inset_1,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)