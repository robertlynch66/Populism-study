# clinton trump figure 1 main plot
library(dplyr)

p<- readRDS("../data files/populism_data_new2.rds")
p$total_votes <- p$trump_16+p$clinton_16

# get change in social capital index - do not scale (1 unit equals 1 sd)
p$sk2014_over_2005 <- p$sk14-p$sk05

# census data differences
p$pop_change_16_to_10 <- log(p$pop16E) -log(p$pop_2010)
p$median_hh_income_16_to_10 <- log(p$hh_income16E) - log(p$hh_income10E)
p$white_16_to_10 <- (p$white16E/p$pop16E)-(p$white10E/p$pop_2010)
p$bachelors_16_to_10 <- (p$bachelors16E/p$pop16E)-(p$bachelors10E/p$pop_2010)
p$male_unemplmt_16_to_10 <- (p$male_unemployed16E/p$pop16E)-(p$male_unemployed10E/p$pop_2010)
p$female_unemplmt_16_to_10 <- (p$female_unemployed16E/p$pop16E)-(p$female_unemployed10E/p$pop_2010)
p$for_born_16_to_10 <- (p$foreign_born16E/p$pop16E)-(p$foreign_born10E/p$pop_2010)

# cdc data differences
p$alcohol_16_to_8 <- (p$alcohol_16/p$pop16E)*10e4-(p$alcohol_08/p$pop10E)*10e4
p$drugs_16_to_8 <- (p$drug_16/p$pop16E)*10e4-(p$drug_08/p$pop10E)*10e4
p$suicides_16_to_8 <- (p$suicides_16/p$pop16E)*10e4-(p$suicides_08/p$pop10E)*10e4





p <- p %>% select ("county","state","trump_16","clinton_16","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$clinton_16 <- as.integer(p$clinton_16)
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
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm5.rds")
#M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
#M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",sk2014_over_2005,alcohol_16_to_8:suicides_16_to_8)


# get the county predictions

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_votes", "county_id")
p1 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws
suicides_seq <- seq (from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
#  
us_sk_change <- sum(p$sk2014_over_2005*p$pop_16)/mean(p$pop_16)/nrow(p)
us_pop_change <- sum(p$pop_change_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_hhi_change <- sum(p$median_hh_income_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_bach_change <- sum(p$bachelors_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_male_u_change <- sum(p$male_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_female_u_change <- sum(p$female_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_fb_change <- sum(p$for_born_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_alcho_change <- sum(p$alcohol_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_drug_change <- sum(p$drugs_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_suicide_change <- sum(p$suicides_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)  
us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)

ps <- data.frame(
  trump_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  white_change = rep(us_white_change,41),
  suicides_change = suicides_seq) %>% as.data.frame()

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
drugs_seq  <- seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pd <- data.frame(
  trump_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = drugs_seq,
  white_change = rep(us_white_change,41),
  suicides_change = rep(us_suicide_change)) %>% as.data.frame()

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
alcohol_seq <- seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pa <- data.frame(
  trump_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  white_change = rep(us_white_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = alcohol_seq,
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

a <-posterior_predict(M1, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$alcohol <- rownames(a_mu)
names(a_mu) <- c("predicted_trump_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"


### add social capital predictions
sc_seq <- seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
psc <- data.frame(
  trump_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  sk_change=sc_seq,
  white_change = rep(us_white_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

sc <-posterior_predict(M1, newdata=psc,re.form=~0) 
sc <- as.data.frame(sc)
sc_mu <- apply(sc,2,mean) %>% as.data.frame()
sc_pi <- apply(as.matrix(sc), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
sc_lower <- sc_pi[1,]
sc_upper <- sc_pi[2,]
sc_mu$social_capital <- rownames(sc_mu)
names(sc_mu) <- c("predicted_trump_percent_mean", "mort")
sc_mu$lower <- sc_lower
sc_mu$upper <- sc_upper
sc_mu$mort_cat <- "sk2014_over_2005"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu,sc_mu)

pred_df$mort <- c(seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41),
                  seq(from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41),
                  seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41),
                  seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41))
#pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),4)
pred_df$trump_percent <- (pred_df$predicted_trump_percent_mean)/100 
pred_df$lower <- (pred_df$lower)/100 
pred_df$upper <- (pred_df$upper)/100 

p1$mort_cat<- as.factor(p1$mort_cat)
p1$trump_percent <- p1$trump_16/p1$total_votes
p1$predicted_trump_percent <- p1$predicted_trump_votes/p1$total_votes
pred_df$predicted_trump_percent <- pred_df$trump_percent
######################################################
####### facet grid plot ######
# label facets
library(cowplot)



### my plots ###########################  Clinton Trump#######
### Based on predictions
p1$mort_cat = factor(p1$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides",sk2014_over_2005="Social capital")
pred_df2 <- transform(pred_df, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                               mort))
p4 <- transform(p1, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                    mort))
p4$mort_cat = factor(p4$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df2$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))




# inset plot
facet_plot_main_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton",
                     breaks=c(0.4,0.45,0.50,0.55,0.60),limits=c(0.4,0.6),
                     labels=c('40%','45%','50%','55%','60%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k and\nsocial capital index between 2008 and 2016",
                     limits=c(-40,40), breaks=c(-40,-20,0,20,40),labels=c('Q1','Q2','Q3','Q4','Q5'))+
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=8,face="bold"),
        axis.title.y = element_text (size=8,face="bold"),
        legend.title = element_text(size=8,face = "bold"),
        panel.border = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1




facet_plot_inset_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  
  
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.50,0.75),limits=c(0.00,1.00),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",
                     breaks=c(-50,0,100),labels=c("-","0","+"))+
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_blank(),
        plot.title = element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_blank (),
        legend.title = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y=element_text(size=4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

facet_plot_inset_1



# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1, x = 0.86, y = .05, width = .14, height = .22)
inset_1
## save it
ggsave(filename = "trump-clinton_predictions.with.inset.png", 
       plot = inset_1,
       width = 36, 
       height = 12,
       units = "cm",
       dpi = 300)
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
#### get sanders 
# clinton sanders figure 2 main plot
library(dplyr)

#p<- readRDS("../data files/populism_data_old.rds")
p<- readRDS("../data files/populism_data_new2.rds")
p$total_votes <- p$sanders_primary1+p$clinton_primary1

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




p <- p %>% select ("county","state","sanders_primary1","clinton_primary1","total_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","white_16_to_10","pop_16") 

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
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm4.rds")
#M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",sk2014_over_2005,alcohol_16_to_8:suicides_16_to_8)


# get the county predictions

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_sanders_votes", "county_id")
p1 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws


library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
#  
us_sk_change <- sum(p$sk2014_over_2005*p$pop_16)/mean(p$pop_16)/nrow(p)
us_pop_change <- sum(p$pop_change_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_hhi_change <- sum(p$median_hh_income_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_bach_change <- sum(p$bachelors_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_male_u_change <- sum(p$male_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_female_u_change <- sum(p$female_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_fb_change <- sum(p$for_born_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_alcho_change <- sum(p$alcohol_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_drug_change <- sum(p$drugs_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_suicide_change <- sum(p$suicides_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)  
us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)  
suicides_seq <- seq (from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41)

ps <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  white_change = rep(us_white_change,41),
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = suicides_seq) %>% as.data.frame()

s <-posterior_predict(M1, newdata=ps,re.form=~0) 
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
drugs_seq  <- seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pd <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  white_change = rep(us_white_change,41),
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = drugs_seq,
  suicides_change = rep(us_suicide_change)) %>% as.data.frame()

d <-posterior_predict(M1, newdata=pd,re.form=~0) 
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
alcohol_seq <- seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pa <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), # total votes here?
  white_change = rep(us_white_change,41),
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = alcohol_seq,
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

a <-posterior_predict(M1, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$alcohol <- rownames(a_mu)
names(a_mu) <- c("predicted_sanders_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"


### add social capital predictions
sc_seq <- seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
psc <- data.frame(
  sanders_votes = rep(0,41),
  clinton_votes = rep(100,41), 
  white_change = rep(us_white_change,41),
  sk_change=sc_seq,
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

sc <-posterior_predict(M1, newdata=psc,re.form=~0) 
sc <- as.data.frame(sc)
sc_mu <- apply(sc,2,mean) %>% as.data.frame()
sc_pi <- apply(as.matrix(sc), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
sc_lower <- sc_pi[1,]
sc_upper <- sc_pi[2,]
sc_mu$social_capital <- rownames(sc_mu)
names(sc_mu) <- c("predicted_sanders_percent_mean", "mort")
sc_mu$lower <- sc_lower
sc_mu$upper <- sc_upper
sc_mu$mort_cat <- "sk2014_over_2005"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu,sc_mu)

pred_df$mort <- c(seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41),
                  seq(from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41),
                  seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41),
                  seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41))
#pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),4)
pred_df$sanders_percent <- (pred_df$predicted_sanders_percent_mean)/100 
pred_df$lower <- (pred_df$lower)/100 
pred_df$upper <- (pred_df$upper)/100 

p1$mort_cat<- as.factor(p1$mort_cat)
p1$sanders_percent <- p1$sanders_primary1/p1$total_votes
p1$predicted_sanders_percent <- p1$predicted_sanders_votes/p1$total_votes
pred_df$predicted_sanders_percent <- pred_df$sanders_percent
######################################################
####### facet grid plot ######
# label facets
library(cowplot)



### my plots ###########################  Clinton Trump#######
### Based on predictions
p1$mort_cat = factor(p1$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides",sk2014_over_2005="Social capital")
pred_df2 <- transform(pred_df, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                               mort))
p4 <- transform(p1, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                    mort))
p4$mort_cat = factor(p4$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df2$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))




# inset plot
facet_plot_main_1 <- ggplot(p4, aes(x=new_mort,y=predicted_sanders_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="Percentage of votes for\nSanders vs Clinton",
                     breaks=c(0.35,0.40,0.45,0.50),limits=c(0.35,0.50),
                     labels=c('35%','40%','45%','50%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k and\nsocial capital index between 2008 and 2016",
                     limits=c(-40,40), breaks=c(-40,-20,0,20,40),labels=c('Q1','Q2','Q3','Q4','Q5'))+
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=8,face="bold"),
        axis.title.y = element_text (size=8,face="bold"),
        legend.title = element_text(size=8,face = "bold"),
        panel.border = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1




facet_plot_inset_1 <- ggplot(p4, aes(x=new_mort,y=predicted_sanders_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  
  
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.50,0.75),limits=c(0.00,1.00),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",
                     breaks=c(-50,0,100),labels=c("-","0","+"))+
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_blank(),
        plot.title = element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_blank (),
        legend.title = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y=element_text(size=4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

facet_plot_inset_1



# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1,  x = 0.86, y = .05, width = .14, height = .22)
inset_1
## save it
ggsave(filename = "sanders_clinton_predictions.with.inset2.png", 
       plot = inset_1,
       width = 36, 
       height = 12,
       units = "cm",
       dpi = 300)


######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
#### 
# Trump Cruz figure 3 main plot
library(dplyr)

p<- readRDS("../data files/populism_data_old.rds")
p$total_votes <- p$trump_primary_votes+p$cruz_votes

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



p <- p %>% select ("county","state","trump_primary_votes","cruz_votes","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$cruz_votes <- as.integer(p$cruz_votes)
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
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm4.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",sk2014_over_2005,alcohol_16_to_8:suicides_16_to_8)


# get the county predictions

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_votes", "county_id")
p1 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws


library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
#  
us_sk_change <- sum(p$sk2014_over_2005*p$pop_16)/mean(p$pop_16)/nrow(p)
us_pop_change <- sum(p$pop_change_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_hhi_change <- sum(p$median_hh_income_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_bach_change <- sum(p$bachelors_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_male_u_change <- sum(p$male_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_female_u_change <- sum(p$female_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_fb_change <- sum(p$for_born_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_alcho_change <- sum(p$alcohol_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_drug_change <- sum(p$drugs_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_suicide_change <- sum(p$suicides_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)  
us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)  
suicides_seq <- seq (from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41)

ps <- data.frame(
  trump_votes = rep(0,41),
  cruz_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = suicides_seq) %>% as.data.frame()

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
drugs_seq  <- seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pd <- data.frame(
  trump_votes = rep(0,41),
  cruz_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = drugs_seq,
  suicides_change = rep(us_suicide_change)) %>% as.data.frame()

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
alcohol_seq <- seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pa <- data.frame(
  trump_votes = rep(0,41),
  cruz_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  white_change = rep (us_white_change,41),
  alcohol_change = alcohol_seq,
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

a <-posterior_predict(M1, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$alcohol <- rownames(a_mu)
names(a_mu) <- c("predicted_trump_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"


### add social capital predictions
sc_seq <- seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
psc <- data.frame(
  trump_votes = rep(0,41),
  cruz_votes = rep(100,41), # total votes here?
  sk_change=sc_seq,
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  white_change = rep (us_white_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

sc <-posterior_predict(M1, newdata=psc,re.form=~0) 
sc <- as.data.frame(sc)
sc_mu <- apply(sc,2,mean) %>% as.data.frame()
sc_pi <- apply(as.matrix(sc), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
sc_lower <- sc_pi[1,]
sc_upper <- sc_pi[2,]
sc_mu$social_capital <- rownames(sc_mu)
names(sc_mu) <- c("predicted_trump_percent_mean", "mort")
sc_mu$lower <- sc_lower
sc_mu$upper <- sc_upper
sc_mu$mort_cat <- "sk2014_over_2005"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu,sc_mu)

pred_df$mort <- c(seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41),
                  seq(from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41),
                  seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41),
                  seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41))
#pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),4)
pred_df$trump_percent <- (pred_df$predicted_trump_percent_mean)/100 
pred_df$lower <- (pred_df$lower)/100 
pred_df$upper <- (pred_df$upper)/100 

p1$mort_cat<- as.factor(p1$mort_cat)
p1$trump_percent <- p1$trump_primary_votes/p1$total_votes
p1$predicted_trump_percent <- p1$predicted_trump_votes/p1$total_votes
pred_df$predicted_trump_percent <- pred_df$trump_percent
######################################################
####### facet grid plot ######
# label facets
library(cowplot)



### my plots ###########################  Clinton Trump#######
### Based on predictions
p1$mort_cat = factor(p1$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides",sk2014_over_2005="Social capital")
pred_df2 <- transform(pred_df, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                               mort))
p4 <- transform(p1, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                    mort))
p4$mort_cat = factor(p4$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df2$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))




# inset plot
facet_plot_main_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="Percentage of votes for\nTrump vs Cruz",
                     breaks=c(0.60,0.65,0.70),limits=c(0.60,0.70),
                     labels=c('60%','65%','70%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k and\nsocial capital index between 2008 and 2016",
                     limits=c(-40,40), breaks=c(-40,-20,0,20,40),labels=c('Q1','Q2','Q3','Q4','Q5'))+
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=8,face="bold"),
        axis.title.y = element_text (size=8,face="bold"),
        legend.title = element_text(size=8,face = "bold"),
        panel.border = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1




facet_plot_inset_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  
  
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.50,0.75),limits=c(0.00,1.00),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",
                     breaks=c(-50,0,100),labels=c("-","0","+"))+
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_blank(),
        plot.title = element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_blank (),
        legend.title = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y=element_text(size=4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

facet_plot_inset_1



# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1,  x = 0.86, y = .05, width = .14, height = .22)
inset_1
## save it
ggsave(filename = "trump_cruz_predictions.with.inset.png", 
       plot = inset_1,
       width = 36, 
       height = 12,
       units = "cm",
       dpi = 300)

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
#### 
# Trump kasich figure 4 main plot
library(dplyr)

#p<- readRDS("../data files/populism_data_old.rds")
p<- readRDS("../data files/populism_data_new2.rds")
p$kasich_votes <- as.numeric(p$kasich_votes)
p$total_votes <- p$trump_primary_votes+p$kasich_votes

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



p <- p %>% select ("county","state","trump_primary_votes","kasich_votes","total_votes","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","pop_16") 

p <- p[complete.cases(p), ]
library(rethinking)
p$state_id <- coerce_index(p$state)
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))

#make dv integers
p$trump_primary_votes<- as.integer(p$trump_primary_votes)
p$kasich_votes <- as.integer(p$kasich_votes)
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
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm4.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm4.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# get the mortality and put it in category
p$county_id <- rownames(p)
library(tidyr)
p1 <- gather(p, key = "mort_cat",
             value = "mort",sk2014_over_2005,alcohol_16_to_8:suicides_16_to_8)


# get the county predictions

z <-posterior_predict(M1) 
z <- as.data.frame(z)
z <- apply(z,2,mean) %>% as.data.frame()
z$county_id <- rownames(z)
names(z) <- c("predicted_trump_votes", "county_id")
p1 <- p1 %>% left_join(z,by="county_id")
# make matrix for random effect
# make a new ggplot with alcohol, suicides and drugs against support for Trump vs Clinton on Y (both raw data and predcitions)

library(plotly)

# plot it in ggplot
# use p3 for predicted values
# alternative using 'predictions' hoding all other variables constant model' instead of draws


library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
#  
us_sk_change <- sum(p$sk2014_over_2005*p$pop_16)/mean(p$pop_16)/nrow(p)
us_pop_change <- sum(p$pop_change_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_hhi_change <- sum(p$median_hh_income_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_bach_change <- sum(p$bachelors_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_male_u_change <- sum(p$male_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_female_u_change <- sum(p$female_unemplmt_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_fb_change <- sum(p$for_born_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)
us_alcho_change <- sum(p$alcohol_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_drug_change <- sum(p$drugs_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)
us_suicide_change <- sum(p$suicides_16_to_8*p$pop_16)/mean(p$pop_16)/nrow(p)  
us_white_change <- sum(p$white_16_to_10*p$pop_16)/mean(p$pop_16)/nrow(p)  


suicides_seq <- seq (from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41)

ps <- data.frame(
  trump_votes = rep(0,41),
  kasich_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = suicides_seq) %>% as.data.frame()

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
drugs_seq  <- seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41)
library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pd <- data.frame(
  trump_votes = rep(0,41),
  kasich_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  white_change = rep (us_white_change,41),
  drugs_change = drugs_seq,
  suicides_change = rep(us_suicide_change)) %>% as.data.frame()

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
alcohol_seq <- seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
pa <- data.frame(
  trump_votes = rep(0,41),
  kasich_votes = rep(100,41), # total votes here?
  sk_change=rep(us_sk_change,41),
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  for_born_change = rep(us_fb_change,41),
  white_change = rep (us_white_change,41),
  alcohol_change = alcohol_seq,
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

a <-posterior_predict(M1, newdata=pa,re.form=~0) 
a <- as.data.frame(a)
a_mu <- apply(a,2,mean) %>% as.data.frame()
a_pi <- apply(as.matrix(a), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
a_lower <- a_pi[1,]
a_upper <- a_pi[2,]
a_mu$alcohol <- rownames(a_mu)
names(a_mu) <- c("predicted_trump_percent_mean", "mort")
a_mu$lower <- a_lower
a_mu$upper <- a_upper
a_mu$mort_cat <- "alcohol_16_to_8"


### add social capital predictions
sc_seq <- seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41)

library(dplyr)
#if any variables were transformed (e.g. rescaled) in the data used to fit the model, 
#then these variables must also be transformed in newdata. This only applies if variables
# were transformed before passing the data to one of the modeling functions and not
#  if transformations were specified inside the model formula.
psc <- data.frame(
  trump_votes = rep(0,41),
  kasich_votes = rep(100,41), # total votes here?
  sk_change=sc_seq,
  pop_change=rep(us_pop_change,41),
  median_hh_income_change = rep(us_hhi_change,41),
  perc_bachelors_change = rep(us_bach_change,41),
  male_unemplmt_change = rep(us_male_u_change,41),
  female_unemplmt_change = rep(us_female_u_change,41),
  white_change = rep (us_white_change,41),
  for_born_change = rep(us_fb_change,41),
  alcohol_change = rep(us_alcho_change,41),
  drugs_change = rep(us_drug_change,41),
  suicides_change = rep(us_suicide_change,41)) %>% as.data.frame()

sc <-posterior_predict(M1, newdata=psc,re.form=~0) 
sc <- as.data.frame(sc)
sc_mu <- apply(sc,2,mean) %>% as.data.frame()
sc_pi <- apply(as.matrix(sc), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
sc_lower <- sc_pi[1,]
sc_upper <- sc_pi[2,]
sc_mu$social_capital <- rownames(sc_mu)
names(sc_mu) <- c("predicted_trump_percent_mean", "mort")
sc_mu$lower <- sc_lower
sc_mu$upper <- sc_upper
sc_mu$mort_cat <- "sk2014_over_2005"
# rowbind
pred_df <- rbind(a_mu,s_mu,d_mu,sc_mu)

pred_df$mort <- c(seq (from = min(p$alcohol_16_to_8), to = max(p$alcohol_16_to_8), length.out=41),
                  seq(from = min(p$suicides_16_to_8), to = max(p$suicides_16_to_8), length.out=41),
                  seq (from = min(p$drugs_16_to_8), to = max(p$drugs_16_to_8), length.out=41),
                  seq (from =min(p$sk2014_over_2005), to = max(p$sk2014_over_2005), length.out=41))
#pred_df$mort2 <- rep(seq(from=-200, to= 200, by =10),4)
pred_df$trump_percent <- (pred_df$predicted_trump_percent_mean)/100 
pred_df$lower <- (pred_df$lower)/100 
pred_df$upper <- (pred_df$upper)/100 

p1$mort_cat<- as.factor(p1$mort_cat)
p1$trump_percent <- p1$trump_primary_votes/p1$total_votes
p1$predicted_trump_percent <- p1$predicted_trump_votes/p1$total_votes
pred_df$predicted_trump_percent <- pred_df$trump_percent
######################################################
####### facet grid plot ######
# label facets
library(cowplot)



### my plots ###########################  Clinton Trump#######
### Based on predictions
p1$mort_cat = factor(p1$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides",sk2014_over_2005="Social capital")
pred_df2 <- transform(pred_df, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                               mort))
p4 <- transform(p1, new_mort=ifelse(mort_cat=="sk2014_over_2005", mort*359/19,
                                    mort))
p4$mort_cat = factor(p4$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))
pred_df2$mort_cat = factor(pred_df$mort_cat, levels=c('drugs_16_to_8','alcohol_16_to_8','suicides_16_to_8','sk2014_over_2005'))




# inset plot
facet_plot_main_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="Percentage of votes for\nTrump vs Kasich",
                     breaks=c(0.75,0.80,0.85),limits=c(0.75,0.85),
                     labels=c('75%','80%','85%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k and\nsocial capital index between 2008 and 2016",
                     limits=c(-40,40), breaks=c(-40,-20,0,20,40),labels=c('Q1','Q2','Q3','Q4','Q5'))+
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(size=10,face = "bold", hjust = 0.5),
        axis.title.x =element_text (size=8,face="bold"),
        axis.title.y = element_text (size=8,face="bold"),
        legend.title = element_text(size=8,face = "bold"),
        panel.border = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

facet_plot_main_1




facet_plot_inset_1 <- ggplot(p4, aes(x=new_mort,y=predicted_trump_percent, colour=mort_cat)) + 
  
  geom_point(aes(size = pop_16), alpha=0.7)+
  
  scale_size_area(name   = "County population",
                  breaks = c(10000, 100000, 1000000),
                  max_size =5,
                  #range= c(.5,5),
                  labels = c("10k", "100k",  "1 mil"))+
  
  
  #facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  facet_grid(.~mort_cat, labeller=labeller(mort_cat=labels),scales="free", shrink=TRUE)+
  
  #### Fix the geom_point y axis point
  #geom_line(aes(data=y=y, x=x, colour = mort_cat))+
  geom_ribbon(data=pred_df2, mapping=aes(ymin=lower, ymax=upper, x=new_mort,
                                         colour=mort_cat, fill = mort_cat,alpha = 2),show.legend=FALSE)+
  
  geom_vline(xintercept=c(0), linetype="dotted")+
  
  scale_colour_viridis_d(name="Change in per capita deaths and\nsocial capital by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  c("colour","fill"),
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8","sk2014_over_2005"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  scale_y_continuous(name="",
                     breaks=c(0.25,0.50,0.75),limits=c(0.2,1),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",
                     breaks=c(-50,0,100),labels=c("-","0","+"))+
  guides(color=guide_legend("95% Crediblity Intervals"))+
  
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides","Social capital"))+
  
  
  # Label appearance
  theme_bw()+
  theme(legend.text = element_blank(),
        plot.title = element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_blank (),
        legend.title = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y=element_text(size=4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=5),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black"))

facet_plot_inset_1



# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main_1) +
  draw_plot(facet_plot_inset_1,  x = 0.86, y = .05, width = .14, height = .22)
inset_1
## save it
ggsave(filename = "trump_kasich_predictions.with.inset.png", 
       plot = inset_1,
       width = 36, 
       height = 12,
       units = "cm",
       dpi = 300)

###############################
###############################
###############################
###############################
###############################
###############################
# County maps
# #############################


# read in my data
library(dplyr)
library(ggplot2)
# read in the geom file
p <- readRDS("../data files/populism_data_w_geom.rds")

# make populist counties variable (2=populist, 1=traditonal 0= neutral)
#p$populist  <- ifelse(p$trump_16 > p$clinton_16 &  p$sanders_primary1 > p$clinton_primary1 & p$obama_08 >p$mccain_08 , 2,
 #                     ifelse(p$trump_16 < p$clinton_16 & p$sanders_primary1 < p$clinton_primary1 ,1,0))

# make only populist cat
p$populist  <- ifelse(p$trump_16 > p$clinton_16 &  p$sanders_primary1 > p$clinton_primary1  
                        & p$obama_08 >p$mccain_08 , 1,0)
                     
p$mortality <- p$suicides_16_to_8+p$alcohol_16_to_8+p$drugs_16_to_8

#plot(p["suicides_16_to_8"])
library(mapview)
## try to save this interactive map
d <- mapview(p, zcol = "suicides_16_to_8", legend = TRUE)

## try to save the image
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

## create standalone .html
#mapshot(d, url = paste0(getwd(), "/map.html"))

## create standalone .png; temporary .html is removed automatically unless
## 'remove_url = FALSE' is specified
#mapshot(d, file = paste0(getwd(), "/map.png"))
#mapshot(d, file = paste0(getwd(), "/map.png"),
  #      remove_controls = c("homeButton", "layersControl"))

## create .html and .png
#mapshot(d, url = paste0(getwd(), "/map.html"),
 #       file = paste0(getwd(), "/map.png"))


mapshot(d, url=("../../Interactive map files/suicides_map.html"))


# # # save a stand-alone, interactive map as an html file
library(htmlwidgets)
saveWidget(widget = d, file = 'map.html', selfcontained = T)
saveWidget(widget = d, file = "../../Interactive map standalone/suicides_map2.html", selfcontained = T)
# # # save a snapshot as a png file
library(mapview)
mapshot(d, file = 'map.png')
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
# make social capital groups
p$social_capital <- ifelse(p$sk2014_over_2005>0,1,-1)
# make suicide groups
p$suicides <- ifelse(p$suicides_16_to_8 >0,1,-1 )
# make alcohol groups
p$alcohol <- ifelse(p$alcohol_16_to_8>0,1,-1)
p$drug <- ifelse(p$drugs_16_to_8>0,1,-1)
p$mortality <- ifelse(p$mortality>0,1,-1)
# check means of groups
aggregate(drugs_16_to_8 ~ populist, FUN=mean, data=p)
aggregate(sk2014_over_2005~ populist, FUN=mean,data =p)
aggregate(alcohol_16_to_8 ~ populist, FUN=mean,data =p)
aggregate(suicides_16_to_8 ~ populist, FUN=mean,data =p)
aggregate(mortality ~ populist, FUN=mean,data=p)
library(plotrix) # required for stnd error
aggregate(sk2014_over_2005 ~ populist, data =p, 
          FUN = function(x) c(mean = mean(x), se = std.error(x)))
aggregate(mortality ~ populist, data =p, 
          FUN = function(x) c(mean = mean(x), se = std.error(x)))
# get standard errors
sqrt(var(pop$mortality)/sum(pop$clinton_16))
sqrt(var(nonpop$mortality, na.rm=TRUE)/sum(nonpop$clinton_16,na.rm=TRUE))
     
# get the populist counties only
pop <- p %>% filter (populist==1)
nonpop <- p %>% filter (populist==0)
sum(pop$clinton_16)
# count instances of increase or decrease by populist group
p %>% group_by(populist) %>% count(mortality)
# make cats
p$cat <- ifelse(p$social_capital==1 & p$populist==1,1,ifelse(p$social_capital==-1 & p$populist==1,2,
                                                             ifelse(p$social_capital==1 & p$populist==0,3,
                                                                    ifelse(p$social_capital==-1 & p$populist==0,4,5))))
p$cat[is.na(p$cat)] <- 5
# make category labels for populist counties (cat2)
p$cat <- factor(p$cat,labels = c("Increase", "Decrease", "3","4","5"))
# filter p to only include traditional or populist counties

w1 <- ggplot() + 
  # geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = p, aes(color=as.factor(cat),fill = as.factor(cat)), size = 0.1)+
  
  scale_fill_manual("Social capital index", 
                     breaks = c("Increase", "Decrease"),
                
                     values = c("#F0E442","#0072B2","#000000","#000000","#000000")) +
                     
                     #labels = c("Decrease", "Increase"))+
  
 
  
  
  scale_color_manual("", 
                     breaks = c("Increase", "Decrease"),
                    values = c("#F0E442","#0072B2","#000000","#000000","#000000"),
                    
                     guide=FALSE)+ ggtitle ("Change in Social Capital\nin Populist counties")+ 
  theme_classic()+
  theme(axis.title.x=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

w1
# make category labels for traditional counties (cat3)

p$cat <- ifelse(p$mortality==1 & p$populist==1,1,ifelse(p$mortality==-1 & p$populist==1,2,
                                                             ifelse(p$mortality==1 & p$populist==0,3,
                                                                    ifelse(p$mortality==-1 & p$populist==0,4,5))))
p$cat[is.na(p$cat)] <- 5

# make category labels for populist counties (cat2)
p$cat <- factor(p$cat,labels = c("Increase", "Decrease", "3","4","5"))
# filter p to only include traditional or populist counties

w2 <- ggplot() + 
  # geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = p, aes(color=as.factor(cat),fill = as.factor(cat)), size = 0.1)+
  
  scale_fill_manual("Combined Mortality", 
                    breaks = c("Increase", "Decrease"),
                    
                    values = c("#F0E442","#0072B2","#000000","#000000","#000000")) +
  
  #labels = c("Decrease", "Increase"))+
  
  
  
  
  scale_color_manual("", 
                     breaks = c("Increase", "Decrease"),
                     values = c("#F0E442","#0072B2","#000000","#000000","#000000"),
                     
                     guide=FALSE)+ ggtitle ("Change in Mortality\nin Populist counties")+ 
  theme_classic()+
  theme(axis.title.x=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
w2
# make category labels for traditional counties (cat3)


# save both plots as grid
library(ggpubr)
w6<- ggarrange(w1,w2, vjust=2,nrow=2,labels = c("A", "B"))


#m3 <- ggarrange(plot3+ rremove("x.title"), plot3a_s, 
  #              labels = c("A", "B"),
           #     ncol = 1, nrow = 2)
ggsave(w6, filename = "Social capital and mortality county maps.png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


