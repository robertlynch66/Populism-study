#### Do the posterior dists
#### 
# clinton trump figure 1 main plot
library(dplyr)
p<- readRDS("../data files/populism_data_new2.rds")


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



p <- p %>% select ("county","state","white_16_to_10",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", 
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]

# from desktop
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm5.rds")
M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm4.rds")
#OR

M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm7.rds")

M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm7.rds")
# load packages
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(rstanarm)


####### show all predcitors and standardize for comparison
##Trump Clinton (ALL)

###### all posteriors 
posterior <- as.matrix(M1)
posterior <- as.data.frame(posterior)
plot_title <- ggtitle("Trump vs Clinton\nPosterior distributions\nwith medians and 99% intervals")

# rescale and standardize (0 to 1)
posterior$sk_change <- posterior$sk_change*(sd(p$sk2014_over_2005))
posterior$pop_change <- posterior$pop_change*(sd(p$pop_change_16_to_10))
posterior$median_hh_income_change <- posterior$median_hh_income_change*(sd(p$median_hh_income_16_to_10))
posterior$male_unemplmt_change <- posterior$male_unemplmt_change*(sd(p$male_unemplmt_16_to_10))
posterior$female_unemplmt_change <- posterior$female_unemplmt_change*(sd(p$female_unemplmt_16_to_10))
posterior$for_born_change <- posterior$for_born_change*(sd(p$for_born_16_to_10))
posterior$alcohol_change <- posterior$alcohol_change*(sd(p$alcohol_16_to_8))
posterior$drugs_change <- posterior$drugs_change*(sd(p$drugs_16_to_8))
posterior$suicides_change <- posterior$suicides_change*(sd(p$suicides_16_to_8))
posterior$perc_bachelors_change <- posterior$perc_bachelors_change*(sd(p$bachelors_16_to_10))
posterior$white_change <- posterior$white_change*(sd(p$white_16_to_10))

posterior <- posterior[,2:12]


color_scheme_set("green")
p1a <- mcmc_intervals(posterior,
                     regex_pars = c("suicides_change","drugs_change","alcohol_change","sk_change",
                                    "female_unemplmt_change","male_unemplmt_change","median_hh_income_change",
                                    "perc_bachelors_change","for_born_change","white_change","pop_change"), prob = 0.99, prob_outer = 1) 

p1a 


plot_1a<- p1a + scale_y_discrete(limits=c("pop_change","white_change","for_born_change","perc_bachelors_change","median_hh_income_change",
                                          "male_unemplmt_change","female_unemplmt_change","sk_change",
                                          "alcohol_change","drugs_change","suicides_change"),labels=c("Population",
                                                                                                      "White",
                                                                                                      "Foreign born",
                                                                                                      "Bachelor degree",
                                                                                                      "Median household income",
                                                                                                      "Male unemployment",
                                                                                                      "Female unemployment",
                                                                                                      "Social Capital index",
                                                                                                      "Alchohol deaths",
                                                                                                      "Drug deaths",
                                                                                                      "Suicides"))+
                                        
  scale_x_continuous(name="Odd ratios\n[1 SD change in predictor]",limits=c(-0.4,0.5), breaks=c(-0.35,-0.20,-0.1,0,0.1,0.2,0.4), 
                     labels=c("0.7","0.8","0.9","1","1.1","1.2","1.5"))+
  plot_title +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))

plot_1a

ggsave(plot_1a, filename = "Figure S1a.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")



########################
###  sanders clinton
###  #################
###  
###  ###################
posterior <- as.matrix(M2)
posterior <- as.data.frame(posterior)
plot_title <- ggtitle("Sanders vs Clinton\nPosterior distributions\nwith medians and 99% intervals")

# rescale and standardize (0 to 1)
posterior$sk_change <- posterior$sk_change*(sd(p$sk2014_over_2005))
posterior$pop_change <- posterior$pop_change*(sd(p$pop_change_16_to_10))
posterior$median_hh_income_change <- posterior$median_hh_income_change*(sd(p$median_hh_income_16_to_10))
posterior$male_unemplmt_change <- posterior$male_unemplmt_change*(sd(p$male_unemplmt_16_to_10))
posterior$female_unemplmt_change <- posterior$female_unemplmt_change*(sd(p$female_unemplmt_16_to_10))
posterior$for_born_change <- posterior$for_born_change*(sd(p$for_born_16_to_10))
posterior$alcohol_change <- posterior$alcohol_change*(sd(p$alcohol_16_to_8))
posterior$drugs_change <- posterior$drugs_change*(sd(p$drugs_16_to_8))
posterior$suicides_change <- posterior$suicides_change*(sd(p$suicides_16_to_8))
posterior$perc_bachelors_change <- posterior$perc_bachelors_change*(sd(p$bachelors_16_to_10))
posterior$white_change <- posterior$white_change*(sd(p$white_16_to_10))

posterior <- posterior[,2:12]

color_scheme_set("green")
p2a <- mcmc_intervals(posterior,
                      regex_pars = c("suicides_change","drugs_change","alcohol_change","sk_change",
                                     "female_unemplmt_change","male_unemplmt_change","median_hh_income_change",
                                     "perc_bachelors_change","for_born_change","white_change","pop_change"), prob = 0.99, prob_outer = 1) 

p2a 


plot_2a<- p2a + scale_y_discrete(limits=c("pop_change","white_change","for_born_change","perc_bachelors_change","median_hh_income_change",
                                          "male_unemplmt_change","female_unemplmt_change","sk_change",
                                          "alcohol_change","drugs_change","suicides_change"),labels=c("Population",
                                                                                                      "White",
                                                                                                      "Foreign born",
                                                                                                      "Bachelor degree",
                                                                                                      "Median household income",
                                                                                                      "Male unemployment",
                                                                                                      "Female unemployment",
                                                                                                      "Social Capital index",
                                                                                                      "Alchohol deaths",
                                                                                                      "Drug deaths",
                                                                                                      "Suicides"))+
  
  scale_x_continuous(name="Odd ratios\n[1 SD change in predictor]",limits=c(-0.1,0.15), breaks=c(-0.1,0,0.1), 
                     labels=c("0.90","1","1.1"))+
  plot_title+
  
   theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
      axis.ticks.x = element_line(size = 1),
      axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
      axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
      axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
      plot.title = element_text(hjust = 0.5))



ggsave(plot_2a, filename = "Figure S1b.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")

### trump cruz
### 
### #################
posterior <- as.matrix(M3)
posterior <- as.data.frame(posterior)
plot_title <- ggtitle("Trump vs Cruz\nPosterior distributions\nwith medians and 99% intervals")

# rescale and standardize (0 to 1)
posterior$sk_change <- posterior$sk_change*(sd(p$sk2014_over_2005))
posterior$pop_change <- posterior$pop_change*(sd(p$pop_change_16_to_10))
posterior$median_hh_income_change <- posterior$median_hh_income_change*(sd(p$median_hh_income_16_to_10))
posterior$male_unemplmt_change <- posterior$male_unemplmt_change*(sd(p$male_unemplmt_16_to_10))
posterior$female_unemplmt_change <- posterior$female_unemplmt_change*(sd(p$female_unemplmt_16_to_10))
posterior$for_born_change <- posterior$for_born_change*(sd(p$for_born_16_to_10))
posterior$alcohol_change <- posterior$alcohol_change*(sd(p$alcohol_16_to_8))
posterior$drugs_change <- posterior$drugs_change*(sd(p$drugs_16_to_8))
posterior$suicides_change <- posterior$suicides_change*(sd(p$suicides_16_to_8))
posterior$perc_bachelors_change <- posterior$perc_bachelors_change*(sd(p$bachelors_16_to_10))
posterior$white_change <- posterior$white_change*(sd(p$white_16_to_10))
posterior <- posterior[,2:12]


color_scheme_set("green")
p3a <- mcmc_intervals(posterior,
                      regex_pars = c("suicides_change","drugs_change","alcohol_change","sk_change",
                                     "female_unemplmt_change","male_unemplmt_change","median_hh_income_change",
                                     "perc_bachelors_change","for_born_change","white_change","pop_change"), prob = 0.99, prob_outer = 1) 



plot_3a<- p3a + scale_y_discrete(limits=c("pop_change","white_change","for_born_change","perc_bachelors_change","median_hh_income_change",
                                          "male_unemplmt_change","female_unemplmt_change","sk_change",
                                          "alcohol_change","drugs_change","suicides_change"),labels=c("Population",
                                                                                                      "White",
                                                                                                      "Foreign born",
                                                                                                      "Bachelor degree",
                                                                                                      "Median household income",
                                                                                                      "Male unemployment",
                                                                                                      "Female unemployment",
                                                                                                      "Social Capital index",
                                                                                                      "Alchohol deaths",
                                                                                                      "Drug deaths",
                                                                                                      "Suicides"))+
  
  scale_x_continuous(name="Odd ratios\n[1 SD change in predictor]",limits=c(-0.1,0.1), breaks=c(-0.1,0,0.1), 
                     labels=c("0.9","0","1.1"))+
  plot_title +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))

plot_3a

ggsave(plot_3a, filename = "Figure S1c.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
### trump kasich
### 
### #################
color_scheme_set("green")
posterior <- as.matrix(M4)
posterior <- as.data.frame(posterior)
plot_title <- ggtitle("Trump vs Kasich\nPosterior distributions\nwith medians and 99% intervals")

# rescale and standardize (0 to 1)
posterior$sk_change <- posterior$sk_change*(sd(p$sk2014_over_2005))
posterior$pop_change <- posterior$pop_change*(sd(p$pop_change_16_to_10))
posterior$median_hh_income_change <- posterior$median_hh_income_change*(sd(p$median_hh_income_16_to_10))
posterior$male_unemplmt_change <- posterior$male_unemplmt_change*(sd(p$male_unemplmt_16_to_10))
posterior$female_unemplmt_change <- posterior$female_unemplmt_change*(sd(p$female_unemplmt_16_to_10))
posterior$for_born_change <- posterior$for_born_change*(sd(p$for_born_16_to_10))
posterior$alcohol_change <- posterior$alcohol_change*(sd(p$alcohol_16_to_8))
posterior$drugs_change <- posterior$drugs_change*(sd(p$drugs_16_to_8))
posterior$suicides_change <- posterior$suicides_change*(sd(p$suicides_16_to_8))
posterior$perc_bachelors_change <- posterior$perc_bachelors_change*(sd(p$bachelors_16_to_10))
posterior$white_change <- posterior$white_change*(sd(p$white_16_to_10))

posterior <- posterior[,2:12]


color_scheme_set("green")
p4a <- mcmc_intervals(posterior,
                      regex_pars = c("suicides_change","drugs_change","alcohol_change","sk_change",
                                     "female_unemplmt_change","male_unemplmt_change","median_hh_income_change",
                                     "perc_bachelors_change","for_born_change","white_change","pop_change"), prob = 0.99, prob_outer = 1) 



plot_4a<- p4a + scale_y_discrete(limits=c("pop_change","white_change","for_born_change","perc_bachelors_change","median_hh_income_change",
                                          "male_unemplmt_change","female_unemplmt_change","sk_change",
                                          "alcohol_change","drugs_change","suicides_change"),labels=c("Population",
                                                                                                      "White",
                                                                                                      "Foreign born",
                                                                                                      "Bachelor degree",
                                                                                                      "Median household income",
                                                                                                      "Male unemployment",
                                                                                                      "Female unemployment",
                                                                                                      "Social Capital index",
                                                                                                      "Alchohol deaths",
                                                                                                      "Drug deaths",
                                                                                                      "Suicides"))+
  
  scale_x_continuous(name="Odd ratios\n[1 SD change in predictor]",limits=c(-0.3,0.2), breaks=c(-0.28,-0.1,0,0.1,0.18), 
                     labels=c("0.75","0.90","1","1.1","1.2"))+
  plot_title +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))

plot_4a

ggsave(plot_4a, filename = "Figure S1d.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")


#### posterior predictive checks
#### ################################
#### #################################
#### Trump vs Clinton
#### ####################################
# pp_check function graphically compares observed to model predictions
# # Scatterplot of y vs. average yrep
# 
# I am pretty sure these are the county predictions vs the county observed
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(rstanarm)
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm5.rds")
M2 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm4.rds")
M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm7.rds")
M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm7.rds")
color_scheme_set('viridis')
p1a <- pp_check(M1, plotfun = "scatter_avg")+ scale_y_continuous(name="Observed", 
                                                                 breaks=c(0.25,0.5,0.75),
                                                                 labels=c('25%','50%','75%')) + 
  scale_x_continuous(name="Predicted", 
                     breaks=c(0.25,0.5,0.75),
                     labels=c('25%','50%','75%')) + 
  ggtitle("Posterior Predictive Check\nPercentage of Votes for Trump vs Clinton\nby County")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # add text r=0.55 
  annotate("text", x = 0.8, y = 0.2, label = "r=0.55", colour="red")
p1a

ggsave(p1a, filename = "Figure S2a.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# get correlation
a <- posterior_predict(M1)
a <- as.data.frame(a)
a <- apply(a,2,mean) %>% as.data.frame()
# get the trump clinton dataframe (p)
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
p<- readRDS("../data files/populism_data_new2.rds")

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



# make dv's integers
p$trump_votes <- as.integer(p$trump_16)
p$clinton_votes <- as.integer(p$clinton_16)


p <- p %>% select ("county","state","trump_votes","clinton_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]
#bayesplot::ppc_scatter_avg(y = p$trump_primary_votes, yrep = posterior_predict(M1))
a$preds <- a
names(a) <-c("preds","preds")
z <- cbind(a,p)
z$total_votes <- z$trump_votes+z$clinton_votes
z$predicted <- z$preds/z$total_votes
z$observed <- z$trump_votes/z$total_votes
cor.test(z$predicted,z$observed) # r= 0.37
##########################################################################################################
##########################################################################################################
##############################Sanders vs Clinton##############################################
##############################
#################################################################################################
color_scheme_set('viridis')
p1b <- pp_check(M2, plotfun = "scatter_avg")+ scale_y_continuous(name="Observed", 
                                                                 breaks=c(0.25,0.5,0.75),
                                                                 labels=c('25%','50%','75%')) + 
  scale_x_continuous(name="Predicted", 
                     breaks=c(0.25,0.5,0.75),
                     labels=c('25%','50%','75%')) + 
  ggtitle("Posterior Predictive Check\nPercentage of Votes for Sanders vs Clinton\nby County")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # add text r=0.55 
  annotate("text", x = 0.8, y = 0.2, label = "r=0.80", colour="red")


ggsave(p1b, filename = "Figure S2b.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# get correlation
a <- posterior_predict(M2)
a <- as.data.frame(a)
a <- apply(a,2,mean) %>% as.data.frame()
# get the sanders clinton dataframe (p)
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
p<- readRDS("../data files/populism_data_new2.rds")

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



# make dv's integers
p$sanders_votes <- as.integer(p$sanders_primary1)
p$clinton_votes <- as.integer(p$clinton_primary1)


p <- p %>% select ("county","state","sanders_votes","clinton_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]
#bayesplot::ppc_scatter_avg(y = p$trump_primary_votes, yrep = posterior_predict(M1))
a$preds <- a
names(a) <-c("preds","preds")
z <- cbind(a,p)
z$total_votes <- z$sanders_votes+z$clinton_votes
z$predicted <- z$preds/z$total_votes
z$observed <- z$sanders_votes/z$total_votes
cor.test(z$predicted,z$observed) # r= 0.80
##########################################################################################################
##########################################################################################################
############################Cruz vs Trump ################################################################
##########################################################################################################color_scheme_set('viridis')
color_scheme_set('viridis')
p1c <- pp_check(M3, plotfun = "scatter_avg")+ scale_y_continuous(name="Observed", 
                                                                 breaks=c(0.25,0.5,0.75),
                                                                 labels=c('25%','50%','75%')) + 
  scale_x_continuous(name="Predicted", 
                     breaks=c(0.25,0.5,0.75),
                     labels=c('25%','50%','75%')) + 
  ggtitle("Posterior Predictive Check\nPercentage of Votes for Trump vs Cruz\nby County")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # add text r=0.55 
  annotate("text", x = 0.8, y = 0.2, label = "r=0.93", colour="red")


ggsave(p1c, filename = "Figure S2c.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# get correlation
a <- posterior_predict(M3)
a <- as.data.frame(a)
a <- apply(a,2,mean) %>% as.data.frame()
# get the trump cruz dataframe (p)
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
p<- readRDS("../data files/populism_data_new2.rds")

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

# make dv's integers
p$cruz_votes <- as.integer(p$cruz_votes)
p$trump_votes <- as.integer(p$trump_primary_votes)


p <- p %>% select ("county","state","trump_votes","cruz_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]
#bayesplot::ppc_scatter_avg(y = p$trump_primary_votes, yrep = posterior_predict(M1))
a$preds <- a
names(a) <-c("preds","preds")
z <- cbind(a,p)
z$total_votes <- z$trump_votes+z$cruz_votes
z$predicted <- z$preds/z$total_votes
z$observed <- z$trump_votes/z$total_votes
cor.test(z$predicted,z$observed) # r=
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
color_scheme_set('viridis')
p1d <- pp_check(M4, plotfun = "scatter_avg")+ scale_y_continuous(name="Observed", 
                                                                 breaks=c(0.25,0.5,0.75),
                                                                 labels=c('25%','50%','75%')) + 
  scale_x_continuous(name="Predicted", 
                     breaks=c(0.25,0.5,0.75),
                     labels=c('25%','50%','75%')) + 
  ggtitle("Posterior Predictive Check\nPercentage of Votes for Trump vs Kasich\nby County")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # add text r=0.55 
  annotate("text", x = 0.8, y = 0.2, label = "r=0.85", colour="red")


ggsave(p1d, filename = "Figure S2d.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# get correlation
a <- posterior_predict(M1)
a <- as.data.frame(a)
a <- apply(a,2,mean) %>% as.data.frame()
# get the kasich dataframe (p)
library(dplyr)
library(rstanarm)
# path to the folder with the R data files
p<- readRDS("../data files/populism_data_new2.rds")

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



# make dv's integers
p$trump_primary_votes <- as.integer(p$trump_primary_votes)+1
p$kasich_votes <- as.integer(p$kasich_votes)+1


p <- p %>% select ("county","state","trump_primary_votes","kasich_votes",
                   "sk2014_over_2005","pop_change_16_to_10","median_hh_income_16_to_10","bachelors_16_to_10", 
                   "male_unemplmt_16_to_10","female_unemplmt_16_to_10","for_born_16_to_10", "white_16_to_10",
                   "alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8")
p <- p[complete.cases(p), ]
#bayesplot::ppc_scatter_avg(y = p$trump_primary_votes, yrep = posterior_predict(M1))
a$preds <- a
names(a) <-c("preds","preds")
z <- cbind(a,p)
z$total_votes <- z$trump_primary_votes+z$kasich_votes
z$predicted <- z$preds/z$total_votes
z$observed <- z$trump_primary_votes/z$total_votes
cor.test(z$predicted,z$observed)
#######################trace plots#####################################
#######################
#Clinton trump
library(bayesplot)
library("rstan")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
#M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
posterior <- as.array(M1)
dim(posterior)

dimnames(posterior)
posterior <- posterior[1:3500, 1:4, 2:11]
dimnames(posterior)$parameters <- c('Social\nCapital','Population','Income','Bachelors',
                                    'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides')
#color_scheme_set("mix-blue-red")
p1 <- mcmc_trace(posterior, pars = c('Social\nCapital','Population','Income','Bachelors',
                                     'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides'), 
           facet_args = list(ncol = 1, strip.position = "left"))


color_scheme_set("viridis")

plot_1<- p1 + 
  
  ggtitle("Trace plots for Trump Clinton")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=6,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
ggsave(plot_1, filename = "Figure S3a (Trace plots).png", width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

##
#Clinton sanders
library(bayesplot)
library("rstan")
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
#M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
posterior <- as.array(M1)
dim(posterior)

dimnames(posterior)
posterior <- posterior[1:3500, 1:4, 2:11]
dimnames(posterior)$parameters <- c('Social\nCapital','Population','Income','Bachelors',
                                    'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides')
#color_scheme_set("mix-blue-red")
p2 <- mcmc_trace(posterior, pars = c('Social\nCapital','Population','Income','Bachelors',
                                     'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides'), 
                 facet_args = list(ncol = 1, strip.position = "left"))


color_scheme_set("viridis")

plot_2<- p2 + 
  
  ggtitle("Trace plots for sanders Clinton")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=6,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_2
ggsave(plot_2, filename = "Figure S3b (Trace plots).png", width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

# trump cruz trace plots
#Clinton trump
library(bayesplot)
library("rstan")
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
#M4 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
posterior <- as.array(M1)
dim(posterior)

dimnames(posterior)
posterior <- posterior[1:3500, 1:4, 2:11]
dimnames(posterior)$parameters <- c('Social\nCapital','Population','Income','Bachelors',
                                    'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides')
#color_scheme_set("mix-blue-red")
p1 <- mcmc_trace(posterior, pars = c('Social\nCapital','Population','Income','Bachelors',
                                     'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides'), 
                 facet_args = list(ncol = 1, strip.position = "left"))


color_scheme_set("viridis")

plot_1<- p1 + 
  
  ggtitle("Trace plots for Trump Cruz")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=6,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
ggsave(plot_1, filename = "Figure S3c (Trace plots).png", width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

#kasich trump
library(bayesplot)
library("rstan")
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/sanders_vs_clinton_change_only_rstanarm3.rds")
#M1 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_change_only_rstanarm3.rds")
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_kasich_change_only_rstanarm3.rds")
posterior <- as.array(M1)
dim(posterior)

dimnames(posterior)
posterior <- posterior[1:3500, 1:4, 2:11]
dimnames(posterior)$parameters <- c('Social\nCapital','Population','Income','Bachelors',
                                    'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides')
#color_scheme_set("mix-blue-red")
p1 <- mcmc_trace(posterior, pars = c('Social\nCapital','Population','Income','Bachelors',
                                     'Male\nUnemp','Female\nUnemp','Foreign\nBorn','Alcohol','Drugs','Suicides'), 
                 facet_args = list(ncol = 1, strip.position = "left"))


color_scheme_set("viridis")

plot_1<- p1 + 
  
  ggtitle("Trace plots for Trump Kasich")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=6,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
ggsave(plot_1, filename = "Figure S3d (Trace plots).png", width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")