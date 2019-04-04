library(cowplot)
### First run data from 'new figures' rscript!!!!
### my plots ###########################  Clinton Trump#######
### Based on predictions
facet_plot_main <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
geom_point()+

geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
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

facet_plot_main

# next do inset
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_inset <- ggplot(p3, aes(x=mort,y=trump_percent, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k",  "1 mil"))+
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
  scale_y_continuous(name="",
                     breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="",limits=c(-180,180),
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
 
facet_plot_inset

# make combined plot
inset_1 <-
  ggdraw() +
  draw_plot(facet_plot_main) +
  draw_plot(facet_plot_inset, x = 0.7, y = .00, width = .3, height = .30)
inset_1
## save it
ggsave(filename = "plot.with.inset.png", 
plot = inset_1,
width = 17, 
height = 12,
units = "cm",
dpi = 300)
########################################
########################################
########################################
########################################
########################################
########################################
 # based on draws from the posterior
 #p3$mort_cat<- as.factor(p3$mort_cat)
# label facets
labels=c(alcohol_16_to_8="Alcohol",drugs_16_to_8="Drugs",suicides_16_to_8="Suicides")
facet_plot_main_2 <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
  #geom_point()+
  
  geom_point(aes(size = pop_16))+
  scale_size(name   = "County population",
             breaks = c(10000, 100000, 1000000),
             labels = c("10k", "100k","1 mil"))+
  facet_wrap(~mort_cat, labeller=labeller(mort_cat=labels))+
  geom_abline(data = newdf, mapping=aes(intercept = intercept, slope = mort_change,colour = mort_cat),
              size = 0.1, alpha = 0.1)+
  
  geom_abline(data = newdf2, aes (intercept = model_intercept, slope = model_slope, colour=mort_cat),size=0.2,alpha=0.8)+
  
  geom_vline(xintercept=0, linetype="dotted")+
  
  
  scale_colour_viridis_d(name="Change in per capita\ndeaths by county", alpha=0.4,option="viridis",
                         begin=0, end=0.8,  direction=-1,aesthetics =  "fill",
                         breaks = c("alcohol_16_to_8", "drugs_16_to_8","suicides_16_to_8"),
                         #values=c("alcohol_16_to_8"="pink", "drugs_16_to_8"="blue", "suicides_16_to_8"="black"), 
                         labels = c("Alcohol", "Drugs","Suicides"))+
  
  
  guides(color=guide_legend("Change in Mortality\nPosterior distribution\n(all samples)"))+
  scale_color_hue(labels = c("Alcohol", "Drugs","Suicides"))+
  
  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",breaks=c(0.25,0.5,0.75),limits=c(0.05,0.95),
                     labels=c('25%','50%','75%'))+
  
  
  scale_x_continuous(name="Change in deaths per 100k between 2008 and 2016",limits=c(-75,75),
                     breaks=c(-50,0,50),labels=c("-50","0","50"))+
  

  scale_y_continuous(name="Percentage of votes for\nTrump vs Clinton in 2016",
                     breaks=c(0.48,0.49,0.50),limits=c(0.48,0.50),
                     labels=c('48%','49%','50%'))+
  
  
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

facet_plot_main_2
# inset
# 
# #######################################################
facet_plot_inset_2 <- ggplot(p3, aes(x=mort,y=predicted_trump_votes/total_votes, colour=mort_cat)) + 
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
                     breaks=c(-100,0,100),labels=c("-100","0","100"))+
  
  
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

facet_plot_inset_2

inset_1a <-
  ggdraw() +
  draw_plot(facet_plot_main_2) +
  draw_plot(facet_plot_inset_2, x = 0.7, y = .00, width = .25, height = .28)
inset_1a
## save it
ggsave(filename = "plot.with.inset1a.png", 
       plot = inset_1a,
       width = 17, 
       height = 12,
       units = "cm",
       dpi = 300)
## get the cruz files
M3 <- readRDS("C:/Users/rofrly/Dropbox/Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")





#### Do the posterior dists
# from desktop
M1 <- readRDS("../../Populism ms files/Model results/trump_vs_clinton_full_rstanarm.rds")
M3 <- readRDS("../../Populism ms files/Model results/trump_vs_cruz_full_rstanarm3.rds")
# load packages
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)

# get posteriors and select columns
post_model1 <- extract.samples(M1) %>% as.data.frame()
post_model1 <- post_model1 %>% select (1:7,947)
post_model1 <- post_model1 [c(2,3,4,5,6,7,8)]

post_model2 <- extract.samples(M2) %>% as.data.frame()
post_model2 <- post_model2 %>% select (1:7,947)
post_model2 <- post_model2 [c(2,3,4,5,6,7,8)]


# make figures of posteriors
color_scheme_set("green")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1) 

color_scheme_set("blue")
p2 <- mcmc_areas(post_model2, prob = 0.8, prob_outer =1)

# make titles and plot graphics 
plot_1<- p1 + scale_y_discrete(limits=c("Lotta","Lotta X Age","Reproduced within\nthe last 2 years","Agricultural","Education",
                                        "First child born\nafter 1944",
                                        "Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-2.45,0.75), labels=c("0.13","0.45","0.67","1","1.5"),
                     breaks=c(-2,-0.8,-0.4, 0, 0.4)) +
  ggtitle("Time to reproduction after 1944")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
plot_2<- p2 + scale_y_discrete(limits=c("Lotta","Lotta X Age","Reproduced within\nthe last 2 years","Agricultural","Education",
                                        "First child born\nafter 1944",
                                        "Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-0.5,0.5), labels=c("0.67","1","1.5"),
                     breaks=c(-0.4, 0, 0.4)) +
  ggtitle("Total children birthed after 1944")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_2
# Put marriage plots next to eachother  - This is Figure 2
m_plots <- ggarrange(plot_1,plot_2, labels=c("Time to reproduction after 1944", 
                                             "Total children birthed after 1944"),
                     vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)

