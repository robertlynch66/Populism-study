suppressMessages(local({
  library(dplyr)
  library(ggplot2)
  library(survey)
  library(knitr)
  library(tidyr)
  library(broom)
}))

set.seed(666)

N <- 30 # number of observations

# Aggregated data
aggregated <- data.frame(x=1:5) %>%
  mutate( y = round(2 * x + 2 + rnorm(length(x)) ),
          freq = as.numeric(table(sample(1:5, N, 
                                         replace=TRUE, prob=c(.3, .4, .5, .4, .3))))
  )
aggregated

# Disaggregated data
individuals <- aggregated[ rep(1:5, aggregated$freq) , c("x", "y") ]

models <- list( 
  ind_lm = lm(y ~ x, data=individuals),
  raw_agg = lm( y ~ x, data=aggregated),
  ind_svy_glm = svyglm(y~x, design=svydesign(id=~1, data=individuals),
                       family=gaussian() ),
  ind_glm = glm(y ~ x, family=gaussian(), data=individuals),
  wei_lm = lm(y ~ x, data=aggregated, weight=freq),
  wei_glm = glm(y ~ x, data=aggregated, family=gaussian(), weight=freq),
  svy_glm = svyglm(y ~ x, design=svydesign(id=~1, weights=~freq, data=aggregated),
                   family=gaussian())
)

results <- do.call("rbind", lapply( names(models), function(n) cbind(model=n, tidy(models[[n]])) )) %>%
  gather(stat, value, -model, -term)

results %>% filter(stat=="estimate") %>% 
  select(model, term, value) %>%
  spread(term, value)

# Standard Errors
results %>% filter(stat=="std.error") %>%
  select(model, term, value) %>%
  spread(term, value)
# p-values
results %>% filter(stat=="p.value") %>%
  mutate(p=format.pval(value)) %>%
  select(model, term, p) %>%
  spread(term, p)

library(rethinking)

glimmer(trump_vote ~ year*population + year*sucides + year*income + (1|county_id/state_id)+ data=d)


data(cars)
glimmer( dist ~ speed , data=cars)

glimmer( trump_16~ perc_hisp * condition - condition ,
         data=chimpanzees , family=binomial )

library(rethinking)
data(UCBadmit)
d <- UCBadmit

glimmer( cbind(admit,reject) ~ 1 , data=d , family=binomial )

## aggregated binomial


data <- full_pop_data %>% select (3,4,26,29,115,120,121)
data2<- data2[complete.cases(data2),]
data2 <- data2 %>% arrange(state_id)
data2$state_id_seq <- cumsum(c(1,as.numeric(diff(data2$state_id)) != 0))
data2 <- data2 %>% arrange(county_id)
data2$county_id_seq <- cumsum(c(1,as.numeric(diff(data2$county_id)) != 0))

glimmer( cbind(trump_16,clinton_16) ~ year*perc_white + year*perc_hisp +(1|state_id_seq)+(1|county_id_seq), data=data2 , family=binomial )


# in chimpanzee model 
#the main effect of condition is taken out because there is no expectation that a chimp sitting on the other side of the table by itself will affect
#a chimps likelihood of pulling left (see below)
glimmer( pulled_left ~ prosoc_left * condition - condition ,
      data=chimpanzees , family=binomial )

glimmer (cbind(trump_16,clinton_16) ~ year*perc_white)

### the brms package
### # the lme4 template for glms
response ~ pterms + (gterms | group)
library(brms)
library(rstan)
# its just turtles in brms - bayesoan models - add models to your models
#y ~ b1 * (1 - exp(-(x / b2) ^ b3)
 #         b1 ~ z + (1|ID|g)
  #        b2 ~ (1|ID|g)
    #      b3 ~ (1|ID|g)
#The : operator creates a new grouping factor that consists of the
# combined levels of g1 and g2. The / operator indicates nested grouping structures and expands one
# grouping factor into two or more when using multiple / within one term. For instance, if the user
# were to write (1 | g1/g2), brms will expand this to (1 | g1) + (1 | g1:g2). 
# 
trump_percent ~ pterms + (1|state_id_seq/county_id_seq)

# beta distribution available in brms
Beta(link = "logit", link_phi = "log")

# s1 is school 1, s2 is school 2
fit_mm <- brm(y ~ 1 + (1 | mm(s1, s2)), data = data_mm)
fit_mymodel <- brm (trump_percent ~ 1 + (1 | mm(year1, year2)), data=data)

# add model weights
fit_mymmodel2 <- brm(trump_percent ~ 1+ (1 | mm(year1, year2, weights = cbind(weight1, weight2))),
data = data)

data$tot <- data$clinton_16+data$trump_16
# try to regain the intercept here  - This works now I think!!!
p <- brm(trump_perc_16|weights(tot) ~ social_capital*year + sucides*year + (1|state_id_seq/county_id_seq),
                     data = data, Beta(link = "logit", link_phi = "log"))


# without weights the above produces an intercept of 0.66 - the inv.logit of which is 0.66
# with weights the above produces an intercept of -0.06 - the inv.logit of which is 0.4850045!!! This is correct
# 
# What brm does with the weight: It is not the "posteriors" that are weighted but the likelihood contributions of each observation.

# brms takes the weights literally, which means that an observation with weight 2 receives 2 times more weight than 
# an observation with weight 1.
# It also means that using a weight of 2 is equivalent to adding the corresponding observation 
# twice to the data frame.

# get actual perecntage of votes for Trump
sum(data$trump_16)/sum(data$trump_16+data$clinton_16)
# this equals 0.4864583

# # on wikipedia 62,984,828 for Trump	65,853,514 fro clinton
# this equals  0.4888671
### get the intercept back from the weights model
 library(rethinking)
library(betareg)
library(brms)
library(boot)

mean(data$perc_asian, na.rm=T)
data$binary <- ifelse(data$perc_asian<1.4,'0','1')
mean(data$perc_asian, na.rm=T)

data$tot <- data$clinton_16+data$trump_16
model <- betareg(trump_perc_16 ~ binary,
                          weights=tot,
                          data = data)

summary(model)

model <- betareg(trump_perc_16 ~ 1,
                # weights=tot,
                 data = data)

summary(model)

# Call:
#   betareg(formula = trump_perc_16 ~ 1, data = data, weights = tot)
# 
# Standardized weighted residuals 2:
#   Min         1Q     Median         3Q        Max 
# -2401.5868    45.3333    96.5196   158.1101   669.1909 
# 
# Coefficients (mean model with logit link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.0579150  0.0000624  -928.1   <2e-16 ***
#   
#   Phi coefficients (precision model with identity link):
#   Estimate Std. Error z value Pr(>|z|)    
# (phi) 6.7391984  0.0007776    8667   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
inv.logit(-0.0579150)


# this equals 48.55253% which is very close to 48.64583% which is what the actual data show
