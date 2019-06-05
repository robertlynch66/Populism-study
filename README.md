"# Populism-study" 
---
title: "README.md"
author: "Robert Lynch"
date: "June 5, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# Installation of dplyr and rstanarm
```{r install packages}
install.packages('dplyr')
install.packages('rstanarm')
library(dplyr)
library(rstanarm)
```

## Usage on random sample of three small counties to make sure everything is working
### reading in sample data, runinng model and summarizing output in 'simple example.R' (see below) should take less than 30 seconds
Read in random sample of actual data (3 rows from data frame - loving, texas; clark, idaho and greenlee, arizona)
- from dataframe 'sample.rds'
See 'simple example.R' for code

Next make data list from data frame

- use these data to run 'map2stan' model in rethinking

Code in 'simple example.R' file uses 1 chains with 200 iterations and 500 warmup iterations.

Warnings will be shown but code should run if rsranarm was properly installed in previous step. 
Also 9 coefficents will be dropped dues to lack of data
```{r example chains}
fixed-effect model matrix is rank deficient so dropping 9 columns / coefficients

SAMPLING FOR MODEL 'binomial' NOW (CHAIN 1).
Chain 1: 
Chain 1: Gradient evaluation took 0 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
Chain 1: Adjust your expectations accordingly!
Chain 1: 
Chain 1: 
Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1: Iteration:  501 / 2000 [ 25%]  (Sampling)
Chain 1: Iteration:  700 / 2000 [ 35%]  (Sampling)
Chain 1: Iteration:  900 / 2000 [ 45%]  (Sampling)
Chain 1: Iteration: 1100 / 2000 [ 55%]  (Sampling)
Chain 1: Iteration: 1300 / 2000 [ 65%]  (Sampling)
Chain 1: Iteration: 1500 / 2000 [ 75%]  (Sampling)
Chain 1: Iteration: 1700 / 2000 [ 85%]  (Sampling)
Chain 1: Iteration: 1900 / 2000 [ 95%]  (Sampling)
Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
Chain 1: 
Chain 1:  Elapsed Time: 1.424 seconds (Warm-up)
Chain 1:                3.419 seconds (Sampling)
Chain 1:                4.843 seconds (Total)
Chain 1: 
Warning messages:
1: There were 11 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
2: Examine the pairs() plot to diagnose sampling problems
```
-expected output
```{r output}
> summary(model)

Model Info:

 function:     stan_glmer
 family:       binomial [logit]
 formula:      cbind(trump_votes, clinton_votes) ~ sk_change + pop_change + 
	   white_16_to_10 + median_hh_income_change + perc_bachelors_change + 
	   male_unemplmt_change + female_unemplmt_change + for_born_change + 
	   alcohol_change + drugs_change + suicides_change + (1 | state_id)
 algorithm:    sampling
 priors:       see help('prior_summary')
 sample:       1500 (posterior sample size)
 observations: 3
 groups:       state_id (3)

Estimates:
                                          mean   sd    2.5%   25%   50%   75%   97.5%
(Intercept)                               1.3    0.7  -0.3    1.0   1.3   1.6   3.0  
sk_change                                -0.1    0.1  -0.3   -0.1  -0.1   0.0   0.2  
pop_change                               -8.2    8.5 -26.6  -11.8  -8.7  -4.9  11.5  
b[(Intercept) state_id:arizona]          -0.1    0.7  -1.7   -0.3   0.0   0.3   1.4  
b[(Intercept) state_id:idaho]             0.0    0.8  -1.9   -0.2   0.0   0.3   1.9  
b[(Intercept) state_id:texas]             0.0    0.9  -1.8   -0.3   0.0   0.3   2.3  
Sigma[state_id:(Intercept),(Intercept)]   1.0    2.0   0.0    0.0   0.3   1.1   6.1  
mean_PPD                                718.2   13.0 692.8  709.3 718.7 726.7 743.2  
log-posterior                           -22.3    2.0 -27.1  -23.5 -22.0 -20.8 -19.4  

Diagnostics:
                                        mcse Rhat n_eff
(Intercept)                             0.0  1.0   479 
sk_change                               0.0  1.0   654 
pop_change                              0.4  1.0   522 
b[(Intercept) state_id:arizona]         0.0  1.0   710 
b[(Intercept) state_id:idaho]           0.0  1.0   475 
b[(Intercept) state_id:texas]           0.0  1.0   544 
Sigma[state_id:(Intercept),(Intercept)] 0.1  1.0   374 
mean_PPD                                0.3  1.0  1455 
log-posterior                           0.1  1.0   463 
```
#### Full models
For Full models run on full data.  First read in data frame 'populism_data_new2.rds' and for all models use the
'All Final Bayesian models.R' file which is extensively commented and will take approximately 7-12 days for each of the 4 models shown (**includes Cruz-Trump model) with 10,000 iterations including a 20% warmup on 4 chains.