

votes<-map(
  alist(
    #Trump votes is a binomial ditribution based on the number of trials (total votes) 
    #for each row of aggregated data with a probability of a Trump vote given as p 
    trump_votes~ dbinom(total_votes, p),
 
    # the model is the logistic function of p equals the intercept plus some slope times
    #the per-capita suicides in each county
    logit(p) <- a + bs*suicides_per_capita,
    
    a ~ dnorm (0,10),
    bs ~ dnorm (0,10)),data=d, start=list(a=0,bm=0))

#Should I use varying intercepts for counties (i.e. let counties vary)? 
#like depts in UC Berkely data
#I think I want counties to be entered as varying intercept--
#data structure of d
  county suicides_per_capita trump_votes hillary_votes total_votes 
1    1            1                 512          313          825  
2    2            2                  89           19          108    
3    3            2                 353          207          560    
4    4           20                  17            8           25    
5    5           19                 120          205          325    
6    6            4                 202          391          593    

# varying intercepts model
d$county_id <- coerce_index(d$county)
vote_vi<- map2stan(
  alist(
    # again the n value or number of trials is the number of applications
    admit~ dbinom(applications, p),
    # indexed intercepts for all 6 depts
    logit(p) <- a[county_id] + bs*suicides_per_capita,
    # the prior for the intercepts mus be indexed too since we are doing them one at a time
    a[dept_id] ~ dnorm (0,10), 
    bm~ dnorm (0,10)
  ), data=d)

# to check if the varying intercept model allowing county to vary is the best model- use the
# comare funtion and see if this model beats the single intercept model
compare(votes, votes_vi)

#Also do a postcheck to see if the single interceot retordicts the data