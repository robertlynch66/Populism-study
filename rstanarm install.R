library(rstanarm) 

D <- data.frame(S=rep(c("M", "F"), each=3), 
                X=rep(1:3, each=2), 
                K=c(10, 20, 30, 40, 50, 60), 
                N=c(120, 100, 80, 60, 120, 60))

M <- stan_glmer(cbind(K, N-K) ~ X + (1|S), data=D, family=binomial) 



posterior_predict(M, newdata=data.frame(X=4,K=1, N=2), re.form=~0) 


library(rstanarm) 

D <- data.frame(S=rep(c("M", "F"), each=3), 
                X=rep(1:3, each=2), 
                K=c(10, 20, 30, 40, 50, 60), 
                N=c(120, 100, 80, 60, 120, 60)) 

M <- stan_glm(cbind(K, N-K) ~ X , data=D, family=binomial) 

# What counts are these? 
posterior_predict(M, newdata=data.frame(X=4), re.form=~0) 



model <- stan_glmer(formula = cbind(trump_votes, clinton_16)) ~ 1
glimmer(glm(formula = cbind(trump_votes, total_votes_16) ~ 1) )                   
                      
            

library(rethinking)
data(UCBadmit)

f3 <- cbind(admit,reject) ~ (1|dept) + applicant.gender
m3 <- glimmer( f3 , UCBadmit , binomial )
trump_votes ~ dbinom(total_votes_16, p)









### rstandram vignette

data("womensrole", package = "HSAUR3")
womensrole$total <- womensrole$agree + womensrole$disagree
womensrole_glm_1 <- glm(cbind(agree, disagree) ~ education + gender,
                        data = womensrole, family = binomial(link = "logit"))
round(coef(summary(womensrole_glm_1)), 3)


library(rstanarm)
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"), 
                              prior = student_t(df = 7), 
                              prior_intercept = student_t(df = 7),
                              chains = 1, cores = 1)
womensrole_bglm_1


ci95 <- posterior_interval(womensrole_bglm_1, prob = 0.95, pars = "education")
round(ci95, 2)

cbind(Median = coef(womensrole_bglm_1), MAD_SD = se(womensrole_bglm_1))
summary(residuals(womensrole_bglm_1)) # not deviance residuals
cov2cor(vcov(womensrole_bglm_1))

launch_shinystan(womensrole_bglm_1)

y_rep <- posterior_predict(womensrole_bglm_1)
dim(y_rep)
#  make a plot
par(mfrow = 1:2, mar = c(5,3.7,1,0) + 0.1, las = 3)
boxplot(sweep(y_rep[,womensrole$gender == "Male"], 2, STATS = 
                womensrole$total[womensrole$gender == "Male"], FUN = "/"), 
        axes = FALSE, main = "Male", pch = NA,
        xlab = "Years of Education", ylab = "Proportion of Agrees")
with(womensrole, axis(1, at = education[gender == "Male"] + 1, 
                      labels = 0:20))
axis(2, las = 1)
with(womensrole[womensrole$gender == "Male",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))
boxplot(sweep(y_rep[,womensrole$gender == "Female"], 2, STATS = 
                womensrole$total[womensrole$gender == "Female"], FUN = "/"), 
        axes = FALSE, main = "Female", pch = NA,
        xlab = "Years of Education", ylab = "")
with(womensrole, axis(1, at = education[gender == "Female"] + 1,
                      labels = 0:20))


## this is the key line - we must explicitly enter the number of trials in the new data
newdata <- data.frame(agree = c(0,0), disagree = c(100,100), education = c(12,16),
                      gender = factor("Female", levels = c("Male", "Female")))
y_rep <- posterior_predict(womensrole_bglm_2, newdata)
summary(apply(y_rep, 1, diff))
with(womensrole[womensrole$gender == "Female",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))