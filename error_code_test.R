data("womensrole", package = "HSAUR3")
womensrole$total <- womensrole$agree + womensrole$disagree
library(rstanarm)
CORES <- 4
SEED <- 12345
CHAINS <- 4
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"), 
                              prior = student_t(df = 7), 
                              prior_intercept = student_t(df = 7),
                              chains = CHAINS, cores = CORES, seed = SEED)

library(shinystan)
launch_shinystan(womensrole_bglm_1)