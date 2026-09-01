
rm(list=ls())

# Install packages if needed
install.packages("rstanarm", dependencies=TRUE)

library(rstanarm)

#********************************
# IMPORT DATA
#********************************

wine <- read.csv("winequality-red.csv",
                 header=TRUE,
                 sep=",")

dim(wine)
summary(wine)

#********************************
# SCALE SOME VARIABLES
# (similar to professor example)
#********************************

wine1 <- wine

wine1[,1:11] <- scale(wine1[,1:11])

summary(wine1)

#********************************
# LOOK AT THE DATA
#********************************

names(wine1)

pairs(wine1[,c("quality",
               "alcohol",
               "volatile.acidity",
               "sulphates")])

boxplot(wine1$quality ~ round(wine1$alcohol),
        las=1)

#********************************
# BAYESIAN REGRESSION WITH rstanarm
#********************************

rstan1 <- stan_glm(
  quality ~ alcohol +
    volatile.acidity +
    sulphates +
    citric.acid +
    total.sulfur.dioxide,
  
  data = wine1,
  
  # priors
  prior = normal(0, c(10,10,10,10,10)),
  prior_intercept = normal(0,10),
  
  # prior for sigma
  prior_aux = exponential(1),
  
  algorithm = "sampling",
  
  seed = 222,
  
  chains = 1,
  iter = 10000
)

summary(rstan1)


#The Bayesian regression model showed that alcohol content had the strongest positive association with wine quality, 
#while volatile acidity had a negative association. Sulphates showed a modest positive effect, 
#whereas citric acid had minimal influence on wine quality. 
#All MCMC convergence diagnostics indicated successful model convergence (Rhat ≈ 1).

#********************************
# MCMC OUTPUT
#********************************

rstan1Mcmc <- as.data.frame(rstan1)

dim(rstan1Mcmc)
#5000 rows = 5000 MCMC posterior draws
#7 columns = 7 parameters
#Your parameters are: Intercept, alcohol, volatile.acidity, sulphates, citric.acid, total.sulfur.dioxide, sigma

head(rstan1Mcmc)
#shows the first few posterior samples.

#********************************
# TRACE PLOTS + HISTOGRAMS
#********************************

par(mfrow=c(6,2), mar=c(4,4,1,1))

#plots the posterior distribution of a coefficient.
#checks whether the MCMC chain mixed properly.

plot(rstan1Mcmc[,1],
     type="l",
     ylab=names(rstan1Mcmc)[1])

hist(rstan1Mcmc[,1],
     main=names(rstan1Mcmc)[1])

plot(rstan1Mcmc[,2],
     type="l",
     ylab=names(rstan1Mcmc)[2])

hist(rstan1Mcmc[,2],
     main=names(rstan1Mcmc)[2])

plot(rstan1Mcmc[,3],
     type="l",
     ylab=names(rstan1Mcmc)[3])

hist(rstan1Mcmc[,3],
     main=names(rstan1Mcmc)[3])

plot(rstan1Mcmc[,4],
     type="l",
     ylab=names(rstan1Mcmc)[4])

hist(rstan1Mcmc[,4],
     main=names(rstan1Mcmc)[4])

plot(rstan1Mcmc[,5],
     type="l",
     ylab=names(rstan1Mcmc)[5])

hist(rstan1Mcmc[,5],
     main=names(rstan1Mcmc)[5])

plot(rstan1Mcmc[,6],
     type="l",
     ylab=names(rstan1Mcmc)[6])

hist(rstan1Mcmc[,6],
     main=names(rstan1Mcmc)[6])

#********************************
# POSTERIOR MEANS + SDs
#********************************

apply(rstan1Mcmc, 2, mean)
#The posterior mean estimate 

apply(rstan1Mcmc, 2, sd)
#It measures uncertainty in the parameter estimate.
#posterior distribution for the alcohol coefficient varies by about 0.017 around its mean.

#********************************
# CHECK ASSUMPTIONS
#********************************

dev.off()

#Random “cloud” around 0, No pattern
 
plot(rstan1$residuals)
abline(h=0, col="yellow", lwd = 3)

par(mfrow=c(3,1))

#Points scattered randomly around 0, No curve, funnel, or pattern
plot(wine1$alcohol,
     rstan1$residuals)
abline(h=0, col="yellow")

plot(wine1$volatile.acidity,
     rstan1$residuals)
abline(h=0, col="yellow")

plot(wine1$sulphates,
     rstan1$residuals)
abline(h=0, col="yellow")

#Points fall roughly on the straight line
qqnorm(scale(rstan1$residuals))
abline(a=0, b=1, col="yellow", lwd =3 )

#********************************
# INFERENCE
#********************************

summary(rstan1)

#alcohol = 0.31: higher alcohol → higher quality
#volatile acidity = -0.22: more acidity → lower quality
#citric acid ≈ 0: basically no effect in this model

rstan1$coefficients

#alcohol → clearly positive
#volatile acidity → clearly negative
#citric acid → uncertain (includes 0)

# Posterior summaries
round(rstan1$stan_summary[,c(1,3,4,10)],2)

#********************************
# PROBABILITY COEFFICIENT > 0
#********************************

prBetaAlcoholGt0 <-
  sum(rstan1Mcmc$alcohol > 0) /
  dim(rstan1Mcmc)[1]

prBetaVolatileLt0 <-
  sum(rstan1Mcmc$volatile.acidity < 0) /
  dim(rstan1Mcmc)[1]

prBetaSulphatesGt0 <-
  sum(rstan1Mcmc$sulphates > 0) /
  dim(rstan1Mcmc)[1]

prBetaAlcoholGt0
prBetaVolatileLt0
prBetaSulphatesGt0

#The posterior probability that the coefficient for alcohol is positive is 1.00, 
#indicating essentially all posterior draws suggest a positive association with wine quality. 
#Similarly, volatile acidity has a posterior probability of 1.00 of being negative, 
#and sulphates has a posterior probability of 1.00 of being positive. 
#This provides strong Bayesian evidence for the direction of these effects.

#********************************
# CLASSICAL REGRESSION COMPARISON
#********************************

lm1 <- lm(
  quality ~ alcohol +
    volatile.acidity +
    sulphates +
    citric.acid +
    total.sulfur.dioxide,
  
  data=wine1
)

summary(lm1)

plot(lm1)


# Compare coefficients
cbind(rstan1$coefficients,
      lm1$coefficients)



