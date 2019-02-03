data = read.csv(file = ".\\Statisztika\\datasets\\Bwages.csv", header = TRUE)
head(data)

############################################################################### 1 Understand the problem

############################################################################### 2 Plan and properly collect relevant data

############################################################################### 3 Explore data

plot(wage ~ jitter(exper, factor = 4), data = data)
plot(wage ~ jitter(educ, factor = 2), data = data)

# Get the column wage values
#head(data[, c("wage")])
#head(data[, "wage"])

# Returns a new list where the values in the exper column are tested if they are equal to 5
#head(data[, "exper"] == 5)

# Get the elements where the values in the experience column are equal to x
#exper1 = data[(data[, "exper"] == x),]

# Get the data where the experience is 1, 10, 20, and 30
exper1 = data[(data[, "exper"] == 1),]
exper10 = data[(data[, "exper"] == 10),]
exper20 = data[(data[, "exper"] == 20),]
exper30 = data[(data[, "exper"] == 30),]

# Plot the wage density of these experience level datas
plot( density(exper1$wage) )
plot( density(exper10$wage) )
plot( density(exper20$wage) )
plot( density(exper30$wage) )

# Check the mean and the standard deviance of these experience levels
#mean(exper1$wage)
#sd(exper1$wage)
#mean(exper10$wage)
#sd(exper10$wage)
#mean(exper20$wage)
#sd(exper20$wage)
#mean(exper30$wage)
#sd(exper30$wage)

# Plot the mean and signma of wage against each experience level

meanList <- list()
sdList <- list()
for (i in 0:max(data$exper)) {
  #plot( density( (data[(data[, "exper"] == i),])$wage ))
  #print(paste("exper: ", i))
  #print(paste("mean: ", mean( (data[(data[, "exper"] == i),])$wage )))
  #print(paste("std dev: ", sd( (data[(data[, "exper"] == i),])$wage )))
  
  meanList[i] <- mean( (data[(data[, "exper"] == i),])$wage )
  sdList[i] <- sd( (data[(data[, "exper"] == i),])$wage )
}
index <- seq(0, 46, 1)
plot(index, meanList)
plot(index, sdList)


plot(density(data$wage))
hist(data$wage, breaks = 40)

library("rjags")

set.seed(200)

############################################################################### 4 Postulate a model

# Need a model which estimates the wage. 
# The education will be in different categories, but the experience will not.
# Both parameters will be considered in the module, and will have its own parameter in the model

# wage[i] ~ dnorm( mu[i], prec[i] )
# mu[i] = a_mu[educ[i]] + b_mu_exp * exper
# prec[i] = a_prec[educ[i]] + b_prec_exp * exper
#
# a_mu[j] ~ dnorm(0.0, 1.0/1.0e6)
# b_mu_exp ~ dnorm(0.0, 1.0/1.0e6)
#
# a_prec[j] ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
# b_prec_exp ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
#
# sig[i] = sqrt(1.0/prec[i])

mod_string = 
  " model {
    for (i in 1:length(wage)) {
      wage[i] ~ dnorm( mu[i], prec[i] )
      mu[i] = a_mu[educ[i]] + b_mu_exp * exper[i]
      prec[i] = a_prec[educ[i]] + b_prec_exp * exper[i]
    }
    
    for (j in 1:max(educ)){
      a_mu[j] ~ dnorm(0.0, 1.0/1.0e6)
      a_prec[j] ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
    }
    
    b_mu_exp ~ dnorm(0.0, 1.0/1.0e6)
    b_prec_exp ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
    
  } "

data_jags = list(wage=data$wage, educ=data$educ, exper=data$exper)

params = c("a_mu","b_mu_exp","a_prec", "b_prec_exp")

############################################################################### 5 Fit the model

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains = 3)

update(mod, 1e3)

mod_sim = coda.samples(model = mod, variable.names = params, n.iter = 5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

############################################################################### 6 Check the model

# convergence diagnostics
plot(mod_sim, ask=TRUE)
summary(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)


# The b_sig_exp is not well defined, it looks like some of the b_sig_exp has an extreme large value
# Not sure about the reason at the moment

# Examine the generated samples
exa_param = c("mu", "prec", "a_prec", "b_prec_exp")
samples <- jags.samples(model = mod, variable.names = exa_param, n.iter = 5e3)
head(samples$prec)
str(samples$prec)
length(samples$prec)

# Plot the density of precision param
plot(density(samples$prec))

samples$sig = sqrt(1.0/samples$prec)

# Plot the density of sig and mu 
plot(density(samples$sig))
plot(density(samples$mu))

# Generate wage values
exa_pred_wage = rnorm(5e5, samples$mu, samples$sig)
head(exa_pred_wage)
str(exa_pred_wage)
plot(density(exa_pred_wage))
plot(density(data$wage))


# Calculate some sigma from a_prec and b_prec_exp
exa_prec = samples$a_prec[1:1472] + (samples$b_prec_exp[1:1472] * data[,"exper"])

head(samples$b_prec_exp)
head(samples$a_prec)
samples$b_sig_exp = sqrt(1.0/samples$b_prec_exp)
head(samples$b_sig_exp)


plot(log(mod_csim[, "b_sig_exp"]))
plot(mod_csim[, "b_sig_exp"])

mean(mod_csim[, "b_sig_exp"])
sd(mod_csim[, "b_sig_exp"])
median(mod_csim[, "b_sig_exp"])

plot(density(mod_csim[, "b_sig_exp"], to = 3000))
plot(density(mod_csim[, "a_sig[1]"]))





############################################################################### 8 Use the model

# Try to predict some data
head(data)
head(mod_csim)

# Norm( Mu, Sig )
# Mu = a_mu[n] + b_mu_exp * exper[i]
# Sig = a_sig[n] + b_prec_exp * exper[i]
# n = educ[i]

# Generate predicted mu values
pred_mu = mod_csim[1:(99*10), "a_mu[1]"] + mod_csim[1:(99*10), "b_mu_exp"] * data[(data[, "educ"] == 1),]$exper
head(pred_mu)
str(pred_mu)

# Generate predicted sig values
#pred_sig = mod_csim[1:99, "a_sig[1]"] + mod_csim[1:99, "b_sig_exp"] * data[(data[, "educ"] == 1),]$exper
pred_sig = mod_csim[1:(99*10), "a_prec[1]"] + mod_csim[1:(99*10), "b_prec_exp"] * data[(data[, "educ"] == 1),]$exper
head(pred_sig)
str(pred_mu)

# Generate predicted wage value
pred_wage = rnorm(length(pred_sig), mean = pred_mu, sd = pred_sig)
str(pred_wage)
# Check predicted wage values
plot(density(pred_wage))
mean(pred_wage)
sd(pred_wage)

# Check original wage values
plot( density( data[(data[, "educ"] == 1),]$wage ) )
mean( data[(data[, "educ"] == 1),]$wage )
sd( data[(data[, "educ"] == 1),]$wage )

# Compare the predicted and original wage values
error = pred_wage - data[(data[, "educ"] == 1),]$wage
head(error)
str(error)

# Check the difference between the original and predicted values
plot(error)
plot(density(error))
mean(error)
sd(error)





















# New model, removed the standard deviance depenendy on the experience level as it was problematic
mod_string2 = 
  " model {
    for (i in 1:length(wage)) {
      wage[i] ~ dnorm( mu[i], prec[i] )
      mu[i] = a_mu[educ[i]] + b_mu_exp * exper[i]
      prec[i] = a_prec[educ[i]]
    }
    
    for (j in 1:max(educ)){
      a_mu[j] ~ dnorm(0.0, 1.0/1.0e6)
      a_prec[j] ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
      a_sig[j] = sqrt(1.0/a_prec[j])
    }
    
    b_mu_exp ~ dnorm(0.0, 1.0/1.0e6)
    
} "


data_jags = list(wage=data$wage, educ=data$educ, exper=data$exper)

params2 = c("a_mu","b_mu_exp","a_sig")

############################################################################### 5 Fit the model

mod2 = jags.model(textConnection(mod_string2), data=data_jags, n.chains = 3)

update(mod2, 1e3)

mod_sim2 = coda.samples(model = mod2, variable.names = params2, n.iter = 5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

############################################################################### 6 Check the model
# convergence diagnostics
plot(mod_sim2, ask=TRUE)
summary(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
autocorr.plot(mod_sim2)
effectiveSize(mod_sim2)


# Generate predicted mu values
pred_mu = mod_csim2[1:(99*10), "a_mu[1]"] + mod_csim2[1:(99*10), "b_mu_exp"] * data[(data[, "educ"] == 1),]$exper
head(pred_mu)
str(pred_mu)

# Generate predicted sig values
#pred_sig = mod_csim[1:99, "a_sig[1]"] + mod_csim[1:99, "b_sig_exp"] * data[(data[, "educ"] == 1),]$exper
pred_sig = mod_csim2[1:(99*10), "a_sig[1]"]
head(pred_sig)
str(pred_mu)

# Generate predicted wage value
pred_wage = rnorm(length(pred_sig), mean = pred_mu, sd = pred_sig)
str(pred_wage)
# Check predicted wage values
plot(density(pred_wage))
mean(pred_wage)
sd(pred_wage)

# Check original wage values
plot( density( data[(data[, "educ"] == 1),]$wage ) )
mean( data[(data[, "educ"] == 1),]$wage )
sd( data[(data[, "educ"] == 1),]$wage )

# Compare the predicted and original wage values
error = pred_wage - data[(data[, "educ"] == 1),]$wage
head(error)
str(error)

# Check the difference between the original and predicted values
plot(error)
plot(density(error))
mean(error)
sd(error)
