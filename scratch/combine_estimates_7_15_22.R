library(rstan)

#setwd("~/Consensus Estimation - R Code")

data <- read.csv('JohnstonData.csv',header=TRUE)


#############
# Functions #
#############

# Define function that computes R^2 and Lambda from full model output

# Note: this is works generally for any model at any level - the inputs
# theta and mu correspond to theta^(m)_k and mu^(m)_k on p. 244 of Gelman and Pardoe (2006)


rsq_lambda <- function(theta,mu){
  comp_df <- as.data.frame(theta-mu)
  
  #Compute the variance of expected errors
  var_exp_eps <- var(colMeans(comp_df[,1:ncol(theta)]))
  
  #Compute variance of each row V_i=1^K eps_i and take expectation
  comp_df$var <- apply(comp_df,1,var)
  exp_var_eps <- mean(comp_df$var)
  
  #For denominator of R^2, compute denominator E(V_i=1^k y_i) = var(y_i)
  exp_var_theta <- mean(apply(theta,1,var))
  
  return_list <- list()
  return_list$Rsq <- 1-(exp_var_eps/exp_var_theta)
  return_list$lambda <- 1-(var_exp_eps/exp_var_eps)
  #
  return_list$var_exp_eps <- var_exp_eps
  return_list$exp_var_eps <- exp_var_eps
  return_list$exp_var_theta <- exp_var_theta
  return(return_list)
}

# This function computes R^2 and lambda specifically for the model in question, with
# the nuisance parameters nu_i eliminated (hence, no nu)
# It requires y_i and sigma_i, as well as the posterior samples for theta and tau.
# Note that it returns R^2 and lambda for just Level 1 of the full model

rsq_lambda_nonu <- function(y,sigma,theta,tau){
  #y and sigma are N-dimensional vectors
  #theta,tau are the n_samp-dimensional results from the sampling
  N <- length(y)
  n_samp <- length(theta)
  #
  y_mat <- matrix(rep(y,n_samp),ncol=N,byrow=TRUE)
  sigma_sq_mat <- matrix(rep(sigma^2,n_samp),ncol=N,byrow=TRUE)
  tau_sq_mat <- matrix(rep(tau^2,N),ncol=N,byrow=FALSE)
  theta_mat <- matrix(rep(theta,N),ncol=N,byrow=FALSE)
  #
  w_mat <- sigma_sq_mat/(tau_sq_mat + sigma_sq_mat)
  
  #
  E_V_1 <- apply(w_mat*(y_mat-theta_mat),1,var)
  E_V_2 <- (1/N)*rowSums((1-w_mat)*sigma_sq_mat)
  E_V <- mean(E_V_1+E_V_2)
  ##
  V_E_1 <- colMeans(w_mat*(y_mat-theta_mat))
  V_E <- var(V_E_1)
  ##
  lambda <- 1-V_E/E_V
  Rsq <- 1-E_V/var(y)
  
  return_list <- list(lambda,Rsq)
  names(return_list) <- c("lambda","Rsq")
  return(return_list)
}

###############
# IMPORT DATA #
###############

# There are 4 populations in the Johnston paper that have multiple PSEs:
# San Francisco IDUs in 2005 and 2009, and IDUs and FSWs in Mauritius
# Note that one estimate for the 2009 SF IDU population doesn't have confidence intervals, 
# and I do not currently have a prior for FSWs in Mauritius


# Subset Dataframe
select_cc <- "San Francisco" #Select the city/country
select_pop <- "IDU 2005" #Select the population (and year, if appropriate)

df <- data[data$City.Country==select_cc & data$Population == select_pop,]

#pop = 777660 #Estimated population of San Francisco

#Remove the Prior estimate as a percentage
df <- df[df$Provider != "Prior_pct",]

#Number of Estimates
N = nrow(df[df$Type=="Estimate",])

###############
# STAN Models #
###############

#Model without the nu_i
model <- stan_model(file = "combine_estimates_tau.stan")

#Full model with the nu_i
# model_full <- stan_model(file = "combine_estimates_full.stan")

######################
# CONFIDENCE SCALING #
######################

#INPUTS
lognormal = 0
log_transform = FALSE

if (log_transform == TRUE) {
  yhat_CS = log(df[df$Type=='Estimate','Estimate'])
  sigma_CS = (log(df[df$Type=='Estimate','Upper'])-log(df[df$Type=='Estimate','Estimate']))/1.96
  #
  prior_mu_CS = log(df[df$Type=='Prior' & df$Provider=='Prior_raw','Estimate'])
  prior_tau_CS = (log(df[df$Type=='Prior' & df$Provider=='Prior_raw','Upper'])-log(df[df$Type=='Prior' & df$Provider=='Prior_raw','Lower']))/(2*1.96)
} else {
  yhat_CS = df[df$Type=='Estimate','Estimate']
  sigma_CS = (df[df$Type=='Estimate','Upper']-df[df$Type=='Estimate','Estimate'])/1.96
  #
  prior_mu_CS = df[df$Type=='Prior' & df$Provider=='Prior_raw','Estimate']
  prior_tau_CS = (df[df$Type=='Prior' & df$Provider=='Prior_raw','Upper']-df[df$Type=='Prior' & df$Provider=='Prior_raw','Lower'])/(2*1.96)
}

#Compute between-study variance (Der Simonian and Laird)
var_CS <- sigma_CS^2
Q_CS = sum(yhat_CS^2/var_CS)-((sum(yhat_CS/var_CS))^2)/sum(1/var_CS)
C_CS = sum(1/var_CS)-(sum(1/var_CS^2))/sum(1/var_CS)
tau_sq_CS = (Q_CS-(N-1))/C_CS

print(paste("Between-Study Variance Tau^2 (Der Simonian and Laird Estimator): ",tau_sq_CS))
print(paste("Between-Study SD Tau (Der Simonian and Laird Estimator): ",sqrt(tau_sq_CS)))

#Confidence Values
conf_CS = rep(0.5,length(sigma_CS))

########################
# 1. Simplified Model

# Note the parameter "multi," which sets the scale parameter in the prior for
# tau to be multi*var(yhat_CS)

data_CS <- list(N = N,
                yhat = yhat_CS,
                sigma = sigma_CS,
                conf = conf_CS,
                prior_mu = prior_mu_CS,
                prior_tau = prior_tau_CS,
                low=0,
                up=pop,
                log_normal=0,
                var_yhat=var(yhat_CS),
                multi=0.01)

fit_CS <- sampling(model,data=data_CS)

theta_results_CS <- summary(fit_CS,pars=c("theta"))$summary

print(summary(fit_CS)$summary)

stan_dens(fit_CS,c("tau"))

#Transform Results Back
if (log_transform == TRUE){
  df[nrow(df)+1,] <- list(select_cc,select_pop,"Confidence Scaling",exp(theta_results_CS[[1]]),exp(theta_results_CS[[4]]),exp(theta_results_CS[[8]]),"Consensus")
} else {
  df[nrow(df)+1,] <- list(select_cc,select_pop,"Confidence Scaling",theta_results_CS[[1]],theta_results_CS[[4]],theta_results_CS[[8]],"Consensus")
}

#Compute R^2 and Lambda for Level 1 using just the simplified model
R2L <-rsq_lambda_nonu(yhat_CS,sigma_CS,extract(fit_CS)[['theta']],extract(fit_CS)[['tau']])

print(paste("Lambda (Simplified Model): ",R2L$lambda,"R^2 (Simplified Model): ",R2L$Rsq))


# ##############################
# # 2. Full Model
# 
# fit_CS_full <- sampling(model_full,data=data_CS,iter=4000) #This often has required a large number of iterations to converge
# 
# #Check the posterior of tau and other diagnostics
# print(summary(fit_CS_full)$summary)
# 
# stan_dens(fit_CS_full,c("tau"))
# traceplot(fit_CS_full)
# #pairs(fit_CS_full)
# 
# #Extract
# list_CS_full <- extract(fit_CS_full)
# n_samp <- length(list_CS_full[['theta']])
# 
# #Compute R^2 and Lambda for both levels of the model from the full function
# if (log_transform == TRUE){
#   level_1 <- rsq_lambda(exp(matrix(rep(yhat_CS,n_samp),ncol=length(yhat_CS),byrow=TRUE)),exp(list_CS_full[['nu']]))
#   level_2 <- rsq_lambda(exp(list_CS_full[['nu']]),exp(matrix(rep(list_CS_full[['theta']],length(yhat_CS)),ncol=length(yhat_CS))))
# 
# } else {
#   level_1 <- rsq_lambda(matrix(rep(yhat_CS,n_samp),ncol=length(yhat_CS),byrow=TRUE),list_CS_full[['nu']])
#   level_2 <- rsq_lambda(list_CS_full[['nu']],matrix(rep(list_CS_full[['theta']],length(yhat_CS)),ncol=length(yhat_CS)))
# }
# 
# #Print Results for Level 1:
# print(paste("Lambda (Full Model)",level_1$lambda,"R^2 (Full Model)",level_1$Rsq))
# 
# #Reprint for the Simplified Model as well
# print(paste("Lambda (Simplified Model): ",R2L$lambda,"R^2 (Simplified Model): ",R2L$Rsq))
# 

##############
# TAU PRIORS #
##############

# This is for testing the effect of the scale parameter in the prior for tau,
# varying the multiplier (scale parameter is multiplier*var(yhat_CS)) and examining the model results

# Testing Out Priors:
multiplier_ind = 1:9
multipliers = 10^((multiplier_ind-5)/2)

#Empty lists for the samples of theta, tau, and an empty matrix for the results
theta_list <- list()
tau_list <- list()
multi_mat <- matrix(rep(0,length(multiplier_ind)*5),ncol=5)

for(i in multiplier_ind){
  data_temp <- list(N = N,
                  yhat = yhat_CS,
                  sigma = sigma_CS,
                  conf = conf_CS,
                  prior_mu = prior_mu_CS,
                  prior_tau = prior_tau_CS,
                  low=0,
                  up=pop,
                  log_normal=0,
                  var_yhat=var(yhat_CS),
                  multi=multipliers[i])
  fit_temp <- sampling(model,data=data_temp)
  #
  temp_theta <- extract(fit_temp)[['theta']]
  temp_tau <- extract(fit_temp)[['tau']]
  theta_list[[i]] <- temp_theta
  tau_list[[i]] <- temp_tau
  
  #
  temp_rsq_lambda <- rsq_lambda_nonu(yhat_CS,sigma_CS/conf_CS,temp_theta,temp_tau)
  multi_mat[i,] <- c(multipliers[i],temp_rsq_lambda$lambda,temp_rsq_lambda$Rsq,median(temp_theta),median(temp_tau))
  #
  hist(temp_tau,breaks = seq(0,(floor(max(temp_tau)/50)+1)*50,by=50))
}

multi_df <- data.frame(multi_mat)
colnames(multi_df) <- c("Multiplier","Lambda","R^2","Med(theta)","Med(tau)")

print(multi_df)

#########
# PLOTS #
#########

#Reorder Labels
df$Type <- factor(df$Type,levels=c("Prior","Estimate","Consensus"))
df$Provider <- factor(df$Provider,levels=rev(c("Prior_raw",df[df$Type=='Estimate','Provider'],"Anchored Multiplier","Anchored Multiplier - VA","Confidence Scaling")))

#Add columns to show the adjusted confidence intervals (due to confidence scaling)
#Note: I don't think this computation will currently work with log-transformed data
df$Lower_scaled <- rep(NA,nrow(df))
df$Upper_scaled <- rep(NA,nrow(df))

temp_df <- df[df$Type=='Estimate',]
for (i in 1:N){
  row_ind <- which(df[,'Estimate'] == temp_df[i,'Estimate'])
  df$Lower_scaled[i] <- (df[row_ind,'Lower'] - temp_df[i,'Estimate'])/conf_CS[i]+temp_df[i,'Estimate']
  df$Upper_scaled[i] <- (df[row_ind,'Upper'] - temp_df[i,'Estimate'])/conf_CS[i]+temp_df[i,'Estimate']
}

#Plot
#Scaled confidence intervals are dashed, original are solid

p <- ggplot(data=df,aes(x=Estimate,y=Provider,color=Type)) +
  geom_pointrange(aes(xmin=Lower,xmax=Upper),shape=15) + 
  geom_pointrange(aes(xmin=Lower_scaled,xmax=Upper_scaled),shape=15,linetype='dashed') 
  labs(x='Population',y='')

suppressWarnings(print(p))
