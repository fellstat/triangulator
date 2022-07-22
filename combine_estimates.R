library(rstan)
.globals <- new.env(parent = emptyenv())

combine_estimates_stan <- function(yhat, sigma, conf, prior_mu, prior_tau, low=-Inf, up=Inf, multi = .01){
  stan_data <- list(N = length(yhat),
               yhat = yhat,
               sigma = sigma,
               conf=conf,
               prior_mu = prior_mu,
               prior_tau = prior_tau,
               low=low,
               up=up,
               var_yhat = as.numeric(var(yhat)),
               multi=multi)
  if(is.null(.globals$model)){
    model <- stan_model(file = "combine_estimates.stan")
    .globals$model <- model
    cat("attempting to load model from file...\n")
  }
  
  fit <- sampling(.globals$model, data=stan_data)
  theta <- extract(fit, "theta")$theta
  tau <- extract(fit, "tau")$tau
  list(theta=theta, tau=tau, fit=fit)
}

# This function computes R^2 and lambda specifically for the model in question, with
# the nuisance parameters nu_i eliminated (hence, no nu)
# It requires y_i and sigma_i, as well as the posterior samples for theta and tau.
# Note that it returns R^2 and lambda for just Level 1 of the full model

rsq_lambda <- function(y,sigma,theta,tau){
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