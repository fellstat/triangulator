library(rstan)
.globals <- new.env(parent = emptyenv())


combine_estimates_normal <- function(yhat, sigma, conf, prior_mu, prior_tau){
  mu <- prior_mu
  tau <- prior_tau
  for(i in seq_along(yhat)){
    s <- sigma[i] / conf[i]
    mu <- mu + (yhat[i] - mu) * tau^2 / (s^2 + tau^2)
    tau <- sqrt(s^2 * tau^2 /  (s^2 + tau^2))
  }
  list(mu=mu,tau=tau)
}


combine_estimates_stan <- function(yhat, sigma, conf, prior_mu, prior_tau, low=-Inf, up=Inf, log_normal=FALSE){
  stan_data <- list(N = length(yhat),
               yhat = yhat,
               sigma = sigma,
               conf=conf,
               prior_mu = prior_mu,
               prior_tau = prior_tau,
               low=low,
               up=up,
               log_normal=as.integer(log_normal))
  if(is.null(.globals$model))
    .globals$model <- stan_model(file = "combine_estimates.stan")
  fit <- sampling(.globals$model, data=stan_data)
  #fit <- stan(file="combine_estimates.stan", data=stan_data)
  theta <- extract(fit, "theta")$theta
  rm(fit)
  gc()
  theta
}
