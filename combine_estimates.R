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
  if(log_normal)
    low <- max(0, low)
  stan_data <- list(N = length(yhat),
               yhat = yhat,
               sigma = sigma,
               conf=conf,
               prior_mu = prior_mu,
               prior_tau = prior_tau,
               low=low,
               up=up,
               log_normal=as.integer(log_normal))
  if(is.null(.globals$model)){
    model <- stan_model(file = "combine_estimates.stan")
    .globals$model <- model
    cat("attempting to load model from file...\n")
  }
  fit <- sampling(.globals$model, data=stan_data)
  #fit <- stan(file="combine_estimates.stan", data=stan_data)
  theta <- extract(fit, "theta")$theta
  theta
}

log_normal_transform_params <- function(median, std){
  variance <- std^2
  mu <- log(median)
  sigsq <- log(0.5 * (sqrt((exp(2*mu) + 4 * variance) / exp(2 * mu)) + 1))
  list(mu=mu, sigma=sqrt(sigsq))
}
