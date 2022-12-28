#' Bayesian combination of several, possibly contradictory estimates
#'
#' @export
#' @param yhat A vector of estimates
#' @param sigma A vector of standard errors for each estimates
#' @param conf A vector of study confidences
#' @param prior_mu Prior mean
#' @param prior_tau Prior standard deviation
#' @param low Lower bound
#' @param up upper bound
#' @param multi tau multiplier
#' @param ... Additional parameters passed to rstan::sampling
#' @examples
#' y <- c(1,2,3)
#' se <- c(2,2,2)
#' conf <- c(1,1,.5)
#' combine_estimates_stan(y, se, conf, 1, 5)
combine_estimates_stan <- function(yhat, sigma, conf, prior_mu, prior_tau, low=-Inf, up=Inf, multi = .01, ...){
  stan_data <- list(N = length(yhat),
               yhat = yhat,
               sigma = sigma,
               conf=conf,
               prior_mu = prior_mu,
               prior_tau = prior_tau,
               low=low,
               up=up,
               var_yhat = as.numeric(var(yhat)) + .000001, # avoid crash if all estimates equal
               multi=multi)
  model <- stanmodels$combine_estimates
  fit <- rstan::sampling(model, data=stan_data, ...)
  theta <- rstan::extract(fit, "theta")$theta
  tau <- rstan::extract(fit, "tau")$tau
  list(theta=theta, tau=tau, fit=fit)
}

# This function computes R^2 and lambda specifically for the model in question, with
# the nuisance parameters nu_i eliminated (hence, no nu)
# It requires y_i and sigma_i, as well as the posterior samples for theta and tau.
# Note that it returns R^2 and lambda for just Level 1 of the full model
# Updated as of 10/4/22
#' Calculate r-squared and pooling factor for the model
#' @export
#' @param y Estimates
#' @param sigma Standard errors of estimates
#' @param theta Posterior samples of theta
#' @param tau Posterior samples of tau
#'
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
  ##
  V_E <- var(colMeans(w_mat*(y_mat-theta_mat)))
  #
  E_V_2 <- mean(apply(w_mat*(y_mat-theta_mat),2,var))
  E_V_3 <- mean(colMeans((1-w_mat)*sigma_sq_mat))
  E_V <- V_E+E_V_2+E_V_3

  ##
  lambda <- 1-V_E/E_V
  Rsq <- 1-E_V/var(y)

  return_list <- list(lambda,Rsq)
  names(return_list) <- c("lambda","Rsq")
  return(return_list)
}


#' Launches the Triangulator Shiny Application
#'
#' @export
launch_triangulator <- function(){
  application <- "triangulator"
  appDir <- system.file("apps", application, package = "triangulator")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `triangulator`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

