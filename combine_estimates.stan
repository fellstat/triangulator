
data {
  int<lower=0> N;
  vector[N] yhat;
  vector[N] sigma;
  vector[N] conf;
  real prior_mu;
  real prior_tau;
  real low;
  real up;
  int log_normal;
}

parameters {
  real<lower=low, upper=up> theta;
}

model {
  yhat ~ normal(theta, sigma ./ conf);
  if(log_normal > 0.5)
    theta ~ lognormal(prior_mu, prior_tau);
  else
    theta ~ normal(prior_mu, prior_tau);
}

