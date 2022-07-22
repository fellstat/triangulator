
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
  real var_yhat;
  real multi;
}

parameters {
  real<lower=low, upper=up> theta;
  real<lower=0,upper=up> tau;
}

model {
  yhat ~ normal(theta,sqrt(tau .* tau + (sigma ./ conf).*(sigma ./ conf)));
  tau ~ cauchy(0,multi*sqrt(var_yhat)); #normal(0,multi*sqrt(var_yhat)); #
  if(log_normal > 0.5)
    theta ~ lognormal(prior_mu, prior_tau);
  else
    theta ~ normal(prior_mu, prior_tau);
}
