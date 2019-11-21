data {
  int n; int t[n];int n_pred;
  real Y[n]; real y0; real t_pred[n_pred];
}

parameters {
  real <lower=16344,upper=20000>m;
  real <lower=0,upper=5>k;
  real<lower=0> sigma;
}

transformed parameters{
 real mu[n];
 for (i in 1:n) mu[i]=(m*y0)/((m-y0)*exp(-k*t[i])+y0);
}

model{
  for (i in 1:n) Y[i] ~ normal(mu[i], sigma);
}

generated quantities{
	real y_pred[n_pred];
	real log_lik[n];
	for (i in 1:n_pred)
	y_pred[i] = normal_rng((m*y0)/((m-y0)*exp(-k*t_pred[i])+y0), sigma);
	for(i in 1:n)
	log_lik[i] = normal_lpdf(Y[i]|mu[i], sigma);
}
