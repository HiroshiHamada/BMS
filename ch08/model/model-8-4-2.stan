data {
  int n;
  real Y[n];
  int t[n];
  int n_pred;
  real t_pred[n_pred];
  real y0; 
}

parameters {
  real <lower=0,upper=1000>a;
  real<lower=0> sigma;
}

model {
  for (i in 1:n) {
    Y[i] ~ normal(a*t[i]+y0, sigma);
  }
}

generated quantities{
	real y_pred[n_pred];
	real log_lik[n];
	for (i in 1:n_pred){
	y_pred[i] = normal_rng(a*t_pred[i]+y0, sigma);
	}
	for(i in 1:n){
			log_lik[i] = normal_lpdf(Y[i]|a*t[i]+y0, sigma);
	}
}
