data {
  int n;
  real<lower=0> Y[n];
  int<lower=0> FEM[n];
  real AGE[n];  
  real EDU[n];
  }

parameters {
  real b[4];
  real<lower=0> sigma;
}

transformed parameters {
  real mu[n];
  for (i in 1:n){
    mu[i] = b[1]+b[2]*FEM[i]+b[3]*AGE[i]+b[4]*EDU[i];
    }
}

model {
  for (i in 1:n)
    Y[i] ~ lognormal(mu[i], sigma);
}

generated quantities{
	real log_lik[n];
	for(i in 1:n){
			log_lik[i] = lognormal_lpdf(Y[i]|mu[i], sigma);
	}
}
