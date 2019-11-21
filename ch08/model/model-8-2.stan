functions {
  real ZIL_lpdf(real Y, real q, real mu, real sigma) {
    if (Y == 0) {
      return 
        bernoulli_lpmf(1 | q);
    } else {
      return bernoulli_lpmf(0 | q) + lognormal_lpdf(Y | mu, sigma);
    }
  }
}

data {
  int n;
  real<lower=0> Y[n];
  int<lower=0> FEM[n];
  real AGE[n];  real EDU[n];
  }

parameters {
  real a[4];
  real b[4];
  real<lower=0> sigma;
}

transformed parameters {
  real mu[n];
  real<lower=0,upper=1> q[n];
  for (i in 1:n){
    q[i] = inv_logit(a[1]+a[2]*FEM[i]+a[3]*AGE[i]+a[4]*EDU[i]);
    mu[i] = b[1]+b[2]*FEM[i]+b[3]*AGE[i]+b[4]*EDU[i];
    }
}

model {
  for (i in 1:n)
    Y[i] ~ ZIL(q[i], mu[i], sigma);
}

generated quantities{
	real log_lik[n];
	for(i in 1:n){
			log_lik[i] = ZIL_lpdf(Y[i]|q[i], mu[i], sigma);
	}
}
