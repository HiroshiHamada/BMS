data{
  int N;
  int X[N];
  real a;
  real b;
}

parameters{
  real<lower=0> lambda;
}

model{
  for(i in 1:N){
    X[i] ~ poisson(lambda);
  }
  lambda ~ gamma(a,b);
}

generated quantities{
  real log_lik[N];
  for(i in 1:N){
    log_lik[i] = poisson_lpmf(X[i]|lambda);
  }
}

