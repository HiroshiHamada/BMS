data{
  int N;
  int X[N];
  real a;
  real b;
}

parameters{
  real lambda;
}

model{
  target += 1/log(N)*poisson_lpmf(X | lambda);
  target += gamma_lpdf(lambda | a,b);
}

generated quantities{
  real log_lik[N];
  for(n in 1:N){
    log_lik[n] = poisson_lpmf(X[n] | lambda);
  }
}
