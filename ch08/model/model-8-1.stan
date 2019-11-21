data {
  int N;
  int X[N];
  int Z[N];

}

parameters {
  real<lower=0,upper=1> q1;
  real<lower=0,upper=1> q0;
}

model {
  for (n in 1:N) {
    X[n] ~ bernoulli(Z[n]*q1+(1-Z[n])*q0);
  }
}
