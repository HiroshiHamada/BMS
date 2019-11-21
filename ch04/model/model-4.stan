data {
  int N;
  int Y[N];
}

parameters {
  real<lower=0,upper=1> q;
}

model {
  for (n in 1:N) {
    Y[n] ~ bernoulli(q);
  }
}

// generated quantities {
//   real log_lik[N];
//   for (n in 1:N) {
//     log_lik[n] = bernoulli_lpmf(Y[n]|q);
//   }
// }
