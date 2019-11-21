data {
  int<lower=0> N ;
  int<lower=0,upper=9> Y[N] ;
  real<lower=0> X[N] ;
}

parameters {
  real a;
  real<lower=0,upper=8>  mu ;
  real<lower=0> sigma ;
}

transformed parameters {
  real<lower=0,upper=1> pi ;
  pi = inv_logit(a) ;
}

model {
  for( n in 1 : N ) {
    target += binomial_lpmf(Y[n] | 9, pi);
    target += lognormal_lpdf(X[n] | mu, sigma) ;
  }
}
