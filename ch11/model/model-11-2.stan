data {
  int<lower=0> N ;
  int<lower=0,upper=9> Y[N] ;
  real<lower=0> X[N] ;
  int<lower=0> N_p_new ;
  real<lower=0,upper=1> p_new[N_p_new] ;
  int<lower=0> N_X_new ;
  real<lower=0> X_new[N_X_new] ;
}

parameters {
  real a;
  real b;
  real<lower=0,upper=8> mu ;
  real<lower=0> sigma ;
}

transformed parameters {
  real<lower=0,upper=1> pi[N] ;
  for(n in 1 : N) {
    pi[n] = inv_logit(a + b*logit(lognormal_cdf(X[n],mu,sigma))) ;
  }
}

model {
  for( n in 1 : N ) {
    target += binomial_lpmf(Y[n] | 9, pi[n]);
    target += lognormal_lpdf(X[n] | mu, sigma) ;
  }
  target += normal_lpdf(a | 0, 10^2) ;
  target += normal_lpdf(b | 1, 10^2) ;
}

generated quantities {
  real<lower=0,upper=1> pi_p_new[N_p_new] ;
  real<lower=0,upper=1> pi_X_new[N_X_new] ;
  for (n in 1 : N_p_new) {
    pi_p_new[n] =  inv_logit(a + b*logit(p_new[n])) ;
  }
  for (n in 1 : N_X_new) {
    pi_X_new[n] =  inv_logit(a + b*logit(lognormal_cdf(X_new[n],mu,sigma))) ;
  }
}
