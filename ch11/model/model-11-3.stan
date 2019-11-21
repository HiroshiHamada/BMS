data {
  int<lower=0> N ;      // sample size
  int<lower=0,upper=9> Y[N] ;   // explanatory var.
  real<lower=0> X[N] ;   // explained var.
  int<lower=0> K ;      // n. of factors
  int<lower=1,upper=K> Z[N] ; //factors
  int<lower=0> N_p_new ;
  real<lower=0,upper=1> p_new[N_p_new] ;
  int<lower=0> N_X_new ;
  real<lower=0> X_new[N_X_new] ;
}

parameters {
  real a0 ;
  real b0 ;
  real a[K] ;
  real b[K] ;
  real<lower=0> s_a ;
  real<lower=0> s_b ;
  real<lower=0,upper=8> mu[K] ;
  real<lower=0> sigma[K] ;
}

transformed parameters {
  real<lower=0,upper=1> pi[N] ;
  for(n in 1 : N) {
    pi[n] = inv_logit(a[Z[n]] + b[Z[n]]*logit(lognormal_cdf(X[n],mu[Z[n]],sigma[Z[n]]))) ;
  }
}

model {
  for( k in 1 : K ) {
    target += normal_lpdf(a[k] | a0, s_a) ;
    target += normal_lpdf(b[k] | b0, s_b) ;
  }
  for( n in 1 : N ) {
    target += binomial_lpmf(Y[n] | 9, pi[n]) ;
    target += lognormal_lpdf(X[n] | mu[Z[n]],sigma[Z[n]]) ;
  }
  target += normal_lpdf(a0 | 0, 10^2) ;
  target += normal_lpdf(b0 | 1, 10^2) ;
}

generated quantities {
  real<lower=0,upper=1> pi_p_new[K,N_p_new] ;
  real<lower=0,upper=1> pi_X_new[K,N_X_new] ;
  for (k in 1 : K) {
    for (n in 1 : N_p_new) {
      pi_p_new[k,n] =  inv_logit(a[k] + b[k]*logit(p_new[n])) ;
    }
  }
  for (k in 1 : K) {
    for (n in 1 : N_X_new) {
      pi_X_new[k,n] =  inv_logit(a[k] + b[k]*logit(lognormal_cdf(X_new[n],mu[k],sigma[k]))) ;
    }
  }
}
