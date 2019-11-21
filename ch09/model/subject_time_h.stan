data {
  int N;
  int Trial;
  real D[Trial];
  real amount_delay;
  real amount_soon[Trial];
  int<lower=0,upper=1> choice[N,Trial];
}

parameters {
  real<lower=0> k[N];
  real<lower=0> s[N];
  real<lower=0> beta;
  real mu_k;
  real<lower=0> sigma_k;
  real mu_s;
  real<lower=0> sigma_s;
}

model {
  real v_soon;
  real v_delay;
  for (t in 1:Trial) {
    v_soon = amount_soon[t];
    for(n in 1:N){
      v_delay = amount_delay*exp(-k[n]*D[t]^s[n]);
      target += bernoulli_logit_lpmf(choice[n,t] | beta*(v_delay-v_soon));
    }
  }
  target += lognormal_lpdf(k | mu_k,sigma_k);
  target += lognormal_lpdf(s | mu_s,sigma_s);
  target += normal_lpdf(mu_k | 0,10^2);
  target += cauchy_lpdf(sigma_k | 0,5) - cauchy_lccdf(0 | 0,5);
  target += normal_lpdf(mu_s | 0,10^2);
  target += cauchy_lpdf(sigma_s | 0,5) - cauchy_lccdf(0 | 0,5);
  target += cauchy_lpdf(beta | 0,5) - cauchy_lccdf(0 | 0,5);
}

generated quantities{
  vector[N] log_lik = rep_vector(0,N);
  real v_soon;
  real v_delay;
  for (t in 1:Trial) {
    v_soon = amount_soon[t];
    for(n in 1:N){
      v_delay = amount_delay*exp(-k[n]*D[t]^s[n]);
      log_lik[n] += bernoulli_logit_lpmf(choice[n,t] | beta*(v_delay-v_soon));
    }
  }
}
