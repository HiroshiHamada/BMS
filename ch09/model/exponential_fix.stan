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
  real<lower=0> beta;
}

model {
  real v_soon;
  real v_delay;
  for (t in 1:Trial) {
    v_soon = amount_soon[t];
    for(n in 1:N){
      v_delay = amount_delay*exp(-k[n]*D[t]);
      target += bernoulli_logit_lpmf(choice[n,t] | beta*(v_delay-v_soon));
    }
  }
  target += cauchy_lpdf(k | 0,5) - cauchy_lccdf(0 | 0,5);
  target += cauchy_lpdf(beta | 0,5) - cauchy_lccdf(0 | 0,5);
}

generated quantities{
  vector[N] log_lik = rep_vector(0,N);
  real v_soon;
  real v_delay;
  for (t in 1:Trial) {
    v_soon = amount_soon[t];
    for(n in 1:N){
      v_delay = amount_delay*exp(-k[n]*D[t]);
      log_lik[n] += bernoulli_logit_lpmf(choice[n,t] | beta*(v_delay-v_soon));
    }
  }
}
