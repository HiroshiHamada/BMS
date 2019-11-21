//estimate BG model parameters
//model3 をもとにlogisticモデルを推定
//WAICを計算する
data{
      int n;// no. of respondents
      int Y[n];// advancement {0,1}
      int O[n];// origin {0,1}
}

parameters {//upperの制約を修正
  real b0;
  real b1;
}

transformed parameters{// q for bern(q)
	real <lower=0, upper=1> q[n];
	for (i in 1:n)
	q[i] = inv_logit(b0+b1*O[i]);
	}

model {
  b0 ~ normal(0, 10);
  b1 ~ normal(0, 10);

for (i in 1:n)
  Y[i] ~ bernoulli(q[i]);
}

generated quantities{
	vector[n] log_lik;
	for(i in 1:n){
			log_lik[i] = bernoulli_lpmf(Y[i] |q[i]);
			}
}
