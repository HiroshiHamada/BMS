//estimate BG model parameters
data{
      int n;// no. of respondents
      int Y[n];// advancement {0,1}
      int O[n];// origin {0,1}
}

parameters {
  real <lower=0, upper=1> a;//alpha
  real <lower=0, upper=1> b1;//beta_1
  real <lower=0, upper=1-b1> b2;//beta_2
  real <lower=0, upper=a> g1;//gamma_1
  real <lower=0, upper=1-g1> g2;//gamma_1
  real <lower=0> c1; real <lower=0> c2;//Beta分布のパラメータ
  real <lower=0> d1; real <lower=0> d2;//Beta分布のパラメータ
}

transformed parameters{// q for bern(q)
	real <lower=0, upper=1> qs;
	real <lower=0, upper=1> qw;
	qs = 1 - beta_cdf((g1-b1)/(a-b1),c1+1,c2+1);
	qw = 1 - beta_cdf((g1+g2-b1-b2)/(1-b1-b2),d1+1,d2+1);
}

model {
  a ~ uniform(0, 1);
  b1 ~ uniform(0, 1);  b2 ~ uniform(0, 1);
  g1 ~ uniform(0, 1);  g2 ~ uniform(0, 1);
  c1 ~ gamma(5, 0.05);  c2 ~ gamma(5, 0.05);
  d1 ~ gamma(5, 0.05);  d2 ~ gamma(5, 0.05);
  
for (i in 1:n)
  Y[i] ~ bernoulli(qs*O[i]+qw*(1-O[i]));
}

generated quantities{
	vector[n] log_lik;
	for(i in 1:n){
			log_lik[i] = bernoulli_lpmf(Y[i]|qs*O[i]+qw*(1-O[i]));
	}
}
