data{
      int N;// sample size
      real y[N];//log of income
}

parameters {
  real <lower=0, upper=1> p; //成功確率 
  real <lower=0, upper=1> b; //投資利益率
 }

transformed parameters{//y0=10, n=10を仮定
  real m;
  real s;
	real y0;// initial capital
  real n;// number of chance
  y0=10;n=10;
  m = log(y0)+n*log(1-b)+log((1+b)/(1-b))*n*p;
  s = sqrt(n*p*(1-p))*log( (1 + b )/(1 - b));
  }

model {
	for (i in 1:N)y[i] ~ normal(m, s);
}
//所得はデータ上対数変換済み

generated quantities{
	vector[N] log_lik;
	 	for (i in 1:N){ 
			log_lik[i] = normal_lpdf(y[i] | m, s);
	 	}
}
