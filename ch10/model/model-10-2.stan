// base model: m and s あてはめ推定 
data {
	int N;
	real y[N];
}

parameters {
	real <lower=0> s;
	real m;
}

model {
	for (n in 1:N)
		y[n] ~ normal(m, s);
}

generated quantities{
	vector[N] log_lik;
	 	for (i in 1:N) 
			log_lik[i] = normal_lpdf(y[i] | m, s);
}
