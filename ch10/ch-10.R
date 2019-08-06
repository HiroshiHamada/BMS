library(rstan)

rstan_options(auto_write=TRUE)
#コンパイルを保存
options(mc.cores=parallel::detectCores())
#マルチコア計算


#####################################################
# stanに送るデータの作成
#####################################################

data <- read.csv("income.csv")

data.ssp2015 = list(
	y = data$income_log,
	N = length(data$income_log)
)



#####################################################
# Stanによる推定
#####################################################

#####################################################
# 分布生成モデルによる推定  
#####################################################

fit1 <- stan(file='model/model-10-1.stan',pars=c('m','s','p','b','log_lik'),
             data=data.ssp2015,seed=1234,warmup=500, iter=1500,chains = 4)

#事後分布の要約
summary(fit1)$summary[1:5,c(1,4,8,9,10)]

#####################################################
# あてはめ分布モデルによる推定 
#####################################################

fit2 <- stan(file='model/model-10-2.stan',pars=c('m','s','log_lik'),
						 data=data.ssp2015,seed=1234,warmup=500, iter=1500,chains = 4)

#事後分布の要約
summary(fit2)$summary[1:3,c(1,4,8,9,10)]
