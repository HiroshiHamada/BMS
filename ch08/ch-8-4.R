library(rstan)
library(loo)

# stan modelを自動保存するオプション
rstan_options(auto_write = TRUE)
# マルチコアを使うオプション
options(mc.cores = parallel::detectCores())


################################################
## 1993年から2016年までのデータ読み込み
## X:年度, Y:契約者数（万人）
################################################

d <- read.csv(file='mobile-rate.csv')
n_pred <- 60 #予測分布用
t_pred <- seq(from=1, to=24, length=n_pred)#予測分布用に説明変数を作成
y0 <- 213.1 #契約者数の初期値
data <- list(n=nrow(d), t=d$X, Y=d$Y,t_pred=t_pred, n_pred=n_pred,y0=y0)


################################################
#  Stan による推定
################################################

################################################
# differential　微分方程式モデル model-8-4-1.stan
################################################
fit <- stan(file='model/model-8-4-1.stan', warmup=500, data=data, seed=1234, thin=1)
options(scipen=10,digits = 3)
summary(fit)$summary[1:3,c(1,4,8,9,10)]

################################################
# linear線形回帰モデル model-8-4-2.stan
################################################

fit2 <- stan(file='model/model-8-4-2.stan', warmup=500, data=data, seed=1234, thin=1)
options(scipen=10,digits = 3)
summary(fit2)$summary[1:3,c(1,4,8,9,10)]

###################################
###  waicの計算（loo パッケージ使用）
###################################
waic(extract(fit)$log_lik)
loo(extract(fit)$log_lik)

waic(extract(fit2)$log_lik)
loo(extract(fit2)$log_lik)

