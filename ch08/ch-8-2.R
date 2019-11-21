library(rstan)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#####################################
# Set data
#このデータはオリジナルのssp2015調査デーから
#1000サンプルをランダムに抜き出したデータです．
#分析の結果はテキストと完全には一致しません
#####################################

d<-read.csv("SSPI2015.csv")
data <- list(n=nrow(d),Y=d$Y, FEM=d$FEM,AGE=d$AGE,EDU=d$EDU)
data2 <- list(n=nrow(d),Y=d$Y2, FEM=d$FEM,AGE=d$AGE,EDU=d$EDU)


#####################################
# run-model-8-2 (hurdle lognormal)
#####################################
model<- stan_model(file='model/model-8-2.stan')
fit <- sampling(model, data=data, seed=1234,
            pars = c("a","b","sigma","log_lik"),warmup=500, iter=1000,chains = 3)
options(scipen=10,digits = 3)
summary(fit)$summary[1:10,c(1,4,8,9,10)]

#traceplot(fit)

saveRDS(fit, "model/model-8-2-fit.rds")
load("model/model-8-2-fit.rds")



########################################
#  run-model-8-2-3 (generalized linear model)
########################################

model2<- stan_model(file='model/model-8-2-3.stan')
fit2 <- sampling(model2, data = data2,  pars = c("b","sigma","log_lik"),
                 warmup=500, iter=1000,chains = 3)
options(scipen=10,digits = 3)
summary(fit2)$summary[1:7,c(1,4,8,9,10)]

#traceplot(fit)
# saveRDS(fit2, "model/model-8-2-3-fit.rds")
# load("model/model-8-2-3-fit.rds")


######################################
# compute WAIC 
######################################

# waic for hurdle model
waic(extract(fit)$log_lik)
loo(extract(fit)$log_lik)

# waic for generalized linear model
waic(extract(fit2)$log_lik)
loo(extract(fit2)$log_lik)
