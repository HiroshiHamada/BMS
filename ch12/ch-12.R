library(rstan)
library(loo)
library(bayesplot)


rstan_options(auto_write=TRUE)
#コンパイルを保存
options(mc.cores=parallel::detectCores())
#マルチコア計算

############################################################
#  make data for stan
############################################################
data <- read.csv("education.csv")

# Package the data for shipping to Stan:
data.ssp2015 = list(
	Y = data$edu.h,
	O = data$origin,
	n = length(data$edu.h)
)


############################################################
# compile and sampling
############################################################

# B&G model 
model <- stan_model(file='model/model-12-1.stan')

fit <- sampling(model,data=data.ssp2015,iter = 5500, warmup =500, seed=1234,chains = 3,
		control = list(adapt_delta = 0.8),
		init=function(){
		list(a=0.6,b1=0.1,g1=0.2,c1=5,c2=5,d1=5,d2=5)
				},thin=2)

options(scipen=2,digits=3)
summary(fit)$summary[1:12,c(1,4,8,9,10)]

traceplot(fit,c("a","b1","b2","g1","g2","qs","qw","c1","c2","d1","d2"))


######################################################################
#  logistic 回帰による推定
######################################################################

model.logistic <- stan_model(file='model/model-12-2.stan')
# model a,b,gの事前分布を[0,1]一様分布 c1,d1,c2,d2にいろいろ制限
fit.logistic <- sampling(model.logistic,data=data.ssp2015,
			 iter = 5500, warmup =500, seed=1234,chains = 3, thin=2)
options(scipen=10,digits=4)
summary(fit.logistic)$summary[c(1,2,2003),c(1,4,8,9,10)]
# 2003行目がlp__


######################################################################
#  waicの比較
######################################################################

waic(extract(fit)$log_lik)
waic(extract(fit.logistic)$log_lik)




################################################################
#  qsとqwの事後分布比較　
################################################################

mcmc <- as.data.frame(fit)
samplesize <- length(mcmc$qs)
data.b <- data.frame(
	mcmcsample=c(mcmc$qs,mcmc$qw),
	index=factor(c(rep("qs",samplesize),rep("qw",samplesize)))
)
g <- ggplot(data.b, aes(x=mcmcsample, fill=index))+theme_bw()+
	geom_histogram(position="identity", alpha=1, bins = 300)+
scale_fill_manual(values = c("black","grey"))+
	theme_bw() +theme(axis.title=element_blank(),text=element_text(size=10))
g



########################################################
# 事後分布比較　図12.3: b0とb1の事後分布比較　
########################################################

color_scheme_set("gray")
posterior <- as.matrix(fit.logistic)
mcmc_hist(posterior,pars = c("b0", "b1"), binwidth = 0.01)+	
  theme(text=element_text(size=8))

