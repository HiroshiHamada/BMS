library(tidyverse)

#### figure1 ####

z <- seq(0,10)
y <- dpois(z,3)
barplot(y,names.arg=z)

ggplot(data=data_frame(x=z,y=y)) +
  geom_col(aes(x,y)) + 
  scale_x_continuous(breaks = z , labels = z) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("")
# ggsave("../TeX/ch07_fig1.pdf",width=8,height=5,unit="cm")


#### figure2 ####

z <- seq(0,10)
y <- -dpois(z,3,log=T)
barplot(y,names.arg=z)

ggplot(data=data_frame(x=z,y=y)) +
  geom_col(aes(x,y)) + 
  scale_x_continuous(breaks = z , labels = z) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("")  
# ggsave("../TeX/ch07_fig2.pdf",width=8,height=5,unit="cm")


#### figure3 ####

z <- seq(0,10,length.out=101)
y <- dgamma(z,3,1)
plot(z,y,type="l")

ggplot(data=data_frame(x=z,y=y)) +
  geom_line(aes(x,y)) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("")  
# ggsave("../TeX/ch07_fig3.pdf",width=8,height=5,unit="cm")


#### figure4 ####

#marginal likelihood
ml <- function(x){
  gamma(sum(x)+a)/(gamma(a)*prod(gamma(x+1)))*(b^a/(length(x)+b)^(sum(x)+a))
}

a <- 3
b <- 1
z <- seq(0,10)
y <- sapply(z,ml)
barplot(y,names.arg=z)

ggplot(data=data_frame(x=z,y=y)) +
  geom_col(aes(x,y)) +
  scale_x_continuous(breaks = z , labels = z) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig4.pdf",width=8,height=5,unit="cm")


#### figure5 ####

#free energy
F_n <- function(x){
  -lgamma(sum(x)+a)+sum(lgamma(x+1))+
    (sum(x)+a)*log(length(x)+b)+lgamma(a)-a*log(b)
}

z <- seq(0,10)
y <- sapply(z,F_n)
barplot(y,names.arg=z)

ggplot(data=data_frame(x=z,y=y)) +
  geom_col(aes(x,y)) + 
  scale_x_continuous(breaks = z , labels = z) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig5.pdf",width=8,height=5,unit="cm")

## M_0
ex<-c(3,4,2,7,8)
-sum(ex*log(3))+sum(log(gamma(ex+1)))+length(ex)*3

## M_1
F_n(ex)


#### figure6 ####

lambda_prime <- 3 #lambda*
n <- 30 #sample size

set.seed(123)
m0 <- c()
m1 <- c()
for(i in 1:1000){
  x <- rpois(n,lambda_prime)
  m0[i] <- sum(-dpois(x,3,log=T))
  m1[i] <- F_n(x)
}
mean(m0)
mean(m1)

boxplot(m0,m1,names=c("M0","M1"),ylim=c(20,100))
data_frame(M0=m0,M1=m1) %>%
  gather(key = "x",value = "y") %>% 
  ggplot() +
  geom_boxplot(aes(x,y), lwd=0.5,outlier.shape = 21, outlier.size = 0.5) + 
  ylim(c(20,100)) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig6.pdf",width=8,height=5,unit="cm")


#### figure7 ####

lambda_prime2 <- 6
set.seed(123)
m0 <- c()
m1 <- c()
for(i in 1:1000){
  x <- rpois(n,lambda_prime2)
  m0[i] <- sum(-dpois(x,3,log=T))
  m1[i] <- F_n(x)
}
mean(m0)
mean(m1)

boxplot(m0,m1,names=c("M0","M1"),ylim=c(60,140))

data_frame(M0=m0,M1=m1) %>%
  gather(key = "x",value = "y") %>% 
  ggplot() +
  geom_boxplot(aes(x,y),lwd=0.5,outlier.shape = 21, outlier.size = 0.5) +
  ylim(c(60,140)) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig7.pdf",width=8,height=5,unit="cm")


#### figure8 ####

z <- seq(0,10)
y0 <- -dpois(z,3,log=T)
plot(z,y0,ylim=c(0,6),type="b",ylab="")
y1 <- sapply(z,F_n)
par(new=T)
plot(z,y1,ylim=c(0,6),type="b",lty=2,pch=2,ylab="y")
legend("topleft",legend=c("M0","M1"),pch=c(1,2),lty=c(1,2))

data_frame(M0=y0,M1=y1,x=z) %>%
  gather(key = "model",value = "y",-x) %>% 
  ggplot(aes(x=x,y=y,group=model)) +
  geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) +
  scale_x_continuous(breaks = z , labels = z) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") # +
  # scale_linetype_manual(values = c("M0" = "dashed", "M1" = "solid")) 
# ggsave("../TeX/ch07_fig8.pdf",width=10,height=5,unit="cm")


#### figure9 ####

lambda_prime <- 3
n <- 30
set.seed(123)
x <- rpois(n,lambda_prime) #q(x)

post_dist <- function(lambda,x,beta){
  n <- length(x)
  a_n <- beta*sum(x) + a
  b_n <- beta*n + b
  dgamma(lambda,a_n,b_n)
}
z <- seq(0,10,length.out=101)
y <- post_dist(z,x,beta=1)
plot(z,y,type="l")

ggplot(data=data_frame(x=z,y=y)) +
  geom_line(aes(x,y)) +
  scale_x_continuous(breaks = 0:10 , labels = 0:10) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig9.pdf",width=8,height=5,unit="cm")


#### figure10 ####
library(rstan)
library(magrittr)
library(bridgesampling)

L_n <- function(lambda,x){
  temp <- - mean(x)*log(lambda) + mean(lgamma(x+1)) + lambda
  return(temp)
}
WBIC <- function(x){
  n <- length(x)
  wbic_temp <- function(lambda){
    n*L_n(lambda,x)*post_dist(lambda,x,beta=1/log(n))
  }
  integrate(wbic_temp,0,Inf)$value
}

WBIC(x)
F_n(x)

model.fe <- stan_model("FE_poisson.stan")
model.wbic <- stan_model("FE_poisson_WBIC.stan")

WBIC.mcmc <- function(log_lik){
  -mean(rowSums(log_lik))
}
fit.wbic <- sampling(model.wbic,data=list(N=n,X=x,a=a,b=b),iter=4000,chains=4)
WBIC.mcmc(rstan::extract(fit.wbic)$log_lik)

WBIC_e <- c()
BIC_e <- c()
BS_e <- c()
WBIC.mcmc_e <- c()
F_n_e <- c()
set.seed(123)
for(i in 1:100){
  x <- rpois(n,lambda_prime)
  WBIC_e[i] <- WBIC(x)
  F_n_e[i] <- F_n(x)
  fit.fe <- sampling(model.fe,data=list(N=n,X=x,a=a,b=b),iter=20000,chains=4)
  BS_e[i] <- fit.fe %>% bridge_sampler(method="warp3") %>% logml %>% multiply_by(-1)
  fit.wbic <- sampling(model.wbic,data=list(N=n,X=x,a=a,b=b),iter=4000,chains=4)
  WBIC.mcmc_e[i] <- fit.wbic %>% rstan::extract() %$% log_lik %>% WBIC.mcmc
}

mean(WBIC_e)
mean(WBIC.mcmc_e)
mean(BS_e)
mean(F_n_e)

boxplot(data.frame(WBIC_e,WBIC.mcmc_e,BS_e,F_n_e),names=c("WBIC","WBIC.mcmc","BS","F_n"))

F_n_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12))
par(new=T)
WBIC_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12),lty=2)
legend("topleft",legend=c("F_n","WBIC"),lty=c(1,2))

F_n_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12))
par(new=T)
WBIC.mcmc_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12),lty=2)
legend("topleft",legend=c("F_n","WBIC.mcmc"),lty=c(1,2))

F_n_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12))
par(new=T)
BS_e %>% density %>% plot(xlim=c(45,70),ylim=c(0,0.12),lty=2)
legend("topleft",legend=c("F_n","BS"),lty=c(1,2))


WBICsim<-data_frame(F_n=F_n_e,
           WBIC=WBIC_e,
           WBIC.mcmc=WBIC.mcmc_e,
           BS=BS_e)
# write.csv(WBICsim,"WBICsim.csv")
# WBICsim<-read.csv("WBICsim.csv")[,-1]

WBICsim %>% gather(key=index,value=x,-F_n, factor_key = TRUE) %>% 
ggplot() +
  geom_line(aes(x=x),stat="density",linetype = "longdash", size=0.5,alpha=0.5) +
  geom_line(aes(x=F_n),stat="density",linetype = "solid", size=0.5) +
  facet_grid(index ~.)   +
  xlim(45, 70) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig10.pdf",width=16,height=16,unit="cm")


#### figure11 ####

#predictive distribution
pred_dist <- function(x_d,x){
  a_n <- sum(x)+a
  b_n <- length(x)+b
  temp <- lgamma(x_d+a_n)-lgamma(a_n)-lgamma(x_d+1)+a_n*log(b_n)-(x_d+a_n)*log(1+b_n)
  return(exp(temp))
}

#information amount of predictive distribution
i_pred_dist <- function(x_d,x){
  a_n <- sum(x)+a
  b_n <- length(x)+b
  -lgamma(x_d+a_n) + lgamma(a_n)+lgamma(x_d+1) - a_n*log(b_n) + (x_d+a_n)*log(1+b_n)
}

z <- seq(0,10)
y <- pred_dist(z,x)
barplot(y,names.arg=z)

ggplot(data=data_frame(x=z,y=y)) +
  geom_col(aes(x,y)) +  
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") +
  scale_x_continuous(breaks = z , labels = z)
# ggsave("../TeX/ch07_fig11.pdf",width=8,height=5,unit="cm")


#### figure12 ####
library(rstan)

#generalization loss
G_n <- function(lambda_prime,x){
  temp <- 0
  for(j in 0:20){
    temp = temp + dpois(j,lambda_prime)*i_pred_dist(j,x)
  }
  return(temp)
}

#emprical loss
T_n <- function(x){
  temp <- c()
  for(i in 1:length(x)){
    temp[i] = i_pred_dist(x[i],x)
  }
  return(mean(temp))
}

#functional variance
V_n <- function(x){
  a_n <- sum(x)+a
  b_n <- length(x)+b
  V_comp <- function(x){
    poisgamma <- function(lambda){
      dpois(x,lambda,log=T)*dgamma(lambda,a_n,b_n)
    }
    poisgamma2 <- function(lambda){
      dpois(x,lambda,log=T)^2*dgamma(lambda,a_n,b_n)
    }
    temp1 <- integrate(poisgamma2,0,Inf)$value
    temp2 <- integrate(poisgamma,0,Inf)$value
    return(temp1-temp2^2)
  }
  temp <- c()
  for(i in 1:length(x)){
    temp[i] <- V_comp(x[i])
  }
  return(sum(temp))
}

#Watanabe-Akaike Information Criterion
WAIC <- function(x){
  T_n(x)+V_n(x)/length(x)
}

waic_mcmc <- function(log_lik){
  T_n <- mean(-log(colMeans(exp(log_lik))))
  V_n_divide_n <- mean(apply(log_lik,2,var))
  waic <- T_n + V_n_divide_n
  return(waic)
}

model.waic <- stan_model("poisson_WAIC.stan")

WAIC_e <- c()
WAIC_mcmc <- c()
G_n_e <- c()
set.seed(123)
for(i in 1:100){
  x <- rpois(n,lambda_prime)
  WAIC_e[i] <- WAIC(x)
  fit.waic <- sampling(model.waic, data=list(N=n,X=x,a=a,b=b),cores=1)
  log_lik <- rstan::extract(fit.waic)$log_lik
  WAIC_mcmc[i] <- waic_mcmc(log_lik)
  G_n_e[i] <- G_n(lambda_prime,x)
}
mean(WAIC_e)
mean(WAIC_mcmc)
mean(G_n_e)

boxplot(data.frame(WAIC_e, WAIC_mcmc,G_n_e),names=c("WAIC","WAIC_mcmc","G_n"))


data_frame(WAIC=WAIC_e,WAIC_mcmc=WAIC_mcmc,G_n=G_n_e) %>%
  gather(key = "x",value = "y") %>% 
  mutate(x = forcats::fct_inorder(x)) %>% 
  ggplot() +
  geom_boxplot(aes(x,y),lwd=0.5,outlier.shape = 21, outlier.size = 0.5) + 
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  xlab("") + ylab("") 
# ggsave("../TeX/ch07_fig12.pdf",width=12,height=5,unit="cm")


