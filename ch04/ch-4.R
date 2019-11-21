library(tidyverse)
library(magrittr)

## data ----------------------------------------
data<-c(0,1,1,1,1,0,1,1,0,1)

## functions  ----------------------------------
## Bernoulli likelihood function
L_Bern<-function(x,q) { 
  q^sum(x)*(1-q)^(length(x)-sum(x))
}
# plot(seq(0,1,0.01),L_Bern(data,seq(0,1,0.01)),type="l")

## joint probability distribution
prior_beta<-function(q,a,b) dbeta(q,a,b) #same as dunif(q,0,1)
joint<-function(x,q) L_Bern(x,q)*prior_beta(q,1,1)
# plot(seq(0,1,0.01),joint(data,seq(0,1,0.01)),
#      type="l")

## Metropolis algorithm -------------------------

set.seed(8931)
Metropolis<-function(current) {
  propose<-runif(1,0,1)
  r<-joint(data,propose)/joint(data,current)
  pmove<-min(r,1)
  if(pmove>=runif(1,0,1)) propose else current 
}

nsteps<-10
mcmcsample<-c()
mcmcsample[1]<-0.5 # initial position
for (i in 1:nsteps) {
  mcmcsample[i+1]<-Metropolis(mcmcsample[i])
}

## figure 4.1
ggplot(data=tibble(x=0:10,y=mcmcsample)) +
  geom_line(aes(x,y)) + ylim(0,1) +
  scale_x_continuous(breaks=0:10) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "top") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("Bernoulli_MCMC_traceplot_first.pdf",width=8,height=4,unit="cm")


nsteps<-11000 # number of steps
nburn<-1000 # number of burn in
mcmcsample<-c()
mcmcsample[1]<-0.5 # initial position

for (i in 1:nsteps) {
  mcmcsample[i+1]<-Metropolis(mcmcsample[i])
}

## figure 4.2
ggplot(data=tibble(x=1:10000,y=mcmcsample[-(1:(nburn+1))])) +
  geom_line(aes(x,y),size=0.1) + ylim(0,1) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "top") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("Bernoulli_MCMC_traceplot.pdf",width=10,height=4,unit="cm")

## figure 4.3
ggplot() +
  geom_histogram(data=tibble(x=mcmcsample[-(1:(nburn+1))]),aes(x=x,y=..density..),
                 breaks = seq(0,1,0.05), fill = "lightgrey", color="black") +
  geom_line(data=tibble(x=seq(0,1,0.01),
                            y=dbeta(seq(0,1,0.01),1+sum(data),1+length(data)-sum(data))),
            aes(x=x,y=y),color="black",size = 1) +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "top") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("Bernoulli_MCMC_hist.pdf",width=8,height=5,unit="cm")

## Rhat --------------------------------------------------
set.seed(8931)

nchains<-4   # number of chains
nsteps<-3500 # number of steps
nburn<-1000 # number of burn in
mcmcsamples<-matrix(rep(rep(NA,nsteps + 1),nchains),nsteps+1,nchains)
mcmcsamples[1,]<-rep(0.5,nchains) # initial position

for (j in 1:nchains) {
  for (i in 1:nsteps) {
    mcmcsamples[i+1,j]<-Metropolis(mcmcsamples[i,j])
    }
}

## figure 4.4
mcmcsamples %>%
  set_colnames(c("chain1","chain2","chain3","chain4")) %>% 
  as_tibble() %>%
  slice(-(1:(nburn+1))) %>% mutate(t=row_number()) %>% 
  gather(key=chain,value = q, -t) %>% 
  ggplot() + 
  geom_point(aes(x=t,y=q,group=chain,col=chain,shape=chain),size=1) +
  geom_line(aes(x=t,y=q,group=chain,col=chain),size=0.1) + ylim(0,1) +
  theme_bw() +
  scale_colour_grey() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "top") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("Bernoulli_MCMC_chains.pdf",width=10,height=6,unit="cm")

B<-(nsteps-nburn)*var(apply(mcmcsamples[-(1:(nburn+1)),],2,mean))
W<-mean(apply(mcmcsamples[-(1:(nburn+1)),],2,var))
var_post<-W*(nsteps-nburn-1)/(nsteps-nburn)+B/(nsteps-nburn)
Rhat<-sqrt(var_post/W)
Rhat

## MCMC by Stan ----------------------------------------

library(rstan)
Y<-c(0,1,1,1,1,0,1,1,0,1)
N<-length(Y)
data_for_stan<-list(Y,N)

fit <- stan(file="model/model-4.stan", data=data_for_stan, seed=1234)
print(fit)
stan_trace(fit)
stan_hist(fit)
