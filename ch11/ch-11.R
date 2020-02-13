library(tidyverse)
library(rstan)
# library(bridgesampling)

#################################################
# ## データに関して
# このデータはオリジナルのssp2015調査デーから
# 1000サンプルをランダムに抜き出したデータです．
# 分析の結果はテキストと完全には一致しません
# 
# ## bridgesamplingについて
# bridgesamplingによる対数周辺尤度の推定には
# 大きなメモリ領域が必要となります．とくに，
# 階層モデルでの実行の際にはご注意ください．
#################################################

read.csv("SSP2015_for_ch11.csv") %>% 
  as_tibble() %>% 
  mutate(
    gender = factor(gender,levels = c("male","female")),
    demo_cate = factor(case_when(
      gender == "male" & agecohort == "ar30" ~ "m30",
      gender == "male" & agecohort == "ar40" ~ "m40",
      gender == "male" & agecohort == "ar50" ~ "m50",
      gender == "male" & agecohort == "ar60" ~ "m60",
      gender == "female" & agecohort == "ar30" ~ "f30",
      gender == "female" & agecohort == "ar40" ~ "f40",
      gender == "female" & agecohort == "ar50" ~ "f50",
      gender == "female" & agecohort == "ar60" ~ "f60"),
      levels = c("m30","m40","m50","m60","f30","f40","f50","f60"))
    ) -> data_incomeci


## empirical distributions -----------------------------------

## figure 11.1
incomebins<-c(seq(0,400,50),seq(450,2050,100))
ggplot(data = data_incomeci, aes(x = indivincome)) +
  geom_histogram(aes(y = ..density..), breaks = incomebins,
                 fill="grey", color= "black", size=0.1) +
  stat_function(size=0.5,fun=dlnorm,
                args=list(mean=mean(log(data_incomeci$indivincome)),
                          sd=sd(log(data_incomeci$indivincome)))) +
  xlim(0,2000) + theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank())
# ggsave("SSPI2015_Comparison_model_incomedist_bw.pdf",
#        width=8,height=5,unit="cm")

## figure 11.2
incomebins<-c(seq(0,400,50),seq(450,2050,100))
data_incomeci %>% group_by(gender,agecohort) %>% 
  summarise(mu = mean(log(indivincome)),
            sigma = sd(log(indivincome))
  )  %>% uncount(.,2000,.id = "id") %>%
  mutate(x =id-1,dens = dlnorm(x,mu,sigma)) %>% 
  select(gender,agecohort,x,dens) ->data_for_lognormal_dist

ggplot(data = data_incomeci, aes(x = indivincome)) +
  geom_histogram(aes(y = ..density..), breaks = incomebins, 
                 fill="grey", color= "black",size=0.1) +
  geom_line(size=0.5,data=data_for_lognormal_dist,
            aes(x=x,y=dens),color="black") +
  facet_grid(gender ~ agecohort) +
  xlim(0,2000) + theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("SSPI2015_Comparison_model_incomedist_group_bw.pdf",
#        width=12,height=6,unit="cm")


## figure 11.3
ggplot(data = data_incomeci, aes(x = evalincome)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, fill="grey", color= "black", size=0.1) +
  scale_x_continuous(breaks = 1:10)  +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank())
# ggsave("SSPI2015_Comparison_model_evaldist_bw.pdf",
#        width=8,height=5,unit="cm")

## figure 11.4
ggplot(data = data_incomeci, aes(x = evalincome)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, fill="grey", color= "black",size = 0.1) +
  facet_grid(gender ~ agecohort) +
  scale_x_continuous(breaks = 1:10) + 
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("SSPI2015_Comparison_model_evaldist_group_bw.pdf",
#        width=12,height=6,unit="cm")


###################################################
################ Bayesian modeling ################
###################################################

## figure 11.8
ab <- tibble(a=c(0,0,0,0), b=c(0,0.2,1,2))
ggplot(tibble(p=seq(0,1,0.01)), aes(x=p)) +
  mapply(
    function(a, b, co) 
      stat_function(fun=function(p,a,b) 1/(1+exp(-(a+b*log(p/(1-p))))),
                    args=list(a=a, b=b), aes_q(linetype=co)),
    ab$a, ab$b, sprintf("b=%.1f", ab$b) 
  ) + coord_fixed(ratio=1) + 
  labs(linetype="parameter") + ylab("pi") +
  scale_linetype_manual(values=c("solid", "twodash", "dotted", "dashed")) +
  theme_bw() +
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10,face="bold"),  
        axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        strip.text.x = element_text(size=8,face="bold"),
        strip.text.y = element_text(size=8,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_p_pi_bw.pdf",
#        width=12,height=8,unit="cm")

## figure 11.9
ab <- tibble(a=c(0,0,0,0), b=c(0,0.2,1,2))
ggplot(tibble(x=seq(0,1000)), aes(x=x)) +
  mapply(
    function(a, b, co) 
      stat_function(fun=function(x,a,b) 1/(1+exp(-a-b*log(plnorm(x,5,1)
                                                          /(1-plnorm(x,5,1))))),
                    args=list(a=a, b=b), aes_q(linetype=co)),
    ab$a, ab$b, sprintf("b=%.1f", ab$b) 
  ) + labs(linetype="parameter") + ylab("pi") +
  scale_linetype_manual(values=c("solid", "twodash", "dotted", "dashed")) +
  theme_bw() +
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10,face="bold"),  
        axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        strip.text.x = element_text(size=8,face="bold"),
        strip.text.y = element_text(size=8,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_x_pi_bw.pdf",
#        width=12,height=8,unit="cm")


## null model -----------------------------------
Y = data_incomeci$evalincome -1
N = length(data_incomeci$evalincome) 
X = data_incomeci$indivincome

# For parallelizing
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

null_model <- stan_model( file="model/model-11-1.stan" ) 
stanFit_null <- sampling( object=null_model , 
                          data = list(Y=Y, N=N, X=X) , 
                          chains = 4 ,
                          iter = 6000 , 
                          warmup = 1000 , 
                          thin = 1 ,
                          seed = 8931)

# saveRDS(stanFit_null,"stanFit_null.rds")
# stanFit_null<-readRDS("stanFit_null.rds")

rstan::traceplot(stanFit_null,pars=c("a","pi","mu","sigma"))
rstan::stan_dens(stanFit_null,pars=c("a","pi","mu","sigma"))

summaryfit<-summary(stanFit_null,digit=3,pars=c("a","pi","mu","sigma"))$summary
summaryfit
# write.csv( summaryfit, "null_summaryfit.csv")

# ## bridgesampling
# bs_null <- bridge_sampler(stanFit_null)
# logml_null<-logml(bs_null)
# logml_null ## log marginal likelihood of the model


## linear biased model -----------------------------------

Y = data_incomeci$evalincome -1
N = length(data_incomeci$evalincome) 
X = data_incomeci$indivincome
p_new <- seq(0.01,0.99,0.01)
N_p_new <- length(p_new)
X_new <- seq(1,2000)
N_X_new <- length(X_new)

# For parallelizing
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

linear_biased_model <-
  stan_model( file="model/model-11-2.stan" ) 
stanFit_linear_biased <- sampling( object=linear_biased_model , 
                          data = list(Y=Y, N=N, X=X, p_new=p_new, 
                                      N_p_new=N_p_new, X_new=X_new, N_X_new=N_X_new) , 
                          chains = 4 ,
                          iter = 6000 , 
                          warmup = 1000 , 
                          thin = 1 ,
                          seed = 8931) 

# saveRDS(stanFit_linear_biased,"stanFit_linear_biased.rds")
# stanFit_linear_biased<-readRDS("stanFit_linear_biased.rds")

rstan::traceplot(stanFit_linear_biased,pars=c("a","b","mu","sigma"))
rstan::stan_dens(stanFit_linear_biased,pars=c("a","b","mu","sigma"))

summaryfit2<-summary(stanFit_linear_biased,digit=3,pars=c("a","b","mu","sigma"))$summary
summaryfit2
# write.csv( summaryfit2, "linear_biased_summaryfit.csv")

## relation bw p and pi
ms<-rstan::extract(stanFit_linear_biased)
p_pi_mcmc_qtile<-apply(ms$pi_p_new, 2, quantile, probs=c(2.5, 25, 50, 75, 97.5)/100)

linear_biased_p_pi<-tibble(
  p=p_new,
  t_pi = p_new,
  pi=p_pi_mcmc_qtile["50%",],
  p2_5=p_pi_mcmc_qtile["2.5%",],
  p25=p_pi_mcmc_qtile["25%",],
  p75=p_pi_mcmc_qtile["75%",],
  p97_5=p_pi_mcmc_qtile["97.5%",])

## figure 11.8
ggplot(data=linear_biased_p_pi) +
  geom_line(aes(x=p, y=pi)) +
  geom_ribbon(aes(x=p, ymin=p2_5, ymax=p97_5), alpha=1/6) +
  geom_ribbon(aes(x=p, ymin=p25, ymax=p75), alpha=2/6) +
  geom_line(aes(x=p, y=t_pi), linetype ="dashed") +
  xlim(0,1) + ylim(0,1) + coord_fixed(ratio=1) +
  theme_bw() +
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10,face="bold"),  
        axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        strip.text.x = element_text(size=8,face="bold"),
        strip.text.y = element_text(size=8,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_lbiased_p_pi_bw.pdf",
#        width=9,height=8,unit="cm")

## relation bw X and pi
X_pi_mcmc_qtile<-apply(ms$pi_X_new, 2, quantile, probs=c(2.5, 25, 50, 75, 97.5)/100)

linear_biased_X_pi<-tibble(
  X=X_new,
  t_pi = plnorm(X_new,mean(log(data_incomeci$indivincome)),
                sd(log(data_incomeci$indivincome))),
  pi=X_pi_mcmc_qtile["50%",],
  p2_5=X_pi_mcmc_qtile["2.5%",],
  p25=X_pi_mcmc_qtile["25%",],
  p75=X_pi_mcmc_qtile["75%",],
  p97_5=X_pi_mcmc_qtile["97.5%",])

## figure 11.9
ggplot(data=linear_biased_X_pi) +
  geom_line(aes(x=X, y=pi)) +
  geom_ribbon(aes(x=X, ymin=p2_5, ymax=p97_5), alpha=1/6) +
  geom_ribbon(aes(x=X, ymin=p25, ymax=p75), alpha=2/6) +
  geom_line(aes(x=X, y=t_pi), linetype ="dashed") +
  xlim(0,2000) + ylim(0,1) +
  theme_bw() +
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10,face="bold"),  
        axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        strip.text.x = element_text(size=8,face="bold"),
        strip.text.y = element_text(size=8,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_lbiased_X_pi_bw.pdf",
#        width=12,height=8,unit="cm")

# ## bridgesampling
# bs_linear_biased <- bridge_sampler(stanFit_linear_biased)
# logml_linear_biased<-logml(bs_linear_biased)
# logml_linear_biased ## log marginal likelihood of the model
# 
# # ## log BF
# logBF<-logml_linear_biased-logml_null
# 
# logml_summary<-c(logml_null,logml_linear_biased,logBF)
# names(logml_summary)<-c("logml_null","logml_linear_biased","logBF_lbiased_null")
# logml_summary


## linear biased hierarchical model ----------------------------------

Y = data_incomeci$evalincome -1
N = length(data_incomeci$evalincome) 
X = data_incomeci$indivincome
Z = as.numeric(data_incomeci$demo_cate)
K = length(levels(data_incomeci$demo_cate))
p_new <- seq(0.01,0.99,0.01)
N_p_new <- length(p_new)
X_new <- seq(1,2000)
N_X_new <- length(X_new)

# For parallelizing
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

hierarchical_model <-
  stan_model( file="model/model-11-3.stan" ) 
stanFit_hierarchical <- sampling( object=hierarchical_model , 
                                   data = list(Y=Y, N=N, X=X, Z=Z, K=K, p_new=p_new,
                                               N_p_new=N_p_new, X_new=X_new, N_X_new=N_X_new) , 
                                   chains = 4 ,
                                   iter = 6000 , 
                                   warmup = 1000 , 
                                   thin = 1 ,
                                   seed = 8931)

# saveRDS(stanFit_hierarchical,"stanFit_hierarchical.rds")
# stanFit_hierarchical<-readRDS("stanFit_hierarchical.rds")

rstan::traceplot(stanFit_hierarchical,
                 pars=c("a","b","mu","sigma","a0","b0","s_a","s_b"))
rstan::stan_dens(stanFit_hierarchical,
                 pars=c("a","b","mu","sigma","a0","b0","s_a","s_b"))

summaryfit3<-summary(stanFit_hierarchical,digit=3,
                     pars=c("a","b","mu","sigma","a0","b0","s_a","s_b"))$summary
summaryfit3
# write.csv( summaryfit3, "stanFit_hierarchical.csv")

## relation bw p and pi
ms2<-rstan::extract(stanFit_hierarchical)
p_pi_mcmc_qtile<-matrix(NA, nrow = 5, ncol = (N_p_new)*K)
for (k in 1 : K) {
  p_pi_mcmc_qtile[,(1+N_p_new*(k-1)):(N_p_new*k)]<-apply(ms2$pi_p_new[,k,],
                       2, quantile, 
                       probs=c(2.5, 25, 50, 75, 97.5)/100)
}

hierarchical_p_pi<-tibble(
  gender = factor(c(rep("male",4*N_p_new),rep("female",4*N_p_new)),
    levels = c("male","female")),
  agecohort = factor(rep(c(rep("ar30",N_p_new),rep("ar40",N_p_new),
                           rep("ar50",N_p_new),rep("ar60",N_p_new)),2)),
  p=rep(p_new,K),
  t_pi = rep(p_new,K),
  pi=p_pi_mcmc_qtile[3,],
  p2_5=p_pi_mcmc_qtile[1,],
  p25=p_pi_mcmc_qtile[2,],
  p75=p_pi_mcmc_qtile[4,],
  p97_5=p_pi_mcmc_qtile[5,])

## figure 11.10
ggplot(data=hierarchical_p_pi) +
  geom_line(aes(x=p, y=pi)) +
  geom_ribbon(aes(x=p, ymin=p2_5, ymax=p97_5), alpha=1/6) +
  geom_ribbon(aes(x=p, ymin=p25, ymax=p75), alpha=2/6) +
  geom_line(aes(x=p, y=t_pi), linetype ="dashed") + 
  facet_grid(gender ~ agecohort) +
  xlim(0,1) + ylim(0,1) + coord_fixed(ratio=1)  +
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_hierarchical_p_pi_bw.pdf",
#        width=14,height=8,unit="cm")

## relation bw X and pi
X_pi_mcmc_qtile<-matrix(NA, nrow = 5, ncol = (N_X_new)*K)
for (k in 1 : K) {
  X_pi_mcmc_qtile[,(1+N_X_new*(k-1)):(N_X_new*k)]<-apply(ms2$pi_X_new[,k,],
                                                         2, quantile, 
                                                         probs=c(2.5, 25, 50, 75, 97.5)/100)
}

data_incomeci %>% group_by(gender,agecohort) %>% 
  summarise(mu = mean(log(indivincome)),
            sigma = sd(log(indivincome))
  )  %>% uncount(.,2000,.id = "id") %>% 
  mutate(X =id,t_pi = plnorm(X,mu,sigma)) %>% 
  select(gender,agecohort,X,t_pi)  %>% 
  bind_cols(pi=X_pi_mcmc_qtile[3,],
            p2_5=X_pi_mcmc_qtile[1,],
            p25=X_pi_mcmc_qtile[2,],
            p75=X_pi_mcmc_qtile[4,],
            p97_5=X_pi_mcmc_qtile[5,])->hierarchical_X_pi

## figure 11.11
ggplot(data=hierarchical_X_pi) +
  geom_line(aes(x=X, y=pi)) +
  geom_ribbon(aes(x=X, ymin=p2_5, ymax=p97_5), alpha=1/6) +
  geom_ribbon(aes(x=X, ymin=p25, ymax=p75), alpha=2/6) +
  geom_line(aes(x=X, y=t_pi), linetype ="dashed") + 
  facet_grid(gender ~ agecohort) +
  xlim(0,2000) + ylim(0,1) + 
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        strip.text.y = element_text(size=6,face="bold"),
        legend.position = "right")
# ggsave("SSPI2015_Comparison_model_hierarchical_X_pi_bw.pdf",
#        width=16,height=8,unit="cm")

# ## bridgesampling
# rm(stanFit_null, null_model,
#    stanFit_linear_biased, linear_biased_model,
#    linear_biased_p_pi, linear_biased_X_pi, ms,
#    hierarchical_p_pi, hierarchical_X_pi, ms2)
# gc()
# 
# bs_hierarchical <- bridge_sampler(stanFit_hierarchical)
# logml_hierarchical<-logml(bs_hierarchical)
# logml_hierarchical ## log marginal likelihood of the model
# 
# ## log BF
# logBF2<-logml_hierarchical-logml_summary[2]
# 
# logml_summary2<-c(logml_summary[1],logml_summary[2],
#                   logml_hierarchical,logml_summary[3],logBF2)
# names(logml_summary2)<-c("logml_null","logml_linear_biased","logml_hierarchical",
#                          "logBF_lbiased_null","logBF_hierarchical_lbiased")
# logml_summary2
# # write.csv(logml_summary2, file="logml_summary2.csv")





