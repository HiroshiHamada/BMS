library(tidyverse)

## Fujii model -----------------------------------------------
## Fujii's match record up to 100 matches
tibble(
  game = 1:100,
  win = unname(unlist(read.table("Fujii.txt"))),
  lose = 1-win
) %>%
  mutate(
    cumwin = cumsum(win),
    cumlose = cumsum(lose),
    mle = cumwin/game
  ) ->data

## figure 8.2
ggplot(data=filter(data,game<=50),aes(x=game,y=win+lose,fill=win)) +
  geom_point(size=2.5,shape=21,stroke=0.5) + 
  scale_fill_gradient(low = "black",high = "white") + 
  scale_x_continuous(breaks = c(1,seq(10,50,10))) +
  ylim(0.9,1.1) +
  theme( panel.background = element_blank(),
         panel.grid = element_blank(),
         axis.title.x = element_blank(),
         axis.text=element_text(size=6),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none") 
# ggsave("..Fujii_hoshitorihyo1.pdf",width=12,height=1,unit="cm")

ggplot(data=filter(data,game>50),aes(x=game,y=win+lose,fill=win)) +
  geom_point(size=2.5,shape=21,stroke=0.5) + 
  scale_fill_gradient(low = "black",high = "white") + 
  scale_x_continuous(breaks = c(51,seq(60,100,10))) +
  ylim(0.9,1.1) +
  theme( panel.background = element_blank(),
         panel.grid = element_blank(),
         axis.title.x = element_blank(),
         axis.text=element_text(size=6),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none") 
# ggsave("..Fujii_hoshitorihyo2.pdf",width=12,height=1,unit="cm")

## posterior
postdata<-tibble(
  game = 0:100,
  post_alpha = c(1,data$cumwin+1),
  post_beta = c(1,data$cumlose+1),
  mle = c(NA,data$mle),
  post_mu = post_alpha/(post_alpha+post_beta),
  post_med = qbeta(0.5,post_alpha,post_beta),
  CI_lower = qbeta(0.025,post_alpha,post_beta),
  CI_upper = qbeta(0.975,post_alpha,post_beta)
)

## figure 8.3
ggplot(data=postdata) +
  geom_ribbon(aes(x=game,y=post_mu,
                  ymin=CI_lower, ymax=CI_upper), fill="grey80")+
  geom_line(aes(x=game,y=mle,linetype = "MLE")) +
  geom_line(aes(x=game,y=post_mu,linetype = "post. dist.")) +
  labs(linetype="Estimation")+
  theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.y = element_blank())
# ggsave("..Fujii_pred.pdf",width=12,height=6,unit="cm")


## figure 8.4
ggplot(data=tibble(q=c(0,1)),aes(x=q)) +
  pmap(list(a=postdata$post_alpha,b=postdata$post_beta,co=postdata$game),
    function(a, b, co) 
      stat_function(fun=dbeta, args=list(shape1=a, shape2=b), aes_q(color=co),size=0.1)
  ) + labs(color="game") +
  scale_colour_gradient(low = "grey80", high = "black")  + theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text=element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("..Fujii_post.pdf",width=10,height=6,unit="cm")


## figure 8.4 with colored gradiation
# ggplot(data=tibble(p=c(0,1)),aes(x=p)) +
#   pmap(list(a=postdata$post_alpha,b=postdata$post_beta,co=postdata$game),
#        function(a, b, co) stat_function(fun=dbeta, args=list(shape1=a, shape2=b), aes_q(color=co))
#   ) + labs(color="game",y="density")


## Fujii model considering first or second hand -----------------------

library(rstan)

data2<-read.table("Fujii_first.txt",header = TRUE)
standata<-list(N=nrow(data2),X=data2$win,Z=data2$first)

fit <- stan(file='model/model-8-1.stan',
            data=standata, 
            iter=5000,
            warmup=1000,
            thin=1,
            chains=4,
            seed=8931)

print(fit)
stan_trace(fit)

## figure 8.5
stan_plot(fit,point_est = "mean",ci_level = 0.95,outer_level = 1.00,
          show_density = T,fill_color = "grey") + theme_bw() +
  theme(legend.text=element_text(size=6),
        legend.title=element_text(size=8,face="bold"),  
        axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=8,face="bold"),
        axis.title=element_text(size=8,face="bold"),
        strip.text.x = element_text(size=6,face="bold"),
        legend.position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave("..Fujii_first_stan.pdf",width=10,height=6,unit="cm")

# stan_hist(fit)
# stan_dens(fit)
# stan_ac(fit,"q_first")
# stan_ac(fit,"q_second")

