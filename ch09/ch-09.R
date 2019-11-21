
## packages ----------------------------------------------
library(rstan)
library(bridgesampling)

## make stan data ----------------------------------------
dat <- read.csv("discount_data.csv")
N <- ncol(dat[-(1:3)])
Trial <- nrow(dat)
D <- dat$D
amount_soon <- dat$amount_soon/10000
amount_delay <- 5
choice <- t(dat[-(1:3)]-1)

datastan <- list(N=N,Trial=Trial,D=D,
                 amount_delay=amount_delay,amoutn_soon=amount_soon,
                 choice=choice)

## MCMC --------------------------------------------------

#exponential discounting
model.ex <- stan_model("exponential.stan")
fit.ex <- sampling(model.ex,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)

#hyperbolic discounting
model.hb <- stan_model("hyperbolic.stan")
fit.hb <- sampling(model.hb,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)


#exponential discounting fixed model
model.ex.f <- stan_model("exponential_fix.stan")
fit.ex.f <- sampling(model.ex.f,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)

#hyperbolic discounting fixed model
model.hb.f <- stan_model("hyperbolic_fix.stan")
fit.hb.f <- sampling(model.hb.f,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)


#exponential discounting hierarchical model
model.ex.h <- stan_model("exponential_h.stan")
fit.ex.h <- sampling(model.ex.h,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)

#hyperbolic discounting hierarchical  model
model.hb.h <- stan_model("hyperbolic_h.stan")
fit.hb.h <- sampling(model.hb.h,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)

#subjective time model
model.st.h <- stan_model("subject_time_h.stan")
fit.st.h <- sampling(model.st.h,data=datastan,iter=11000,warmup=1000,chains=4,cores=4)


## bridge sampling ---------------------------------------

bs.ex <- bridge_sampler(fit.ex, method= "warp3")
logml(bs.ex)

bs.hb <- bridge_sampler(fit.hb, method= "warp3")
logml(bs.hb)


bs.ex.f <- bridge_sampler(fit.ex.f, method= "warp3")
logml(bs.ex.f)

bs.hb.f <- bridge_sampler(fit.hb.f, method= "warp3")
logml(bs.hb.f)


bs.ex.h <- bridge_sampler(fit.ex.h, method= "warp3")
logml(bs.ex.h)

bs.hb.h <- bridge_sampler(fit.hb.h, method= "warp3")
logml(bs.hb.h)


bs.st.h <- bridge_sampler(fit.st.h, method= "warp3")
logml(bs.st.h)
