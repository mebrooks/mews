library(mews)
library(parallel)

nobs=1000
R0=8
m=-.03
theta=15
sigma=.2
t=seq(1,150, length=nobs)
X0=18
-R0/tail(t,1)
log_a=log(1+m*tail(t,1)/R0)
ptrue=list(R0=R0, m=m, log_theta=log(theta), log_sigma=log(sigma))
ptruebnd=list(log_R0=log(R0), log_a=log_a, log_theta=log(theta), log_sigma=log(sigma))

object=list(X=data.frame(time=t, value=c(X0, rep(NA, length(t)-1))), 
	pars=c(R0=R0, m=m, theta=theta, sigma=sigma), 
	model="LSN", 
	loglik = NA,
	convergence=0)
class(object) <- c("mews", "list")

sim <- simulate(object)
plot(sim[,1], sim[,2])

#B0 <- stability_model_mews(sim, "LSN")
#B <- stability_model_mews_TMB(sim, "LSN")
#########################################################
#Bounded version

p <- list(log_R0=log(1/max(time(sim[,1]))), m=0, log_theta=log(mean(sim[,2])), log_sigma=log(sd(sim[,2])))
f2 <- MakeADFun(data=list(times= sim[,1], obs= sim[,2]), parameters=p, DLL="OU")    
o <- nlminb(f2 $par, f2 $fn, f2 $gr)
p_est <- o$par  # & use the OU estimated pars as starting guess
p_est["R0"]=exp(p_est["log_R0"])
R0 <- as.numeric(p_est["R0"]^2)
theta <- as.numeric(exp(p_est["log_theta"])+p_est["R0"])
sigma <- as.numeric(abs(exp(p_est["log_sigma"])/sqrt(2*p_est["R0"]+ exp(p_est["log_theta"]))))
p <- list(log_R0=log(R0), log_a=0, log_theta=log(theta), log_sigma=log(sigma))
#p <- list(log_R0=log(1), log_a=0, log_theta=log(1), log_sigma=log(1), R0=.1)
f3 <- MakeADFun(data=list(times=sim[,1], obs= sim[,2]), parameters=p, DLL="LSN")
#f3$env$tracepar=TRUE
o <- nlminb(f3 $par, f3 $fn, f3 $gr)

f4 <- MakeADFun(data=list(times=sim[,1], obs= sim[,2]), parameters=ptruebnd, DLL="LSN")
#f4$env$tracepar=TRUE
o2 <- nlminb(f4 $par, f4 $fn, f4 $gr)

o$par
ptruebnd

o$objective
f4 $fn(ptruebnd)

###If that works, then try stability_model comparison
A= stability_model_mews(sim,"OU")
B=stability_model_mews(sim, "LSN")
observed <- -2 * (logLik(A) - logLik(B))
reps <- lapply(1:100, function(i) compare(A,B))
lr <- lik_ratios(reps)
roc <- roc_data(lr)
require(ggplot2)
ggplot(lr) + geom_density(aes(value, fill=simulation), alpha=0.6) + geom_vline(aes(xintercept=observed))
ggplot(roc) + geom_line(aes(False.positives, True.positives))
