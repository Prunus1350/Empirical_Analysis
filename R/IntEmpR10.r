# 	法学セミナー「法律家のための実証分析入門」第10回 Rソースコード
#	(C) 2012 MORITA Hatsuru

library(lattice)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

# setting conditions
n_obs1 <- 100
n_obs2 <- 50
n_rep  <- 10000

# monte carlo for omitted variable biases
result1 <- data.frame(cbind(unbiased=rep(NA,n_rep),biased=rep(NA,n_rep),corr=rep(NA,n_rep)))

for (i in 1:n_rep){
	educ <- 14 + rnorm (n=n_obs1, mean=0, sd=2)
	abil <- educ * 7 + 2 + rnorm (n=n_obs1, mean=0, sd=8)
	wage <- 50 + educ*2 + abil*0.8 + rnorm (n=n_obs1, mean=0, sd = 20)
	result1$unbiased[i] <- (lm(wage~educ+abil))$coefficients[2]
	result1$biased[i]   <- (lm(wage~educ))$coefficients[2]
	result1$corr[i]     <- cor(educ,abil)
}

summary(result1)

densityplot(~result1$unbiased+result1$biased,
	xlab = "Estimated Value",
	ylab = "Density",
	panel = function(x,...){
              panel.densityplot(result1$unbiased, plot.points = FALSE, type="l", col="blue", lty=1)
              panel.densityplot(result1$biased,   plot.points = FALSE, type="l", col="red",  lty=2)
	},
	key = list(text=list(c("Unbiased Estimates","Biased Estimates")),
		lines=list(col=c("blue","red"),lty=c(1,2)), 
		space="top", border=T)
)

# monte carlo for advantage of large data sets
result2 <- data.frame(cbind(obs100=rep(NA,n_rep),obs50=rep(NA,n_rep)))

for (i in 1:n_rep){
	educ <- 14 + rnorm (n=n_obs1, mean=0, sd=2)
	abil <- educ * 7 + 2 + rnorm (n=n_obs1, mean=0, sd=8)
	wage <- 50 + educ*2 + abil*0.8 + rnorm (n=n_obs1, mean=0, sd = 20)
	result2$obs100[i] <- (lm(wage~educ+abil))$coefficients[2]
	result2$obs50[i]  <- (lm(wage[1:n_obs2]~educ[1:n_obs2]+abil[1:n_obs2]))$coefficients[2]
}

summary(result2)

densityplot(~result2$obs100+result2$obs50,
	xlab = "Estimated Value",
	ylab = "Density",
	panel = function(x,...){
              panel.densityplot(result2$obs100, plot.points = FALSE, type="l", col="blue", lty=1)
              panel.densityplot(result2$obs50 , plot.points = FALSE, type="l", col="red",  lty=2)
	},
	key = list(text=list(c("Obs. = 100","Obs. = 50")),
		lines=list(col=c("blue","red"),lty=c(1,2)), 
		space="top", border=T)
)

