# 	法学セミナー「法律家のための実証分析入門」第13回 Rソースコード
#	(C) 2012 MORITA Hatsuru

library(lattice)
library(MASS)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

# writing graphs for ML

y <- c(1.364, 0.235, -0.846, -0.285, -1.646)
mu <- mean(y)
x <- c(0,0,0,0,0)

tau <- dnorm(y, mean=-2, sd=1)

xyplot(x~y,
	aspect=0.7,
	xlab = "y",
	ylab = "Probability",
	xlim = c(-6.4,6.4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
	panel.mathdensity(dmath=dnorm, args=c(-2,1), n=100, col="red")
	panel.xyplot(x,y, pch=21) 
	lsegments(1.364, 0, 1.364, tau[1], col="black", lty=2)
	lsegments(0.235, 0, 0.235, tau[2], col="black", lty=2)
	lsegments(-0.846, 0, -0.846, tau[3], col="black", lty=2)
	lsegments(-0.285, 0, -0.285, tau[4], col="black", lty=2)
	lsegments(-1.646, 0, -1.646, tau[5], col="black", lty=2)
	}
	)

# dev.print(file="hoge.ps")

xyplot(x~y,
	aspect=0.7,
	xlab = "y",
	ylab = "Probability",
	xlim = c(-6.4,6.4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
	panel.mathdensity(dmath=dnorm, args=c(1.5,1), n=100, col="blue")
	panel.xyplot(x,y, pch=21) 
	}
	)

xyplot(x~y,
	aspect=0.7,
	xlab = "y",
	ylab = "Probability",
	xlim = c(-6.4,6.4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
	panel.mathdensity(dmath=dnorm, args=c(0,1), n=100, col="green")
	panel.xyplot(x,y, pch=21) 
	}
	)

xyplot(x~y,
	aspect=0.7,
	xlab = "y",
	ylab = "Probability",
	xlim = c(-6.4,6.4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
	panel.mathdensity(dmath=dnorm, args=c(mu,1), n=100, col="orange")
	panel.xyplot(x,y, pch=21) 
	}
	)

xyplot(x~y,
	aspect=0.8,
	xlab = "y",
	ylab = "Probability",
	xlim = c(-6.4,6.4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
		panel.mathdensity(dmath=dnorm, args=c(-2,1), n=100, col="red", lty=2)
		panel.mathdensity(dmath=dnorm, args=c(1.5,1), n=100, col="blue", lty=3)
		panel.mathdensity(dmath=dnorm, args=c(0,1), n=100, col="green", lty=4)
		panel.mathdensity(dmath=dnorm, args=c(mu,1), n=100, col="orange", lty=1)
		panel.xyplot(x,y, pch=21)
		},
	key = list(text=list(c("mu=-2","mu=1.5","mu=0","mu=mean")),
		lines=list(col=c("red","blue","green","orange"),lty=c(2,3,4,1)), 
		space="top", border=T
		)
	)


## simulation for Poisson regression

# generating the data (you can skip this part)
n_obs <- 200
law <- floor(runif(n_obs, min=0, max=2))
pop <- abs(rnorm(n_obs, mean=100, sd=20))
bankruptcy <- floor(exp(-4 + 0.04*pop - law + rnorm (n_obs, mean=0, sd =1)))
bankruptcy
support <- data.frame(cbind(bankruptcy, pop, law))
write.csv(support, file="bankruptcy_sim.csv", row.names=FALSE)

# simulation

support <- read.csv(file="bankruptcy_sim.csv", header=TRUE)

histogram(~bankruptcy, data=support, breaks=20, type="percent")

result.poisson <- glm(bankruptcy~pop+law, data=support, family=poisson)
summary(result.poisson)
logLik(result.poisson)

result.nb <- glm.nb(bankruptcy~pop+law, data=support)
summary(result.nb)
logLik(result.nb)

result.ols <- lm(bankruptcy~pop+law, data=support)
summary(result.ols)

result.ols2 <- glm(bankruptcy~pop+law, data=support, family=gaussian(link="identity"))
summary(result.ols2)
logLik(result.ols2)

# computing z-value threshold
qnorm(0.95)
qnorm(0.975)
qnorm(0.995)
qnorm(0.9995)
