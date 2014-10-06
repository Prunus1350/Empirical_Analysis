# 	法学セミナー「法律家のための実証分析入門」第26回 Rソースコード
#	(C) 2013 MORITA Hatsuru

library(lattice)
library(R2WinBUGS)
library(BRugs)
library(coda)

setwd("c:/Users/Hatsuru/Documents/ec/")

#
# plotting the update from prior to posterior

x <- seq(-20,20,length=1000)
y <- dnorm(x, mean=0, sd=10)
z <- dnorm(x, mean=4, sd=3)
lik <- dnorm(x, mean=5, sd=2.5)

xyplot(y~x,
	xlab = "beta",
	ylab = "density",
	ylim = c(-0.005, 0.17),
	panel = function(x,y){
		panel.xyplot(x,y, type="l", col="blue", lty=2)
		panel.xyplot(x,lik, type="l", col="black", lty=3)
		panel.xyplot(x,z, type="l", col="red", lty=1)
	},
	key = list(text=list(c("Prior","Likelihood","Posterior")),
		lines=list(col=c("blue","black","red"),lty=c(2,3,1)), 
		space="top", border=T
	)
)


#
# computing pi via monte carlo

s <- 1000000
a <- runif(s,-1,1)
b <- runif(s,-1,1)
(pi <- 2*2*sum(a^2+b^2 < 1)/s)

#
# running OpenBUGS

# the data
x <- c(22,4,2,2,9,3,4,5,2,6)
N <- length(x)

pois.data <- list("N","x")
bugs.data(pois.data)

pois.inits <- function(){
	list(
		alpha=rlnorm(1,0,5),
		beta=rlnorm(1,0,5),
		lambda=runif(N,0,10))
}

## alternative initial values
#pois.inits <- list(
#		list(alpha=  1, beta=  1),
#		list(alpha=0.5, beta=0.5),
#		list(alpha=0.1, beta=0.1))

pois.parameters <- c("alpha", "beta", "lambda")

# run in OpenBUGS
pois.sim <- bugs(pois.data, pois.inits, pois.parameters,
		"IntEmpR26.bug", n.chain=3, n.iter=10000,
		n.thin=1, program="OpenBUGS")

print(pois.sim)
plot(pois.sim)

