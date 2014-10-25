# 	法学セミナー「法律家のための実証分析入門」第12回 Rソースコード
#	(C) 2012 MORITA Hatsuru

library(lattice)

porco <-  read.csv("../csv/PorcoRosso.csv", header=TRUE)

result.multi <- lm(flying~age+educ+spouseeduc+otherfaminc+exp+expsq+kids6+kids18, data=porco)
summary(result.multi)

result.multi2 <- lm(flying~age+educ+spouseeduc+otherfaminc+exp+kids6+kids18, data=porco)
summary(result.multi2)

result.multi.prb <- glm(flying~age+educ+spouseeduc+otherfaminc+exp+kids6+kids18, data=porco, family=binomial(link="probit"))
summary(result.multi.prb)

result.multi2.prb <- glm(flying~age+educ+spouseeduc+otherfaminc+exp+kids6+kids18, data=porco, family=binomial(link="probit"))
summary(result.multi2.prb)

result.multi2.lgt <- glm(flying~age+educ+spouseeduc+otherfaminc+exp+kids6+kids18, data=porco, family=binomial(link="logit"))
summary(result.multi2.lgt)

result.age <- lm(flying ~ age, data=porco)
summary(result.age)

xyplot(flying ~ age, data = porco,
	xlab = "Age",
	ylab = "Flying",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result.educ <- lm(flying ~ educ, data=porco)
summary(result.educ)

xyplot(flying ~ educ, data = porco,
	xlab = "Education",
	ylab = "Flying",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result.otherfaminc <- lm(flying ~ otherfaminc, data=porco)
summary(result.otherfaminc)

xyplot(flying ~ otherfaminc, data = porco,
	xlab = "Other Family Income",
	ylab = "Flying",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result.exp <- lm(flying ~ exp, data=porco)
summary(result.exp)

result.exp.prb <- glm(flying ~ exp, data=porco, family=binomial(link="probit"))
summary(result.exp.prb)

valuex <- seq(from=min(porco$exp), to=max(porco$exp)+0.5, length.out=10000)

probitFit <- data.frame(cbind(exp=valuex,probability=pnorm(result.exp.prb$coef["(Intercept)"]+result.exp.prb$coef["exp"]*valuex, mean=0, sd=1)))

xyplot(flying ~ exp, data = porco,
	xlab = "Flying Experience",
	ylab = "Flying",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

xyplot(flying ~ exp, data = porco,
	xlab = "Flying Experience",
	ylab = "Flying",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=2, col="green")
		panel.xyplot(probitFit$exp,probitFit$probability, type="l", col="red", lty=1)
		},
	key = list(text=list(c("Probit Fit","Linear Fit")),
		lines=list(col=c("red","green"),lty=c(1,2)), 
		space="top", border=T)
	)

exp_1 <- subset(porco, flying==1)
exp_0 <- subset(porco, flying==0)

# another way to extract subset from a data frame
# exp_1 <- porco[porco$flying==1,]
# exp_0 <- porco[porco$flying==0,]

xyplot(flying ~ exp, data = porco,
	xlab = "Flying Experience",
	ylab = "Flying",
	panel = function(x,y){
		panel.lmline(x,y, lty=1, col="red")
		panel.densityplot(exp_1$exp, plot.points=FALSE, lty=2, col="blue")
		panel.densityplot(exp_0$exp, plot.points=FALSE, lty=3, col="green")
		}
	)

densityplot(~exp, data = porco,
	xlab = "Flying Experience",
	ylim = c(-0.002,0.09),
	panel = function(x,y){
		panel.densityplot(exp_1$exp, plot.points=FALSE, lty=1, col="red")
		panel.densityplot(exp_0$exp, plot.points=FALSE, lty=2, col="blue")
		},
	key = list(text=list(c("Flying","Not Flying")),
		lines=list(col=c("red","blue"),lty=c(1,2)), 
		space="top", border=T)
	)


## Simulation Part

# creating up a data set
# if you want to create a data set where LPM doesn't matter, you should set the sd to, say, 10.
LimitedDep <- data.frame(cbind(status=rep(1:0, each=50),cause=(c(rnorm(50,mean=5,sd=3),rnorm(50,mean=1,sd=3)))))

valuex <- seq(from=min(LimitedDep$cause)-7, to=max(LimitedDep$cause)+7, length.out=10000)

result.ols <- lm(status~cause, data=LimitedDep)
result.prb <- glm(status~cause, data=LimitedDep, family=binomial(link="probit"))

probitFit <- data.frame(cbind(val=valuex,probability=pnorm(result.prb$coef["(Intercept)"]+result.prb$coef["cause"]*valuex, mean=0, sd=1)))

xyplot(status~cause, data=LimitedDep,
	xlab="Cause",
	ylab="Status/Probability",
	panel=function(x,y){
		panel.xyplot(x,y,pch=1,col="blue")
		panel.lmline(x,y,type="l",col="green",lty=2)
		panel.xyplot(probitFit$val,probitFit$probability, type="l", col="red", lty=1)
		},
	key = list(text=list(c("Normal Dist Fit","Linear Fit")),
		lines=list(col=c("red","green"),lty=c(1,2)), 
		space="top", border=T)
	)

probitFit2 <- data.frame(cbind(val=valuex,probability=pnorm(result.prb$coef["(Intercept)"]+1+result.prb$coef["cause"]*valuex, mean=0, sd=1)))
probitFit3 <- data.frame(cbind(val=valuex,probability=pnorm(result.prb$coef["(Intercept)"]+0.24+result.prb$coef["cause"]*valuex*0.7, mean=0, sd=1)))

xyplot(status~cause, data=LimitedDep,
	xlab="Cause",
	ylab="Status/Probability",
	panel=function(x,y){
		panel.xyplot(x,y,pch=1,col="blue")
		panel.xyplot(probitFit$val,probitFit$probability, type="l", col="red", lty=1)
		panel.xyplot(probitFit2$val,probitFit2$probability, type="l", col="purple", lty=3)
		panel.xyplot(probitFit2$val,probitFit3$probability, type="l", col="green", lty=2)
		},
	key = list(text=list(c("Normal Dist Fit","Diff Intercept","Diff Slope")),
		lines=list(col=c("red","purple","green"),lty=c(1,3,2)), 
		space="top", border=T)
	)

summary(result.ols)
summary(result.prb)


