# 	法学セミナー「法律家のための実証分析入門」第14回 Rソースコード
#	(C) 2012 MORITA Hatsuru

rm(list=ls())

library(lattice)

t <- c(1:49)
lambda <- 0.4
exponential <- rep(lambda, 49)
weibull08 <- (0.4*t)^(-0.2)*0.4*0.8
weibull12 <- (0.4*t)^(0.2)*0.4*1.2
loglogistic2 <- ((0.4*t))*0.4*4/(1+(0.4*t)^2)

xyplot(loglogistic2~t,
	xlab = "Time",
	ylab = "Probability (Hazard Rate)",
	ylim = c(-0.1,1.1),
	panel = function(x,y){
		panel.xyplot(t,exponential, type="l", col="blue", lty=1)
		panel.xyplot(t,weibull08, type="l", col="red", lty=2)
		panel.xyplot(t,weibull12, type="l", col="orange", lty=3)
		panel.xyplot(t,loglogistic2, type="l", col="green", lty=4)
	},
	key = list(text=list(c("Exponential","Weibull (p=0.8)","Weibull (p=1.2)","Log-Logistic (p=2)")),
		lines=list(col=c("blue","red","orange","green"),lty=c(1,2,3,4)), 
		space="top", border=T
	)
)

