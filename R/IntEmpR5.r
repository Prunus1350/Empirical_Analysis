# 	法学セミナー「法律家のための実証分析入門」第5回 Rソースコード
#	(C) 2011 MORITA Hatsuru

library(lattice)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

crimes <-  read.csv("criminal.csv", header=TRUE)

crimes$lncrime <- log(crimes$crime)
for (i in 1:length(crimes$lncrime)){
	if (crimes$lncrime[i] == -Inf) crimes$lncrime[i] <- NA
	}

crimes$lnunemp <- log(crimes$unemp)
for (i in 1:length(crimes$lnunemp)){
	if (crimes$lnunemp[i] == -Inf) crimes$lnunemp[i] <- NA
	}


result.level <- lm(crime~unemp, data=crimes)
summary(result.level)

xyplot(crime ~ unemp, data = crimes,
	aspect=1,
	xlab = "unemployment",
	ylab = "number of crimes",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="black")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result.log <- lm(lncrime~lnunemp, data=crimes)
summary(result.log)

xyplot(lncrime ~ lnunemp, data = crimes,
	aspect=1,
	xlab = "unemployment (ln)",
	ylab = "number of crimes (ln)",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="black")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result.multi <- lm(lncrime~lnunemp+I(log(pop)), data=crimes)
summary(result.multi)

result.rate <- lm(I(crime/pop*10000)~I(unemp/pop*10000), data=crimes)
summary(result.rate)

x <- seq(1,5,length=1000)
lnx <- log(x)
lny <- 3*lnx
y <- exp(lny)


xyplot(y~x,
	xlab = "x",
	ylab = "y",
	panel = function(x,y){
		panel.xyplot(x,y, type="l", col="blue", lty=1)
	},
	)


xyplot(lny~lnx,
	xlab = "ln(x)",
	ylab = "ln(y)",
	panel = function(x,y){
		panel.xyplot(x,lny, type="l", col="red", lty=1)
	},
	)


