# 	法学セミナー「法律家のための実証分析入門」第4回 Rソースコード
#	(C) 2011 MORITA Hatsuru

library(lattice)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

crimes <-  read.csv("criminal.csv", header=TRUE)

result1 <- lm(crime~unemp, data=crimes)
summary(result1)

xyplot(crime ~ unemp, data = crimes,
	aspect=1,
	xlab = "unemployment",
	ylab = "number of crimes",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="black")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

crimes$wrongresid <- crimes$crime + 263 - crimes$unemp*0.555

xyplot(result1$residuals ~ crimes$unemp,
	aspect=1,
	xlab = "unemployment",
	ylab = "residuals",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="red")
		}
	)

xyplot(crimes$wrongresid ~ crimes$unemp,
	aspect=1,
	xlab = "unemployment",
	ylab = "residuals",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result1b <- lm(crime~unemp+pop, data=crimes)
summary(result1b)

result1c <- lm(I(crime/pop)~I(unemp/pop), data=crimes)
summary(result1c)

result2 <- lm(log1p(crime)~log1p(unemp), data=crimes)
summary(result2)

xyplot(log1p(crime) ~ log1p(unemp), data = crimes,
	aspect=1,
	xlab = "unemployment",
	ylab = "number of crimes",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="black")
		panel.lmline(x,y, lty=1, col="red")
		}
	)

result2b <- lm(log1p(crime)~log1p(unemp)+log1p(pop), data=crimes)
summary(result2b)

result2c <- lm(I(crime/pop)~I(unemp/pop), data=crimes)
summary(result2c)
