# 	法学セミナー「法律家のための実証分析入門」第11回 Rソースコード
#	(C) 2012 MORITA Hatsuru

library(lattice)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")
crimes <-  read.csv("criminal.csv", header=TRUE)

result.ols <- lm(crime~unemp, data=crimes)
summary(result.ols)

xyplot(result.ols$residuals ~ crimes$unemp, 
	xlab = "unemployment",
	ylab = "residuals",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="black")
		}
	)

# computing robust SE
library(sandwich)
vcov(result.ols)
vcovHC(result.ols, type="HC0")
sqrt(vcovHC(result.ols, type="HC0")["unemp","unemp"])

# Breusch-Pagan test
library(lmtest)
bptest(crime~unemp, data=crimes)

# WLS
result.wls <- lm(crime~unemp, data=crimes, weights=pop)
summary(result.wls)

library(nlme)
result.wls2 <- gls(crime~unemp, data=crimes, weights=varFixed(~1/pop))
summary(result.wls2)

# FGLS
fgls.weight <- lm(I(log(result.ols$residuals^2)) ~ unemp, data=crimes) 
result.fgls <- lm(crime~unemp, weights = 1/exp(fgls.weight$fitted.values), data=crimes)
summary(result.fgls)


