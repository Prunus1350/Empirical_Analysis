# 	法学セミナー「法律家のための実証分析入門」第8回 Rソースコード
#	(C) 2011 MORITA Hatsuru

library(lattice)

qt(0.95,100)
qt(0.975,100)
qt(0.995,100)

crimes <-  read.csv("../csv/criminal.csv", header=TRUE)

crimes$lncrime <- log(crimes$crime)
for (i in 1:length(crimes$lncrime)){
	if (crimes$lncrime[i] == -Inf) crimes$lncrime[i] <- NA
	}

crimes$lnunemp <- log(crimes$unemp)
for (i in 1:length(crimes$lnunemp)){
	if (crimes$lnunemp[i] == -Inf) crimes$lnunemp[i] <- NA
	}

result.log <- lm(lncrime~lnunemp, data=crimes)
summary(result.log)

result.multi <- lm(lncrime~lnunemp+I(log(pop)), data=crimes)
summary(result.multi)

cor(crimes$crime,crimes$pop)

result.rate <- lm(I(crime/pop*10000)~I(unemp/pop*10000), data=crimes)
summary(result.rate)

# plotting f dist.
x <- seq(0,5,length=1000)
y <- df(x, 5,100)
threshold <- qf(0.95, 5, 100)
xyplot(y~x,
	xlab = "",
	ylab = "",
	panel = function(x,y){
		panel.xyplot(x,y, type="l", col="blue", lty=1)
		panel.abline(v=threshold, col="red", lty=2)
	},
)

