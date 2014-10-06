# 	法学セミナー「法律家のための実証分析入門」第6回 Rソースコード
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



result.log <- lm(lncrime~lnunemp, data=crimes)
summary(result.log)

result.log.noint <- lm(lncrime~lnunemp-1, data=crimes)
summary(result.log.noint)


result.multi <- lm(lncrime~lnunemp+I(log(pop)), data=crimes)
summary(result.multi)

result.multi.noint <- lm(lncrime~lnunemp+I(log(pop))-1, data=crimes)
summary(result.multi.noint)

result.rate <- lm(I(crime/pop*10000)~I(unemp/pop*10000), data=crimes)
summary(result.rate)

