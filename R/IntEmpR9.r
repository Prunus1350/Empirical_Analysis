# 	法学セミナー「法律家のための実証分析入門」第9回 Rソースコード
#	(C) 2012 MORITA Hatsuru

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

result.sim <- lm(lncrime~lnunemp+I(log(pop)), data=crimes)
summary(result.sim)

result.int <- lm(lncrime~lnunemp+I(log(pop))+I(lnunemp*log(pop)), data=crimes)
summary(result.int)

