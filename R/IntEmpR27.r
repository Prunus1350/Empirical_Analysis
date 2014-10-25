# 	法学セミナー「法律家のための実証分析入門」第27回 Rソースコード
#	(C) 2013 MORITA Hatsuru

library(lattice)

akb <-  read.csv("../csv/AKB2012.csv", header=TRUE)

akb$logvote <- log(akb$vote)
akb$logrank <- log(akb$rank)
result.zipf <- lm(logvote ~ logrank, data=akb)
summary(result.zipf)

xyplot(logvote ~ logrank, data = akb,
	xlab = "Ln(Rank)",
	ylab = "Ln(Vote)",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		panel.loess(x,y, lty=2, col="green")
		}
	)

result.zipf10 <- lm(logvote ~ logrank, data=akb[akb$rank<17,])
summary(result.zipf10)

xyplot(logvote ~ logrank, data = akb[akb$rank<17,],
	xlab = "Ln(Rank)",
	ylab = "Ln(Vote)",
	panel = function(x,y){
		panel.xyplot(x,y, pch=1, col="blue")
		panel.lmline(x,y, lty=1, col="red")
		panel.loess(x,y, lty=2, col="black")
		},
	key = list(text=list(c("OLS","loess")),
		lines=list(col=c("red","black"),lty=c(1,2)), 
		space="top", border=T
	)
	)


