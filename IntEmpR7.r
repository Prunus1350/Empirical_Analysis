# 	法学セミナー「法律家のための実証分析入門」第7回 ソースコード
#	(C) 2011 MORITA Hatsuru

library(lattice)

# standard normal dist. vs t dist.
x <- seq(-4,4,length=1000)
y <- dnorm(x, mean=0, sd=1)
z <- dt(x, df=3)
xyplot(y~x,
	xlab = "",
	ylab = "",
	panel = function(x,y){
		panel.xyplot(x,y, type="l", col="blue", lty=2)
		panel.xyplot(x,z, type="l", col="red", lty=1)
	},
	key = list(text=list(c("標準正規分布","t分布 (df=3)")),
		lines=list(col=c("blue","red"),lty=c(2,1)), 
		space="top", border=T
	)
)

