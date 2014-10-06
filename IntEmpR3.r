# 	法学セミナー「法律家のための実証分析入門」第3回 ソースコード
#	(C) 2011 MORITA Hatsuru

library(lattice)

# Essay #3 reemployment data
reemp <- c(7.1, 3.5, 14.3, 6.3, 18.0, 5.4, 6.9, 9.2, 11.0, 7.7)

hist(reemp)
histogram(reemp)
densityplot(reemp)

mean(reemp)
median(reemp)
summary(reemp)

# two distributions
x <- seq(40,160,length=1000)
y <- dnorm(x, mean=100, sd=20)*400
z <- dnorm(x, mean=100, sd=10)*400
xyplot(y~x,
	xlab = "x",
	ylab = "pop",
	ylim = c(-1,18),
	panel = function(x,y){
		panel.xyplot(x,y, type="l", col="blue", lty=2)
		panel.xyplot(x,z, type="l", col="red", lty=1)
	},
	key = list(text=list(c("SD=20","SD=10")),
		lines=list(col=c("blue","red"),lty=c(2,1)), 
		space="top", border=T
	)
)

# not population, but sample!
var(reemp)
sd(reemp)

# reemployment + training data
training <- c(3,2,1,2,1,3,3,2,2,1)
reemptrain <- cbind(reemp, training)

xyplot(training~reemp,
	xlab ="Reemployment",
	ylab ="Training",
	panel = function(x,y){
		panel.xyplot(reemp,training, col="red", pch=4)
	},
)

cor(reemptrain)
#not population, but sample!
cov(reemptrain)


