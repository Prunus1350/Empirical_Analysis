# 	法学セミナー「法律家のための実証分析入門」第14回 Rソースコード
#	(C) 2012 MORITA Hatsuru

rm(list=ls())

library(lattice)
library(MASS)
library(nnet)
library(MNP)
library(coda)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

## writing a graph for ordered probit

Xb <- c(0.3,1.1,2.2)
x <- c(0,0,0)
tau <- dnorm(Xb, mean=0, sd=1)

xyplot(x~Xb,
	xlab = "X",
	ylab = "Probability",
	xlim = c(-4,4),
	ylim = c(-0.01,0.41),
	panel = function(x,y){
	panel.mathdensity(dmath=dnorm, args=c(0,1), n=100, col="red")
	panel.xyplot(x,y, pch=21) 
	lsegments(0.3, 0, 0.3, tau[1], col="blue", lty=2)
	lsegments(1.1, 0, 1.1, tau[2], col="blue", lty=2)
	lsegments(2.2, 0, 2.2, tau[3], col="blue", lty=2)
	}
	)

dev.print(file="oprobit.eps")


##	Analyzing public comments for corporate law reform 2005


sink(file="IntEmpR14.r.log",type="output",split=TRUE)

#	Gendaika data
gd_bin <- read.table("gdb.txt", header=TRUE)
gd_mul <- read.table("gdm.txt", header=TRUE)

#	regressions of binary dependent variable

summary(gd_bin)

res.bin1 <- glm(ref05yn ~ yes + no, family = binomial(link = "probit"), data=gd_bin)
summary(res.bin1)
logLik(res.bin1)

res.bin2 <- glm(ref05yn ~ yes + no + I(expression == 0), family = binomial(link = probit), data=gd_bin)
summary(res.bin2)
logLik(res.bin2)

res.bin3 <- glm(ref05yn ~ yes4 + yes5 + yes6 + yesother + no4 + no5 + no6 + noother, family = binomial(link = "probit"), data=gd_bin)
summary(res.bin3)
logLik(res.bin3)

res.bin4 <- glm(ref05yn ~ yes4 + yes5 + yes6 + yesother + no4 + no5 + no6 + noother + I(expression == 0), family = binomial(link = "probit"), data=gd_bin)
summary(res.bin4)
logLik(res.bin4)

res.bin5 <- glm(ref05yn ~ yes4 + yes5 + yes6 + yesother + no4 + no5 + no6 + noother, family = binomial(link = "probit"), data=gd_bin[gd_bin$expression==0,])
summary(res.bin5)
logLik(res.bin5)

#	regressions of multinomial dependent variable

gd_mul <- read.table("./softlaw/pc/gd_mul.txt", header=TRUE)

summary(gd_mul)

res.mul1 <- polr(ordered(ref05) ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother, Hess=TRUE, method = "probit", data=gd_mul)
summary(res.mul1)
logLik(res.mul1)

res.mul2 <- polr(ordered(ref05) ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother + I(expression==0) + I((expression==0)*regulatory), Hess=TRUE, method = "probit", data=gd_mul)
summary(res.mul2)
logLik(res.mul2)


#	in order to let basecategory be 0, rearrange the dependent variable
gd_mul$regen05 <- replace(gd_mul$ref05, (gd_mul$ref05==-1), "REG")

res.mul3 <- multinom(factor(regen05) ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother, data=gd_mul)
summary(res.mul3)
logLik(res.mul3)

res.mul4 <- multinom(factor(regen05) ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother + I(expression == 0) + I((expression==0)*regulatory), data=gd_mul)
summary(res.mul4)
logLik(res.mul4)

#	CAUTION! do not run!
#res.mul5 <- mnp(ref05 ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother, base = "0", data=gd_mul, n.draws = 10000)
#summary(res.mul5)
#
#res.mul6 <- mnp(ref05 ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother + I(expression == 0), base = "0", data=gd_mul, n.draws = 10000)
#summary(res.mul6)

res.mul7 <- polr(ordered(ref05) ~ en4 + en5 + en6 + enother + reg4 + reg5 + reg6 + regother, Hess=TRUE, method = "probit", data=gd_mul[gd_mul$expression==0,])
summary(res.mul7)
logLik(res.mul7)

sink()
