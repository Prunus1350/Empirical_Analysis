# 	法学セミナー「法律家のための実証分析入門」第23回 Rソースコード
#	(C) 2013 MORITA Hatsuru

rm(list=ls())

library(foreign)
library(sampleSelection)

setwd("c:/Users/MORITA Hatsuru/Documents/ec/")

mroz <- read.dta("mroz.dta")

result.ols <- lm(lwage~educ+exper+expersq, data=mroz)

result.heckit <- heckit(inlf~educ+exper+expersq+nwifeinc+age+kidslt6+kidsge6,
	lwage~educ+exper+expersq, method="2step", data=mroz)
result.heckml <- selection(inlf~educ+exper+expersq+nwifeinc+age+kidslt6+kidsge6,
	lwage~educ+exper+expersq, data=mroz)

summary(result.ols)
summary(result.heckit)
summary(result.heckml)

