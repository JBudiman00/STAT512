senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic.csv", header=TRUE, sep=",")
senic.mod<-lm(length~infection, senic)
summary(senic.mod)
anova(senic.mod)
plot(length~infection, senic)
confint(lm(length~infection, senic),"infection",level=0.95)
#Question 1a
yBar <- mean(senic$length)
xBar <- mean(senic$infection)
SSXY <- sum((senic$infection-xBar)*(senic$length-yBar))
SSX <- sum((senic$infection-xBar)^2)
b1 <- SSXY/SSX
b0 <- yBar - b1*xBar
yHat <- b0 + b1 * 5

t <- qt(0.975, 111)
SSE = sum((senic$length - (b0 + b1 * senic$infection))^2)
s = sqrt(SSE / (111))
varMean <- sqrt(s^2 * ((1/113) + (((5-xBar)^2)/SSX)))

marginError = t * varMean
yHat + marginError
yHat - marginError

#Question 1b
varSingle <- sqrt(varMean^2 + s^2)

marginError = t * varSingle
yHat + marginError
yHat - marginError

#Question 1c
varMiddle <- sqrt(((SSE/111)/3) + varMean^2)

marginError = t * varMiddle
yHat + marginError
yHat - marginError

new <- data.frame(infection = 5)
ci.reg(senic.mod, new, type='m', alpha = 0.05)
ci.reg(senic.mod, new, type='n', alpha = 0.05)
ci.reg(senic.mod, new, type='nm', m=3, alpha = 0.05)

#Question 2
new <- data.frame(infection = 5)
NE.mod<-lm(length~infection, data = subset(senic, region==1))
NC.mod<-lm(length~infection, data = subset(senic, region==2))
S.mod<-lm(length~infection, data = subset(senic, region==3))
W.mod<-lm(length~infection, data = subset(senic, region==4))
ci.reg(NE.mod, new, type='m', alpha = 0.05)
ci.reg(NC.mod, new, type='m', alpha = 0.05)
ci.reg(S.mod, new, type='m', alpha = 0.05)
ci.reg(W.mod, new, type='m', alpha = 0.05)

#Question 3
summary(senic.mod)
anova(senic.mod)

cor.test(senic$length, senic$infection, conf.level=0.95)

#Question 4
SST = sum((senic$length-yBar)^2)
SSR = sum(((b0 + b1 * senic$infection)-yBar)^2)
F = SSR/(SSE/111)
qf(0.95, 1, 111)
pf(F, 1, 111, lower.tail = FALSE)
