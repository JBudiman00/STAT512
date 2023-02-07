senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic.csv", header=TRUE, sep=",")
#Question 1
xBar <- mean(senic$xray)
yBar <- mean(senic$length)
SSX <- sum((senic$xray-xBar)^2)
SST <- sum((senic$length-yBar)^2)
SSXY <- sum((senic$xray-xBar)*(senic$length-yBar))
b1 <- SSXY/SSX
b0 <- yBar - b1*xBar
SSE <- sum((senic$length-(b0+b1*senic$xray))^2)
MSE <- SSE/(dim(senic)[1] - 2)
SSE <- sum((senic$length-(b0+b1*senic$xray))^2)
SSR <- sum((yBar-(b0+b1*senic$xray))^2)

#Question 2
qt(0.975, 111)
qt(0.95, 111)
SSX <- sum((senic$xray-xBar)^2)
MSE <- SSE/(dim(senic)[1] - 2)
sb1 <- sqrt(MSE / SSX)
qt(0.95, 111)*sb1
qt(0.975, 111)*sb1

#Question 3
SSX <- sum((senic$xray-xBar)^2)
SSXY <- sum((senic$xray-xBar)*(senic$length-yBar))
b1 <- SSXY/SSX
t <- b1 / sb1
#Beta 1 is zero o it's included in the calculation of the test statistic

#Question 4
senic.mod<-lm(length~xray, senic)
summary(senic.mod)
anova(senic.mod)
