senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic.csv", header=TRUE, sep=",")
senic.mod<-lm(length~infection, senic)
summary(senic.mod)

library(ALSM)

x<-data.frame(weight=unique(senic$infection))

SSX <- sum((senic$infection-mean(senic$infection))^2)
s <- sqrt((1.624^2) * ((1/113) + ((5-4.354)^2)/SSX))

cim<-ci.reg(senic.mod, x, type='m',alpha=0.05)
cin<-ci.reg(senic.mod, x, type='n',alpha=0.05)

#Problem 4
#y vector
y <- as.matrix(senic$length)

#Design matrix x
t1<-as.matrix(senic$infection)
t2<-as.matrix(senic$xray)
t3<-as.matrix(senic$facility)
Intercept<-rep(1,length(senic$infection)) 
x<-cbind(Intercept,t1,t2,t3)

#Hat matrix H
xty<-t(x)%*%y
xtx<-t(x)%*%x
xtxinv<-solve(xtx)
xtxinv

hat<-x%*%xtxinv%*%t(x)

#J matrix
n = length(senic[,1])
J <- matrix(1, ncol=n, nrow=n)

#Identity matrix
I <- diag(113)

#b matrix
b <- xtxinv %*% xty

#e matrix
e <- y - (x %*% b)

#SSE
SSE<-t(e)%*%e

#SST
SST<SS-t(y)%*%y-(1/n)*t(y)%*%J%*%y

#SSR
SSR<-SST-SSE

#Problem 5
#Check ANOVA table
senic.mod <- lm(length ~ infection + xray + facility, data=senic)
anova(senic.mod)

#Problem 6
xty<-t(x)%*%y
xtx<-t(x)%*%x
xtxinv<-solve(xtx)
hat<-x%*%xtxinv%*%t(x)
I <- diag(113)
MSE <- SSE / 109
as.numeric(MSE) * (I - hat)

#Problem 7
vcov(senic.mod)

#Problem 8
f <- qf(0.9, 3, 109)
SSM <- sum(((4.8+(0.528 * senic$infection) + (0.0191 * senic$xray) + (0.0227 * senic$facility)) - mean(senic$length))^2)
MSM <- SSM / 3
MSE <- SSE / 109
Fs <- MSM / MSE
