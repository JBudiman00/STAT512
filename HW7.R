senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic_homework6.csv", header=TRUE, sep=",")

#Problem 1

senic.mod <- lm(log10(length)~age+infection+cultur+xray+bed+patient, data=senic)
wts1 <- 1/fitted(lm(abs(residuals(senic.mod))~age+infection+cultur+xray+bed+patient, senic))^2
wts1.mod <- lm(log10(length)~age+infection+cultur+xray+bed+patient, weight=wts1, data=senic)

summary(senic.mod)
summary(wts1.mod)

library(boot)
library(MASS)
boot.wlscoef <- function(data, indices, maxit=20) {
  data <- data[indices,]
  data.mod <- lm(log10(length)~age+infection+cultur+xray+bed+patient, data=data)
  wts <- 1/fitted(lm(abs(residuals(data.mod))~age+infection+cultur+xray+bed+patient, data))^2
  data2.mod <- lm(log10(length)~age+infection+cultur+xray+bed+patient, weight=wts, data=data)
  return(coef(data2.mod))
}

senic.boot <- boot(data=senic, statistic = boot.wlscoef, R=100, maxit=20)
boot.ci(senic.boot, index = 1, type="perc")
boot.ci(senic.boot, index = 2, type="perc")
boot.ci(senic.boot, index = 3, type="perc")
boot.ci(senic.boot, index = 4, type="perc")
boot.ci(senic.boot, index = 5, type="perc")
boot.ci(senic.boot, index = 6, type="perc")
boot.ci(senic.boot, index = 7, type="perc")

#Problem 2
library(lmridge)
ridge.mod <- lmridge(log10(length)~age+infection+cultur+xray+bed+patient, data=senic, K=seq(0,0.5,0.01))
plot(ridge.mod)
vif(ridge.mod)

summary(lmridge(log10(length)~age+infection+cultur+xray+bed+patient, data=senic, K=0))
summary(lmridge(log10(length)~age+infection+cultur+xray+bed+patient, data=senic, K=0.03))

boot.ridgecoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  mod <- lmridge(log10(length)~age+infection+cultur+xray+bed+patient, data=data, K=0.03)
  
  return(coef(mod))
}

senic_bootcoeff <- boot(data=senic, statistic = boot.ridgecoef, R=1000, maxit=100)
boot.ci(senic_bootcoeff, index = 1, type="perc")
boot.ci(senic_bootcoeff, index = 2, type="perc")
boot.ci(senic_bootcoeff, index = 3, type="perc")
boot.ci(senic_bootcoeff, index = 4, type="perc")
boot.ci(senic_bootcoeff, index = 5, type="perc")
boot.ci(senic_bootcoeff, index = 6, type="perc")
boot.ci(senic_bootcoeff, index = 7, type="perc")

#Problem 3
r <- rlm(log10(length)~age+infection+cultur+xray+bed+patient, data=senic, psi=psi.bisquare)

boot.bisquare <- function(data, indices, maxit=100){
  data <- data[indices,]
  mod <- rlm(log10(length)~age+infection+cultur+xray+bed+patient, data=data, maxit=maxit, psi=psi.bisquare)
  coefficients(mod)
}

bisquare.boot <- boot(data=senic, statistic=boot.bisquare, R = 100, maxit=100)
boot.ci(bisquare.boot, index = 1, type="perc")
boot.ci(bisquare.boot, index = 2, type="perc")
boot.ci(bisquare.boot, index = 3, type="perc")
boot.ci(bisquare.boot, index = 4, type="perc")
boot.ci(bisquare.boot, index = 5, type="perc")
boot.ci(bisquare.boot, index = 6, type="perc")
boot.ci(bisquare.boot, index = 7, type="perc")

#Problem 5
library(ALSM)
region.mod <- lm(log10(length)~as.factor(region), data=senic)
summary(region.mod)
qt(0.95,109)

