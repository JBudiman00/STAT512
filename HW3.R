senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic.csv", header=TRUE, sep=",")
senic.mod<-lm(length~cultur, senic)
summary(senic.mod)
plot(length~cultur, senic)
reg <- lm(length~cultur, senic)
abline(reg)

#Lack of Fit test
senicF.mod<-lm(length~factor(cultur), senic)
senic.mod<-lm(length~cultur, senic)
anova(senic.mod)
anova(senic.mod, senicF.mod)

qf(0.05,94,17)

#Question 1
avg1 <- matrix(0, nrow = 1, ncol = 1)
for (i in 1:length(senic$cultur)){
  sum <- 0
  count <- 0
  for (j in 1:length(senic$cultur)){
    if (senic$cultur[i] == senic$cultur[j]){
      sum <- sum + senic$length[j]
      count <- count + 1
    }
  }
  avg1[i] = sum/count
}

write_xlsx(avg1, "")


#Question 2

final = matrix(0, nrow = 1, ncol = 1)
for (i in 1:length(senic$cultur)){
  sum <- 0
  count <- 0
  for (j in 1:length(senic$cultur)){
    if (senic$cultur[j] >= floor(senic$cultur[i]) & senic$cultur[j] < (floor(senic$cultur[i]) + 1)){
      sum <- sum + senic$cultur[j]
      count <- count + 1
    }
  }
  final[i] = sum/count
}

revised <- data.frame("X" = final, "Y" = senic$length)

new <- data.frame(revised)
senicFnew.mod<-lm(new[,2]~factor(new[,1]), new)
senicNew.mod<-lm(new[,2]~new[,1], new)
anova(senicNew.mod)
anova(senicNew.mod, senicFnew.mod)


#Question 4
#Linear relationship
senic.mod<-lm(length~xray, senic)
summary(senic.mod)
dataset <- data.frame(senic$xray, senic$length)
datasetRM <- lm(senic$length~senic$xray, dataset)
datasetFM <- lm(senic$length~factor(senic$xray), dataset)
anova(datasetRM, datasetFM)

plot(senic$xray, senic$length)

yHat <- 6.56637 + (0.03776 * senic$xray)
senic.res = senic$length - yHat
plot(senic$xray, senic.res, xlab = "X-ray ratio", ylab="Residual")
abline(0,0)

plot(yHat, senic.res, xlab = "X-ray ratio", ylab="Residual")
abline(0,0)

plot(senic$id, senic.res)

library(onewaytests)
senic$group <- cut(senic$xray, 5)
senic$residual <- senic.mod$residuals
bf.test(residual~group, senic)

g <- rep(1, 113)
g[senic$xray <= 81]=0
bftest(lm(length~xray, senic), g)

shapiro.test(residuals(senic.mod))
qqnorm(residuals(senic.mod))
qqline(residuals(senic.mod))

boxplot(senic$xray, horizontal = TRUE)

#Question 6
library(MASS)
bcmle <- boxcox(lm(length~xray, data=senic), lambda=seq(-3, 3, by=0.1))
lambda <- bcmle$x[which.max(bcmle$y)]
lambda
senic$newY <- (senic$length)^(lambda)
senicNew.mod <- lm(newY~xray, senic)
summary(senicNew.mod)

qqnorm(residuals(senicNew.mod))
qqline(residuals(senicNew.mod))
shapiro.test(residuals(senicNew.mod))

datasetN <- data.frame(senic$xray, senic$newY)
datasetRMN <- lm(senic$newY~senic$xray, datasetN)
datasetFMN <- lm(senic$newY~factor(senic$xray), datasetN)
anova(datasetRMN, datasetFMN)

senic$Group <- cut(senic$xray, 5)
senic$residual <- senicNew.mod$residuals
bf.test(residual~group, senic)

senic.res = residuals(senicNew.mod)
plot(senic$xray, senic.res, xlab = "X-ray ratio", ylab="Residual")
abline(0,0)



senic$Y2 <- log(senic$length)
senic2.mod <- lm(Y2~xray, senic)
summary(senic2.mod)
anova(senic2.mod)

qqnorm(residuals(senic2.mod))
qqline(residuals(senic2.mod))
shapiro.test(residuals(senic2.mod))

dataset2 <- data.frame(senic$xray, senic$Y2)
datasetRM2 <- lm(senic$Y2~senic$xray, dataset2)
datasetFM2 <- lm(senic$Y2~factor(senic$xray), dataset2)
anova(datasetRM2, datasetFM2)

senic$Group <- cut(senic$xray, 5)
senic$residual <- senic2.mod$residuals
bf.test(residual~group, senic)
