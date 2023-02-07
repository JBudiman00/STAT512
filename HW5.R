senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic.csv", header=TRUE, sep=",")
library(car)

#Question 1
#Y ~ X1+X2
anova(lm(length~age+infection, data=senic))

#Y ~ X2
anova(lm(length~infection, data=senic))

#Question 2
#Y ~ X1+x2+X3
anova(lm(length~age+infection+cultur, data=senic))

#Question 3
#Y ~ X1+x2+X4
anova(lm(length~age+infection+patient, data=senic))

#Y ~ X1+x2+X5
anova(lm(length~age+infection+nurse, data=senic))

#Y ~ X1+x2+X6
anova(lm(length~age+infection+facility, data=senic))

#Question 4
#Y ~ x1+x1^2
x11=senic$age^2
senic <- cbind(senic, x11)
anova(lm(length~age+x11, data=senic))
anova(lm(length~x11+age, data=senic))
summary(lm(length~age+x11, data=senic))

newX1 <- scale(senic$age)[,1]
scaleValue <- 4.461607
newX11 <- newX1^2
senic <- cbind(senic, newX1, newX11)
summary(lm(length~newX1+newX11, data=senic))
anova(lm(length~newX1+newX11, data=senic))
anova(lm(length~newX11+newX1, data=senic))

B <- qt(1-0.025, 111)
X1lb <- 0.3728/scaleValue - (B*0.1783)/scaleValue
X1ub <- 0.3728/scaleValue + (B*0.1783)/scaleValue
X11lb <- 0.1148/scaleValue - (B*.0140)/scaleValue
X11ub <- 0.1148/scaleValue + (B*.0140)/scaleValue
  

