senic <- read.table("C:/Users/13145/Desktop/Dev/Fall2022/Stat512/senic_homework6.csv", header=TRUE, sep=",")

#Problem 1
#Part a
#Convert region to X2 = or 1
newRegion <- as.numeric(lapply(senic$region, 
                    function(x){
                      if(x == 1 || x == 2){
                        0
                      } else{
                        1
                      }
                    }
                      ))

senic <- cbind(senic, newRegion)
summary(lm(length~bed+factor(newRegion)+bed*factor(newRegion), data=senic))

#Part b
anova(lm(length~bed+factor(newRegion)+bed*factor(newRegion), data=senic))

#Problem 2
#Part a
region.mod <- lm(length~bed+as.factor(region)+bed*as.factor(region), data=senic)
summary(region.mod)
anova(region.mod)

#Part b
wrong.mod <- lm(length~bed*region, data=senic)
summary(wrong.mod)
anova(wrong.mod)

#Problem 3
nX1 = senic$bed-mean(senic$bed)
senic <- cbind(senic, nX1)
regionNew.mod <- lm(length~nX1+as.factor(region)+nX1*as.factor(region), data=senic)
summary(regionNew.mod)
anova(regionNew.mod)

#Problem 4
library(car)
fullModel <- lm(length~age+infection+cultur+xray+bed+patient+nurse+facility, data=senic)
crPlots(fullModel)

#Problem 5
library(ALSM)
bs <- BestSub(cbind(senic[3:7], senic[10:12]), log10(senic[2]), num=1)

#Problem 6
fullModel <- lm(log10(length)~age+infection+cultur+xray+bed+patient+nurse, data=senic)
dffits(fullModel)
cooks.distance(fullModel)
dfbetas(fullModel)

qf(0.2, 4, 109)
qf(0.5, 4, 109)

#Problem 7
library(fmsb)
VIF(lm(age~infection+cultur+xray+bed+patient+nurse, data=senic))
VIF(lm(infection~age+cultur+xray+bed+patient+nurse, data=senic))
VIF(lm(cultur~infection+age+xray+bed+patient+nurse, data=senic))
VIF(lm(xray~infection+cultur+age+bed+patient+nurse, data=senic))
VIF(lm(bed~infection+cultur+xray+age+patient+nurse, data=senic))
VIF(lm(patient~infection+cultur+xray+bed+age+nurse, data=senic))
VIF(lm(nurse~infection+cultur+xray+bed+patient+age, data=senic))
