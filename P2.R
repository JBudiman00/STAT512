library(car)
library(fmsb)

#Read in dataset
df_raw <- read.table(file = "C:/Users/13145/Desktop/Dev/Fall2022/STAT512/Project/TravelMode.csv", header=TRUE, sep=',')
#Does wait time and travel time have different impacts on the generalized cost?
#Rename variables (For simplicity)
names(df_raw)[3] <- "X1"
names(df_raw)[5] <- "X2"
names(df_raw)[6] <- "X3"
names(df_raw)[7] <- "X4"
names(df_raw)[9] <- "X5"
names(df_raw)[10] <- "X6"
names(df_raw)[8] <- "Y"

#For cars
#Transformed model required that reduces multicollinearity
df_car <- df_raw[df_raw$X1 == "car",]

#Compute full and reduced model
full.mod <- lm(Y^0.697~X2+X3+X4+X5+X3*X4, data = df_car)
reduced.mod <- lm(Y^0.697~X2+X3+X4+X5, data = df_car)

#Analyze results of GLT test to see whether the interaction effect is significant
anova(reduced.mod, full.mod)

#For train
df_train <- df_raw[df_raw$X1 == "train",]

#Compute full and reduced model
full.mod <- lm(Y~X2+X3+X4+X5+X3*X4, data = df_train)
reduced.mod <- lm(Y~X2+X3+X4+X5, data = df_train)

#Analyze results of GLT test to see whether the interaction effect is significant
anova(reduced.mod, full.mod)

#For bus
df_bus <- df_raw[df_raw$X1 == "bus",]

#Compute full and reduced model
full.mod <- lm(Y~X2+X3+X4+X5+X3*X4, data = df_bus)
reduced.mod <- lm(Y~X2+X3+X4+X5, data = df_bus)

#Analyze results of GLT test to see whether the interaction effect is significant
anova(reduced.mod, full.mod)

#For air
df_air <- df_raw[df_raw$X1 == "air",]

#Compute full and reduced model
full.mod <- lm(Y~X2+X3+X4+X5+X3*X4, data = df_air)
reduced.mod <- lm(Y~X2+X3+X4+X5, data = df_air)

#Analyze results of GLT test to see whether the interaction effect is significant
anova(reduced.mod, full.mod)

