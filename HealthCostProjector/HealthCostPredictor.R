setwd("D:/UC/Flex2/DAM")
data<-read.csv(file="insurance.csv", header=TRUE, sep=",")

install.packages("psych")
library(psych)
pairs.panels(data)
hist(data$smoker, breaks = 5)

data<-read.csv(file="insurance.csv", header=TRUE, sep=",")
pairs.panels(data)
describe(data)

model <- lm(data$charges ~ data$age+data$bmi+data$children+data$smoker
            +data$region)
summary(model)
plot (model)

install.packages("gridExtra")
library(car)
vif(model)
library(gridExtra)

plot.sex <- ggplot(data, aes(x = sex, y = charges)) + geom_boxplot()
plot.smoker <- ggplot(data, aes(x = smoker, y = charges)) + geom_boxplot()
plot.child <- ggplot(data, aes(x = as.factor(children), y = charges))+ geom_boxplot()
plot.region <- ggplot(data, aes(x = region, y = charges)) + geom_boxplot()
grid.arrange(plot.sex, plot.smoker, plot.child, plot.region, ncol=2, nrow=2)

plot.age <- ggplot(data, aes(x = age, y = charges)) +geom_point()
plot.bmi <- ggplot(data, aes(x = bmi, y = charges)) +geom_point()
grid.arrange(plot.age, plot.bmi, ncol=2)

              
model <- lm(data$charges ~ data$age+data$bmi+data$children+data$smoker
+data$region)

summary(model)

model1 <- lm(charges ~ smoker + age + bmi + sex + children + region, data = data)
model2 <- lm(charges ~ smoker + age + bmi + children + region, data = data)
model3 <- lm(charges ~ smoker + age + bmi + children , data = data)
summary(model1)
summary(model2)
summary(model3)
plot(model2)

qqnorm(model3$residuals)
qqline(model3$residuals)

#model3 <- lm(charges ~ smoker + age + bmi + children + region+bmi:smoker, data = data)
confint(model2)

data


data$smokerYes=ifelse(data$smoker=="yes",1,0)
data$smokerNo=ifelse(data$smoker=="no",1,0)
model3 <- lm(charges ~ age + bmi + children + region+smoker+smoker*bmi, data = data)
summary(model3)
data <- data %>%  mutate(age.square = age^2) 
plot.age2 <- ggplot(data, aes(x = age^2, y = charges)) + geom_point()
grid.arrange(plot.age, plot.age2, ncol=2)
model4 <- lm(charges ~ smoker + age.square + bmi + children + region, data = data)

summary(model4)
plot(model4)

ggplot(data, aes(x = bmi, y = charges, col = smoker)) + geom_point()

model5 <- lm(charges ~smoker + age.square + bmi + children + 
             region + smoker*bmi, data = data)
summary(model5)
plot(model5)
library(tidyverse)
rstandard(model2)
ggplot(data, aes(x = age, y = charges))+ geom_point()

anova(model4)
anova(model5)

abline(model5)

plot(data$charges~data$bmi+data$age+data$smoker+data$children)
abline(model1)

which(data$outlier.residual==T)

modelb <- lm(charges ~ as.factor(smoker) + age + bmi + as.factor(region), data = data)

layout(matrix(c(1,2,3,4),2,2, byrow=T))
plot(charges ~ smoker + age + bmi + region, data = data)

abline(modelb)

ggplot(data, aes(x = age, y =charges)) + geom_point()
abline(lm(charges ~ smoker + age + bmi + region, data = data))
plot(model5)
par(mfrow = c(1,1))
plot(data$age, data$charges)
abline(lm(charges~age, data = data))
plot(data$bmi, data$charges)
abline(lm(charges~bmi, data = data))

dataTest<-read.csv(file="insurance - Copy.csv", header=TRUE, sep=",")
predict(model5,newdata=dataTest)









