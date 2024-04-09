library(PerformanceAnalytics)
setwd("C:/Users/agust/Downloads/Econometria")
data <- read.csv("wagesmicrodata.csv")
attach(data)
names(data)
fit1 <- lm(log(wage)~ ., data=data)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

X <- data[,-6]
chart.Correlation(X)

library(corpcor)
cor2pcor(cov(X))

library(mctest)
omcdiag(fit1)
imcdiag(fit1)

library(ppcor)
pcor(X, method = "pearson")

fit2<- lm(log(wage)~occupation+sector+union+education+age+sex+marital+race+south)
summary(fit2)

library(car)
vif(fit2)
