library(ISLR2)
library(tidyverse)
library(ggplot2)
library(MASS)
library(msm)
library(msmtools)

head(Default)

default <- Default$default
student <- Default$student
balance <- Default$balance
income <- Default$income

# outcome <- factor(outcome) # para que no sea numerica la 1 y 0

datos <- data.frame(default, balance, income)
head(datos)

# Gráficos ----------------------------------------------------------------

boxplot(balance ~ default, data = datos, col=c("blue","red") )
boxplot(income ~ default, data = datos, col=c("blue","red") )

ggplot(datos,mapping = aes(x=default,y=balance,fill=default))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos,mapping = aes(x=default,y=income,fill=default))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos)+
  geom_point(mapping = aes(x=balance, y=income, col=default), alpha=0.6, shape=default)+
  scale_color_manual(values = c("blue","red"))

# Recodificación ----------------------------------------------------------

default <- recode(default,"No"=0,"Yes"=1)
head(default)

datos <- data.frame(default,balance,income)

# Ajuste de modelo lineal -------------------------------------------------

# geom_point es un grafico de dispersion
# geom_smooth nos ayuda para la regesion lineal, se intervalos de confianza,  

ggplot(data=datos, aes(x=balance,y=default))+
  geom_point(aes(color=default),shape=1)+
  geom_smooth(method = "lm", color="green",se=FALSE)+
  theme_bw()+
  labs(title = "Regresión Lineal simple",y="Prob. default")+
  theme(legend.position = "right")

# este modelo no sirve, entonces vamos a usar el modelo logistico

# #Ajuste de modelo logístico ---------------------------------------------

modlog <- glm(default~balance,data = datos,family = "binomial")
summary(modlog)

ggplot(data=datos, aes(x=balance,y=default))+
  geom_point(aes(color=default),shape=1)+
  stat_function(fun = function(x){predict(modlog,
                                          newdata = data.frame(balance=x),
                                          type = "response")},color="red")+
  theme_bw()+
  labs(title = "Regresión logística",y="Prob. default")+
  theme(legend.position = "right")

