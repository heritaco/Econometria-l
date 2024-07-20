library(ISLR2)
library(tidyverse)
library(ggplot2)
library(MASS)
library(msm)
library(msmtools)
library(PerformanceAnalytics)

library(readr)
data = datos = diabetes <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/diabetes.csv")
# View(diabetes)
head(diabetes)


pregnancies <- diabetes$Pregnancies
glucose <- diabetes$Glucose
bloodPressure <- diabetes$BloodPressure
skinThickness <- diabetes$SkinThickness
insulin <- diabetes$Insulin
bmi <- diabetes$BMI
pedigree <- diabetes$DiabetesPedigreeFunction
age <- diabetes$Age

outcome <- diabetes$Outcome
outcome <- factor(outcome)



# Gráficos ----------------------------------------------------------------

ggplot(datos, mapping = aes(x=outcome,y=pregnancies,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=glucose,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=bloodPressure,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=skinThickness,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=insulin,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=bmi,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=pedigree,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=outcome,y=age,fill=outcome))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)




# Recode para lm y glm ---------------------------------------------------

outcome <- recode(outcome,"0"=0,"1"=1)

# #Ajuste de modelo logístico ---------------------------------------------

modlog <- glm(outcome ~ . ,data = datos, family = "binomial")
summary(modlog)

modlog <- glm(outcome ~ pregnancies, data = datos, family = "binomial")
summary(modlog)

modlog <- glm(outcome ~ pregnancies + glucose + bloodPressure + skinThickness + insulin + bmi + pedigree + age, data = datos, family = "binomial")
summary(modlog)

modlog <- glm(outcome ~ pregnancies + glucose + bloodPressure + bmi + pedigree, data = datos, family = "binomial")
summary(modlog)


library(ISLR)
library(ISLR2)
library(PerformanceAnalytics)


coef(modlog)


#probabilidad de que dado unos valores fijos de las predictoras, a que categoria pertenece el tipo de direccion
glmprobs <- predict(modlog, type = "response")  # Si es mayor a .5 va para arriba y al reves
head(glmpred)
glmprobs[1:10]

contrasts(factor(outcome)) # Como no tenemos r cuadrada no tenemos una metrica que ayude a evaluar el modelo, 
# entonces se hace por una matriz de observacion, se puede transformar a una de mosaico
glmpred <- rep("0", 768) # repite el string 1250 veces.
head(glmpred) # esto pero 1250 veces
glmpred[glmprobs > 0.5] = "1" # lo vamos a modificar con el filtro de la pribabilidad, si es mayor a .5 se va a up
glmprobs[1:20]
glmpred[1:20]

library(vcd)
table(glmpred, factor(outcome)) # tabla de frecuencias, es el contraste

table(glmpred, factor(outcome))
# Lo que pronostica y lo que en verdad paso
# pred abajo y esta abajo, pred abajo y esta arriba
# pred arriba y esta abajo, pred arriba y esta arriba

mosaic(table(glmpred, factor(outcome)),gp=gpar(fill=matrix(c("green4", "red3", "red3", "green4"),2,2)))
#verde aciertos y rojo errores


# ñ -----------------------------------------------------------------------

view(diabetes)

768*.8
train_ind <- sample(1:nrow(diabetes), 614) 

train = (Year < 2005 ) # conjunto de entrenamiento, se agarra un 80 porciento, para luego veer que tal le fue contra lo demas
Smarket2005 = data[!train, ] # conjunto de prueba, #de la base de datos, quita los del entrenamiento
dim(Smarket2005)

Direction2005 <- Direction[!train]
glmtrain <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Volume, data = data, family = "binomial", subset = train)
# subset, restringe a que solo tomemos los entrenamientos
summary(glmtrain)

glmprobst <- predict(glmtrain, Smarket2005, type = "response")
glmpredt <- rep("Down", 252)
glmpredt[glmprobst > 0.5] = "Up"
table(glmpredt, Direction2005)
mosaic(table(glmpredt, Direction2005),gp=gpar(fill=matrix(c("green4", "red3", "red3", "green4"),2,2)))
# esta tabla fue hecha con el porcentaje de entrenamiento, hasta el año 2005, con 252 obs.

# vamos a comparar para ver cual fue mejor

table(glmpred, Direction)
table(glmpredt, Direction2005)


# investiga ---------------------------------------------------------------

# https://statisticsglobe.com/r-split-data-into-train-and-test-sets

split_dummy <- sample(c(rep(0, 0.8 * nrow(datos)),  # Create dummy for splitting
                        rep(1, 1 + 0.2 * nrow(datos)))) # + 1 para que tengan el mismo tamaño 154.6
split_dummy  

table(split_dummy)                                 # Table of dummy
# 614 van a train y predecimos 153

data_train <- data[split_dummy == 0, ]             # Create train data

head(data_train)                                   # First rows of train data

data_test <- data[split_dummy == 1, ]              # Create test data
head(data_test)                                    # First rows of test data



