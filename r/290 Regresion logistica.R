# Título: Análisis y Predicción de Diabetes con Regresión Logística en R
# Descripción: Este archivo realiza un análisis exploratorio y visualización de datos con el conjunto de datos 'diabetes.csv'.
# Se construye un modelo de regresión logística para predecir la presencia de diabetes en pacientes basándose en varias mediciones diagnósticas.
# Se generan gráficos para visualizar la relación entre las variables y la presencia de diabetes.
# Se divide el conjunto de datos en un conjunto de entrenamiento y prueba, y se ajusta el modelo al conjunto de entrenamiento.
# Se evalúa el rendimiento del modelo en el conjunto de prueba utilizando varias métricas, incluyendo precisión, sensibilidad, especificidad, y la tasa de falsos positivos y negativos.

library(ISLR2)
library(tidyverse)
library(ggplot2)
library(MASS)
library(msm)
library(msmtools)
library(readr)
library(vcd)
library(PerformanceAnalytics)

data <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/diabetes.csv")
head(data)
detach(data)
attach(data)

# Construye un modelo -----------------------------------------------------

modlog <- glm(Outcome ~ ., 
              data = data, 
              family = "binomial")
summary(modlog)


# Intro ------------------------------------------------------------------

Introducción al Conjunto de Datos de Diabetes
El Conjunto de Datos de Diabetes, originalmente recopilado por el Instituto Nacional de Diabetes y Enfermedades Digestivas y Renales. Este conjunto de datos se compone de varias variables médicas predictivas y una variable objetivo, “Outcome”. Las variables predictoras incluyen el número de embarazos que la paciente ha tenido, su IMC, nivel de insulina, edad y más.
El objetivo de este conjunto de datos es predecir si un paciente tiene diabetes, basándose en ciertas mediciones diagnósticas incluidas en el conjunto de datos. En particular, todos los pacientes aquí son mujeres de al menos 21 años de origen indio Pima.
Contenido del Conjunto de Datos
Este conjunto de datos presenta información específica sobre pacientes, todos ellos mujeres de al menos 21 años y de herencia Pima India. Las variables incluidas son las siguientes:
1.	Pregnancies (Embarazos): Número de veces que la paciente ha estado embarazada.
2.	Glucose (Glucosa): Concentración de glucosa en plasma 2 horas después de un examen de tolerancia a la glucosa oral.
3.	BloodPressure (Presión Arterial): Presión arterial diastólica en milímetros de mercurio (mm Hg).
4.	SkinThickness (Grosor del Pliegue Cutáneo): Grosor del pliegue cutáneo del tríceps en milímetros (mm).
5.	Insulin (Insulina): Nivel de insulina en suero 2 horas después de un examen (en unidades por mililitro - mu U/ml).
6.	BMI (Índice de Masa Corporal): Índice de masa corporal, calculado como el peso en kilogramos dividido por la altura en metros al cuadrado (kg/(m^2)).
7.	DiabetesPedigreeFunction (Función de Pedigrí de Diabetes): Función que representa la probabilidad de tener diabetes según el historial familiar.
8.	Age (Edad): Edad de la paciente en años.
9.	Outcome (Resultado): Variable cualitativa que indica si la paciente tiene diabetes (1) o no (0).


# Gráficos ----------------------------------------------------------------

chart.Correlation(data[ , -9])

ggplot(data, mapping = aes(x=factor(Outcome),y= Pregnancies,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= Glucose,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= BloodPressure,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= SkinThickness,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= Insulin,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= BMI,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y= DiabetesPedigreeFunction,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(data, mapping = aes(x=factor(Outcome),y=Age,fill=factor(Outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

# Mejor modelo ------------------------------------------------------------

modlog <- glm(Outcome ~ . - SkinThickness - Insulin - Age - BloodPressure, 
              data = data, 
              family = "binomial")
summary(modlog)


# 5. Construccion de datatrain y datatest ---------------------------------

split_dummy <- sample(c(rep(0, 0.8 * nrow(data)), rep(1, 1 + 0.2 * nrow(data))))
table(split_dummy)

data_train <- data[split_dummy == 0, ]
data_test <- data[split_dummy == 1, ]

Outcometest <- data_test$Outcome


# 6. Ajuste de modelo logistico con datatrain -----------------------------
  
glmtrain <- glm(Outcome ~ . - SkinThickness - Insulin - Age - BloodPressure, 
                data = data_train, 
                family = "binomial")
summary(glmtrain)


# 7. Construcción de glmprobs ---------------------------------------------

glmprobst <- predict(glmtrain, data_test, type = "response")
glmpredt <- rep("Down", nrow(data_test))
glmpredt[glmprobst > 0.5] = "Up"


# 8. Matriz de confusion --------------------------------------------------

tablepred <- table(glmpredt, Outcometest)
tablepred

mosaic(tablepred,gp=gpar(fill=matrix(c("#588c7e", "#c83349", "#c83349", "#588c7e"),2,2)))


# 9  ----------------------------------------------------------------------

tableper <- tablepred / (length(Outcometest))*100
round(tableper, 2)

tn <- tableper[1,1]
fn <- tableper[1,2]
fp <- tableper[2,1]
tp <- tableper[2,2]

# Accuracy: cantidad de predicciones positivas que fueron correctas.
accuracy <- tp + tn
round(accuracy, 2)

# Precision: porcentaje de casos positivos detectados.
precision <- tp / (tp + fp) * 100
round(precision, 2)


# La sensibilidad (o recall) representa la tasa de verdaderos positivos (True Positive Rate) ó TP. Es la proporción entre los casos positivos bien clasificados por el modelo, respecto al total de positivos. Para calcularlo en este caso:
sensibilidad <-  tp / (tp + fn) * 100
round(sensibilidad, 2)

# F1
f1 <- (2 * precision * sensibilidad) / (precision + sensibilidad)
round(f1, 2)

# Especificidad
especificidad <-tn / (tn + fp) * 100
round(especificidad, 2)

# Tasa de falsos positivos: Es similar a la precisión, pero se calcula considerando la proporción de 
# verdaderos positivos sobre todas las instancias predichas como positivas (TP / (TP + FP)) 
tfp <- tp / ( tp + fp ) * 100
round(tfp, 2)

# Tasa de falsos negativos: Mide la proporción de verdaderos negativos sobre todas las 
# instancias predichas como negativas (TN / (TN + FN)).
tfn <- tn / (tn + fn) * 100
round(tfn, 2)