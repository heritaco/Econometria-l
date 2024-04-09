install.packages(ISLR)
library(ISLR)
library(ISLR2)
library(PerformanceAnalytics)

data <- Smarket

View(data)
names(data)
head(data)
#La única binaria es Direction, va a ser la variable de respuesta
cor(data[ , -9])

chart.Correlation(data) # así no corre
chart.Correlation(data[ , -9]) # tenemos que eliminar  [fila, columna], eliminamos la fila 9
# Está feo, todos estan en medio
attach(data) # Documentar !! nos hace falta saber que es

glmfit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data, family = "binomial")
summary(glmfit)

glmfit1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Volume, data = data, family = "binomial")
summary(glmfit1)
#nos da los betas
coef(glmfit)
coef(glmfit1)
#probabilidad de que dado unos valores fijos de las predictoras, a que categoria pertenece el tipo de direccion
glmprobs <- predict(glmfit1, type = "response")  # Si es mayor a .5 va para arriba y al reves
head(glmpred)
glmprobs[1:10]

contrasts(data$Direction) # Como no tenemos r cuadrada no tenemos una metrica que ayude a evaluar el modelo, 
# entonces se hace por una matriz de observacion, se puede transformar a una de mosaico
glmpred <- rep("Down", 1250) # repite el string 1250 veces.
head(glmpred) # esto pero 1250 veces
glmpred[glmprobs > 0.5] = "Up" # lo vamos a modificar con el filtro de la pribabilidad, si es mayor a .5 se va a up
head(glmpred) 

library(vcd)
table(glmpred, Direction) # tabla de frecuencias, es el contraste

table(glmpred, Direction)
# Lo que pronostica y lo que en verdad paso
# pred abajo y esta abajo, pred abajo y esta arriba
# pred arriba y esta abajo, pred arriba y esta arriba

mosaic(table(glmpred, Direction),gp=gpar(fill=matrix(c("green4", "red3", "red3", "green4"),2,2)))
#verde aciertos y rojo errores


# ñ -----------------------------------------------------------------------
data <- Smarket

train = (Year < 2005 ) # conjunto de entrenamiento, se agarra un 80 porciento, para luego veer que tal le fue contra lo demas
train
Smarket2005 = data[!train, ] # conjunto de prueba, #de la base de datos, quita los del entrenamiento
dim(Smarket2005)

Direction2005 <- Direction[!train] # prueba
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

# tabla frecuencias 100% datos
146/1250
136/1250
456/1250
512/1250

#la clasif correcta es 
146/1250 + 512/1250

# la precision del entrenamiento es mas real, luego se hace muchas veces y se da el promedio

# tabla frecuencias entrenamienito
77/252
97/252
34/252
44/252

77/252 + 44/252 
