library(readxl)
library(lmtest)
library(nortest)

data = Ejemplo_subasta <- read_excel("DataFrames/Ejemplo subasta.xlsx")

names(data) # Nombres de las columnas
head(data) # Muestra los primeros 5 datos

x1 <- data$Edad # Predictora 1
x2 <- data$`Num Postores` # Predicora 2
y <- data$`Precio de subasta` # Variable de respuesta
n <- length(x1) # Número de datos en cada variable

plot(x1,y) # Edad y precio
plot(x2,y) # Num postores y precio
pairs(data) # Te da una matriz donde puedes observar:
# edad-edad   | num-edad    | precio-edad
# edad-num    | num-num     | precio-num
# edad-precio | num-precio  | precio-precio

# Modelo 1
modelo1 <- lm(y ~ x1)
summary(modelo1)
AIC(modelo1) # Coeficiente de informacion de Akaike ¡te dice cual es el mejor modelo! Menor AIC es mejor

# Modelo 2
modelo2 <- lm(y ~ x2)
summary(modelo2)
AIC(modelo2) # Coeficiente de informacion de Akaike ¡te dice cual es el mejor modelo! Menor AIC es mejor

# Rectas de regresión lineal
plot(x1, y, abline(modelo1))
plot(x2, y, abline(modelo2))

########################################
# Modelo de regresión lineal múlitiple #
########################################

modelo_de_regresion_lineal_multiple = mrlm <- lm(y ~ x1 + x2)
summary(mrlm) # Ahora nos vamos a fijar en el Adjusted R-Squared. Coeficiente de determinación. No le sacamos la raiz
anova # H0: B1 = B2 = 0   vs   H1: Al menos uno es distinto de 0
anova <- aov(mrlm)
summary(anova)
AIC(mrlm) # ElAIC es mucho más bajo, este es un mejor modelo

b0 <- mrlm$coefficients[1]
b0
b1 <- mrlm$coefficients[2]
b1
b2 <- mrlm$coefficients[3]
b2

#################
edad <- 140 # Pronosticar un reloj de 140 años
num_post <- 15 # con 15 postores
precio <- b0 + b1*edad + b2*num_post
precio # Es el valor que se espera en lo que se vende el reloj

######################
# Gráfica del modelo #
######################

rangox1 <- range(x1)
rangox1
rangox2 <- range(x2)
rangox2

        # Tienes del minimo,      al maximo, lo divides en 2
xedad <- seq(from=rangox1[1], to=rangox1[2], length.out=20)
xpost <- seq(from=rangox2[1], to=rangox2[2], length.out=20)


##################
# Lo vamos a ver # 
##################

predictores <- outer(X=xedad,Y=xpost, FUN=function(x1,x2){
  predict(object = mrlm,newdata = data.frame(x1,x2))
})

plano <- persp(x=xedad,y=xpost,z=predictores,col = "skyblue",theta = 30,phi=25)
observaciones <- trans3d(x1,x2,y,plano)
error <- trans3d(x1,x2,fitted(mrlm),plano)
points(observaciones,col="red",pch=19)
segments(observaciones$x,observaciones$y,error$x,error$y)

####################################
# Ahora en el summary nos vamos a fijar en el F-statistic,  para saber si el modelo es significativo
  # En este caso el DF1 = 2 y el DF2 = 29 
  # Se calcula el pvalor para saber si es o no significativo

qf(0.05, 2, 29)

#######################################
error <- mrlm$residuals
plot(error)
lines(1:n, rep(0,n))

hist(error)
boxplot(error)
qqnorm(error)
qqline(error)

# Normalidad
shapiro.test(error)
ad.test(error)

# Correlacion
dwtest(mrlm)
