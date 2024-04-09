
library(ISLR2)
library(PerformanceAnalytics) # Mejor que el pair
library(MASS)
library(lmtest)
library(nortest)

head(Boston)

# https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html AGREGAR EL LINK AL RMD

datos <- data.frame(Boston)

crim <- datos$crim
zn  <- datos$zn
indus <- datos$indus
chas <- datos$chas
nox <- datos$nox
rm <- datos$rm
age <- datos$age
dis <- datos$dis
rad <- datos$rad
tax <- datos$tax
ptratio <- datos$ptratio
black <- datos$black
lstat <- datos$lstat
medv <- datos$medv
# variable de respuesta medv



chart.Correlation(datos) # Solo acepta variables numéricas


# Descartaremos a las variables zn, indus, chas, tac y black por ser variables categoricas
# Las variables que ayudarán a predecir a medv pueden ser las variables stat, dis, rad y age.

m1 <- lm(medv ~ rm + lstat + ptratio + age)
summary(m1)
plot(m1) 

stepAIC(m1, direction = "both")


# queremos que este la linea recta
# 1. test de normalidad de los errores
# 2. luego vemos
# 3. te ayuda a determinar cuales son los datos que mas influyen en el modelo
# 4. nos ayuda a eliminar outliers para hacer un mejor modelo

m2 <- lm(medv ~ rm + lstat + ptratio)
summary(m2)
AIC(m2)
er2 <- m2$residuals
shapiro.test(er2)
dwtest(m2)

# paso 1, ver la prueba general. como el p-value: < 2.2e-16. El modelo general es significativo
# paso 2, vemos las pruebas individuales para concluir si son o no diferentes de 0
# paso 3, adjusted r squared, mejor de .5 es bueno
# paso 3.5 test aic
# paso 4 analisis de residuales
# paso 5 no independencia de los errores

m3 <- lm(medv ~ rm + lstat + I(lstat^2) + ptratio)
summary(m3)
AIC(m3)
er3 <- m3$residuals
shapiro.test(er3)
dwtest(m3)


m4 <- lm(medv ~ rm +  I(rm^2) + lstat + I(lstat^2) + ptratio)
summary(m4)
AIC(m4)
er4 <- m4$residuals
shapiro.test(er4)
dwtest(m4)


m5 <- lm(medv ~ rm +  I(rm^2) + lstat + I(lstat^2) + ptratio + I(crim^2), data = datos)
summary(m5)
AIC(m5)
er4 <- m4$residuals
shapiro.test(er4)
dwtest(m4)



stepAIC(m5, direction = "both")

