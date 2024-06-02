library(readxl)
library(lmtest)
library(nortest)
library(ggplot2)
library(qqplotr)

# Datos -------------------------------------------------------------------


data = Ejemplo_subasta <- read_excel("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/Ejemplo subasta.xlsx")
names(data) # Nombres de las columnas
head(data) # Muestra los primeros 5 datos

# Inicio ------------------------------------------------------------------


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


# Modelo de regresión lineal múltiple -------------------------------------


mrlm <- lm(y ~ x1 + x2) # Modelo de regresión lineal múltiplie
summary(mrlm) # Ahora nos vamos a fijar en el Adjusted R-Squared. Coeficiente de determinación. No le sacamos la raiz
anova # H0: B1 = B2 = 0   vs   H1: Al menos uno es distinto de 0
anova <- aov(mrlm)
summary(anova)
AIC(mrlm) # ElAIC es mucho más bajo, este es un mejor modelo


# Extracción del modelo ---------------------------------------------------

b0 <- mrlm$coefficients[1]
b0
b1 <- mrlm$coefficients[2]
b1
b2 <- mrlm$coefficients[3]
b2

# También se puede así
result <- summary(mrlm)
result.anova <- summary(mrlm)
b0 <- result$coefficients["(Intercept)", "Estimate"]
b0

s <- result$sigma
s

scuad <- s*s
scuad

# Prueba general del modelo -----------------------------------------------


#$$H_0: \beta_1 = \beta_2 = 0 \quad v.s. \quad H_1: \text{Al menos una diferente}$$

fc <- result$fstatistic[[1]]
fc

pvaluef <- pf(result$fstatistic[[1]], result$fstatistic[[2]],result$fstatistic[[3]], lower.tail = FALSE)
pvaluef

alpha = .05
if (pvaluef < alpha){
  cat("Se rechaza H_0, es decir, el modelo es significativo")
  }else{
  cat("No existe evidencia para afirmar H_1 a  un nivel alpha =", alpha)
  }


# Pruebas individuales ----------------------------------------------------

#$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$

result
pvalueb1 <- result$coefficients[2,4]
pvalueb2 <- result$coefficients[3,4]

if (pvalueb1 < alpha){
  cat("Se rechaza H_0, es decir, b1 es significativo para el modelo")
  }else{
  cat("No existe evidencia para afirmar que b1 sea diferente de cero")
  }

if (pvalueb2 < alpha){
  cat("Se rechaza H_0, es decir, b2 es significativo para el modelo")
  }else{
  cat("No existe evidencia para afirmar que b2 sea diferente de cero")
  }


# Intervalo de confianza para bi ------------------------------------------

#Lo de arriba se hace con ctrl + shift + r

qa <- qt(alpha/2, n-1, lower.tail = FALSE)

#lim inf y sup para b1
b1 - qa * result$coefficients[2,2]
b1 + qa * result$coefficients[2,2]

#lim inf y sup para b1
b2 - qa * result$coefficients[3,2]
b2 + qa * result$coefficients[3,2]
# La tarea ----------------------------------------------------------------

rcuad <- result$r.squared
r.ajust <- result$adj.r.squared


# Ejemplo -----------------------------------------------------------------

edad <- 140 # Pronosticar un reloj de 140 años
num_post <- 15 # con 15 postores
precio <- b0 + b1*edad + b2*num_post
precio # Es el valor que se espera en lo que se vende el reloj

# Gráfica del modelo ------------------------------------------------------


rangox1 <- range(x1)
rangox1
rangox2 <- range(x2)
rangox2

        # Tienes del minimo,      al maximo, lo divides en 2
xedad <- seq(from=rangox1[1], to=rangox1[2], length.out=20)
xpost <- seq(from=rangox2[1], to=rangox2[2], length.out=20)

# Gráfica

predictores <- outer(X=xedad,Y=xpost, FUN=function(x1,x2){
  predict(object = mrlm,newdata = data.frame(x1,x2))
})

plano <- persp(x=xedad,y=xpost,z=predictores,col = "skyblue",theta = 30,phi=25)
observaciones <- trans3d(x1,x2,y,plano)
error <- trans3d(x1,x2,fitted(mrlm),plano)
points(observaciones,col="red",pch=19)
segments(observaciones$x,observaciones$y,error$x,error$y)

# Ahora en el summary nos vamos a fijar en el F-statistic,  para saber si el modelo es significativo
  # En este caso el DF1 = 2 y el DF2 = 29 
  # Se calcula el pvalor para saber si es o no significativo

qf(0.05, 2, 29)


# Gráficos de errores -----------------------------------------------------

error <- mrlm$residuals
n <- length(error)
x <- 1:n

# Crear el gráfico utilizando ggplot2
ggplot(data = data.frame(x = x, error = error), aes(x = x, y = error)) +
  geom_point(color = "#0072B2") +
  geom_line() +
  geom_hline(yintercept = 0, color = "#0061A1") +
  labs(title = "Gráfico de errores", x = "Índice", y = "Error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histograma
ggplot(data = data.frame(error), aes(x = error)) +
  geom_histogram(binwidth = 20, fill = "#0072B2", color = "white", alpha = 0.7) +
  labs(title = "Histograma de Error", x = "Error", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot
ggplot(data = data.frame(error), aes(y = error)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.7) +
  labs(title = "Boxplot de Error", y = "Error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico QQ
ggplot(mapping = aes(sample = error)) + 
  stat_qq_band(alpha = 0.05, fill = "blue") +
  stat_qq_point(color = "#0072B2", alpha = 0.7) +
  stat_qq_line (color = "#0061A1") +
  labs(title = "Gráfico QQ de Error", x = "Cuantiles teóricos", y = "Cuantiles observados") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Normalidad  -------------------------------------------------------------

shapiro.test(error)
ad.test(error)


# Correlación -------------------------------------------------------------

dwtest(mrlm)

