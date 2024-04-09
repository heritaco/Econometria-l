library(readxl)
SistemaInmune <- read_excel("C:/Users/herie/Downloads/SistemaInmune.xlsx")
library(readxl)
SistemaInmune <- read_excel("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/SistemaInmune.xlsx")
View(SistemaInmune)

datos <- data.frame(SistemaInmune)

y <-  datos$IgG
x <- datos$MaxOxygen
range(x)
plot(x,y)

#1. Genera un modelo lineal simple
mod1 <- lm(y~x)
summary(mod1)
b0_1 <- mod1$coefficients[[1]]
b0_1
b1_1 <- mod1$coefficients[[2]]
b1_1

f1 <- function(x){
  b0_1+b1_1*x
} 
#2. Genera un modelo cuadrático simple
mod2 <- lm(y~ I(x^2))
summary(mod2)
b0_2 <- mod2$coefficients[[1]]
b0_2
b1_2 <- mod2$coefficients[[2]]
b1_2

f2 <- function(x){
  b0_2+b1_2*x^2
}


#3. Genera un modelo cuadrático completo
mod3 <- lm(y~ x+I(x^2))
summary(mod3)
b0_3 <- mod3$coefficients[[1]]
b0_3
b1_3 <- mod3$coefficients[[2]]
b1_3
b2_3 <- mod3$coefficients[[3]]
b2_3

f3 <- function(x){
  b0_3+b1_3*x+b2_3*x^2
}

plot(x,y)
curve(f1,from = 30,to=70,add = TRUE)
curve(f2,from = 30,to=70,add = TRUE)
curve(f3,from = 30,to=70,add = TRUE)


mod4 <- lm(y~ poly(x,3))
summary(mod4)

# Realiza un análisis de cada modelo y concluye cual es el mejor
#Entrega viernes 17