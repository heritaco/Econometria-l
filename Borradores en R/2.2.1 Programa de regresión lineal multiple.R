library(readxl)
library(lmtest)
library(nortest)
library(ggplot2)
library(qqplotr)
library(GGally)

library(tidyverse)
library(DT)
library(kableExtra)


basic <- c("stripped", "boarded", "hover", "condensed", "responsive")

# Nueva sección con ctrl + shift + r


# Datos -------------------------------------------------------------------

data = Ejemplo_subasta <- read_excel("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/Ejemplo subasta.xlsx")

x1 <- data$Edad # Predictora 1
x2 <- data$`Num Postores` # Predicora 2
y <- data$`Precio de subasta` # Variable de respuesta
n <- length(x1) # Número de datos en cada variable

# Funciones  ------------------------------------------------------------------

graf.d.x1 <- function(x1, x2 , y, α){
  ggplot(data.frame(x1, y), aes(x1, y)) +
    geom_point(color = "#0072B2") +
    labs(title = "Gráfico de edad vs. precio", x = "Edad", y = "Precio") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
graf.d.x2 <- function(x1, x2 , y, α){
  ggplot(data.frame(x2, y), aes(x2, y)) +
    geom_point(color = "#0072B2") +
    labs(title = "Gráfico de postores vs. precio", x = "Postores", y = "Precio") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
ddp <- function(d){
  ggpairs(data,
          aes( bg = "lo que sea, no lo puedo colorear o algo", alpha = 0.5)) +
    labs(title = "Diagrama de pares") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}
graf.mrls.x1 <- function(x1, x2 , y, α){
  ggplot(data,aes(x1, y)) +
    geom_point(color = "#0072B2") +
    geom_smooth(method='lm', fill = "lightblue") + 
    labs(title = "Recta de regresión de edad vs. precio", x = "Edad", y = "Precio") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
graf.mrls.x2 <- function(x1, x2 , y, α){
  ggplot(data,aes(x2, y)) +
    geom_point(color = "#0072B2") +
    geom_smooth(method='lm', fill = "lightblue") + 
    labs(title = "Recta de regresión de postores vs. precio", x = "Postores", y = "Precio") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
aic<- function(x1, x2 , y, α){
  D <- c()
  d <- c()
  
modelo1 <- lm(y ~ x1)
modelo2 <- lm(y ~ x2)
mrlm <- lm(y ~ x1 + x2) 

d[1] = AIC(modelo1)
d[2] = AIC(modelo2) 
d[3] = AIC(mrlm) 

D <- c("1", "2", "Múltiple")

options(scipen = 999)
data.frame("Modelo" = D, "AIC" = d) %>%
  kbl(caption = "Test Akaike") %>%
  kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}
extr.datos  <-  function(x1 ,x2, y, α){
  
  #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
  D <- c()
  d <- c()
  
  mrlm <- lm(y ~ x1 + x2)
  result <- summary(mrlm)
  
  
  b0 <- mrlm$coefficients[1]
  b1 <- mrlm$coefficients[2]
  b2 <- mrlm$coefficients[3]
  s <- result$sigma
  scuad <- s*s
  
  d[1] = b0
  d[2] = b1
  d[3] = b2
  d[4] = s
  d[5] = scuad
  
  D <- c("β₀", "β₁", "β₂", "s", "s²")
  
  options(scipen = 999)
  data.frame("Variable" = D, "Valor" = d) %>%
    kbl(caption = "Extracción de datos") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}
prueb.glob <-  function(x1 ,x2, y, α){
  
  #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
  D <- c()
  d <- c()
  
  mrlm <- lm(y ~ x1 + x2)
  result <- summary(mrlm)
  
  #$$H_0: \beta_1 = \beta_2 = 0 \quad v.s. \quad H_1: \text{Al menos una diferente}$$
  
  result$fstatistic
  fc <- result$fstatistic[[1]]
  d[1] = fc
  D[1] = "FC"
  
  pvaluef <- pf(result$fstatistic[[1]], result$fstatistic[[2]],result$fstatistic[[3]], lower.tail = FALSE)
  d[2] = pvaluef
  D[2] = "P value f"
  
  D[3] = "Conclusión"
  
  if (pvaluef < α){
    d[3] = "Se rechaza H0, es decir, el modelo es significativo"
  }else{
    d[3] = "No existe evidencia para afirmar H1 a  un 0.05"
  }
  
  options(scipen = 999)
  data.frame("Variable" = D, "Valor" = d) %>%
    kbl(caption = "Pruebas global para βi") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
}
prueb.indiv <-  function(x1 ,x2, y, α){
  
  #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
  D <- c()
  d <- c()
  E <- c()
  e <- c()
  
  mrlm <- lm(y ~ x1 + x2)
  result <- summary(mrlm)
  
  pvalueb1 <- result$coefficients[2,4]
  pvalueb2 <- result$coefficients[3,4]
  
  d[1] <- pvalueb1
  e[1] <- pvalueb2
  
  D[1] <- "P value β₁"
  D[2] <- "Concusión"
  E[1] <- "P value β₂"
  E[2] <- "Conclusión"
  
  if (pvalueb1 < α){
    d[2] = "Se rechaza H0, es decir, β₁ es significativo para el modelo"
  }else{
    d[2] = "No existe evidencia para afirmar que b1 sea diferente de cero"
  }
  
  if (pvalueb2 < α){
    e[2] = "Se rechaza H0, es decir, β₂ es significativo para el modelo"
  }else{
    e[2] = "No existe evidencia para afirmar que b2 sea diferente de cero"
  }
  
  options(scipen = 999)
  uno = data.frame("Variable" = D, "Valor" = d) %>%
    kbl(caption = "Prueba individual para β₁") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  print(uno)
  
  dos = data.frame("Variable" = E, "Valor" = e) %>%
    kbl(caption = "Prueba individual para β₂") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  print(dos)
}
int.conf.bi <- function(x1, x2, y, α){
  
  d <- c()
  e <- c()
  
  mrlm <- lm(y ~ x1 + x2)
  result <- summary(mrlm)
  
  b0 <- mrlm$coefficients[1]
  b1 <- mrlm$coefficients[2]
  b2 <- mrlm$coefficients[3]
  
  qa <- qt(α/2, n-1, lower.tail = FALSE)
  
  #lim inf y sup para b1
  d[1] = b1 - qa * result$coefficients[2,2]
  d[2] = b1 + qa * result$coefficients[2,2]
  
  #lim inf y sup para b2
  e[1] = b2 - qa * result$coefficients[3,2]
  e[2] = b2 + qa * result$coefficients[3,2]
  
  Datos = c("Inferior   ", "Superior   ")
  options(scipen = 999)
  a = data.frame("Límites" = Datos, "Valor" = d) %>%
    kbl(caption = "Int. de confianza β₁") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
  b = data.frame("Límites" = Datos, "Valor" = e) %>%
    kbl(caption = "Int. de confianza β₂") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
  print(a)
  print(b)
}
corr <- function(x1, x2, y, α){
  
  d <- c()
  
  mrlm <- lm(y ~ x1 + x2)
  
  result <- summary(mrlm)
  
  rcuad <- result$r.squared
  r.ajust <- result$adj.r.squared
  
  if (abs(rcuad) >= 0.6) {
    d[1] = "Hay una correlación fuerte"
  } else if (abs(rcuad) <= 0.4) {
    d[1] = "Hay una correlación baja"
  } else {
    d[1] = "Hay una correlación media"
  }
  d[2] = rcuad
  

  if (abs(r.ajust) >= 0.6) {
    d[3] = "\nHay una correlación fuerte"
  } else if (abs(r.ajust) <= 0.4) {
    d[3] = "\nHay una correlación baja"
  } else {
    d[3] =  "\nHay una correlación media"
  }
  d[4] = r.ajust
  
  Datos = c("R cuad", "Valor", "R adj", "Valor")
  options(scipen = 999)
  data.frame("Rho" = Datos, "Conclsuión" = d) %>%
    kbl(caption = "Coeficiente de correlación") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}
graf.mrlm <- function(x1 ,x2, y, α){
  
  mrlm <- lm(y ~ x1 + x2)
  result <- summary(mrlm)

  rangox1 <- range(x1)
  rangox2 <- range(x2)
  
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
  
  qf(α, result$fstatistic[[2]],  result$fstatistic[[3]])
  
}
graf.err <-  function(x1 ,x2, y, α){
  
  mrlm <- lm(y ~ x1 + x2)
  error <- mrlm$residuals
  n <- length(error)
  x <- 1:n
  
  # Crear el gráfico utilizando ggplot2
  err = ggplot(data = data.frame(x = x, error = error), aes(x = x, y = error)) +
    geom_point(color = "#0072B2") +
    geom_line() +
    geom_hline(yintercept = 0, color = "#0061A1") +
    labs(title = "Gráfico de errores", x = "Índice", y = "Error") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Histograma
  hist = ggplot(data = data.frame(error), aes(x = error)) +
    geom_histogram(binwidth = 20, fill = "#0072B2", color = "white", alpha = 0.7) +
    labs(title = "Histograma de Error", x = "Error", y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Boxplot
  boxplot = ggplot(data = data.frame(error), aes(y = error)) +
    geom_boxplot(fill = "#0072B2", alpha = 0.7) +
    labs(title = "Boxplot de Error", y = "Error") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Gráfico QQ
  qqgraf = ggplot(mapping = aes(sample = error)) + 
    stat_qq_band(alpha = 0.05, fill = "blue") +
    stat_qq_point(color = "#0072B2", alpha = 0.7) +
    stat_qq_line (color = "#0061A1") +
    labs(title = "Gráfico QQ de Error", x = "Cuantiles teóricos", y = "Cuantiles observados") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  print(err)
  print(hist)
  print(boxplot)
  print(qqgraf)
}
norm.err <-  function(x1 ,x2, y, α){
    D <- c()
    d <- c()
    e <- c()
    E <- c()
    
    # Análisis de residudales
    
    modelo = mrlm <- lm(y ~ x1 + x2)
    errores <- summary(modelo)$residuals
    
    # Test de normalidad
    if (n < 30){
      normalidad_shapiro <- shapiro.test(errores)
      d[1] = normalidad_shapiro$p.value
      if (normalidad_shapiro$p.value >= α) {
        d[2] = "p valor ≥ α, no rechazamos H0        "
      } else {
        d[2] = "p valor < α, rechazamos H0. No son normales"
      }
    } else {
      normalidad_lillie <- lillie.test(errores)
      lillie.pavalor <- normalidad_lillie$p.value
      d[1] = lillie.pavalor
      if (lillie.pavalor >= α) {
        d[2] = "p valor ≥ α, no rechazamos H0  ."
      } else {
        d[2] = "p valor < α, rechazamos H0. No son normales"
      }
    }
    
    normalidad_anderson <- ad.test(errores)
    anderson.pvalor <- normalidad_anderson$p.value
    e[1] = anderson.pvalor
    if (normalidad_anderson$p.value >= α) {
      e[2] = "p valor ≥ α, no rechazamos H0             "
    } else {
      e[2] = "p valor < α, rechazamos H0. No son normales"
    }
  
    
    D <- c("P valor", "Conclusión     ")
    
    options(scipen = 999)
    a1 = data.frame("Test Shapiro" = D, "Valor" = d) %>%
      kbl(caption = "Normalidad \nH0: Err. norm vs H1: Err. no norm") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
    
    a2 = data.frame("Test Lillie" = D, "Valor" = d) %>%
      kbl(caption = "Normalidad \nH0: Err. norm vs H1: Err. no norm") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial")
    
    a3 = data.frame("Test Anderson" = D, "Valor" = e) %>%
      kbl(caption = "Normalidad \nH0: Err. norm vs H1: Err. no norm") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial")
    
    
    if (n < 30){print(a1)}
    else {print(a2)}
    
    print(a3)
  }
corr.err <-  function(x1 ,x2, y, α){
  
  mrlm <- lm(y ~ x1 + x2)
  
  d <- c()

  dwtest(mrlm)
  dw <- dwtest(mrlm)$statistic
  pvalor.dw <- dwtest(mrlm)$p.value
  
  if (dw <= 1){
    d[3] <- "Hay una correlación positiva en los residuos"
  } else if (dw >= 3) {
    d[3] <- "Hay una correlación negativa en los residuos"
  } else {
    d[3] <- "No hay una correlación en los residuos"
  }
  
  d[1] <- dw
  d[2] <- pvalor.dw
  
  Datos <- c("DW", "P valor", "Conclusión")
  options(scipen = 999)
  data.frame("Test" = Datos, "Conclusión" = d) %>%
    kbl(caption = "Test Durbin-Watson") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}


# Resultados  ---------------------------------------------------------------

graf.d.x1(x1, x2, y, 0.05)
graf.d.x2(x1, x2, y, 0.05)
ddp(data)
graf.mrls.x1(x1, x2, y, 0.05)
graf.mrls.x2(x1, x2, y, 0.05)
aic(x1, x2, y, 0.05)
extr.datos(x1, x2, y, 0.05)
prueb.glob(x1, x2, y, 0.05)
prueb.indiv(x1, x2, y, 0.05)
int.conf.bi(x1, x2, y, 0.05)
corr(x1, x2, y, 0.05)
graf.mrlm(x1, x2, y, 0.05)
graf.err(x1, x2, y, 0.05)
norm.err(x1, x2, y, 0.05)
corr.err(x1, x2, y, 0.05)



