
library(tidyverse)
library(DT)
library(kableExtra)
library(readr)
basic <- c("stripped", "boarded", "hover", "condensed", "responsive")


# datos -------------------------------------------------------------------

library(readxl)
data = Life_Expectancy_Data <- read_excel("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/Life Expectancy Data.xlsx")
View(Life_Expectancy_Data)

data <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/Expectativa de WHO/archive/Life Expectancy Data.csv")

View(data)

life_expectancy <- data$`Life expectancy`
adult_mortality <- data$`Adult Mortality`
infant_deahts <- data$`infant deaths`
hepatitis <- data$`Hepatitis B`
measles <- data$Measles
bmi <- data$BMI
ufived <- data$`under-five deaths`
polio <- data$Polio
diphteria <- data$Diphtheria
hiv <- data$`HIV/AIDS
gdp <- data$GDP
schooling <- data$Schooling
population <- data$Population


# funciones ---------------------------------------------------------------

modelo_lineal_datos(population, schooling, 0.95 )

modelo_lineal_datos <-   function(x, y, α){
  
  modelo <- lm(y ~ x)
  d <- c()
  
  # Plots
  # Grafico de dispersión
  plot(x,y, col = "#004C99", xlab = "Variable de predictora", ylab = "Variable de respuesta", main = "Gráfico dispersión")
  # Boxplot
  boxplot(x, col = "#004C99", border = "#003366", xlab = "Variable de predictora",  ylab = "Frecuencia", main = "Boxplot x")
  # Histograma
  hist(x, col = "#003366", border = "white", xlab = "Variable de predictora", main = "Histograma x", breaks = 20)
  # Scatteplot
  plot(x, y, col = "#003366", lwd = 1,  cex = 0.5,
       xlab = "Variable predictora", ylab = "Variable de respuesta", main = "Recta de regresión",
       abline( lm( y ~ x ), col = "#004C99", lwd = 3) )
  
  
  # Identificar cosas
  n <- length(x)
  # Identificar β0
  β0 <- modelo$coefficients["(Intercept)"]
  # Identificar β1
  β1 <- modelo$coefficients["x"]
  # Ecuación de la recta
  yhat = recta <- β0  +  β1 * x
  # s
  s <- summary(modelo)$sigma
  # S cuad
  scuad <- s^2
  # CV
  cv <- 100 * s / mean(y)
  
  
  # Inferencia para β1# H0: β1 = 0 vs H1: β1 ≠ 0
  # Obtener los grados de libertad
  df <- summary(modelo)$df[2]
  # Obtener los cuantiles
  qti <- qt(α/2, df, lower.tail = TRUE)
  qtd <- qt(α/2, df, lower.tail = FALSE)
  # Obtener el estadistico de prueba
  tc <- summary(modelo)$coefficients["x", "t value"]
  # Obtener pvalor
  pvalor <- summary(modelo)$coefficients["x", "Pr(>|t|)"]
  
  
  # Intervalo de confianza para β1
  intconfβ1 <- confint(modelo, level = 1 - α)
  intconfβ1min <- intconfβ1["x", 1]
  intconfβ1max <- intconfβ1["x", 2]
  # Error estándar de yhat (intervalos de confianza)
  SCx <- sum(x^2)  -  n * mean(x)^2
  erroryhat <- s * sqrt( (1/n)  +  (x - mean(x))^2 / SCx ) 
  errorpredic <- s * sqrt( 1  +  (1/n)  +  (x - mean(x))^2 / SCx ) 
  # Límites del error estándar de yhat
  lieeyhat <- recta  -  qtd * erroryhat
  lseeyhat <- recta  +  qtd * erroryhat
  # Límites del error estándar de la predicción
  lieepredic <- recta  -  qtd * errorpredic
  lseepredic <- recta  +  qtd * errorpredic
  # Gráfica
  plot(x, y, col = "#003366", lwd = .5, cex = 0.5,
       xlab = "Variable predictora", ylab = "Variable de respuesta", main = "Intervalos de confianza")
  abline (a = β0, b = β1, col = "#0066cc", lwd = 2)
  lines (x = sort(x),  y = sort(lieeyhat),  col="#3399ff",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lseeyhat),  col="#3399ff",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lieepredic),  col="#00ffff",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lseepredic),  col="#00ffff",  lty = 2, lwd = 2)
  
  
  # Análisis de residudales
  errores <- summary(modelo)$residuals
  prom_errores <- mean(errores)
  var_errores <- var(errores)
  # Gráfica para saber si es varianza fija
  plot(errores, col="#003366", type = "o",lwd = 2,
       xlab = "Índice de Observación", ylab = "Valor de Error", main = "Gráfico de Errores")
  lines(1:n, rep(0,n), col="#3399ff", lwd = 2) #  Si hay dos horizontales que encierran la grafica, varianza constante
  # Histograma
  hist(errores, col = "#003366", border = "white",
       main = "Histograma de Errores", xlab = "Errores", ylab = "Frecuencia")
  # Boxplot
  boxplot(errores, main = "Boxplot de errores", col = "#004C99", border = "#003366")
  # QQ
  qqnorm(errores, col = "#003366", lwd = 2, cex = 0.1,
         main = "Gráfico QQ de Errores", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los errores") 
  qqline(errores, col = "#3399ff", lwd = 2)
  
  
  # Test de normalidad
  library(nortest)
  if (n < 30){
    normalidad_shapiro <- shapiro.test(errores) # Para n ≤ 30
    d[16] = shapiro.pavlor <- normalidad_shapiro$p.value
    d[17] = "Shap. P valor"
  } else {
    normalidad_lillie <- lillie.test(errores)
    d[16] = lillie.pavalor <- normalidad_lillie$p.value
    d[17] = "Lillie P valor"
  }
  normalidad_anderson <- ad.test(errores)
  anderson.pvalor <- normalidad_anderson$p.value
  
  # Test de correlacion
  # Pearson 
  rho1 = correlacion_pearson <- cor(x, y)
  # Spearman
  rho2 = correlacion_spearman <- cor(x, y, method = "spearman")
  
  # Autocorrelacion de los errores
  library(lmtest)
  dwtest(modelo)
  dw <- dwtest(modelo)$statistic
  pvalor.dw <- dwtest(modelo)$p.value
  
  Datos <- c("α", "β0","β1","S","S^2", "CV", "df","qti","qtd","tc","P valor","Mín. β1","Máx. β1","Prom. err.", "Var. error" , d[17], "Anderson P valor", "Rho Pearson","Rho Spearman","dw")
  Valores <- c( α , β0 , β1, s ,  scuad  ,  cv ,df, qti , qtd , tc , pvalor, intconfβ1min , intconfβ1max, prom_errores, var_errores, d[16],  anderson.pvalor, rho1, rho2, dw)
  
  data.frame(Datos, Valores) %>%
    kbl(caption = "Informacion del modelo") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
}
modelo_lineal_correlacion <- function(x, y, α){
  d <- c()
  
  # Pearson 
  rho1 = correlacion_pearson <- cor(x, y)
  if (abs(correlacion_pearson) >= 0.6) {
    d[1] = "Hay una correlación fuerte"
  } else if (abs(correlacion_pearson) <= 0.4) {
    d[1] = "Hay una correlación baja"
  } else {
    d[1] = "Hay una correlación media"
  }
  # Spearman
  rho2 = correlacion_spearman <- cor(x, y, method = "spearman")
  if (abs(correlacion_spearman) >= 0.6) {
    d[2] = "\nHay una correlación fuerte"
  } else if (abs(correlacion_spearman) <= 0.4) {
    d[2] = "\nHay una correlación baja"
  } else {
    d[2] =  "\nHay una correlación media"
  }
  
  Datos = c("Pearson", "Spearman")
  options(scipen = 999)
  data.frame("Rho" = Datos, "Conclsuión" = d) %>%
    kbl(caption = "Coeficiente de correlación") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}
modelo_lineal_inferencia_β1 <-   function(x, y, α){
  
  modelo <- lm(y ~ x)
  d <- c()
  
  # Inferencia para β1# H0: β1 = 0 vs H1: β1 ≠ 0
  df <- summary(modelo)$df[2]
  # Obtener los cuantiles
  qtd <- qt(α/2, df, lower.tail = FALSE)
  # Obtener el estadistico de prueba
  tc <- summary(modelo)$coefficients["x", "t value"]
  # Obtener pvalor
  pvalor <- summary(modelo)$coefficients["x", "Pr(>|t|)"]
  
  # Prueba de hipotesis para β1
  if( abs(tc) > qtd ){
    d[1] = "Rechazamos H0. β1 ≠ 0"} 
  else {
    d[1] = "No hay evidencia suficiente para rechazar H0"}
  
  if(2*pvalor < α){
    d[2] = "Rechazamos H0. β1 ≠ 0"}
  else{
    d[2] = "No hay evidencia suficiente para rechazar H0"}
  
  D <- c("tc", "p valor")
  
  options(scipen = 999)
  data.frame("Prueba de hipótesis" = D, "Conclusión" = d) %>%
    kbl(caption = "Inferencia para β1      H0: β1 = 0 vs H1: β1 ≠ 0") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
}
modelo_lineal_analisis_residuales <-  function(x, y, α){
  D <- c()
  d <- c()
  # Análisis de residudales
  n <- length(x)
  
  modelo <- lm(y ~ x)
  errores <- summary(modelo)$residuals
  
  # Test de normalidad
  library(nortest)
  
  if (n < 30){
    D[1] = "Normalidad Shapiro"
    normalidad_shapiro <- shapiro.test(errores) # Para n ≤ 30
    if (normalidad_shapiro$p.value >= α) {
      d[1] = "pvalor ≥ α, no rechazamos H0"
    } else {
      d[1] = "pvalor < α, rechazamos H0. No son normales"
    }
  } else {
    D[1] = "Normalidad Lillie"
    normalidad_lillie <- lillie.test(errores)
    lillie.pavalor <- normalidad_lillie$p.value
    if (lillie.pavalor >= α) {
      d[1] = "pvalor ≥ α, no rechazamos H0"
    } else {
      d[1] = "pvalor < α, rechazamos H0. No son normales"
    }
  }
  
  # Anderson
  D[2] = "Normalidad Anderson"
  normalidad_anderson <- ad.test(errores)
  anderson.pvalor <- normalidad_anderson$p.value
  if (normalidad_anderson$p.value >= α) {
    d[2] = "pvalor ≥ α, no rechazamos H0"
  } else {
    d[2] = "pvalor < α, rechazamos H0. No son normales"
  }
  
  options(scipen = 999)
  data.frame("Test de normalidad" = D, "Valor" = d) %>%
    kbl(caption = "Test de normalidad \nH0: Los datos son normales vs H1: Los datos no son normales") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
}
modelo_lineal_autocorrelacion_errores <-  function(x, y, α){
  modelo <- lm(y ~ x)
  d <- c()
  # Autocorrelacion de los errores
  library(lmtest)
  dwtest(modelo)
  dw <- dwtest(modelo)$statistic
  pvalor.dw <- dwtest(modelo)$p.value
  
  if (dw <= 1){
    d[1] <- "Hay una correlación positiva en los residuos"
  } else if (dw >= 3) {
    d[1] <- "Hay una correlación negativa en los residuos"
  } else {
    d[1] <- "No hay una correlación en los residuos"
  }
  
  Datos <- c("dw")
  
  options(scipen = 999)
  data.frame("Test" = Datos, "Conclusión" = d) %>%
    kbl(caption = "Test Durbin-Watson") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}



# ejemplos ----------------------------------------------------------------



modelo_lineal_datos(schooling,gdp,0.05)
modelo_lineal_datos(life_expectancy,gdp,0.05)
modelo_lineal_datos(adult_mortality,infant_deahts,0.05)
modelo_lineal_datos(hepatitis,gdp,0.05)

modelo_lineal_datos(hepatitis,gdp,0.05)
modelo_lineal_datos(measles,gdp,0.05)
modelo_lineal_datos(hepattis,gdp,0.05)
modelo_lineal_datos(ufived,gdp,0.05)
modelo_lineal_datos(hepatitis,gdp,0.05)

