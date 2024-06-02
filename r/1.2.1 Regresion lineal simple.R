# install.packages("readr")
library(readxl)
library(DT)
library(kableExtra)
library(readxl)
basic <- c("stripped", "boarded", "hover", "condensed", "responsive")

# Ruta a la carpeta
ruta_carpeta <- "C:/Users/herie/OneDrive/Documentos/GitHub/Econometria l/DataFrames/"
Ejemplo_subasta <- read_excel(paste0(ruta_carpeta, "Ejemplo subasta.xlsx"))
data = Ejemplo_subasta

# Variables predictoras: Edad, Número de postores Variable de respuesta: Edad
edad     <- data$Edad
postores <- data$`Num Postores`
precio   <- data$`Precio de subasta`

modelo_lineal <-   function(x, y, α){
  modelo <- lm(y ~ x)
  
  d <- c()
  e <- c("β0","β1","S","S cuad", "cv", "df","qti","qtd","tc","p value","Int. conf.","","Prom. err.", "Var. err.","P. Hip. β1 Est.","P. Hip. β1 Pvalue","Rho","Rho2","dw","dw p. value")

  # Plots
  # Boxplot
  boxplot(x, col = "lightblue1", xlab = "Variable de predictora", main = "Boxplot x")
  # Histograma
  hist(x, col = "darkseagreen1", xlab = "Variable de predictora", main = "Histograma x", breaks = 20)
  # Grafico de dispersión
  plot(x,y, col = "coral1", xlab = "Variable de predictora", ylab = "Variable de respuesta", main = "Gráfico dispersión")
  # Scatteplot
  plot(x, y, col = "#761E1E", pch = 16, lwd = 1,  cex = 0.5,
       xlab = "Variable predictora", ylab = "Variable de respuesta", main = "Recta de regresión",
       abline( lm( y ~ x ), col = "#bc4749", lwd = 3) )
  
  # Identificar cosas
  n <- length(x)
  # Identificar β0
  β0 <- modelo$coefficients["(Intercept)"]
  d[1] <- β0
  # Identificar β1
  β1 <- modelo$coefficients["x"]
  d[2] <- β1
  # Ecuación de la recta
  yhat = recta <- β0  +  β1 * x
  # s
  s <- summary(modelo)$sigma
  d[3] <- s
  # S cuad
  scuad <- s^2
  d[4] <- s^2
  # Coef de correlacion
  # rho <- sqrt(summary(modelo)$r.squared)
  # d[17] <- rho
  # CV
  cv <- 100 * s / mean(y)
  d[5] <- cv
  
  # Inferencia para β1# H0: β1 = 0 vs H1: β1 ≠ 0
  # Obtener los grados de libertad
  df <- summary(modelo)$df[2]
  d[6] <- df
  # Obtener los cuantiles
  qti <- qt(α/2, df, lower.tail = TRUE)
  d[7] <- qti
  qtd <- qt(α/2, df, lower.tail = FALSE)
  d[8] <- qtd
  # Obtener el estadistico de prueba
  tc <- summary(modelo)$coefficients["x", "t value"]
  d[9] <- tc
  tc
  # Obtener pvalor
  pvalor <- summary(modelo)$coefficients["x", "Pr(>|t|)"]
  d[10] <- pvalor
  # Prueba de hipotesis para β1
  if( abs(tc) > qtd ){
    d[15] = "Rechazamos H0, β1 ≠ 0"
  }else{
    d[15] = "No hay evidencia suficiente para rechazar H0"
  }
  
  if(pvalor < α/2){
    d[16] = "Rechazamos H0, β1 ≠ 0"
  }else{
    d[16] = "No hay evidencia suficiente para rechazar H0"
  }
  
  # Intervalo de confianza para β1
  intconfβ1 <- confint(modelo, level = 1 - α)
  intconfβ1 <- intconfβ1["x", ]
  d[11] <- intconfβ1[1]
  d[12] <- intconfβ1[2]
  # Error estándar de yhat (intervalos de confianza)
  SCx <- sum(x^2)  -  n * mean(x)^2
  erroryprom <- s * sqrt( (1/n)  +  (x - mean(x))^2 / SCx ) 
  errorpredic <- s * sqrt( 1  +  (1/n)  +  (x - mean(x))^2 / SCx ) 
  # Límites del error estándar de yhat
  lieeyprom <- recta  -  qtd * erroryprom
  lseeyprom <- recta  +  qtd * erroryprom
  # Límites del error estándar de la predicción
  lieepredic <- recta  -  qtd * errorpredic
  lseepredic <- recta  +  qtd * errorpredic
  # Gráfica
  plot(x, y, col = "#3b1f2b", lwd = .5, pch = 16, cex = 0.5,
       xlab = "x", ylab = "y", main = "Intervalos de confianza")
  abline (a = β0, b = β1, col = "#ff5a5f", lwd = 2)
  lines (x = sort(x),  y = sort(lieeyprom),  col="#ff4d00",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lseeyprom),  col="#ff4d00",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lieepredic),  col="#ffd100",  lty = 2, lwd = 2)
  lines (x = sort(x),  y = sort(lseepredic),  col="#ffd100",  lty = 2, lwd = 2)
  
  
  # Análisis de residudales
  errores <- summary(modelo)$residuals
  d[13] <- mean(errores)
  d[14] <- var(errores)
  # Gráfica para saber si es varianza fija
  plot(errores, col="#0d47a1", type = "o",lwd = 2, pch = 16,
       xlab = "Índice de Observación", ylab = "Valor de Error", main = "Gráfico de Errores")
  lines(1:n, rep(0,n), col="#df3e3e", lwd = 2) #  Si hay dos horizontales paralelas que puedan encerrar la grafica la varianza es constante
  # Histograma
  hist(errores, col = "#415a77", border = "white",
       main = "Histograma de Errores", xlab = "Errores", ylab = "Frecuencia")
  # Boxplot
  boxplot(errores, main = "Boxplot", col = "#588157", medcol = "#344e41", border = "#344e41")
  # QQ
  qqnorm(errores, col = "#ad2831", lwd = 2, pch = 0, cex = 0.1,
         main = "Gráfico QQ de Errores", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los errores") 
  qqline(errores, col = "#640d14", lwd = 2)
  
  # tabla_contingencia <- table(x, y)
  
  
  # Test de normalidad
  # Anderson
  library(nortest)
  normalidad_anderson <- ad.test(errores) 
  if (normalidad_anderson$p.value >= α) {
    cat("\nComo el pvalor ≥ α, no rechazamos H0, los datos son normales, y no hay evidencia que garantice que NO son normales\n",
        normalidad_anderson$p.value, "≥", α)
  } else {
    cat("\nComo el pvalor < α, rechazamos H0, los datos NO son normales\n", 
        normalidad_anderson$p.value, "<", α)
  }
  d[15] <- normalidad_anderson$p.value
  
  # Lilline
  normalidad_lilline <- lillie.test(errores)
  if (normalidad_lilline$p.value >= α) {
    cat("\nComo el pvalor ≥ α, no rechazamos H0, los datos son normales, y no hay evidencia que garantice que NO son normales\n",
        normalidad_lilline$p.value, "≥", α)
  } else {
    cat("\nComo el pvalor < α, rechazamos H0, los datos NO son normales\n", 
        normalidad_lilline$p.value, "<", α)
  }
  
  d[16] <- normalidad_lilline$p.value
  
  
  # Correlacion
  # Pearson 
  correlacion_pearson <- cor(x, y)
  
  if (abs(correlacion_pearson) >= 0.6) {
    mensaje <- "\nHay una correlación fuerte,"
  } else if (abs(correlacion_pearson) <= 0.4) {
    mensaje <- "\nHay una correlación baja,"
  } else {
    mensaje <- "\nHay una correlación media,"
  }
  
  cat(mensaje, "coeficiente de correlación de Pearson:", correlacion_pearson)
  d[17] <- correlacion_pearson 
  
  
  # Spearman
  correlacion_spearman <- cor(x, y, method = "spearman")
  if (abs(correlacion_spearman) >= 0.6) {
    mensaje <- "\nHay una correlación fuerte,"
  } else if (abs(correlacion_spearman) <= 0.4) {
    mensaje <- "\nHay una correlación baja,"
  } else {
    mensaje <- "\nHay una correlación media,"
  }
  
  cat(mensaje, "coeficiente de correlación de Spearman:", correlacion_spearman)
  d[18] <- correlacion_spearman
  
  # Autocorrelacion de los errores
  # El estadísticp de Durbin-Watson toma valores entre 0 y 4
  # un valos ceercano a 2 indica independendencia de los errores
  # valores significativamente diferentes de 2 indican autocorrelacion positiva o negativa
  library(lmtest)
  dwtest(modelo) # Prueba de correlacion
  # Si p-valor mayor a 0.05 los errores no son independientes
  # Si p-valor menor a 0.05 no hay evidencia suficiente para rechazar la hipótesis nula de ausencia de autocorrelación serial en los residuos.
  dw <- dwtest(modelo)$statistic
  
  pvalor.dw <- dwtest(modelo)$p.value
  
  if(abs(dw) >= 1){
    mensaje <- "\nHay una correlación de los errores,"
  } else {
    mensaje <- "\nNo hay una correlación de los errores,"
  }
  cat(mensaje, "dw:", dw)
  d[19] <- dw
  
  if(pvalor.dw >= α){
    mensaje <- "\nLos errores no son independientes,"
  } else {
    mensaje <- "\nNo hay evidencia suficiente para rechazar la hipótesis nula de ausencia de autocorrelación serial en los residuos.,"
  }
  cat(mensaje, "pvalor.dw:", pvalor.dw)
  d[20] <- pvalor.dw
  
  
  data.frame("Datos" = e, "Valores" = d) %>%
    kbl(caption = "Informacion del modelo") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Cambria")
}

modelo_lineal(  edad  , precio, .05)
modelo_lineal(postores, precio, .05)

