# install.packages("readr")
library(readr)
data <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Advertising.csv")

# Variables predictoras: TV, Radio, Newpaper. Variable de respuesta: Sales
tv    <- data$TV
radio <- data$radio
newsp <- data$newspaper
sales <- data$sales

funcion <-   function(x, y, α){
  modelo <- lm(y ~ x)
  # Identificar cosas
        # Identificar β0
          β0 <- modelo$coefficients["(Intercept)"]
        # Identificar β1
          β1 <- modelo$coefficients["x"]
        # Ecuación de la recta
          recta <- β0  +  β1 * x
        # s
          s <- summary(modelo)$sigma # Promedio de cuanto varian los valores con la recta
        # S cuad
          scuad <- s^2
        # CV
          cv <- 100 * s / mean(x)
          
          cat("β0:", β0, "\nβ1: ", β1, "\nS: ", s, "\nS^2: ", scuad, "\ncv: ", cv, "\n")
          
  # Inferencia para β1# H0: β1 = 0 vs H1: β1 ≠ 0
        # Obtener df
          df <- summary(modelo)$df[2]
        # Obtener las qt
          qti <- qt(α/2, df, lower.tail = TRUE)
          qtd <- qt(α/2, df, lower.tail = FALSE)
        # Obtener tc
          tc <- summary(modelo)$coefficients["x", "t value"]
        # Obtener pvalor
          pvalor <- summary(modelo)$coefficients["x", "Pr(>|t|)"]
          cat("df: ", df, "\nqti: ", qti, "\nqtd: ", qtd, "\ntc: ", tc, "\npvalor: ", pvalor, "\n")
        # Prueba de hipotesis para β1
          cat("\nPrueba de Hipótesis para β1\n")
          if( abs(tc) > qtd ){ cat("Rechazamos H0, β1 ≠ 0\n", β1, "≠ 0\n")
          }else{ print("No existe evidencia para rechazar H0\n")}
          if( pvalor < α){ cat("Rechazamos H0, β1 ≠ 0\n", β1, "≠ 0\n")
          }else{ print("No existe evidencia para rechazar H0\n")}
  # Intervalo de confianza para β1
          intconfβ1 <- confint(modelo, level = 1 - α)
          intconfβ1 <- intconfβ1["x", ]
          cat("\nEl intervalo de confianza para β1 es: [", intconfβ1[1], ",", intconfβ1[2], "]\n")
  # Error estándar de promy
        n <- length(x)
        erroryprom <- s * sqrt( (1/n)  +  (x - mean(x))^2 / (sum(x^2)  -  n * mean(x)^2  ) ) 
        errorpredic <- s * sqrt( 1  +  (1/n)  +  (x - mean(x))^2 / (sum(x^2)  -  n * mean(x)^2) ) 
        # Límite inferior error estándar del promedio de y
          lieeyprom <- recta  -  qtd * erroryprom
        # Límite superior error estándar del promedio de y
          lseeyprom <- recta  +  qtd * erroryprom
        # Límite inferior del error estándar de la predicción
          lieepredic <- recta  -  qtd * errorpredic
        # Límite superior del error estándar de la predicción
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
          error <- y - recta
          cat("Promedio de errores:", mean(error), "\nVarianza de errores:", var(error), "\n")
        # Gráfica para saber si es varianza fija
          plot(error, col="#0d47a1", type = "o",lwd = 2, pch = 16, xlab = "Índice de Observación", ylab = "Valor de Error", main = "Gráfico de Errores")
          lines(1:n, rep(0,n), col="#df3e3e", lwd = 2)
        # Histograma
          hist(error, col = "#415a77", border = "white", main = "Histograma de Errores", xlab = "Errores", ylab = "Frecuencia")
        # Boxplot
          boxplot(error, main = "Boxplot", col = "#588157", medcol = "#344e41", border = "#344e41")
        # QQ
          qqnorm(error, col = "#ad2831", lwd = 2, pch = 0, cex = 0.1, main = "Gráfico QQ de Errores", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los errores") 
                 qqline(error, col = "#640d14", lwd = 2)
}

funcion(   tv, sales, .05)
funcion(radio, sales, .05)
funcion(newsp, sales, .05)