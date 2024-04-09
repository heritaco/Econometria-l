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

# Datos -------------------------------------------------------------------

data = Ejemplo_subasta <- read_excel("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/Ejemplo subasta.xlsx")

x1 <- data$Edad # Predictora 1
x2 <- data$`Num Postores` # Predicora 2
y <- data$`Precio de subasta` # Variable de respuesta
n <- length(x1) # Número de datos en cada variable

# Funciones ----------- --------------------------------------------------

 # tipo 1 modelo con interacción
 # tipo 2 modelo cuadrático
 # tipo 3 modelo cuadrático completo 

extr.datos <-  function(modelo, α, tipo){
  if (tipo == 1){
    b0 <- summary(modelo)$coefficients[[1]]
    b1 <- summary(modelo)$coefficients[[2]]
    b2 <- summary(modelo)$coefficients[[3]]
    b3 <- summary(modelo)$coefficients[[4]]
    s <- summary(modelo)$sigma
    scuad <- s*s
    
    D <- c("β₀", "β₁", "β₂", "β₃",  "s", "s²", "AIC")
    
    d <- c()
    d[1] = b0
    d[2] = b1
    d[3] = b2
    d[4] = b3
    d[5] = s
    d[6] = scuad
    d[7] = AIC(modelo) 
    
    options(scipen = 999)
    a = data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Extracción de datos") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
    print(a)
  }
  if (tipo == 2){
    b0 <- summary(modelo)$coefficients[1]
    b1 <- summary(modelo)$coefficients[2]
    b2 <- summary(modelo)$coefficients[3]
    s <- summary(modelo)$sigma
    scuad <- s*s
    
    D <- c("β₀", "β₁", "β₂", "s", "s²", "AIC")
    d <- c()
    d[1] = b0
    d[2] = b1
    d[3] = b2
    d[4] = s
    d[5] = scuad
    d[6] = AIC(modelo) 
    
    options(scipen = 999)
    a = data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Extracción de datos") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
    print(a)
  }
  if (tipo == 3){
    b0 <- summary(modelo)$coefficients[[1]]
    b1 <- summary(modelo)$coefficients[[2]]
    b2 <- summary(modelo)$coefficients[[3]]
    b3 <- summary(modelo)$coefficients[[4]]
    b4 <- summary(modelo)$coefficients[[5]]
    b5 <- summary(modelo)$coefficients[[6]]
    s <- summary(modelo)$sigma
    scuad <- s*s
    
    D <- c("β₀", "β₁", "β₂", "β₃", "β₄",  "β₅",  "s", "s²", "AIC")
    
    d <- c()
    d[1] = b0
    d[2] = b1
    d[3] = b2
    d[4] = b3
    d[5] = b4
    d[6] = b5
    d[7] = s
    d[8] = scuad
    d[9] = AIC(modelo) 
    
    options(scipen = 999)
    data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Extracción de datos") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  }
}
prueb.glob <-  function(modelo, α){
  
  #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
  
  D <- c("FC", "P value F", "--------------------", "Conclusión")
  d <- c()
  
  fc <- summary(modelo)$fstatistic[[1]]
  d[1] = fc
  
  pvaluef <- pf(summary(modelo)$fstatistic[[1]], summary(modelo)$fstatistic[[2]],summary(modelo)$fstatistic[[3]], lower.tail = FALSE)
  d[2] = pvaluef
  
  d[3] = " "
  
  if (pvaluef < α){
    d[4] = "Se rechaza H0, es decir, el modelo es significativo"
  }else{
    d[4] = "No existe evidencia para afirmar H1 a  un 0.05"
  }
  
  options(scipen = 999)
  data.frame("Variable" = D, "Valor" = d) %>%
    kbl(caption = "Pruebas global para βi") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}
prueb.indiv <-  function(modelo, α, tipo){
  
  if(tipo == 1){
    #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
    D <- c("P value β₁", "Concusión", "--------------------",
           "P value β₂", "Conclusión","--------------------",
           "P value β₃", "Conclusión" )
    d <- c()
    
    d[3] = " "
    d[6] = " "
    
    pvalueb1 <- summary(modelo)$coefficients[2,4]
    pvalueb2 <- summary(modelo)$coefficients[3,4]
    pvalueb3 <- summary(modelo)$coefficients[4,4]
    
    d[1] <- pvalueb1
    d[4] <- pvalueb2
    d[7] <- pvalueb3
    
    if (pvalueb1 < α){
      d[2] = "Se rechaza H0, es decir, β₁ es significativo para el modelo"
    }else{
      d[2] = "No existe evidencia para afirmar que β₁ sea diferente de cero"
    }
    
    if (pvalueb2 < α){
      d[5] = "Se rechaza H0, es decir, β₂ es significativo para el modelo"
    }else{
      d[5] = "No existe evidencia para afirmar que β₂ sea diferente de cero"
    }
    
    if (pvalueb3 < α){
      d[8] = "Se rechaza H0, es decir, β₃ es significativo para el modelo"
    }else{
      d[8] = "No existe evidencia para afirmar que β₃ sea diferente de cero"
    }
    
    options(scipen = 999)
    a = data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Prueba individual para βᵢ") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
    print(a)
  }
  if(tipo == 2){
    #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
    D <- c("P value β₁", "Concusión", "--------------------",
           "P value β₂", "Conclusión")
    d <- c()
    
    d[3] = " "
    
    pvalueb1 <- summary(modelo)$coefficients[2,4]
    pvalueb2 <- summary(modelo)$coefficients[3,4]
    
    d[1] <- pvalueb1
    d[4] <- pvalueb2
    
    if (pvalueb1 < α){
      d[2] = "Se rechaza H0, es decir, β₁ es significativo para el modelo"
    }else{
      d[2] = "No existe evidencia para afirmar que β₁ sea diferente de cero"
    }
    
    if (pvalueb2 < α){
      d[5] = "Se rechaza H0, es decir, β₂ es significativo para el modelo"
    }else{
      d[5] = "No existe evidencia para afirmar que β₂ sea diferente de cero"
    }
    
    
    options(scipen = 999)
    a = data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Prueba individual para βᵢ") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
    print(a)
  }
  if(tipo == 3){
    #$$H_0: \beta_i = 0 \quad v.s. \quad H_1: Beta_i dif 0 $$
    D <- c("P value β₁", "Conclusión","--------------------", 
           "P value β₂", "Conclusión","--------------------",
           "P value β₃", "Conclusión","--------------------",
           "P value β₄", "Conclusión","--------------------",
           "P value β₅", "Conclusión"
    )
    d <- c()
   
    
    
    d[3] = " "
    d[6] = " "
    d[9] = " "
    d[12] = " "
    
    pvalueb1 <- summary(modelo)$coefficients[2,4]
    pvalueb2 <- summary(modelo)$coefficients[3,4]
    pvalueb3 <- summary(modelo)$coefficients[4,4]
    pvalueb4 <- summary(modelo)$coefficients[5,4]
    pvalueb5 <- summary(modelo)$coefficients[6,4]
    
    d[1] <- pvalueb1
    d[4] <- pvalueb2
    d[7] <- pvalueb3
    d[10] <- pvalueb2
    d[13] <- pvalueb3
    
    if (pvalueb1 < α){
      d[2] = "Se rechaza H0, es decir, β₁ es significativo para el modelo"
    }else{
      d[2] = "No existe evidencia para afirmar que β₁ sea diferente de cero"
    }
    
    if (pvalueb2 < α){
      d[5] = "Se rechaza H0, es decir, β₂ es significativo para el modelo"
    }else{
      d[5] = "No existe evidencia para afirmar que β₂ sea diferente de cero"
    }
    
    if (pvalueb3 < α){
      d[8] = "Se rechaza H0, es decir, β₃ es significativo para el modelo"
    }else{
      d[8] = "No existe evidencia para afirmar que β₃ sea diferente de cero"
    }
    
    if (pvalueb4 < α){
      d[11] = "Se rechaza H0, es decir, β₄ es significativo para el modelo"
    }else{
      d[11] = "No existe evidencia para afirmar que β₄ sea diferente de cero"
    }
    
    if (pvalueb5 < α){
      d[14] = "Se rechaza H0, es decir, β₅ es significativo para el modelo"
    }else{
      d[14] = "No existe evidencia para afirmar que β₅ sea diferente de cero"
    }
    
    options(scipen = 999)
    a = data.frame("Variable" = D, "Valor" = d) %>%
      kbl(caption = "Prueba individual para βᵢ") %>%
      kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial")
    print(a)
  }
}
norm.err <-  function(modelo, α){
  d <- c()
  
  error <- summary(modelo)$residuals
  
  if (n < 30){
    D <- c("Shapiro", "Conclusión", " ", "Anderson", "Conclusión")
    normalidad_shapiro <- shapiro.test(error)
    d[1] = normalidad_shapiro$p.value
    if (normalidad_shapiro$p.value >= α) {
      d[2] = "p valor ≥ α, no rechazamos H0        "
    } else {
      d[2] = "p valor < α, rechazamos H0. No son normales"
    }
  } else {
    D <- c("Lillie", "Conclusión", "--------------------", "Anderson", "Conclusión")
    normalidad_lillie <- lillie.test(error)
    lillie.pavalor <- normalidad_lillie$p.value
    d[1] = lillie.pavalor
    if (lillie.pavalor >= α) {
      d[2] = "p valor ≥ α, no rechazamos H0  ."
    } else {
      d[2] = "p valor < α, rechazamos H0. No son normales"
    }
  }
  d[3] = ""
  
  normalidad_anderson <- ad.test(error)
  anderson.pvalor <- normalidad_anderson$p.value
  d[4] = anderson.pvalor
  if (normalidad_anderson$p.value >= α) {
    d[5] = "p valor ≥ α, no rechazamos H0             "
  } else {
    d[5] = "p valor < α, rechazamos H0. No son normales"
  }
  
  options(scipen = 999)
  data.frame("Test" = D, "Valor" = d) %>%
    kbl(caption = "Normalidad \nH0: Err. norm vs H1: Err. no norm") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
  
}
corr.err <-  function(modelo, α){
  
  d <- c()
  
  dwtest(modelo)
  dw <- dwtest(modelo)$statistic
  pvalor.dw <- dwtest(modelo)$p.value
  
  d[3] = " "
  
  if (dw <= 1){
    d[4] <- "Hay una correlación positiva en los residuos"
  } else if (dw >= 3) {
    d[4] <- "Hay una correlación negativa en los residuos"
  } else {
    d[4] <- "No hay una correlación en los residuos"
  }
  
  d[1] <- dw
  d[2] <- pvalor.dw
  
  Datos <- c("DW", "P valor", "--------------------", "Conclusión")
  options(scipen = 999)
  data.frame("Test" = Datos, "Conclusión" = d) %>%
    kbl(caption = "Test Durbin-Watson") %>%
    kable_classic(bootstrap_options = basic, full_width = F, html_font = "Arial") 
}

# Resultados --------------------------------------------------------------

# modelo con interacciones
mi <- lm( y ~ x1 + x2 + x1 * x2 )
summary(mi)

# modelo cuadratico simple
mc1 <- lm( y ~ x1 + I(x1^2) )
summary(mc1) 

mc2 <- lm( y ~ x2 + I(x2^2) )
summary(mc2)

# modelo cuadratico completo
mcc <- lm( y ~ x1 + x2 + x1*x2 + I(x1^2) + I(x2^2) )
summary(mcc)

extr.datos(mi, 0.05, 1)
extr.datos(mc1, 0.05, 2)
extr.datos(mc2, 0.05, 2)
extr.datos(mcc, 0.05, 3)

prueb.glob(mi, 0.05)
prueb.glob(mc1, 0.05)
prueb.glob(mc2, 0.05)
prueb.glob(mcc, 0.05)

prueb.indiv(mi, 0.05, 1)
prueb.indiv(mc1, 0.05, 2)
prueb.indiv(mc2, 0.05, 2)
prueb.indiv(mcc, 0.05, 3)

norm.err(mi, 0.05)
norm.err(mc1, 0.05)
norm.err(mc2, 0.05)
norm.err(mcc, 0.05)

corr.err(mi, 0.05)
corr.err(mc1, 0.05)
corr.err(mc2, 0.05)
corr.err(mcc, 0.05)

