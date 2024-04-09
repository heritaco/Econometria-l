# Custom linear regression function with significance level (alpha)
mifuncion <- function(x, y, alpha) {
  n <- length(x)
  xprom <- mean(x)
  yprom <- mean(y)
  
  SCxy <- sum(x * y) - n * xprom * yprom
  SCx <- sum(x^2) - n * xprom^2
  SCy <- sum(y^2) - n * yprom^2
  
  b1 <- SCxy / SCx
  b0 <- yprom - b1 * xprom
  
  residuals <- y - (b0 + b1 * x)
  
  model <- list(coefficients = c(b0, b1),
                residuals = residuals,
                fitted.values = b0 + b1 * x,
                df = c(1, n - 2),
                call = match.call())
  
  class(model) <- "lm"
  
  # Calculate values for hypothesis testing and confidence interval
  df <- n - 2
  qti <- qt(alpha / 2, df, lower.tail = TRUE)
  qtd <- qt(alpha / 2, df, lower.tail = FALSE)
  scuad <- (sum(residuals^2) / (n - 2))
  S <- sqrt(scuad)
  tc <- b1 / (S / sqrt(SCx))
  CV <- 100 * sqrt(scuad) / yprom
  r <- SCxy / sqrt(SCx * SCy)
  liminf <- b1 - qtd * S / sqrt(SCx)
  limsup <- b1 + qtd * S / sqrt(SCx)
  pvalor <- 2 * pt(-abs(tc),df)
  
  cat("n:", n, "\n")
  cat("Degrees of freedom:", df, "\n")
  cat("Quantile (lower tail):", qti, "\n")
  cat("Quantile (upper tail):", qtd, "\n")
  cat("Cuadratic error of b1:", scuad, "\n")
  cat("Standard error of b1:", S, "\n")
  cat("t-statistic:", tc, "\n")
  cat("CV:", CV, "\n")
  cat("r:", r, "\n")
  cat("Límite inferior:", liminf, "\n")
  cat("Limite superior:", limsup, "\n")
  
  # Region of rejection for hypothesis test
  critical_value <- qt(alpha / 2, df = n - 2, lower.tail = FALSE)
  
  if (abs(tc) > critical_value) {
    cat("Hypothesis rejected: b1 is significantly different from 0\n")
  } else {
    cat("No evidence to reject the hypothesis: b1 is not significantly different from 0\n")
  }
  
  if(pvalor < alpha){
    print("Rechzamos H0, es decir, b1 es diferente de b0")
  }else{
    print("No existe evidencia para rechazar H0")
  }
  
  
  # ... (rest of the code)
  
  return(model)
}

# Example data
alpha <- 0.05
x <- c(39, 43, 21, 64, 57, 47, 28, 75, 34, 52)
y <- c(65, 78, 52, 82, 92, 89, 73, 98, 56, 75)

# Fit the custom linear regression model with alpha = 0.05
mimodelo <- mifuncion(x, y, alpha)
mifuncion
# Print model summary
print(mimmodelo)
