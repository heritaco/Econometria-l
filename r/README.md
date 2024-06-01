m - Modelo

ls - Lineal simple
lm - Lineal múltiple
ci - Con interacciones
cs - Cuadrático simple
cc - Cuadrático completosss
po - Polinómico

mcc <- lm( y ~ x1 + x2 + x1*x2 + I(x1^2) + I(x2^2) )
