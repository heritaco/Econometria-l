# 1. Imprimir un mensaje sencillo
print("Hola, mundo!")

# 2. Asignar valores a variables y operaciones básicas
x <- 5
y <- 3
suma <- x + y
resta <- x - y
multiplicacion <- x * y
division <- x / y

# 3. Estructuras de control: condicional if-else
if (x > y) {
  print("x es mayor que y")
} else {
  print("x no es mayor que y")
}

# 4. Estructuras de control: bucle for
for (i in 1:5) {
  print(paste("Iteración", i))
}

# 5. Vectores y funciones básicas
numeros <- c(2, 4, 6, 8, 10)
promedio <- mean(numeros)
maximo <- max(numeros)
minimo <- min(numeros)

# 6. Crear una función propia
calcular_cuadrado <- function(numero) {
  return(numero^2)
}
resultado_cuadrado <- calcular_cuadrado(4)

# 7. Trabajar con data frames (tablas)
datos <- data.frame(
  nombre = c("Ana", "Luis", "María"),
  edad = c(25, 30, 28)
)
promedio_edad <- mean(datos$edad)

# 8. Gráficos simples con ggplot2
library(ggplot2)
grafico <- ggplot(data=datos, aes(x=nombre, y=edad)) +
  geom_bar(stat="identity") +
  labs(title="Edades de personas", x="Nombre", y="Edad")
grafico

# 9. Lectura y escritura de archivos
write.csv(datos, "datos.csv", row.names=FALSE)
datos_nuevos <- read.csv("datos.csv")

# 10. Uso de paquetes externos y análisis más avanzado
library(dplyr)
datos_filtrados <- datos %>%
  filter(edad > 25) %>%
  arrange(desc(edad))

# Crear una matriz 3x3
matriz <- matrix(1:9, nrow = 3, ncol = 3)

print(matriz)

# Convertir la matriz en un dataframe
datos <- as.data.frame(matriz)

# Crear un gráfico de dispersión
ggplot(data = datos, aes(x = V1, y = V2)) +
  geom_point()