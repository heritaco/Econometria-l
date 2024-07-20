# Descripción: Este archivo contiene ejemplos de manipulación de
# datos utilizando el lenguaje de programación R. Se utilizan los conjuntos de datos
# 'gapminder' e 'iris' para demostrar diversas operaciones como la selección de columnas, 
# filtrado de datos, transformación de datos y creación de nuevas columnas. También se muestran 
# ejemplos de cómo usar operadores lógicos en el filtrado de datos.

# Instalar el paquete 'gapminder'
install.packages("gapminder")

# Cargar el paquete 'gapminder'
library(gapminder)
library(dplyr)

### Datos ###
data("gapminder")

### Encabezado ###
head(gapminder)

### Nombre de encabezado de columnas ###
colnames(gapminder)

### Seleccion de datos ###
x <- gapminder %>% select(country)

### Seleccion de varias columnas ###
y <- gapminder %>% select(country, pop)         # Seleccionar columnas por nombre
y <- gapminder %>% select(c("country", "pop"))  # Seleccionar columnas por nombre
columnas_de_interes <- c("country", "pop")
y <- gapminder %>% select(columnas_de_interes)

### Seleccionar rangos y "eliminar" columnas ###
z <- gapminder %>% select(2:4)
z <- gapminder %>% select(1,3:5)
z <- gapminder %>% select(-3)
z <- gapminder %>% select(-columnas_de_interes)

### Ejercicio 1 ###
columnas_eliminar <- c("year", "gdpPercap")
z <- gapminder %>% select(-columnas_eliminar)

### Seleccionar y filtrar por informacion ###
data("iris")
head(iris)
a <- iris %>% select(starts_with("Sepal"))
a <- iris %>% select(ends_with("Length"))
a <- iris %>% select(containy("."))
a <- gapminder %>% filter(country == "Mexico")
a <- gapminder %>% filter(lifeExp == 40)
a <- gapminder %>% filter(lifeExp > 40)
a <- gapminder %>% filter(country != "Mexico")

### Filtrar por mas de un valor ###
b <- gapminder %>% filter(country == "Mexico", lifeExp <= 55)
b <- gapminder %>% filter(country %in% c("Mexico", "Spain"))
b <- gapminder %>% filter(year %in% c(1952, 1957))

### Operadores logicos ###
#! negacion
#& and
#| or
b <- gapminder %>% filter(country == "Mexico" & year == 2007)
b <- gapminder %>% filter(country == "Mexico" | year == 2007)
b <- gapminder %>% filter(!country == "Mexico")
b <- gapminder %>% filter(!year == 1952)

### Ejercicio 2 ###
b <- gapminder %>% filter(!country %in% c(1952, 1957))

### Agregar datos ###
c <- mutate(pop_m_r = round(pop/1000000,0))

### Ejercicio 3 ###
df4 <- gapminder %>% filter(country == "Mexico") %>% mutate(log_pop = log(pop))

### Transformacion por casos ###
d <- gapminder %>% distinct(continent) #distintas respuestas cualitativas
d <- gapminder %>% mutate(cont_es = case_when(
    continent == "Asia" ~ "Asia",
    continent == "Europe" ~ "Europa",
    continent == "Africa" ~ "Africa",
    continent == "Americas" ~ "America",
    continent == "Oceania" ~ "Oceania"))

### Ejercicio 4 ###
d <- d %>% select(-2)
d
