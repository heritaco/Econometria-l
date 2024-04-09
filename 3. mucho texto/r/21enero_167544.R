##### 20 de enero
library(tidyverse)
install.packages("gapminder")
library(gapminder)
data("gapminder")
### ordenar por poblacion 
gapminder %>%
  arrange(pop)
#descendente
gapminder %>%
  arrange(desc(pop))
### ordenar por pais para el año 2007
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(country))
### ordenar por varias columnas
gapminder %>%
  arrange(continent, country)
#ascendente por continente y descendente por pais en 2007
gapminder %>%
  filter(year == 2007) %>%
  arrange(continent, desc(country))
### sumarizar o resumir en tabla
#maximo de esperanza de vida
gapminder %>%
  summarise(max_edv = max(lifeExp))
### encontrar pais con esta esperanza de vida
gapminder %>%
  filter(lifeExp >= 82.6)
### calcular suma total de la poblacion de america en 2007
gapminder %>%
  filter(year == 2007, continent == "Americas") %>%
  summarise(sum_pop = sum(as.numeric(pop)))
### otras funciones en summarise
# sum(), mean(), median(), min(), max(), max(), n(), group_by()
### esperanza de vida por año
gapminder %>%
  group_by(year) %>%
  summarise(mean_edv = mean(lifeExp))
### poblacion y conteos por continent para el año 2007
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(sum_pop = sum(as.numeric(pop)), n_paises = n())
### sumar poblacion pro año por continente
gapminder %>%
  group_by(year, continent) %>%
  summarise(sum_pop = sum(as.numeric(pop)))
# cambiando el orden
gapminder %>%
  group_by(continent, year) %>%
  summarise(sum_pop = sum(as.numeric(pop)))
### para terminar vamos con grafico
gapminder %>%
  group_by(year, continent) %>%
  summarise(mean_edp = mean(lifeExp)) %>%
  ggplot(aes(x = year,
              y = mean_edp,
              color = continent)) +
  geom_line()
### grafica de puntos y lineas
gapminder %>%
  filter(country == "Mexico") %>%
  ggplot(mapping = aes(x = year, y = lifeExp)) +
  geom_line() +
  geom_point()
### grafico de puntos  con linreas de regresion
gapminder %>%
  filter(country == "Mexico") %>%
  ggplot(mapping = aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm")
### grafico de barras
gapminder %>%
  filter(year == 2007) %>%
  ggplot(mapping = aes(x = factor(continent))) +
  geom_bar(color = "red", fill = "pink") +
  coord_flip()
### histograma
gapminder %>%
  filter(year == 2007) %>%
  ggplot(mapping = aes(x= lifeExp)) +
  geom_histogram(bins = 6, color = "red", fill = "pink")
###boxplot
gapminder %>%
  filter(year == 2007) %>%
  ggplot(mapping = aes(y = lifeExp)) +
  geom_boxplot()
 
gapminder %>%
  filter(year == 2007) %>%
  ggplot(mapping = aes(y = gdpPercap)) +
  geom_boxplot()








##### 21 de enero

### estadisticas
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(vida_media = mean(lifeExp),
            vida_sd = sd(lifeExp),
            npaises = n())
### crear etiquetas para lifeExp
quantile(gapminder$lifeExp)

gapminder$life_label <- gapminder$lifeExp %>%
  cut(breaks = c(0,50,70,100), labels = c("Baja", "Media", "Alta"))

### tableby
library(arsenal)
tab1952 <- gapminder %>%
  filter(year == 2007) %>%
  tableby(continent ~ life_label, data = .) %>%
  summary(text = TRUE) %>%
  as.data.frame()

### cuantiles especificos
gapminder %>%
  filter(year == 2007) %>%
  select(lifeExp) %>%
  pull() %>% #trata elementos seleccionados de un data frame como un vector
  quantile(prob = c(0.36, 0.62))

### cuantiles poblacion con etiqueta
quantile(gapminder$pop)
gapminder$pop_label <- gapminder$pop %>%
  cut(breaks = quantile(gapminder$pop),
      labels = c("Muy Baja", "Baja", "Alta", "Muy Alta"))
tabla_pop <- gapminder %>%
  filter(year == 2007) %>%
  tableby(continent ~ pop_label, data = .) %>%
  summary(text =TRUE) %>%
  as.data.frame()

###  correlaciones
gappop <- gapminder %>%
  filter(year ==2007) %>%
  select(year, lifeExp, pop, gdpPercap) %>%
  pairs()

gapcor <- gapminder %>%
  filter(year == 2007) %>%
  mutate(lnpop = log(pop),
         lngdp = log(gdpPercap)) %>%
  select(lifeExp, lnpop, lngdp)
gapcor %>%
  pairs()

library(corrplot)
gapcor %>%
  cor() %>%
  corrplot(type = "upper", order = "hclust",
           tl.col = "pink", tl_srt = 45)

library(PerformanceAnalytics)
gapcor %>%
  chart.Correlation(histogram = TRUE, pch = 9)

library(GLMsData)
library(titanic)
data("lungcap")
data("Titanic")
head(lungcap)
head(Titanic)
df <- as.data.frame(Titanic)