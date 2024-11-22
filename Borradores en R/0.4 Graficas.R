library(GLMsData)
library(titanic)
library(PerformanceAnalytics)
library(corrplot)
library(arsenal)
library(tidyverse)
library(gapminder)
library(ggthemes)
titanicdata <- as.data.frame(titanic_train)
#La edad influye en la supervivencia de una persona
titanicdata %>%
  filter(Age > 0) %>%
  ggplot(mapping = aes(x = Age)) +
  geom_histogram(bins = 10, color = "red", fill = "pink")
titanicdata %>%
  filter(Survived == 0 & Age > 0) %>%
  ggplot(mapping = aes(x = Age)) +
  geom_histogram(bins = 10, color = "red", fill = "pink") #como superponer dos graficas?

#El sexo influye en la supervivencia de una persona
sexo <- c("Hombre", "Mujer", "Hombre", "Mujer")
tipo <- c(rep("Muertos",2), rep("Sobrevivientes",2))
conteo <- c(nrow(titanicdata %>%
                   filter(Survived == 0 & Sex == "male")),
            nrow(titanicdata %>%
                   filter(Survived == 0 & Sex == "female")),
            nrow(titanicdata %>%
                   filter(Survived == 1 & Sex == "male")),
            nrow(titanicdata %>%
                   filter(Survived == 1 & Sex == "female")))
df <- data.frame(Sexo = sexo, Tipo = tipo, Conteo = conteo)
ggplot() + 
  geom_bar(data = df,aes(x = Sexo,
                         y = Conteo,
                         fill = Tipo), 
           stat='identity', 
           position='dodge') +
  scale_fill_manual(values = c("#CBDBC5", "#B3CDE3")) +
  labs(title = "Relacion Sexo - Supervivencia", x = "Sexo", y = "Numero de muertes", subtitle = "Base de datos: titanic_train") +
  theme(plot.title = element_text(size = 20, face = "bold", color =  "#FBB4AE", hjust = 0.5),
        plot.subtitle = element_text(size = 15, color = "#F4CAE4", face = "bold", hjust = 0.5))

#La clase influye en la supervivencia de una persona
titanicdata %>%
  filter(Survived == 0) %>%
  mutate(Clase = case_when(
    Pclass == 1 ~ "Primera",
    Pclass == 2 ~ "Segunda",
    Pclass == 3 ~ "Tercera")) %>%
  select(2, 13) %>%
  ggplot(mapping = aes(Clase)) +
  geom_bar(color = "red", fill = "pink")
titanicdata %>%
  mutate(Clase = case_when(
    Pclass == 1 ~ "Primera",
    Pclass == 2 ~ "Segunda",
    Pclass == 3 ~ "Tercera")) %>%
  select(2, 13) %>%
  ggplot(mapping = aes(Clase)) +
  geom_bar(color = "red", fill = "pink")


#Lungcap
data(lungcap)
lungdata <- as.data.frame(lungcap)
#La edad y si fuman influye en el FEV
lungdata %>%
  mutate(Respuesta = case_when(
    Smoke == 1 ~ "Fuma",
    Smoke == 0 ~ "No fuma")) %>%
  ggplot(aes(x = Age,
             y = FEV,
             color = Respuesta)) +
  geom_point() +
  geom_smooth()
lungdata %>%
  mutate(Respuesta = case_when(
    Smoke == 1 ~ "Fuma",
    Smoke == 0 ~ "No fuma")) %>%
  group_by(Respuesta) %>%
  summarise(mean_FEV = mean(FEV))

#El genero influye en el FEV
lungdata %>%
  group_by(Gender) %>%
  summarise(mean_FEV = mean(FEV))
 
#La altura influye en el FEV
lungdata %>%
  mutate(Respuesta = case_when(
    Smoke == 1 ~ "Fuma",
    Smoke == 0 ~ "No fuma")) %>%
  ggplot(aes(x = Ht,
             y = FEV,
             color = Respuesta)) +
  geom_point() +
  geom_smooth()