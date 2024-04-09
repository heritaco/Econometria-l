
# https://archive.ics.uci.edu/dataset/27/credit+approval                          # Esta raro, pero abajo lo leo
# Lectura de datos --------------------------------------------------------

credit <- read.csv("C:/Users/herie/Downloads/credit+approval/crx.data")
str(credit)                                                                     # Queremos predecir A16
                                                                                # V2 los lee como character buuuu mal mal

# Ver una imagen ----------------------------------------------------------

# https://www.researchgate.net/figure/Credit-Approval-Dataset-List-of-Attributes_tbl1_3297254
library(imager)
image <- load.image("C:/Users/herie/Downloads/
                    Credit-Approval-Dataset-List-of-Attributes.png")
plot(image)


# Limpiar base de datos ---------------------------------------------------     # Tenemos que limpiarla antes de usar el XgBoost

colnames(credit) <- c("sex", "age", "mtadress", "homestat", "lomismo", 
                      "occupation", "jobstatus", "mtemployers", "othinv", 
                      "bankacc", "twbank", "liabilityred", "accref", 
                      "mhexpense", "savingsbal","class")                        # Le ponemos título
head(credit)

table(credit$lomismo, credit$homestat)                                          # Como es la identidad, son iguales, eliminamos una

credit <- credit[ , -5]                                                         # Eliminar columnas
head(credit)


# Null values -------------------------------------------------------------

lapply(credit, function(x) sum(is.na(x)))                                       # Sumar los valores nulos, donde la celda es vacia 
lapply(credit, function(x) sum( x == "?"))                                      # Cadafila(basededatos, suma( las celdas que son ?))

credit[credit == "?"] <- NA                                                     # Reemplaza las celdas que tienen "?" por "NA", es mejor

lapply(credit, function(x) sum( x == "?"))  

str(credit)


# Caracteres a numeric ----------------------------------------------------

credit$class
credit$class = ifelse(credit$class == "+", 1, 0)                                # Cambiar el + por 1 ifelse 0
credit$class

str(credit$age)
credit$age = as.numeric(credit$age)                                             # Pasamos age a numeric
str(credit$age)

str(credit$mhexpense)
credit$mhexpense = as.numeric(credit$mhexpense)                                 # Pasamos mhexpense a numeric
str(credit$mhexpense)

str(credit$savingsbal)
credit$savingsbal = as.double(credit$savingsbal)                                # XgBoost no trabaja con ints, lo pasasmos a double
str(credit$savingsbal)

str(credit$twbank)
credit$twbank = as.double(credit$twbank)                                        # XgBoost no trabaja con ints, lo pasasmos a double
str(credit$twbank)


# Eliminar valores nulos --------------------------------------------------

complete.cases(credit)                                                          # True, fila es completa
credit = credit[complete.cases(credit), ]                                       # Que solo tome en cuenta las filas que estan comletas
complete.cases(credit)  

str(credit)


# Analisis exploratorio de los datos --------------------------------------     # Comentarios cuando queramos mejorar el modelo

table(credit$sex)
table(credit$homestat)                                                          # Vamos a borrar l, porque tiene 2
table(credit$othinv)
table(credit$occupation)                                                        # r bajo, eliminar
table(credit$jobstatus)                                                         # o y n bajos, eliminar
table(credit$accref)                                                            # Vamos a borrar p, también tiene 2
table(credit$bankacc)
table(credit$liabilityred)
table(credit$class)


# Resumir información numérica --------------------------------------------

data_numeric = Filter(is.numeric, credit)                                       # solo toma en cuenta las numeric
head(data_numeric)
str(data_numeric)

data_numeric = data_numeric[ , -7]                                              # Eliminamos la columna 7
str(data_numeric)

summary(data_numeric)


par(mfrow = c(2, 3))                                                            # Crea un plot de 2x3 gráficos
  
  for (i in colnames(data_numeric)) {
    hist(credit[ , i], main = i)}
  
  for (i in colnames(data_numeric)) {
    boxplot(credit[ , i], main = i)}

par(mfrow = c(1, 1))                                                            # Apagar esta cosa

library(PerformanceAnalytics)

chart.Correlation(data_numeric)


# Categorical to dummy y matrix data --------------------------------------

data_dummy = model.matrix(class ~ . -1, data = credit)                          # -1 porque eliminamos el intercepto
View(data_dummy)


# Split train-test --------------------------------------------------------

library(caret)                                                                  # Caret permite balance entre las clases, nuestra vab de respuesta

trainindex <- createDataPartition(credit$class, p = .80,
                                  list = FALSE, times = 1)                      # en la base de entrenamiento, class tiene un balance
head(trainindex)

data_train = data_dummy[trainindex, ]                                             # estas matrices no tienen la variable de respuesta
credit_train = credit$class[trainindex]

data_test = data_dummy[-trainindex, ]                                             # no me tomes en cuenta esas, por eso el .
credit_test = credit$class[-trainindex]

head(data_train)
head(data_test)


# XgBoost -----------------------------------------------------------------

library(xgboost)                                                                # es un arbol de decision

xgbst_error <- xgboost(data = data_train,                                       # base de datos
                       eta = 0.3,                                               # learning rate, (0,1)
                       gamma = 0,                                               # dividir nodo, 0 mas flexible
                       max.depth = 6,                                           # numero de ramas
                       min_child_weight = 1,                                    # peso necesario para que el nodo se divida
                       max_delta_step = 0,                                      # cuanto puede cambiar la prediccion en cada paso
                       subsample = 1,                                           # datos aleatorios que va a agarrar para predecir
                       sampling_method = "uniform",
                       lambda = 1,                                              # regularización de los pesos, mas grande - mas conservativo
                       alpha = 0,                                               # igual que lambda
                       tree_method = "auto",                                    # ver pagina
                       label = credit_train,                                    # variable que vamos a predecir
                       nrounds = 50,                                            # numero de pasos
                       objective = "binary:logistic",                           # queremos predecir eseto
                       eval.metric = 'error' ,                                  # se centra en los errores
                       verbose = 1)                                             # imprime mensaje de warning  

xgbst_error                                                                     # NO TIENE ERROR EN LA ETAPA 49
xgbst_error$evaluation_log       

pred <- predict(xgbst_error, data_train)                                        # modelo, datos que quiero predecir
pred                                                                            # prob que sea 1, que se le apruebe el crédito
print(length(pred))
print(head(pred))

prediction <- as.numeric(pred > 0.5)                                            # la clasificación de la función sigmoide
print(head(prediction))
prediction                                                                      # 1 se acepta, 0 no

caret::confusionMatrix(table(credit_train, prediction), positive=NULL)

# Predecir valores 

credit_pred <- predict(xgbst_error, data_test)                                  # predecir(modelo, data_test)
head(credit_pred)

credit_pred <- as.numeric(credit_pred > 0.5)
credit_pred

# Visualizar errores 

library(ggplot2)

ggplot(data = xgbst_error$evaluation_log) +
  geom_line(aes(x = iter, y = train_error), color = "red")                      # Como se iban viendo los errores


# Visualizar arboles 

library(DiagrammeR)

xgb.plot.tree(model = xgbst_error)

xgb.plot.tree(model = xgbst_error, trees = 0)                                   # Es el primer arbol creado
xgb.plot.tree(model = xgbst_error, trees = 49)                                  # Es el último arbol creado

# Evaluacion del modelo

xgbst_error_cv <- xgb.cv(data = data_train,                                     # base de datos
                         nfold = 10,
                         eta = 0.3,                                             # learning rate, (0,1)
                         gamma = 0,                                             # dividir nodo, 0 mas flexible
                         max.depth = 6,                                         # numero de ramas
                         min_child_weight = 1,                                  # peso necesario para que el nodo se divida
                         max_delta_step = 0,                                    # cuanto puede cambiar la prediccion en cada paso
                         subsample = 1,                                         # datos aleatorios que va a agarrar para predecir
                         sampling_method = "uniform",
                         lambda = 1,                                            # regularización de los pesos, mas grande - mas conservativo
                         alpha = 0,                                             # igual que lambda
                         tree_method = "auto",                                  # ver pagina
                         label = credit_train,                                  # variable que vamos a predecir
                         nrounds = 50,                                          # numero de pasos
                         objective = "binary:logistic",                         # queremos predecir eseto
                         eval.metric = 'error' ,                                # se centra en los errores
                         verbose = 1)                                           # imprime mensaje de warning  

xgbst_error_cv$evaluation_log                                                   # depues de los 0,0000 se sobreentreno el modelo

library(ggplot2)

ggplot(xgbst_error_cv$evaluation_log) + 
  geom_line(aes(iter, train_error_mean), color = "red") +
  geom_line(aes(iter, test_error_mean), color = "blue")                         # el modelo se sobreajusto

# Métricas

caret::confusionMatrix(table(credit_pred, credit_test), positive=NULL)

# Importancia de variables

importance <- xgb.importance(feature_names = colnames(data_train), 
                             model = xgbst_error) # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes


# logloss -----------------------------------------------------------------


xgbst_logloss <- xgboost(data = data_train,                                     # base de datos
                       eta = 0.3,                                               # learning rate, (0,1)
                       gamma = 0,                                               # dividir nodo, 0 mas flexible
                       max.depth = 6,                                           # numero de ramas
                       min_child_weight = 1,                                    # peso necesario para que el nodo se divida
                       max_delta_step = 0,                                      # cuanto puede cambiar la prediccion en cada paso
                       subsample = 1,                                           # datos aleatorios que va a agarrar para predecir
                       sampling_method = "uniform",
                       lambda = 1,                                              # regularización de los pesos, mas grande - mas conservativo
                       alpha = 0,                                               # igual que lambda
                       tree_method = "auto",                                    # ver pagina
                       label = credit_train,                                    # variable que vamos a predecir
                       nrounds = 50,                                            # numero de pasos
                       objective = "binary:logistic",                           # queremos predecir eseto
                       eval.metric = 'logloss' ,                                # metodo logloss
                       verbose = 1)                                             # imprime mensaje de warning  

xgbst_logloss
xgbst_logloss$evaluation_log       

pred <- predict(xgbst_logloss, data_train)                                      # modelo, datos que quiero predecir
pred                                                                            # prob que sea 1, que se le apruebe el crédito
print((length(pred)))

print(head(pred))

prediction <- as.numeric(pred > 0.5)                                            # la clasificación de la función sigmoide
print(head(prediction))
prediction                                                                      # 1 se acepta, 0 no

caret::confusionMatrix(table(credit_train, prediction), positive=NULL)

# Predecir valores 

credit_pred <- predict(xgbst_logloss, data_test)                                # predecir(modelo, data_test)
head(credit_pred)

credit_pred <- as.numeric(credit_pred > 0.5)
credit_pred

caret::confusionMatrix(table(credit_pred, credit_test), positive=NULL)

importance <- xgb.importance(feature_names = colnames(data_train),
                             model = xgbst_logloss)                             # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes

# Visualizar errores 

library(ggplot2)

ggplot(xgbst_logloss$evaluation_log) + 
  geom_line(aes(iter, train_logloss), color = "red")

# Visualizar arboles 

library(DiagrammeR)

xgb.plot.tree(model = xgbst_logloss)

xgb.plot.tree(model = xgbst_logloss, trees = 0)                                 # Es el primer arbol creado
xgb.plot.tree(model = xgbst_logloss, trees = 49)                                # Es el último arbol creado


# Evaluacion del modelo

xgbst_logloss_cv <- xgb.cv(data = data_train,                                   # base de datos
                         nfold = 10,
                         eta = 0.3,                                             # learning rate, (0,1)
                         gamma = 0,                                             # dividir nodo, 0 mas flexible
                         max.depth = 6,                                         # numero de ramas
                         min_child_weight = 1,                                  # peso necesario para que el nodo se divida
                         max_delta_step = 0,                                    # cuanto puede cambiar la prediccion en cada paso
                         subsample = 1,                                         # datos aleatorios que va a agarrar para predecir
                         sampling_method = "uniform",
                         lambda = 1,                                            # regularización de los pesos, mas grande - mas conservativo
                         alpha = 0,                                             # igual que lambda
                         tree_method = "auto",                                  # ver pagina
                         label = credit_train,                                  # variable que vamos a predecir
                         nrounds = 50,                                          # numero de pasos
                         objective = "binary:logistic",                         # queremos predecir eseto
                         eval.metric = 'logloss' ,                              # se centra en los errores
                         verbose = 1)                                           # imprime mensaje de warning  

xgbst_logloss_cv
xgbst_logloss_cv$evaluation_log                                                 # depues de los 0,0000 se sobreentreno el modelo

library(ggplot2)

ggplot(xgbst_logloss_cv$evaluation_log) + 
  geom_line(aes(iter, train_logloss_mean), color = "red") +
  geom_line(aes(iter, test_logloss_mean), color = "blue")                       # el modelo se sobreajusto

# Métricas

caret::confusionMatrix(table(credit_pred, credit_test), positive=NULL)

# Importancia de variables

importance <- xgb.importance(feature_names = colnames(data_train), 
                             model = xgbst_logloss)                             # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes


# Mejorar el modelo -------------------------------------------------------

xgbst_1 <- xgboost(data = data_train,
                     label = credit_train,
                     eval.metric = 'logloss', # modelo minimiza el error
                     objective = "binary:logistic",
                     nfold = 10, # conjuntos de cross validation
                     nrounds = 100, # 50 arboles que van mejorando
                     max.depth = 6, # 5 ramas
                     eta = 0.03, # que tanto aprende
                     nthread = 2,
                     subsample = 1,
                     colsample_bytree = 0.5, # meustras por arbol
                     lambda = 0.5, # eliminar variabels que no nos sirven
                     alpha = 0.5,
                     min_child_weight = 3 # peso para crar nuevos nodos
)                                           # imprime mensaje de warning  

xgbst_1
xgbst_1$evaluation_log       

pred <- predict(xgbst_1, data_train)                                      # modelo, datos que quiero predecir
pred                                                                            # prob que sea 1, que se le apruebe el crédito
print((length(pred)))

print(head(pred))

prediction <- as.numeric(pred > 0.5)                                            # la clasificación de la función sigmoide
print(head(prediction))
prediction                                                                      # 1 se acepta, 0 no

caret::confusionMatrix(table(credit_train, prediction), positive=NULL)

# Predecir valores 

credit_pred <- predict(xgbst_1, data_test)                                # predecir(modelo, data_test)
head(credit_pred)

credit_pred <- as.numeric(credit_pred > 0.5)
credit_pred

caret::confusionMatrix(table(credit_pred, credit_test), positive=NULL)

importance <- xgb.importance(feature_names = colnames(data_train),
                             model = xgbst_1)                             # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes

# Visualizar errores 

library(ggplot2)

ggplot(xgbst_1$evaluation_log) + 
  geom_line(aes(iter, train_logloss), color = "red")

# Visualizar arboles 

library(DiagrammeR)

xgb.plot.tree(model = xgbst_1)

xgb.plot.tree(model = xgbst_1, trees = 0)                                 # Es el primer arbol creado
xgb.plot.tree(model = xgbst_1, trees = 49)                                # Es el último arbol creado


# Evaluacion del modelo

xgbst_1_cv <- xgb.cv(data = data_train, # Cross validation
                     label = credit_train,
                     eval.metric = 'logloss', # modelo minimiza el error
                     objective = "binary:logistic",
                     nfold = 10, # conjuntos de cross validation
                     nrounds = 100, # 50 arboles que van mejorando
                     max.depth = 6, # 5 ramas
                     eta = 0.03, # que tanto aprende
                     nthread = 2,
                     subsample = 1,
                     colsample_bytree = 0.5, # meustras por arbol
                     lambda = 0.5, # eliminar variabels que no nos sirven
                     alpha = 0.5,
                     min_child_weight = 3 # peso para crar nuevos nodos
)

ggplot(xgb.modelo.1$evaluation_log) + 
  geom_line(aes(iter, train_logloss_mean), color = "red") +
  geom_line(aes(iter, test_logloss_mean), color = "blue") # el modelo se sobreajusto

# Métricas

caret::confusionMatrix(table(credit_pred, credit_test), positive=NULL)

# Importancia de variables

importance <- xgb.importance(feature_names = colnames(data_train), 
                             model = xgbst_logloss)                             # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes

