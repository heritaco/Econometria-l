
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

table(credit$lomismo, credit$homestat)                                                        # Como es la identidad, son iguales, eliminamos una

credit <- credit[ , -5]                                                         # Eliminar columnas
head(credit)


# Null values -------------------------------------------------------------

lapply(credit, function(x) sum(is.na(x)))                                       # Sumar los valores nulos, donde la celda es vacia 
lapply(credit, function(x) sum( x == "?"))                                      # Cadafila(basededatos, suma( las celdas que son ?))

credit[credit == "?"] <- NA                                                     # Reemplaza las celdas que tienen "?" por "NA", es mejor

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

x = Filter(is.numeric, credit)                                                  # solo toma en cuenta las numeric
head(x)
str(x)

x = x[ , -7]                                                                    # Eliminamos la columna 7
str(x)

summary(x)


par(mfrow = c(2, 3))                                                            # Crea un plot de 2x3 gráficos
  
  for (i in colnames(x)) {
    hist(credit[ , i], main = i)}
  
  for (i in colnames(x)) {
    boxplot(credit[ , i], main = i)}

par(mfrow = c(1, 1))                                                            # Apagar esta cosa


# Categorical to dummy y matrix data --------------------------------------

mimatriz = model.matrix(class ~ . -1, data = credit)                            # -1 porque eliminamos el intercepto
View(mimatriz)


# Split train-test --------------------------------------------------------

library(caret)                                                                  # Caret permite balance entre las clases, nuestra vab de respuesta

trainindex <- createDataPartition(credit$class, p = .80,
                                  list = FALSE, times = 1)                      # en la base de entrenamiento, class tiene un balance
head(trainindex)

mimatriz_train = mimatriz[trainindex, ]                                         # estas matrices no tienen la variable de respuesta
y_train = credit$class[trainindex]

mimatriz_test = mimatriz[-trainindex, ]                                         # no me tomes en cuenta esas, por eso el .
y_test = credit$class[-trainindex]

head(mimatriz_train)
head(mimatriz_test)


# XgBoost -----------------------------------------------------------------

library(xgboost)                                                                # es un arbol de decision

bst <- xgboost(data = mimatriz_train, max.depth = 6, eta = 0.3, nthread = 2,
               label = y_train, nrounds = 50, objective = "binary:logistic",
               eval.metric = 'error' , verbose = 1)                             # Ver help
bst                                                                             # NO TIENE ERROR EN LA ETAPA 49

pred <- predict(bst, mimatriz_train)                                            # modelo, datos que quiero predecir
pred                                                                            # prob que sea 1, que se le apruebe el crédito
print((length(pred)))

print(head(pred))

prediction <- as.numeric(pred > 0.5)                                            # la clasificación de la función sigmoide
print(head(prediction))
prediction                                                                      # 1 se acepta, 0 no

table(y_train, prediction)                                                      # Todos los predijo bien?!?!??! lol? habia error 0

accuracy_train <- sum(y_train == prediction) / length(y_train) * 100
accuracy_train # lol

# Predecir valores --------------------------------------------------------

y_pred <- predict(bst, mimatriz_test)                                           # predecir(modelo, data_test)
head(y_pred)

y_pred <- as.numeric(y_pred > 0.5)
y_pred

table(y_test, y_pred)                                                           # Matriz de confusion del data_test

accuracy_test <- sum(y_test == y_pred) / length(y_test) * 100
accuracy_test # wow

err <- mean( y_pred != y_test)
print(paste("test-error", err*100))                                             # Los errores que tuvimos

importance <- xgb.importance(feature_names = colnames(mimatriz_train), 
                             model = bst)                                       # Vamos a ver cuales son las variables importantes
head(importance, n = 10)                                                        # la mas importante es other investments, Gain
xgb.plot.importance(importance_matrix = importance)                             # Gráfico de las varibles más importantes

bst$evaluation_log                                                              # Nos muestra los errores


# Métricas  ---------------------------------------------------------------

caret::confusionMatrix(table(y_pred, y_test), positive=NULL)


# Visualizacion de resultados ---------------------------------------------

importance <- xgb.importance(feature_names = colnames(mimatriz_train), model = bst)
head(importance, n = 10)

xgb.plot.importance(importance_matrix = importance)

bst$evaluation_log


# Visualizar errores ------------------------------------------------------

library(ggplot2)

ggplot(data = bst$evaluation_log) +
  geom_line(aes(x = iter, y = train_error), color = "red")                      # Como se iban viendo los errores


# Visualize trees ---------------------------------------------------------

library(DiagrammeR)

xgb.plot.tree(model = bst)

xgb.plot.tree(model = bst, trees = 0)                                           # Es el primer arbol creado
xgb.plot.tree(model = bst, trees = 49)                                          # Es el último arbol creado

# logloss -----------------------------------------------------------------

bst.logloss <- xgboost(data = mimatriz_train, max.depth = 6, eta = 0.3, nthread = 2,
                       label = y_train, nrounds = 50, objective = "binary:logistic",
                       eval.metric = 'logloss' , verbose = 1)  

bst.logloss$evaluation_log

ggplot(bst.logloss$evaluation_log) + 
  geom_line(aes(iter, train_logloss), color = "red")

caret::confusionMatrix(table(bst.logloss, y_test), positive=NULL)

ConfusionMatrix(y_pred = bst.logloss, y_true = y_train)

class(bst.logloss)
class(y_test)


# mejorar modelo ----------------------------------------------------------

xgb.fit1 <- xgb.cv(data = mimatriz_train, # Cross validation
                   label = y_train,
                   nrounds = 50, # 50 arboles que van mejorando
                   eval.metric = "error", # vamos a medir el error, como podemos predecir
                   max.depth = 6, # 5 ramas
                   eta = 0.3, # que tanto aprende
                   nfold = 5, # conjuntos de cross validation
                   objective = "binary:logistic",
                   verbose = 0)

xgb.fit1$evaluation_log # depues de los 0,0000 se sobreentreno el modelo


library(ggplot2)

ggplot(xgb.fit1$evaluation_log) + 
  geom_line(aes(iter, train_error_mean), color = "red") +
  geom_line(aes(iter, test_error_mean), color = "blue") # el modelo se sobreajusto

bst <- xgboost(data = mimatriz_train, max.depth = 6, eta = 0.3, nthread = 2,
               label = y_train, nrounds = 50, objective = "binary:logistic",
               eval.metric = 'error' , verbose = 1)  
bst


# predecir valores en test ------------------------------------------------

library(MLmetrics)

y_pred_f1 <- predict(bst, mimatriz_test)
y_pred_f1 <- as.numeric(y_pred > 0.5)

caret::confusionMatrix(table(y_pred_f1, y_test), positive=NULL)


# modelo 2 ----------------------------------------------------------------


xgb.modelo.1 <- xgb.cv(data = mimatriz_train, # Cross validation
                   label = y_train,
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

xgb.modelo.1$evaluation_log


caret::confusionMatrix(table(xgb.modelo.1, y_test), positive=NULL)

ggplot(xgb.modelo.1$evaluation_log) + 
  geom_line(aes(iter, train_logloss_mean), color = "red") +
  geom_line(aes(iter, test_logloss_mean), color = "blue") # el modelo se sobreajusto





# performance model 2 -----------------------------------------------------

y_pred_f1.2 <- predict(bst.logloss, mimatriz_test)
y_pred_f1.2 <- as.numeric(y_pred > 0.5)

caret::confusionMatrix(table(y_pred_f1.2, y_test), positive=NULL)


# modelo 3 ----------------------------------------------------------------


xgb.modelo.2 <- xgb.cv(data = mimatriz_train, # Cross validation
                       label = y_train,
                       eval.metric = 'logloss', # modelo minimiza el error
                       objective = "binary:logistic",
                       nfold = 10, # conjuntos de cross validation
                       nrounds = 100, # 50 arboles que van mejorando
                       eval.metric = "error", # vamos a medir el error, como podemos predecir
                       max.depth = 6, # 5 ramas
                       eta = 0.05, # que tanto aprende
                       nthread = 2,
                       subsample = 1,
                       colsample_bytree = 0.5, # meustras por arbol
                       lambda = 0.3, # eliminar variabels que no nos sirven
                       alpha = 0.5,
                       min_child_weight = 3 # peso para crar nuevos nodos
                       )


ggplot(xgb.modelo.2$evaluation_log) + 
  geom_line(aes(iter, train_logloss_mean), color = "red") +
  geom_line(aes(iter, test_logloss_mean), color = "blue") # el modelo se sobreajusto


