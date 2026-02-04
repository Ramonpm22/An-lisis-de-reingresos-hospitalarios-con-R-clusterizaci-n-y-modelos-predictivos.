
#PROYECTO: PREDICCIÓN DE REINGRESO EN UN HOSPITAL
#Datos: ficticios
#Objetivo: Agrupar los pacientes por grupos, y crear un modelo
#adecuado para la predicción de reingreso de un paciente
#hasta pasado 30 días

# Librerias
library(readxl)
library(dplyr)
library(factoextra)
library(forecast)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
# Carga de los datos (son datos ficticios)
datos<- read_excel("C:/Users/pasto/OneDrive/Escritorio/Análisis salud/Análisis salud.xlsx") #poner la ruta en la que guardes el archivo excel

# Tratamiento y depuración de los datos

str(datos) # ver los tipos de datos

colSums(is.na(datos)) #no hay NAs

datos$días_ingreso<- as.numeric(datos$Fecha_Alta-datos$Fecha_Ingreso) # para poder saber los días que permanece ingresado

# Estadística descriptiva

summary(datos) #nos permite conocer mucho más las variables, a priori, no hay nigún outlier

boxplot(datos$Coste_Ingreso) # no hay outlier
boxplot(datos$días_ingreso) #no hay outlier

table(datos$Estado_Alta, datos$Tipo_Seguro) # En este dataset se observa un patrón donde los 
#pacientes del sistema público presentan estados de alta menos 
#favorables y mayor tasa de reingreso. Esto podría sugerir, 
#en un caso real, diferencias en protocolos, presión asistencial 
#o recursos disponibles.

table(datos$Tipo_Seguro, datos$Reingreso_30_dias) #Ninguno de la privada vuelve a estar
#ingresado al cabo de 30 días, esto tiene sentido, ya que como hemos
#podido ver antes, en la privada esperan a que el esta de alta sea 'recuperado'

# Clusterización

datos_clust<- datos %>% select(Edad, Coste_Ingreso, días_ingreso) #nos quedamos con las variables numéricas

fviz_nbclust(datos_clust, kmeans) #recomienda 2,4 o 7 clusteres, nos quedamos con 4

k4<- kmeans(datos_clust, center=4, iter.max = 20)
k4$size #tamaño de cada cluster, el 3ro es el más numeroso, con 150 obs

fviz_cluster(k4, datos_clust, geom='point') #representación gráfica de los clusteres

datos_clust$clust<- k4$cluster
datos_clust_conclusiones<- datos_clust %>% group_by(clust) %>%
                    summarise(edad_media=mean(Edad), coste_medio=mean(Coste_Ingreso),
                              días_ingreso_medio=mean(días_ingreso)) #El cluster 2 es el menos rentable, se ve que cuando mas edad se tenga, mas días 
datos$clust<- k4$cluster                                                                   # se está de media ingresado, y por ende, más coste repercute, el cluster 3 es el más rentable
# Regresión logística(para saber si una persona reingresará a los 30 días o no)

datos$Reingreso_30_dias<- ifelse(datos$Reingreso_30_dias=='Sí',1,0) #necesitamos que la variable explicada sea binaria

set.seed(123)
n<- nrow(datos)

indice<- sample(1:n,0.8*n)
train<- datos[indice,]
test<- datos[-indice,] #vamos a entrenar primero al modelo

modelo_log<- glm(Reingreso_30_dias~Edad+Causa_Ingreso+
                   Tipo_Seguro+ Tipo_Ingreso+Estado_Alta+días_ingreso,train, family = 'binomial')
summary(modelo_log) # Edad y causa de ingreso (Traumatología) son las más significativas

test$pred_log<- predict(modelo_log, newdata = test, type = 'response')
test$class_log<- ifelse(test$pred_log>=0.239, 1, 0 )

confusionMatrix(as.factor(test$class_log), as.factor(test$Reingreso_30_dias), positive='1') #buen modelo, 0.88 de accuracy
# 1 se sensitividad, lo que indica que acierta todos los pacientes que volveran a estar ingresados, la especificidad es más bajas pero
#se mantiene a un buen nivel, que es 0.85

roc_obj_log<- roc(test$Reingreso_30_dias, test$pred_log)
auc(roc_obj_log) #muy buen auc de 0.95
plot(roc_obj_log)
coords(roc_obj_log, 'best') # el punto de corte óptimo es 0.239

# Árbol de decisión (a partir del clustering)

modelo_clust<- rpart(clust~Edad+Causa_Ingreso+
                   Tipo_Seguro+ Tipo_Ingreso+Estado_Alta+días_ingreso+ Reingreso_30_dias,train, method = 'class')
summary(modelo_clust) # Las variables más importantes y definitorias son: 
#Causa de ingreso, Días de ingreso y Edad
rpart.plot(modelo_clust, fallen.leaves = TRUE)
# El grupo tres, que es el más joven, representa todos los casos de ingreso por 
# infecciones y obstetricia.
# A raiz del árbol, se saca la conclusión, de que el grupo 2, es el que más posibilidad tiene de 
#volver a estar ingresado. 
#El paciente con mayor probabilidad de reingreso es aquel ingresado por Cardiología o Cirugía programada, 
#perteneciente al cluster 2, caracterizado por mayor edad, 
#estancias más largas y mayor coste asociado.  
#Este grupo representa un perfil clínico más complejo y vulnerable



#Árbol de decisión (Con la variable explicada de reingreso)
modelo_arbol<- rpart(Reingreso_30_dias~Edad+Causa_Ingreso+
                       Tipo_Seguro+ Tipo_Ingreso+Estado_Alta+días_ingreso+ Reingreso_30_dias,train, method = 'class')
summary(modelo_arbol) # Las variables más importantes y definitorias son: 
#Causa de ingreso, Días de ingreso y Edad

rpart.plot(modelo_arbol, fallen.leaves = TRUE) # El árbol identifica la edad como la variable 
#más discriminante. En particular, los pacientes de 63 años o más 
#forman un nodo donde el 84% de los casos corresponden a reingresos, 
#lo que indica un perfil de riesgo elevado dentro del dataset.

test$pred_arbol<- predict(modelo_arbol, newdata = test, type = 'prob')[,2]
test$class_arbol<- ifelse(test$pred_arbol>=0.446, 1, 0 )

confusionMatrix(as.factor(test$class_arbol), as.factor(test$Reingreso_30_dias), positive='1') #buen modelo, 0.91 de accuracy
# 0.83 se sensitividad, lo que indica que acierta la mayoria de los pacientes que volveran a estar ingresados, la especificidad es más alta en este caso 
#con un 0.92 de acierto en los casos negativos

roc_obj_arbol<- roc(test$Reingreso_30_dias, test$pred_arbol)
auc(roc_obj_arbol) #muy buen auc de 0.88
plot(roc_obj_arbol)
coords(roc_obj_arbol, 'best') # el punto de corte óptimo es 0.446


# Para predecir el reingreso, consideramos que el mejor modelo es el logit,
# ya que aunque tenga un accuracy más bajo que el modelo del arbol de decisión,
# valoramos que el modelo logit acierte al 100 el caso de las personas que volveran a estar ingresadas, al 
# tener una sensitividad de 1. Destacar que ambos modelo presentan calidad predictoria.