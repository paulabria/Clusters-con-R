library(tidyverse)
library(rstatix)

library(readxl)
clientes <- read_excel("clientes (1).xlsx")
View(clientes)


clientes <- clientes %>% remove_rownames %>% column_to_rownames(var="ID")  # Con esta linea podemos nombrar cada fila con el modelo del auto


summary(clientes)

# distribución de las variables
par(mfrow = c(1, 2)) #para emparejar gráficos. Uno al lado del otro.
hist(clientes$INGRESOS,breaks="Sturges",labels=TRUE, main = "Ingresos", xlab = "Euros", ylab="Frecuencias",col = "gold",
     border="tomato1") #para ver si la distribución es simétrica o asimétrica (tiene que ser simétrica para esto)
hist(clientes$EDAD,col = "gold",
     border="tomato1", main = "Edad",labels=TRUE, xlab = "Años",ylab="Frecuencias")
#ingreso es asimétrica. Se puede volver simétrica para usarla como insumo.

# buscando observaciones atípicas - outliers. Es necesario que no haya.
par(mfrow = c(1, 2))
boxplot(clientes$INGRESOS,border="steelblue", col="gold",main = "Ingresos",ylab="Euros")
boxplot(clientes$EDAD,col="gold",border="steelblue",main = "Edad",ylab="Años")
#no hay outliers en estas variables

# standarización de las variables - para que tenga media 0 y desvío 1
Zclientes <- scale(clientes)
library(scales)

# gráfico de dispersión
par(mfrow = c(1, 1))
plot(Zclientes, col = alpha("steelblue", 0.4), pch = 19, las = 1) #parece haber tres grupos
text(Zclientes, rownames(Zclientes), pos = 3, cex = .6)

### clusters jerárquicos ####

d <- dist(Zclientes, method = "euclidean") #una para calcular las distancias euclídeas. Da las distancias entre los individuos

fit <- hclust(d, method="ward.D") #con el insumo "d" se puede hacer un análisis de clusters jerárquicos.
#Utiliza el método Dward para medir las distancias y armar los clusters

#dendrograma
plot(fit, cex = .6, xlab = "", ylab = "Distancia entre grupos", sub = "Cluster jerárquico por el método de Ward para los clientes") 
Numgrupos <- 3
library(RColorBrewer)
plot(fit, cex = .6, xlab = "", ylab = "Distancia entre grupos", sub = "Cluster jerárquico por el método de Ward para los clientes") 
rect.hclust(fit, k = 3, border = brewer.pal(Numgrupos, "Dark2"))


# partición en conglomerados
#una vez que mirando el gráfico anterior me di cuenta de que puedo generar tres grupos o clusters.
#puedo asignar un grupo de pertenencia a cada uno de los individuos
grupos <- cutree(fit, k = 3) #corta el árbol con k (cant de grupos) igual a 3
grupos

clientes$GRUPO <- factor(grupos) #le puse al df una columna nueva que dice a qué grupo pertenece cada individuo (1,2 o 3)

palette(brewer.pal(Numgrupos, "Dark2"))
plot(INGRESOS ~ EDAD, col = alpha(GRUPO, 0.75), pch = 19, data = clientes, las = 1) 
text(clientes, rownames(clientes), pos = 3, cex = .6)
#gráfico en el que veo los tres gruposal cruzar datos con ingresos, por color.

#cluster NO jerarquico ####
Data.km <- kmeans(Zclientes, 3) #(Zclientes, Numgrupos)
Data.km

clientes %>%
  group_by(GRUPO) %>%
  get_summary_stats(type="mean_sd") #media y desvío en cada uno de los grupos

clientes %>% anova_test(EDAD~GRUPO) #hay diferencia en el promedio de edad

clientes %>% anova_test(INGRESOS~GRUPO) #hay diferencia en el promedio de ingresos.
#por lo que la definición de los grupos podría depender de ambas variables!
