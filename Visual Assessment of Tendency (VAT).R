library(mclust)
library(factoextra)
library(ggpubr)
library(hopkins)
library(purrr)

#El set de datos banknote correspondiente al paquete de R mclust , contiene seis mediciones
#realizadas en 100 billetes de 1000 francos suizos auténticos y 100 falsos.
#• Status: genuine (verdadero), counterfeit(falso)
#• Length: longitud del billete (mm)
#• Left : Ancho del borde Izquierdo (mm)
#• Right : Ancho del borde derecho (mm)
#• Bottom : Ancho del m á rgen inferior (mm)
#• Top : Ancho del márgen superior (mm)
#• Diagonal: Longitud diagonal (mm) Los investigadores desean determinar si utilizando un método de conglomerados basados
#en estas longitudes, pueden detectar el status de los billetes , es decir, determinar la
#autenticidad de los mismos . En lo que sigue, se pide realizar una serie de pasos para cumplir
#dicho objetivo.


#Generar un data frame con los datos del problema y transformar la variable status en factor.
bank <- banknote

bank$Status <- as.factor(bank$Status)

#Generar un data frame que excluya la variable status. Este data frame será el que se analizará.
bank <- bank[,-1] #elimino la colmuna 1 


#Generar un data.frame con las variables estandarizadas
bank.e <- scale(bank)


## Datos simulados ##
datos_simulados <- map_df(bank,
                          .f = function(x){runif(n = length(x),
                                                 min = min(x),
                                                 max = max(x))})
datos_simulados <- scale(datos_simulados)


#Determinar si la tabla de datos disponible admite la búsqueda de conglomerados.

#Hopkins
hopkins(bank.e, m=15) 
#0.99 indica una muestra con conglomerados detectables.


#Visual Assessment of Tendency (VAT)
#calculamos las distancias euclídeas
dist_bank <- dist(bank.e, method = "euclidean")
dist_datos_simulados <- dist(datos_simulados, method = "euclidean")

#gráfico de las distancias euclídeas de los datos
p1 <- fviz_dist(dist.obj = dist_bank, show_labels = FALSE) +
  labs(title = "Datos Bank") + theme(legend.position = "bottom")

#gráfico de las distancias euclídeas de los datos simulados
p2 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
  labs(title = "Datos simulados") + theme(legend.position = "bottom")

ggarrange(p1, p2)
#Cuando se comparan los datos reales con los datos simulados, se puede ver claramente
#la diferencia en la distribución de colores. En el gráfico de los datos reales, los colores
#se concentran en grupos, mientras que los colores en el gráfico de datos simulados
#no muestran ningún patrón específico.