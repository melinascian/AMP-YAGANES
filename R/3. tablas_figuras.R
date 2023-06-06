load("Datos/analisisdatos.rda")

##TABLAS
# Manuscrito
#Tabla propiedades de complejidad + mundo pequeño:
# Columnas: Species, Interactions, Linkage density (LD), Connectance, Omnivory (Omn), 
# Path length, Clustering coefficient (Clus. coef.), TL mean, Small world (SW)
library(tidyverse)
smallworld<-sample(c("TRUE"))
propcomplejidad<-cbind(propcomplejidad, smallworld) %>% 
  select(Size, Top) %>% 
  rename(Species=Size)

#Tabla lista de interacciones: Material Suplementario
#Tabla lista de especies: Material Suplementario
#Tabla mundo pequeño: Material Suplementario
#Tabla propiedades de estructura + nivel trofico: Material Suplementario
ID<-sample(1:192,size=192,replace=TRUE)
propestructura<-data.frame(ID,niveles_troficos)
propestructura_tot <- cbind(coeficientes_centralidad,niveles_troficos)

## GRAFICOS DE LA RED ##
## GRAFICO SEGUN NIVELES TROFICOS ##
# Gráfico comparativo índices de centralidad
library(multiweb)
par(mfrow = c(1,3))
set.seed(1)  # mantiene la posición de las especies
plot_troph_level(gok, vertex.size=sqrt(V(gok)$degree.total), ylab = "Trophic level", main = "Degree")
set.seed(1)
plot_troph_level(gok, vertex.size=sqrt(V(gok)$betweeness), main = "Betweenness")
set.seed(1)
plot_troph_level(gok, vertex.size=10^3*V(gok)$closeness,main = "Closeness")
#plot_troph_level(gok)#si no mantengo posicion de las especies se grafica siempre un grafico distinto

# Incluir en el manuscrito y material suplementario
# TABLAS
# Manuscrito:
# 1. Propiedades de complejidad y estructura de la red
# Material Suplementario:
# 1. Tabla lista de interacciones
# 2. Tabla lista de especies
# 3. Tabla mundo pequeño
# 4.Tabla propiedades de estructura + nivel trofico
# GRAFICOS
# 1. Histograma (ggplot2)
# 2. Graficos índices de centralidad (3): degree, closeness, betweenness


