load("Datos/analisisdatos.rda")

library(ggplot2)

##TABLAS
# Manuscrito
#Tabla propiedades de complejidad + mundo pequeño:
# Columnas: Species, Interactions, Linkage density (LD), Connectance, Omnivory (Omn), 
# Path length, Clustering coefficient (Clus. coef.), TL mean, Small world (SW)
library(tidyverse)
smallworld<-sample(c("TRUE"))
propcomplejidad<-cbind(propcomplejidad, smallworld)
borrar<-c("Top","Basal","TLmax","LOmnivory","Components","Vulnerability","VulSD","Generality","GenSD", "Cannib")
Tabla1<- propcomplejidad[ , !(names(propcomplejidad) %in% borrar)]
Tabla1ok<-Tabla1 %>% rename (Species = Size, Interactions=Links, Omn=Omnivory, Clus.coef=Clustering, SW=smallworld)
names(Tabla1ok)
Tabla1final = Tabla1ok [ , c(1,3,4,5,2,8,6,7,9)]



#LISTA DE INTERACCIONES (material suplementario) 
#Columnas = Presa, Depredador, Referencia, Enlace
borrar_int<-c("Grupo.funcional.presa","Expertise.presa","Grupo.funcional.depredador","Expertise.depredador","Estrategia","Fuente", "Confirmado.por.profesional")
listadeinteracciones<-interacciones_ok[ , !(names(interacciones_ok) %in% borrar_int)]

#LISTA DE ESPECOIES (material suplementario)
#Columnas = Especie trofica, Resolucion, referencia, enlace
borrar_sp<-c("Filum", "Clase", "Orden", "Familia","Género","Nombre.común","Hábitat","Dominio.CB","Dominio.CCA","Dominio.CCH","Expertise")
listadeespecies<-especies1[ , !(names(especies1) %in% borrar_sp)]
Listofspecies<-listadeespecies %>% rename (Species = Especie.trófica, Reference=Referencia, Link=Enlace)


#Tabla mundo pequeño: Material Suplementario
rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(propcomplejidad$Size, propcomplejidad$Links, directed = TRUE)#defino redes aleatorias que voy a generar
  
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(propcomplejidad$Size, propcomplejidad$Links, type = "gnm", 
                          directed = TRUE)
  return(e) 
})
mundopeque<-multiweb::calc_swness_zscore(gok, nullDist = rnd_g, weights = NA, ncores = 4)
datosmundopeque<-as.data.frame(mundopeque["da"])
tablamundopeque<-datosmundopeque %>% rename (Clustering = da.Clustering,PathLength = da.PathLength, zCC=da.zCC, zCP=da.zCP, CClow=da.CClow,CChigh=da.CChigh, CPlow=da.CPlow, CPhigh=da.CPhigh, SWness=da.SWness, SWnessCI=da.SWnessCI, isSW=da.isSW, isSWness=da.isSWness)


#TABLA PROPIEDADES DE ESTRUCTURA + NIVEL TRÓFICO (material suplementario)
ID<-sample(1:204,size=204,replace=TRUE)
propestructura<-data.frame(ID,niveles_troficos)
propestructura_tot <- cbind(coeficientes_centralidad,niveles_troficos)

#INDICE DE ESPECIES CLAVE
spclave<-propestructura_tot %>% mutate(indice_spclave=(degree.total+closeness+betweeness/3))
spclave


#TABLA COMPARATIVA ENTRE REDES (material suplementario)
#Cargo redes:
red_beaglechannel <- read.csv("Datos/BeagleChannel_links_original.csv")
red_burdwoodbank <-read.csv("Datos/BurdwoodBank_links_original.csv")
red_northernscotia<-read.csv("Datos/NorthernScotia_links_original.csv")
red_pottercove<-read.csv("Datos/PotterCove_links_original.csv")
red_southernscotia<-read.csv("Datos/SouthernScotia_links_original.csv")                                

#creo objetos g

g_beagle <- graph_from_data_frame(red_beaglechannel, directed=TRUE)
g_burdwood<-graph_from_data_frame(red_burdwoodbank, directed=TRUE)
g_northernscotia<-graph_from_data_frame(red_northernscotia, directed=TRUE)
g_pottercove<-graph_from_data_frame(red_pottercove, directed=TRUE)
g_southernscotia<-graph_from_data_frame(red_southernscotia, directed=TRUE)

#calculo los indices:
beagle_propcomplej<-calc_topological_indices(g_beagle)
burdwood_propcomplej<-calc_topological_indices(g_burdwood)
northernscotia_propcomplej<-calc_topological_indices(g_northernscotia)
pottercove_propcomplej<-calc_topological_indices(g_pottercove)
southernscotia_propcomplej<-calc_topological_indices(g_southernscotia)

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

#Histograma
par(mfrow = c(1,1))
hist(degree(gok,mode="all"))
grado<-as.data.frame(degree(gok))
pl<-ggplot(grado,aes(x=degree(gok)))
pl + geom_histogram()
pl2 <- pl + geom_histogram(binwidth = 1,col='black', fill='black', alpha=0.6)
#binwidth = ancho de la barra
#alpha = transparencia del grafico

pl2
pl2 + xlab('Grado') + ylab('Frecuencia') + ggtitle('Distribucion de grado')

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


