## SETEO CARPETA DE TRABAJO ##
setwd("C:/AMPYAGANES/Datos")

## INSTALO PAQUETES ##
install.packages("pillar")
install.packages("dplyr")
install.packages("igraph") #IGRAPH (Tutorial paquete IGRAPH: igraph.org/R
install.packages("devtools") #esto me permite instalar paquetes desde GitHub
#hay algunos paquetes que no estan en la libreria oficial de R.
# TIM: es la librería base en realidad no la oficial, todos los paquetes que descargas de CRAN son oficiales.
require(devtools)
install_github("lsaravia/multiweb")
#Para Markdown
install.packages("rticles")  # TIM: no hace falta que instales esto ahora.
#Para niveles troficos
install.packages("NetIndices")
library(NetIndices)
library (multiweb)
library(dplyr)
library(igraph)
library(multiweb)  # TIM: repetido en línea 17


## CARGO DATOS ##
lista_interacciones <- read.delim("lista_interacciones.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
lista_interacciones<-read.csv("lista_interacciones.csv")
lista_especies<-read.csv("lista_especies.csv")


## Elimino duplicados segun columna especie trofica
especies1<- lista_especies%>% group_by(Especie.trófica) %>% filter (! duplicated(Especie.trófica))
especies1
save(especies1, file = "especies.rda")#para usar en Markdown

interacciones1<-lista_interacciones%>% group_by(Presa,Depredador) %>% filter (! duplicated(Presa,Depredador))
interacciones1
save(interacciones1, file="interacciones.rda") #para usar en Markdown


## OBJETO G ##
#PRIMERO: Creo tabla con columna presa y predador nada mas, para crear el objeto g
#(a partir de la columna nº3, los toma como atributos de la interaccion,
#para agregar atributos: por ejemplo, cantidad de interacciones por especie).

df<-interacciones1 %>% dplyr::select(Presa, Depredador)%>% distinct()
nrow(df)
df

#SEGUNDO: a partir de df, creo objeto g

g<-graph_from_data_frame(df,directed=TRUE)
as.matrix(df)
g
save(g, file="obejtog.rda")


# TERCERO: creo red de un solo componente
g<- decompose(g, mode = "weak")  #me dice que hay 4 componentes
g

g<-g[[1]] #permite seleccionar un item dentro de una lista
g #red con un solo componente, el mas grande

plot(g,
     vertex.color="coral",    # color de los nodos
     vertex.size=20,          # tamaño de los nodos
     edge.color="black")      # color de las aristas


#save(objeto R, file = "nombre.rda") #generar archivo rda
load("nombre.rda") #para cargar objeto rda, esto lo uso en el markdown

