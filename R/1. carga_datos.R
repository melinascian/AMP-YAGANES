## SETEO CARPETA DE TRABAJO ##
setwd("C:/AMP-YAGANES")


library(NetIndices)
library (multiweb)
library(dplyr)
library(igraph)


## CARGO DATOS ##
#lista_interacciones <- read.delim("Datos/lista_interacciones.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
lista_interacciones<-read.csv("Datos/lista_interacciones.csv", header = T)  # 'header = T' indica que la 1era fila corresponde a los nombres de las columnas 
lista_especies<-read.csv("Datos/lista_especies.csv", header = T)


## Elimino duplicados segun columna especie trofica
especies1 <- lista_especies %>% group_by(Especie.trófica) %>% filter (!duplicated(Especie.trófica))
especies1
save(especies1, file = "especies.rda")#para usar en Markdown

interacciones1 <- lista_interacciones %>% group_by(Presa,Depredador) %>% filter (!duplicated(Presa,Depredador))
interacciones1
# TIM: hay que eliminar las interacciones de baja resolución (*):
library(tidyverse)
interacciones_ok <- interacciones1 %>% 
  filter(!str_detect(Presa, "\\*$")) %>% 
  filter(!str_detect(Depredador, "\\*$")) %>% 
  distinct(Presa, Depredador, .keep_all = TRUE)
save(interacciones_ok, file="interacciones.rda") #para usar en Markdown
# TIM: podés guardar varios objetos R en el mismo .rda. save(especies1, interacciones1, file = "spint_tidy.rda")

## OBJETO G ##
#PRIMERO: Creo tabla con columna presa y predador nada mas, para crear el objeto g
#(a partir de la columna nº3, los toma como atributos de la interaccion,
#para agregar atributos: por ejemplo, cantidad de interacciones por especie).

df <- interacciones_ok %>% dplyr::select(Presa, Depredador) %>% distinct()
nrow(df)
df

#SEGUNDO: a partir de df, creo objeto g

g <- graph_from_data_frame(df, directed=TRUE)
as.matrix(df)
g
save(g, file="obejtog.rda")


# TERCERO: creo red de un solo componente
g <- decompose(g, mode = "weak")  #me dice que hay 4 componentes
g

g <- g[[1]] #permite seleccionar un item dentro de una lista
g #red con un solo componente, el mas grande

plot(g,
     vertex.color="coral",    # color de los nodos
     vertex.size=20,          # tamaño de los nodos
     edge.color="black")      # color de las aristas


#save(objeto R, file = "nombre.rda") #generar archivo rda
load("nombre.rda") #para cargar objeto rda, esto lo uso en el markdown
