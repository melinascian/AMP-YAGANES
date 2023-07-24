

library(dplyr)
library(igraph)


## CARGO DATOS ##
#lista_interacciones <- read.delim("Datos/lista_interacciones.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
lista_interacciones<-read.csv("Datos/lista_interacciones.csv", header = T)  # 'header = T' indica que la 1era fila corresponde a los nombres de las columnas 
lista_especies<-read.csv("Datos/lista_especies.csv", header = T)


## Elimino duplicados segun columna especie trofica
especies1 <- lista_especies %>% group_by(Especie.trófica) %>% filter (!duplicated(Especie.trófica))
especies1


interacciones1 <- lista_interacciones %>% group_by(Presa,Depredador) %>% filter (!duplicated(Presa,Depredador))
interacciones1

# TIM: hay que eliminar las interacciones de baja resolución (*):
library(tidyverse)
interacciones_ok <- interacciones1 %>% 
  filter(!str_detect(Presa, "\\*$")) %>% 
  filter(!str_detect(Depredador, "\\*$")) %>% 
  distinct(Presa, Depredador, .keep_all = TRUE)

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
g


# TERCERO: creo red de un solo componente
gcomp <- decompose(g, mode = "weak")  #me dice que hay 4 componentes
gcomp

gok<- gcomp[[1]] #permite seleccionar un item dentro de una lista
gok #red con un solo componente, el mas grande



save(interacciones1, interacciones_ok, gok, df,  file = "Datos/datosdepurados.rda") #generar archivo rda

