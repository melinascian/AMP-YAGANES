# ---- CARGA DE DATOS ----

# Load data
lista_interacciones <- read.csv("Datos/lista_interacciones.csv", header = T)  # 'header = T' indica que la 1era fila corresponde a los nombres de las columnas 
lista_especies <- read.csv("Datos/lista_especies.csv", header = T)

# Load packages
library(dplyr)
library(igraph)


## Elimino duplicados segun columna especie trofica
lista_interaccionesok <- lista_interacciones %>% 
  group_by(Prey,Predator) %>% 
  filter(!duplicated(Prey,Predator))


# TIM: hay que eliminar las interacciones de baja resolución (*):
#library(tidyverse)
#interacciones_ok <- interacciones1 %>% 
#filter(!str_detect(Presa, "\\*$")) %>% 
#filter(!str_detect(Depredador, "\\*$")) %>% 
#distinct(Presa, Depredador, .keep_all = TRUE)

# ----- OBJETO G  -----
# PASO 1: Creo tabla con columna presa y predador nada mas.
# Para crear el objeto g (a partir de la columna nº3, los toma como atributos de la interaccion,
# para agregar atributos: por ejemplo, cantidad de interacciones por especie).

df <- lista_interaccionesok %>% 
  dplyr::select(Prey, Predator) %>% 
  distinct()

# PASO 2: a partir de df, creo objeto g

g <- graph_from_data_frame(df, directed=TRUE)

# PASO 3: creo red de un solo componente
gcomp <- decompose(g, mode = "weak")  # me dice que hay 2 componentes

gok<- gcomp[[1]]  # permite seleccionar un item dentro de una lista
gok  # red con un solo componente, el mas grande

# Save results
save(lista_interaccionesok, gok, df,
     file = "Datos/datosdepurados.rda")  # generar archivo rda
