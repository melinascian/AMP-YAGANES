# ---- LOAD PACKAGES ----

library(dplyr)
library(igraph)


# ---- LOAD INTERACTIONS DATA ----

lista_interacciones <- read.csv("Datos/lista_interacciones_apr24.csv", header = T)

## Delete duplicates
lista_interaccionesok <- lista_interacciones %>% 
  group_by(Prey, Predator) %>% 
  filter(!duplicated(Prey, Predator))


# ----- CREATE IGRAPH OBJECT  -----

df <- lista_interaccionesok %>% 
  dplyr::select(Prey, Predator) %>% 
  distinct()

g <- graph_from_data_frame(df, directed=TRUE)

# Check number of network components
gcomp <- decompose(g, mode = "weak")
gok <- gcomp[[1]]


# ----- SAVE RESULTS  -----

save(lista_interaccionesok, gok, df,
     file = "Datos/datosdepurados_apr24.rda")
