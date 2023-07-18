# Ajuste distribución de grado

library(NetworkExtinction)
library(igraph)
library(network)

# Cargar datos
load("Datos/datosdepurados.rda")
upgrade_graph(gok)  # actualiza objeto igraph

# Ajustar distribución de grado
g_net <- as.network(as.matrix(gok))
dist_fit <- DegreeDistribution(g_net)
dist_fit
