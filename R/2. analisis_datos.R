# ---- LOAD PACKAGES ----

library(tidyverse)
library(igraph)
library(multiweb)
library(NetIndices)
library(NetworkExtinction)
library(network)


# ---- LOAD TIDY DATA ----

load("Datos/datosdepurados_apr24.rda")


## ---- NETWORK-LEVEL ANALYSES ----

### ---- COMPLEXITY & STRUCTURE ----
fw_props <- multiweb::calc_topological_indices(gok)

### ---- DEGREE DISTRIBUTION ----
upgrade_graph(gok)
g_net <- as.network(as.matrix(gok))
dist_fit <- NetworkExtinction::DegreeDistribution(g_net)

### ---- SMALL-WORLD PATTERN ----
rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(fw_props$Size, fw_props$Links, directed = TRUE) # defino redes aleatorias que voy a generar
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(fw_props$Size, fw_props$Links, type = "gnm", 
                          directed = TRUE)
  return(e) 
})

sw <- multiweb::calc_swness_zscore(gok, nullDist = rnd_g, weights = NA, ncores = 4)
datos_sw <- as.data.frame(sw["da"])


## ---- SPECIES-LEVEL ANALYSES ----
### ---- DEGREE ----

hist(degree(gok,mode="all"))
grado <- as.data.frame(degree(gok))

# Add node attributes
V(gok)$degree.total <- degree(gok, mode="all") 
V(gok)$degree.in <- degree(gok, mode="in") 
V(gok)$degree.out <- degree(gok, mode="out")

### ---- CLOSENESS ----
V(gok)$closeness <- igraph::closeness(gok,mode="all")

### ---- BETWEENESS ----
V(gok)$betweeness <- igraph::betweenness(gok)

a <- intergraph::asDF(gok)  # creo lista a compuesta por nodos e interacciones del obejto g
atributos <- as.data.frame(a["vertexes"])  # creo el data frame utilizando solo los vertex de la lista "a"
indices_centralidad <- atributos %>% 
  rename (ID=vertexes.intergraph_id, name=vertexes.name, degree.total=vertexes.degree.total, 
          degree.in=vertexes.degree.in, degree.out=vertexes.degree.out, closeness=vertexes.closeness, 
          betweeness=vertexes.betweeness)

### ---- KEYSTONE INDEX ----
ind_centralidad <- indices_centralidad %>% 
  mutate(ranking_degree = dense_rank(desc(degree.total)),
         ranking_closeness=dense_rank(desc(closeness)),
         ranking_betweeness=dense_rank(desc(betweeness))) %>% 
  mutate(IEC=(ranking_degree + ranking_closeness + ranking_betweeness)/3)

### ---- TROPHIC LEVEL ----
m <- get.adjacency(gok, sparse = FALSE)  # matriz de adyacencia
tl <- round(TrophInd(m), digits = 3)  # tl es el nivel trofico, y OI es el indice de omnivoria (especies con mayor IO, tienen presas dentro de diferentes niveles troficos)
niveles_troficos <- TrophInd(m)

### ---- RELATION TL-INDEX ----
ind_tl <- sp_level %>% 
  #dplyr::filter(IEC <= 10) %>% 
  ggplot(aes(x=TL, y=IEC)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  theme_classic()


## ---- ALL SPECIES-LEVEL PROPERTIES ----
sp_level <- cbind(ind_centralidad, niveles_troficos)


## ---- SAVE RESULTS ----
save(gok, fw_props, indices_centralidad, ind_centralidad, datos_sw, niveles_troficos, sp_level,
     file="Datos/analisis_datos_apr24.rda")
