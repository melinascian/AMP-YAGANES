# ---- ANALISIS DE DATOS ----

# Load data
load("Datos/datosdepurados.rda")

# Load packages
library(tidyverse)
library(igraph)
library(multiweb)
library(NetIndices)
library(NetworkExtinction)
library(network)


## ---- PROPIEDADES DE COMPLEJIDAD Y ESTRUCTURA ----

fw_props <- multiweb::calc_topological_indices(gok)

## ---- INDICES DE CENTRALIDAD ----
# calculo los diferentes indices de centralidad para los vertices de gok

### ---- GRADO ----
### numero de interacciones de cada nodo
hist(degree(gok,mode="all"))
grado <- as.data.frame(degree(gok))

# agregar como atributo de gok
V(gok)$degree.total <- degree(gok,mode="all") 
V(gok)$degree.in <- degree(gok,mode="in") 
V(gok)$degree.out <- degree(gok,mode="out")

### Ajuste a la distribución de grado
upgrade_graph(gok)  # actualiza objeto igraph
g_net <- as.network(as.matrix(gok))  # convierto gok en red
dist_fit <- NetworkExtinction::DegreeDistribution(g_net)  # ajusto distribución de grado
dist_fit

# menor AIC, mejor modelo
# menor BIC, mejor modelo
#logLik    AIC    BIC model    Normal.Resid family     
#<dbl>  <dbl>  <dbl> <chr>    <chr>        <chr>      
#  82.5 -159.  -154.  Exp      No           Exponential
#  23.9  -41.8  -36.8 Power    No           PowerLaw   
#  18.6  -31.2  -26.2 LogExp   No           Exponential
# -37.3   80.5   85.5 LogPower No           PowerLaw   

### ---- CERCANÍA ----
### mide la proximidad de una especie a todo el resto de especies de la red 
V(gok)$closeness <- igraph::closeness(gok,mode="all")

### ---- INTERMEDIACIÓN ----
### describe el número de veces que una especie está entre un par de otras especies 
# (es decir, cuántos caminos llevan a esa especie)
V(gok)$betweeness <- igraph::betweenness(gok)

a <- intergraph::asDF(gok)  # creo lista a compuesta por nodos e interacciones del obejto g
atributos <- as.data.frame(a["vertexes"])  # creo el data frame utilizando solo los vertex de la lista "a"
indices_centralidad <- atributos %>% 
  rename (ID=vertexes.intergraph_id, name=vertexes.name, degree.total=vertexes.degree.total, 
          degree.in=vertexes.degree.in, degree.out=vertexes.degree.out, closeness=vertexes.closeness, 
          betweeness=vertexes.betweeness)

## ---- INDICE DE ESPECIE CLAVE ----
ind_centralidad <- indices_centralidad %>% 
  mutate(ranking_degree = dense_rank(desc(degree.total)),
         ranking_closeness=dense_rank(desc(closeness)),
         ranking_betweeness=dense_rank(desc(betweeness))) %>% 
  mutate(IEC=(ranking_degree + ranking_closeness + ranking_betweeness)/3)

# RANKING:
# mayor grado, mayor ranking
# mayor cercania, mayor ranking
# mayor intermediación, mayor ranking

## ---- PATRÓN MUNDO PEQUEÑO ----

# coeficiente de agrupamiento alto y distancia corta 
# se debe comparar con redes aleatorias con mismo numero de interacciones y especes, pero eso debo gererarlo
# para redes aleatorias, uso esta funcion, pongo que redes aleatorias me generan, se hace con 100 redes y se compara la estadisitca

rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(fw_props$Size, fw_props$Links, directed = TRUE) # defino redes aleatorias que voy a generar
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(fw_props$Size, fw_props$Links, type = "gnm", 
                          directed = TRUE)
  return(e) 
})

sw <- multiweb::calc_swness_zscore(gok, nullDist = rnd_g, weights = NA, ncores = 4)
datossw <- as.data.frame(sw["da"])
# nulldist = distancia de las redes aleatorias con la mia

## ---- NIVELES TRÓFICOS ----
# https://www.rdocumentation.org/packages/NetIndices/versions/1.4.4.1/topics/TrophInd
# Funcion TrophInd: me permite calcular los niveles troficos (tl) y el indice de omnivoria (IO)

m <- get.adjacency(gok, sparse = FALSE)  # matriz de adyacencia
tl <- round(TrophInd(m), digits = 3)  # tl es el nivel trofico, y OI es el indice de omnivoria (especies con mayor IO, tienen presas dentro de diferentes niveles troficos)
niveles_troficos <- TrophInd(m)

## ---- SPECIES-LEVEL DATA ----
sp_level <- cbind(ind_centralidad, niveles_troficos)

## ---- INDICE-TL ----
(ind_tl <- sp_level %>% 
   #dplyr::filter(IEC <= 10) %>% 
   ggplot(aes(x=TL, y=IEC)) + 
   geom_point() +
   geom_smooth(method = "loess") +
   theme_classic())

# Save results
save(gok, fw_props, indices_centralidad, ind_centralidad, datossw, niveles_troficos,
     file="Datos/analisisdatos.rda")
