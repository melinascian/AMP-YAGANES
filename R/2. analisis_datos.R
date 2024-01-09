# ---- ANALISIS DE DATOS ----

load("Datos/datosdepurados.rda")
library(dplyr)
library(igraph)
library(multiweb)
library(NetIndices)
library(NetworkExtinction)
library(network)


## ---- PROPIEDADES DE COMPLEJIDAD Y ESTRUCTURA ----

propcomplejidad<-calc_topological_indices(gok)

## ---- INDICES DE CENTRALIDAD ----
#calculo los diferentes coeficientes de centralidad para los vertices de gok


### ---- GRADO ----
### numero de interacciones de cada nodo
hist(degree(gok,mode="all"))
grado<-as.data.frame(degree(gok))

V(gok)$degree.total<-degree(gok,mode="all") 
V(gok)$degree.in<-degree(gok, mode="in") 
V(gok)$degree.out<-degree(gok,mode="out")

### Ajuste a la distribución de grado

upgrade_graph(gok)  # actualiza objeto igraph
g_net <- as.network(as.matrix(gok))# convierto gok en red
dist_fit <- DegreeDistribution(g_net)# ajusto a distribución de grado
dist_fit

#menor AIC, mejor modelo
#menor BIC, mejor modelo

#logLik    AIC    BIC model    Normal.Resid family     
#<dbl>  <dbl>  <dbl> <chr>    <chr>        <chr>      
#  1   82.5 -159.  -154.  Exp      No           Exponential
#  2   23.9  -41.8  -36.8 Power    No           PowerLaw   
#  3   18.6  -31.2  -26.2 LogExp   No           Exponential
#  4  -37.3   80.5   85.5 LogPower No           PowerLaw   


### ---- CERCANÍA ----
### mide la proximidad de una especie a todo el resto de especies de la red 
V(gok)$closeness<-closeness(gok,mode="all")

### ---- INTERMEDIACIÓN ----
### describe el número de veces que una especie está entre un par de otras especies 
#(es decir, cuántos caminos llevan a esa especie)
V(gok)$betweeness<-betweenness(gok)

a<-intergraph::asDF(gok) # creo lista a compuesta por nodos e interacciones del obejto g
atributos<-as.data.frame(a["vertexes"])#creo el data frame utilizando solo los vertex de la lista "a"
coeficientes_centralidad<-atributos %>% rename (ID=vertexes.intergraph_id, name=vertexes.name, degree.total=vertexes.degree.total, degree.in=vertexes.degree.in, degree.out=vertexes.degree.out, closeness=vertexes.closeness, betweeness=vertexes.betweeness)

## ---- INDICE DE ESPECIES CLAVE ----
coef_centralidad<-mutate(coeficientes_centralidad,ranking_degree = rank(degree.total),ranking_closeness=rank(closeness),ranking_betweeness=rank(betweeness), IEC=((ranking_degree+ranking_closeness + ranking_betweeness)/3))


#RANKING:
#mayor grado, mayor ranking
#mayor cercania, mayor ranking
#mayor intermediación, mayor ranking

## ---- PATRÓN MUNDO PEQUEÑO ----

#coeficiente de agrupamiento alto y distancia corta, 
#se debe comparar con redes aleatorias con mismo numero de interacciones y especes, pero eso debo gererarlo
#para redes aleatorias, uso esta funcion, pongo que redes aleatorias me generan, se hace con 100 redes y se compara la estadisitca


rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(propcomplejidad$Size, propcomplejidad$Links, directed = TRUE)#defino redes aleatorias que voy a generar
  
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(propcomplejidad$Size, propcomplejidad$Links, type = "gnm", 
                          directed = TRUE)
  return(e) 
})
sw<-multiweb::calc_swness_zscore(gok, nullDist = rnd_g, weights = NA, ncores = 4)
datossw<-as.data.frame(sw["da"])
#nulldist = distancia de las redes aleatorias con la mia


## ---- NIVELES TRÓFICOS ----


#https://www.rdocumentation.org/packages/NetIndices/versions/1.4.4.1/topics/TrophInd
# Funcion TrophInd: me permite calcular los niveles troficos (tl) y el indice de omnivoria (IO)

#1) Genero matriz para funcion TrophInd

m <- get.adjacency(gok, sparse = FALSE) #matriz de adyacencia
tl <- round(TrophInd(m), digits = 3) # tl es el nivel trofico, y OI es el indice de omnivoria (especies con mayor IO, tienen presas dentro de diferentes niveles troficos)

niveles_troficos<- TrophInd(m)


save(gok, propcomplejidad, tablamundopeque, coeficientes_centralidad, niveles_troficos,coef_centralidad, file="Datos/analisisdatos.rda")


