## ANALISIS DE DATOS ##

## INSTALO PAQUETES ##
install.packages("pillar")
install.packages("dplyr")
install.packages("igraph") #IGRAPH (Tutorial paquete IGRAPH: igraph.org/R
install.packages("devtools") #esto me permite instalar paquetes desde GitHub
#hay algunos paquetes que no estan en la libreria oficial de R.
require(devtools)
install_github("lsaravia/multiweb")
#Para Markdown
install.packages("rticles")
#Para niveles troficos
install.packages("NetIndices")

library(dplyr)
library(igraph)
library(multiweb)
library(NetIndices)
library(multiweb)
## PROPIEDADES DE COMPLEJIDAD DE LA RED ##

# Orden (S) = riqueza especifica
vcount(g)

# Numero de interacciones (L)
ecount(g)
E(g)

# Densidad (L/S)
den<-ecount(g)/vcount(g)
den
#hay 2,36 interacciones por nodo

# Conectividad (L/S^2)
con<-ecount(g)/(vcount(g)^2)
con
#quiere decir que el 0,85% de las interacciones posibles, existen para Yaganes.


#Matriz de disntancias
distances(g) 

#Diametro de la red
diameter(g)


## PROPIEDADES DE ESTRUCTURA DE LA RED ##

### Grado = numero de interacciones de cada nodo 
degree(g)
degree(g, "Carnivora*") #numero de interacciones para un nodo especifico
which.max(degree(g)) #especie con mas interacciones (mayor grdo)
which.min(degree(g)) #especie con menos interacciones (menor grado)


### Coeficiente de agrupamiento = probabilidad que se formen clusters o modulos
# se calcula para cada nodo, se refiere a la cantidad de interacciones entre los vecinos de un nodo
# indica que tan cerca estan los vecinos de un nodo
# se espera que haya relacion entre coeficiente de agrupamiento y modularidad
# clusterizacion y distancia entre los nodos me da idea del patron mundo pequeño

# Mundo pequeño:
#coeficiente de agrupamiento alto y distancia corta, 
#se debe comparar con redes aleatorias con mismo numero de interacciones y especes, pero eso debo gererarlo
#para redes aleatorias, uso esta funcion, pongo que redes aleatorias me generan, se hace con 100 redes y se compara la estadisitca

calc_swness_zscore(g)
### Modularidad= que especies hay en cada modulo

clusters(g)


# CENTRALIDAD

# centralidad por cercania = mide la proximidad de una especie a todo el resto de especies de la red 
closeness(g)
which.max(closeness(g))
which.min(closeness(g))


# centralidad por intermediacion = describe el número de veces que una especie está entre un par de otras especies 
#(es decir, cuántos caminos llevan a esa especie)

betweenness(g)
ebs <- edge_betweenness(g)
as_edgelist(g)[ebs == max(ebs), ] #me dice que nodos tienen la mayor de intermediacion

# grado de centralidad
evcent(g)
## NIVELES TROFICOS ##
#https://www.rdocumentation.org/packages/NetIndices/versions/1.4.4.1/topics/TrophInd

## Funcion TrophInd: me permite calcular los niveles troficos (tl) y el indice de omnivoria (IO)

#1) Genero matriz para funcion TrophInd

m <- get.adjacency(g, sparse = FALSE) #matriz de adyacencia
tl <- round(TrophInd(m), digits = 3)
tl
# tl es el nivel trofico, y OI es el indice de omnivoria
# especies con mayor IO, tienen presas dentro de diferentes niveles troficos

calc_topological_indices(g)
TrophInd(m)

m<-TrophInd(as_adjacency_matrix(m,sparse=TRUE))
as_adjacency_matrix(g,sparse=TRUE) #sparse quiere decir que es una matriz que esta poco completa, es laxa (la mayoria va a ser 0)


TrophInd(m)

plot_troph_level(g)



