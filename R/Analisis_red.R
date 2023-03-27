#Seteo carpeta de trabajo
setwd("C:/AMPYAGANES/R")

#Traigo datos
datos_interacciones<-read.delim("lista_interacciones.txt")
datos_especies<-read.csv("lista_especies2.csv")
interacciones<-read.delim("interacciones.txt")

##Instalo paquetes que voy a usar
#IGRAPH
install.packages("igraph") #Tutorial paquete IGRAPH: igraph.org/R
install.packages("devtools") #es un paquete que me permite instalar paquetes desde github,hay algunos paquetes que no estan en la libreria oficial de R, entonces suelen estar disponibles en git hub u otro paquete.
require(devtools)
install_github("lsaravia/multiweb")

library(igraph)
#Creo tabla con columna presa y predador nada mas, para crear el objeto g
#a partir de la columna numero 3, los toma como atributos de la interaccion, y ademas me permite asignarle lista de especies 
#agrgegar atributos: por ejemplo, cantidad de interacciones por especie.
#si llamo al objeto g, me dice el nombre del atributo, y si es atributo del nodo o de la interaccion
df<-datos_interacciones %>% dplyr::select(Presa, Depredador)

#Para crear el obejto g
g<-graph_from_edgelist(as.matrix(df), directed=TRUE)#pide que le ponga una matriz de 2 columnas, sin atributos de la interaccion.
g<-graph_from_data_frame(df,directed=TRUE)

g

#Grafico la red
plot(g)


## PROPIEDADES DE COMPLEJIDAD DE LA RED

# Orden (S)
vcount(g)
#riqeuza especifica

# Numero de interacciones (L)
ecount(g)

# Densidad (L/S)
den<-ecount(g)/vcount(g)
#hay arpoximadamente 4 interacciones por nodo

# Conectividad (L/S^2)
con<-ecount(g)/(vcount(g)^2)
#quiere decir que cerca del 2% de las interacciones posibles, existen para Yaganes.

## PROPIEDADES DE ESTRUCTURA DE LA RED

# Distribucion de grado (P(k)= N(k)/S, con N(k)= # nodos con "k" interacciones
degree(g) #numero de interacciones de cada nodo = grado
which.max(degree(g)) #especie con mas interacciones (mayor grdo)
which.min(degree(g)) #especie con menos interacciones (menor grado)

degree(g, "Gymnoscopelus_fraseri") #numero de interacciones para un nodo especifico


# Coeficiente de agrupamiento

# Centralidad (que tan influyente una especie es)

# centralidad por cercania = mide la proximidad de una especie a todo el resto de especies de la red 


# centralidad por intermediacion = describe el número de veces que una especie está entre un par de otras especies (es decir, cuántos caminos llevan a esa especie)
edge_betweenness(g)
ebs <- edge_betweenness(g)
as_edgelist(g)[ebs == max(ebs), ] #me dice que nodos tienen la mayor de intermediacion


# grado de centralidad
evcent(g)



### NIVELES TROFICOS ###
install.packages("NetIndices")
#https://www.rdocumentation.org/packages/NetIndices/versions/1.4.4.1/topics/TrophInd

TrophInd(df, Import = NULL,
         Export=NULL)

#tengo que crear una matriz

#hay una funcion que calcula, dentro del multiweb, la red por nivel trofico (en funcion de especies basales, intermedias y tope)
#si a una especie le entra una flecha, quiere decir que tiene una presa
#depsues calcula el nivel trofico de una especie en funcion de sus preasas
#los depredadores topes son aquellas especies que no tienen flecha salientes, no tienen depredadores








# # Obtengo nombres de filas y columnas
# nameVals <- sort(unique(unlist(interacciones[1:2])))
# 
# # Construyo matriz de 0 con todas las especies
# matriz <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))
# 
# # Lleno matriz con los valores 1 de la interaccion presa-depredador
# 
# matriz[as.matrix(interacciones[c("Presa", "Depredador")])] <- interacciones[["Interacciones"]]
# matriz
# 
# install.packages("rgl")
# 
install.packages("foodingraph")
# 
# 
# library(foodingraph)
# library(CRANsearcher)
# write.table(matriz, file = "redprueba.csv", append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
# analysis.single(filename = "redprueba.csv", omn = c("TRUE"), cann = c("TRUE"), positions = c("TRUE"), maxlevels = 8)
# #Analisis exploratorio
# 
# class(datos_interacciones)
# str(datos_interacciones) #13 variables con 927 observaciones
# 
# 
# g <- graph.data.frame(interacciones, directed = FALSE)
# class(g)                                     # Clase del objeto
# V(g)$name                                    # Nombres de los vértices
# E(g)$weight                                  # Peso de las aristas
# tkplot(g)                                    # Gráfico dinámico
# plot(g, edge.label = paste(E(g)$weight, sep = "")) # Gráfico de abajo
