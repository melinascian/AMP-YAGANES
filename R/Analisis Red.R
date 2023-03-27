
#Instalo paquetes que voy a usar:

#IGRAPH
install.packages("igraph")
#Tutorial paquete IGRAPH: igraph.org/R 

#Funciones del paquete IGRAPH:

#funcion degree: el numero de interacciones

#para conocer el numero de nodos: vcount (g)


install.packages("devtools") #es un paquete que me permite instalar paquetes desde github
# hay algunos paquetes que no estan en la libreria oficial de R, entonces suelen estar disponibles en git hub u otro paquete.


#de la lista de especies, hacer graficos exploratorios: ordenarlos por frupo funcional  por habitat, pero por ahora concentrarme en la red trofica.

require(devtools)
install_github("lsaravia/multiweb")


vcount(g)
ecount(g)

#sabiendo ecuacion para conectividad, puedo hacer un codigo
vcount/ecount
con<-ecount(g)/(vcount(g)^2)# por convencion se calcula asi

#quiere decir que casi el 2% de las interacciones posibles, exite para YAGANES.
# se puede modificar esta ecuacion para que la ecuacion no considere ni canibalismo ni ciclos (A se come a B y B se come a A). Esto es un ciclo de 2 especies, puede haber otro ciclo de 3. Estos son raros.
# puedo hacer estimacion de conectividad sin tener en cuenta canibalismo ni ciclos

con2<-927/(227*226)
              
library(graphics)
library(igraph)

# grado: medida de centralidad
# puedo agregrar atributos de centralidad al objeto g. Puedo decirle, por ejemplo. que grafique el diametro de los nodos en funcion del numero de interacciones.



#Seteo carpeta de trabajo
setwd("C:/AMPYAGANES/R")

#Traigo datos
datos_interacciones<-read.delim("lista_interacciones.txt")
datos_especies<-read.csv("lista_especies2.csv")
interacciones<-read.delim("interacciones.txt")

df<-datos_interacciones %>% dplyr::select(Presa, Depredador)

g<-graph_from_edgelist(as.matrix(df), directed=TRUE)#pide que le ponga una matriz de 2 columnas, sin atributos de la interaccion.

g<-graph_from_data_frame(df,directed=TRUE)#a partir de la columna numero 3, los toma como atributos de la interaccion, y ademas me permite asignarle lista de especies espea
#agrgegar atributos: por ejemplo, cantidad de interacciones por especie.
#si llamo al objeto g, me dice el nombre del atributo, y si es atributo del nodo o de la interaccion
plot(g)
g

#hay una funcion que calcula, dentro del multiweb, la red por nivel trofico (en funcion de especies basales, intermedias y tope)
#si a una especie le entra una flecha, quiere decir que tiene una presa
#depsues calcula el nivel trofico de una especie en funcion de sus preasas
#los depredadores topes son aquellas especies que no tienen flecha salientes, no tienen depredadores


# ##Creo matriz de interacciones
# 

## por ejemplo el nivel trofico, necesito otro paquete ademas del igraph y el indice de omnivoria, para cada especie, me instalo el paquete: netindices, funcion trophind
install.packages("NetIndices")

#funcion TrophInd>>tengo que crear una matriz a partir del data frame

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
