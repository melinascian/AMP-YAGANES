load("Datos/analisisdatos.rda")

##TABLAS
#Tabla propiedades de complejidad + mundo pequeño:

smallworld<-sample(c("TRUE"))
propcomplejidad<-cbind(propcomplejidad, smallworld)

#Tabla propiedades de estructura + nivel trofico

ID<-sample(1:192,size=192,replace=TRUE)
propestructura<-data.frame(ID,niveles_troficos)

rbind(coeficientes_centralidad,niveles_troficos)

## GRAFICOS DE LA RED ##

#Graffito general
plot(gok)
plot(gok,
     vertex.size=3,
     vertex.color="blue",
     edge.color="black",
     vertex.label=NA,     # sin etiquetas de los nodos.
     edge.arrow.size=0,2)

#Grafico segun grado
v<-as.data.frame(vertex_attr(gok))
V(gok)$degree <- degree(gok)
get.data.frame(gok,what="vertices") %>% head() 
plot(gok,
     vertex.size=5*sqrt(V(gok)$degree),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)
hist(degree(gok))

#Grafico segun cercania
V(gok)$closeness <- closeness(gok, vids = V(gok), mode = c("total"), weights = NULL, 
                            normalized = FALSE, cutoff = -1)

closeness(
  gok,
  vids = V(gok),
  mode = c("total"),
  weights = NULL,
  normalized = FALSE,
  cutoff = -1)


plot(gok,
     vertex.size=log(V(gok)$closeness),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)

which.max(closeness(gok))
which.min(closeness(gok))

#Grafico segun indeterminacion
betweenness(gok)
ebs <- edge_betweenness(gok)
as_edgelist(gok)[ebs == max(ebs),] #me dice que nodos tienen la mayor de intermediacion

V(gok)$betweenness <- betweenness(gok)

plot(gok,
     vertex.size=sqrt(V(gok)$betweenness),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)

## GRAFICO SEGUN NIVELES TROFICOS ##
m <- get.adjacency(gok, sparse = FALSE)
as_adjacency_matrix(gok,sparse=TRUE) #sparse quiere decir que es una matriz que esta poco completa, es laxa (la mayoria va a ser 0)


TrophInd(m)

vertex.attributes(gok)

load("gatributos.rda")


# Gráfico comparativo índices de centralidad
par(mfrow = c(1,3))
set.seed(1)  # mantiene la posición de las especies
plot_troph_level(gok, vertex.size=sqrt(V(gok)$degree), main = "Degree")
set.seed(1)
plot_troph_level(gok, vertex.size=sqrt(V(gok)$betweenness), main = "Betweenness")
set.seed(1)
plot_troph_level(gok, vertex.size=10^3*V(gok)$closeness,main = "Closeness")


#plot_troph_level(gok)#si no mantengo posicion de las especies se grafica siempre un grafico distinto

#Graficos poner: 3 graficos en una figura + histograma.