## GRAFICOS DE LA RED ##

#Graffito general
plot(g)

plot(g,
     vertex.size=3,
     vertex.color="blue",
     edge.color="black",
     vertex.label=NA,     # sin etiquetas de los nodos.
     edge.arrow.size=0,2)

#Grafico segun grado
v<-as.data.frame(vertex_attr(g))
V(g)$degree <- degree(g)
get.data.frame(g,what="vertices") %>% head() 
plot(g,
     vertex.size=5*sqrt(V(g)$degree),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)
hist(degree(g))

#Grafico segun cercania
V(g)$closeness <- closeness(g, vids = V(g), mode = c("total"), weights = NULL, 
                            normalized = FALSE, cutoff = -1)

closeness(
  g,
  vids = V(g),
  mode = c("total"),
  weights = NULL,
  normalized = FALSE,
  cutoff = -1)


plot(g,
     vertex.size=log(V(g)$closeness),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)

which.max(closeness(g))
which.min(closeness(g))

#Grafico segun indeterminacion
betweenness(g)
ebs <- edge_betweenness(g)
as_edgelist(g)[ebs == max(ebs), ] #me dice que nodos tienen la mayor de intermediacion

V(g)$betweenness <- betweenness(g)

plot(g,
     vertex.size=sqrt(V(g)$betweenness),
     vertex.color="blue",
     vertex.label=NA,
     edge.arrow.size=0.1)

## GRAFICO SEGUN NIVELES TROFICOS ##
m <- get.adjacency(g, sparse = FALSE)
as_adjacency_matrix(g,sparse=TRUE) #sparse quiere decir que es una matriz que esta poco completa, es laxa (la mayoria va a ser 0)


TrophInd(m)

vertex.attributes(g)

# Gráfico comparativo índices de centralidad
par(mfrow = c(1,3))
set.seed(1)  # mantiene la posición de las especies
plot_troph_level(g, vertex.size=sqrt(V(g)$degree), main = "Degree")
set.seed(1)
plot_troph_level(g, vertex.size=sqrt(V(g)$betweenness), main = "Betweenness")
set.seed(1)
plot_troph_level(g, vertex.size=10^3*V(g)$closeness, main = "Closeness")

