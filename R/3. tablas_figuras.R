# ---- TABLAS & FIGURAS ----

# Load data
load("Datos/analisis_datos_26jan24.rda")

# Load packages
library(MASS)
library(tidyverse)
library(igraph)
library(multiweb)
library(network)

# mtcars %>% 
#   dplyr::select(cyl, mpg) %>% 
#   group_by(cyl) %>% 
#   summarize(avg_mpg = mean(mpg))

# ---- MANUSCRITO ----
## ---- TABLAS ----

### ---- Table 1 ----
Tabla_propcomplejidad <- fw_props %>%
  dplyr::select(Size, Links, LD, Connectance) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  rename (Species=Size, Interactions=Links)

### ---- Table 2 ----
Tabla_propestructura <- fw_props %>% 
  mutate(SW = "YES") %>% 
  mutate(Intermediate = Size-(Top+Basal)) %>% 
  mutate(Basal = (Basal/Size)*100, Intermediate = (Intermediate/Size)*100, Top = (Top/Size)*100) %>% 
  dplyr::select(Basal, Intermediate, Top, PathLength, Clustering, SW) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  rename(CC=Clustering, CPL=PathLength)

#propcomplejidad<-cbind(propcomplejidad, smallworld)
#borrar<-c("Top","Basal","TLmax","LOmnivory","Components","Vulnerability","VulSD","Generality","GenSD", "Cannib")
#Tabla1<- propcomplejidad[ , !(names(propcomplejidad) %in% borrar)]
#Tabla1ok<-Tabla1 %>% rename (Species = Size, Interactions=Links, Omn=Omnivory, Clus.coef=Clustering, SW=smallworld)
#names(Tabla1ok)
#Tabla1final = Tabla1ok [ , c(1,3,4,5,2,8,6,7,9)]

### ---- Table 3 ----
borrar <- c("ID","degree.total", "degree.in","degree.out","closeness","betweeness",
            "ranking_degree","ranking_closeness","ranking_betweeness")
keysp_index <- ind_centralidad[ , !(names(ind_centralidad) %in% borrar)]
keysp_index <- keysp_index %>% 
  mutate(Ranking=dense_rank(IEC)) %>%
  rename(Trophic_species = name)
Top10_IEC <- keysp_index %>% 
  dplyr::filter(Ranking <= 10) %>% 
  mutate(across(c(IEC), ~ round(., 2))) %>% 
  arrange(Ranking)
# mayor keysp_index, mayor importancia


## ---- GRAFICOS ----

### ---- Figure 2 ----
# Degree distribution
upgrade_graph(gok)  # actualiza objeto igraph
g_net <- as.network(as.matrix(gok))  # convierto gok en red
dist_fit <- NetworkExtinction::DegreeDistribution(g_net)  # ajusto distribución de grado

Fig2 <- dist_fit[["graph"]] +
  labs(y = "Cumulative degree distribution", x = "Degree (k)") +
  theme(axis.title.x = element_text(face = "bold", size = 16),
                   axis.title.y = element_text(face = "bold", size = 16),
                   axis.text.x = element_text(size = 12),
                   axis.text.y = element_text(size = 12),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

ggsave(filename = "Figuras/Fig2.png", plot = Fig2,
       width = 10, units = "in", dpi = 600, bg = "white")

# hist(degree(gok, mode="all"))
# grado <- as.data.frame(degree(gok))
# # pl <- ggplot(grado, aes(x=degree(gok)))
# # pl + geom_histogram()
# # pl2 <- pl + geom_histogram(binwidth = 1, col='black', fill='black', alpha=0.6)
# #binwidth = ancho de la barra
# #alpha = transparencia del grafico
# # pl2
# # pl2 + xlab('Grado') + ylab('Frecuencia') + ggtitle('Distribucion de grado')
# (pl_ok <- ggplot(grado, aes(x = degree(gok))) +
#     geom_histogram(binwidth = 1, col='black', fill='black', alpha=0.6) +
#     xlab('Grado (número de interacciones)') + ylab('Frecuencia') +
#     theme(axis.title.x = element_text(face = "bold", size = 16),
#           axis.title.y = element_text(face = "bold", size = 16),
#           axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12),
#           panel.background = element_blank(),
#           axis.line = element_line(colour = "black")))


### ---- Figure 3 ----
# Gráficos comparativos índices de centralidad
par(mfrow = c(1,1))
set.seed(1)  # mantiene la posición de las especies
deg_plot <- multiweb::plot_troph_level(gok, vertex.size=0.5*(V(gok)$degree.total), ylab = "Trophic level")
set.seed(1)
btw_plot <- multiweb::plot_troph_level(gok, vertex.size=sqrt(V(gok)$betweeness))
set.seed(1)
clo_plot <- multiweb::plot_troph_level(gok, vertex.size=10^3.2*(V(gok)$closeness))
#plot_troph_level(gok)  # si no mantengo posicion de las especies se grafica siempre un grafico distinto

# Standardise each index to its maximum value
# Degree

# Betweeness

# Closeness

### ---- Figure 4 ----
Fig4 <- sp_level %>% 
  ggplot(aes(x=TL, y=IEC)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks=seq(0,6,1)) +
  scale_y_reverse(breaks=c(60,50,40,30,20,10,1)) +
  labs(y = "Keystone Species Index (KSI)", x = "Trophic level") +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
Fig4

ggsave(filename = "Figuras/Fig4.png", plot = Fig4,
       width = 10, units = "in", dpi = 600, bg = "white")

# ---- MATERIAL SUPLEMENTARIO ----

## ---- Lista de interacciones ----
#Lista de interacciones (Columnas = Presa, Depredador, Referencia, Enlace)
##lista de interacciones completa, sin eliminar los duplicados, para conservar las referencias
borrar_int <- c("Prey.funtional.group","Prey.expertise","Predator.funtional.group","Predator.expertise","Strategy","Source", "Observations")
listadeinteracciones <- lista_interacciones[ , !(names(lista_interaccionesok) %in% borrar_int)]

## ---- Lista de especies ----
#(Columnas = Especie trofica, Resolucion, referencia, enlace)
lista_especies<-read.csv("Datos/lista_especies.csv", header = T, sep=",")

## ---- Tabla mundo pequeño ----

rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(propcomplejidad$Size, propcomplejidad$Links, directed = TRUE)#defino redes aleatorias que voy a generar
  
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(propcomplejidad$Size, propcomplejidad$Links, type = "gnm", 
                          directed = TRUE)
  return(e) 
})

sw <- multiweb::calc_swness_zscore(gok, nullDist = rnd_g, weights = NA, ncores = 4)
datossw <- as.data.frame(sw["da"])
Tabla_sw <- datossw %>% rename (Clustering = da.Clustering,PathLength = da.PathLength, zCC=da.zCC, zCP=da.zCP, CClow=da.CClow,CChigh=da.CChigh, CPlow=da.CPlow, CPhigh=da.CPhigh, SWness=da.SWness, SWnessCI=da.SWnessCI, isSW=da.isSW, isSWness=da.isSWness)

## ---- Tabla indices de centralidad + TL + IEC ----
ID <- sample(1:127,size=127,replace=TRUE)

datoscc <- cbind(ind_centralidad,niveles_troficos)
borrar <- c("ranking_degree","ranking_closeness","ranking_betweeness", "ID.1","OI")
datoscc <- datoscc[ , !(names(datoscc) %in% borrar)]
tablacc <- datoscc %>% rename (Trophic_species = name, Total_degree=degree.total, In_degree=degree.in, Out_degree=degree.out, Trophic_level=TL, Closeness=closeness, Betweeness=betweeness)
Tabla_coeficientes_centralidad = tablacc [ , c(1,2,9,3,4,5,6,7,8)]


## Tabla comparativa entre redes pelágicas
# Cargo redes y creo objeto g de cada una
# red_beaglechannel <- read.csv("Datos/BeagleChannel_links_original.csv")
# red_burdwoodbank <-read.csv("Datos/BurdwoodBank_links_original.csv")
# red_northernscotia<-read.csv("Datos/NorthernScotia_links_original.csv")
# red_pottercove<-read.csv("Datos/PotterCove_links_original.csv")
# red_southernscotia<-read.csv("Datos/SouthernScotia_links_original.csv")                                
# g_beagle <- graph_from_data_frame(red_beaglechannel, directed=TRUE)
# g_burdwood<-graph_from_data_frame(red_burdwoodbank, directed=TRUE)
# g_northernscotia<-graph_from_data_frame(red_northernscotia, directed=TRUE)
# g_pottercove<-graph_from_data_frame(red_pottercove, directed=TRUE)
# g_southernscotia<-graph_from_data_frame(red_southernscotia, directed=TRUE)
# 
# calculo los indices para cada red
# beagle_propcomplej<-calc_topological_indices(g_beagle)
# burdwood_propcomplej<-calc_topological_indices(g_burdwood)
# northernscotia_propcomplej<-calc_topological_indices(g_northernscotia)
# pottercove_propcomplej<-calc_topological_indices(g_pottercove)
# southernscotia_propcomplej<-calc_topological_indices(g_southernscotia)
