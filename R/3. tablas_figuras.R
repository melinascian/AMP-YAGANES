# ---- LOAD PACKAGES ----

library(MASS)
library(tidyverse)
library(igraph)
library(multiweb)
library(network)


# ---- LOAD DATA ----

load("Datos/datosdepurados_apr24.rda")
load("Datos/analisis_datos_apr24.rda")


# ---- TABLES ----

## ---- Table 1 ----
Tabla_propcomplejidad <- fw_props %>%
  dplyr::select(Size, Links, LD, Connectance) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  rename (Species=Size, Interactions=Links)

## ---- Table 2 ----
Tabla_propestructura <- fw_props %>% 
  mutate(SW = "YES") %>% 
  mutate(Intermediate = Size-(Top+Basal)) %>% 
  mutate(Basal = (Basal/Size)*100, Intermediate = (Intermediate/Size)*100, Top = (Top/Size)*100) %>% 
  dplyr::select(Basal, Intermediate, Top, PathLength, Clustering, SW) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  rename(CC=Clustering, CPL=PathLength)

## ---- Table 3 ----
names <- c("ID","degree.total", "degree.in","degree.out","closeness","betweeness",
            "ranking_degree","ranking_closeness","ranking_betweeness")
keysp_index <- ind_centralidad[ , !(names(ind_centralidad) %in% names)]
keysp_index <- keysp_index %>% 
  mutate(Ranking=dense_rank(IEC)) %>%
  rename(Trophic_species = name)
Top10_IEC <- keysp_index %>% 
  dplyr::filter(Ranking <= 10) %>% 
  mutate(across(c(IEC), ~ round(., 2))) %>% 
  arrange(Ranking)


# ---- FIGURES ----

## ---- Figure 2 ----
# Degree distribution
upgrade_graph(gok)
g_net <- as.network(as.matrix(gok))
dist_fit <- NetworkExtinction::DegreeDistribution(g_net)

Fig2 <- dist_fit[["graph"]] +
  labs(y = "Cumulative degree distribution", x = "Degree (k)") +
  theme(axis.title.x = element_text(face = "bold", size = 16),
                   axis.title.y = element_text(face = "bold", size = 16),
                   axis.text.x = element_text(size = 12),
                   axis.text.y = element_text(size = 12),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# ggsave(filename = "Figuras/Fig2.png", plot = Fig2,
#        width = 10, units = "in", dpi = 600, bg = "white")

## ---- Figure 3 ----
# Gráficos comparativos índices de centralidad
par(mfrow = c(1,1))
set.seed(1)
deg_plot <- multiweb::plot_troph_level(gok, vertex.size=0.5*(V(gok)$degree.total), ylab = "Trophic level")
set.seed(1)
btw_plot <- multiweb::plot_troph_level(gok, vertex.size=sqrt(V(gok)$betweeness))
set.seed(1)
clo_plot <- multiweb::plot_troph_level(gok, vertex.size=10^3.2*(V(gok)$closeness))

## ---- Figure 4 ----
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

# ggsave(filename = "Figuras/Fig4.png", plot = Fig4,
#        width = 10, units = "in", dpi = 600, bg = "white")


# ---- SUP MATERIAL ----

## ---- Table S1 ----
tbl_s1 <- lista_interaccionesok %>% 
  dplyr::select(Prey, Predator, Reference, Link, Observations) %>% 
  distinct(Prey, Predator, .keep_all = TRUE)

## ---- Table S2 ----
tbl_s2 <- sp_level %>% 
  dplyr::select(name, degree.total, closeness, betweeness, TL, IEC)

