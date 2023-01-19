library(dplyr)

#Seteo carpeta de trabajo
setwd("C:/AMPYAGANES/R")

#Llamo a los datos
datos<-read.csv("listaespecies.csv")


#class(datos$`Especie.trófica`)
#class(datos$Expertise)
#class(datos)

#Lista de especies
#datos <- read.csv("listaespecies.csv")
#colnames(datos)
#datos<- datos %>% 
#  add_count(Expertise, name = "Bentos") %>% 
#  mutate(Expertise = fct_reorder(Expertise, Bentos, .desc = TRUE))


#Crear nuevo dataframe con el nombre de la especie. Filtro tabla original por columna.
bentos <- filter(datos, Expertise == "Bentos")

#View(bentos)

#Guardo en csv el dataframe filtrado por la especie elegida
write.csv(bentos, "bentos.csv")
