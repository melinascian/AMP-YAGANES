library(dplyr)

#
## Al crear un R project el working directory se setea automáticamente
#
#Seteo carpeta de trabajo
setwd("C:/AMPYAGANES/R")

#
## Cuando intento cargar el archivo me tira el error "Error in make.names(col.names, unique = TRUE) : invalid multibyte string 6"
# Las últimas dos columnas están casi vacías. Modifiqué el archivo y cree uno nuevo "listaespecies_TIM.csv"
#
#Llamo a los datos
datos<-read.csv("listaespecies.csv")
datos <- read.csv("listaespecies_TIM.csv")


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
