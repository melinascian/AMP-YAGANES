# Cruzar datos lista de especies2 y lista de interacciones. c/ las de tomas. Append y borrar duplicados
# Crear nueva tabla juntando las 2 y eliminar duplicados

#Seteo carpeta de trabajo
setwd("C:/AMPYAGANES/R")
library(dplyr)


#Traigo datos interacciones Tomas
# datos_interaccionesYo<-read.delim("lista_interacciones.txt")
# datos_interaccionesTomas<-read.csv("ListaInteracciones_AMPNBB_jan23.csv")
#datos_unidos_interacciones<-merge(x = datos_interaccionesYo, y = datos_interaccionesTomas, all = TRUE)



#Traigo datos interacciones especies
datos_especiesTomas<-read.csv("ListaEspecies_AMPNBB_jan23.csv")
datos_especiesTomas<-rename(datos_especiesTomas,"Especie trofica"="TrophicSpecies")
datos_especiesYo<-read.csv2("lista_especies2.csv")

datos_especiesYo<-rename(datos_especiesYo,"Especie trofica"="Especie.trofica")
datos_especiesTomas<-rename(datos_especiesTomas,"Filum"="Phylum")
datos_especiesTomas<-rename(datos_especiesTomas,"Clase"="Class")
datos_especiesTomas<-rename(datos_especiesTomas,"Orden"="Order")
datos_especiesTomas<-rename(datos_especiesTomas,"Familia"="Family")
datos_especiesTomas<-rename(datos_especiesTomas,"Genero"="Genus")


datos_join_especies<-merge(x = datos_especiesYo, y = datos_especiesTomas, all=TRUE)
df<-unique(datos_join_especies$`Especie trofica`)
                           