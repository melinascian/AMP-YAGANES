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
<<<<<<< HEAD
datos2<-read.csv("listainteracciones.csv")

#Filtro lista de especies por Expertise
Bentos<-filter(datos, Expertise =="Bentos")
Mamiferos_marinos<-filter(datos, Expertise == "Mamiferos marinos")
Aves_marinas<-filter(datos, Expertise == "Aves marinas")
Ascidias<-filter(datos, Expertise == "Ascidias")
Peces<-filter(datos, Expertise == "Peces")
Condrictios<-filter(datos, Expertise == "Condrictios")
Cnidarios<-filter(datos, Expertise == "Cnidarios")
Porifera<-filter(datos, Expertise == "Porifera")
Poliplacoforos<-filter(datos, Expertise == "Poliplacoforos")
Braquiopodos<-filter(datos, Expertise == "Braquiopodos")
Gasteropodos<-filter(datos, Expertise == "Gasteropodos")
Cefalopodos<-filter(datos, Expertise == "Cefalopodos")
Erizos_de_mar<-filter(datos, Expertise == "Erizos de mar")
Poliquetos<-filter(datos, Expertise == "Poliquetos")
Crustaceos<-filter(datos, Expertise == "Crustaceos")
Crustaceos_isopodos<-filter(datos, Expertise == "Crustaceos isopodos")
Crustaceos_peracaridos<-filter(datos, Expertise == "Crustaceos peracaridos")
Holoturoideos<-filter(datos, Expertise == "Holoturoideos")
Ofiuroideos<-filter(datos, Expertise == "Ofiuroideos")
Asteroideos<-filter(datos, Expertise == "Asteroideos")
Sardinas<-filter(datos, Expertise == "Sardinas")
Fitoplancton<-filter(datos, Expertise == "Fitoplancton")
Zooplancton<-filter(datos, Expertise == "Zooplancton")
Ictioplancton<-filter(datos, Expertise == "Ictioplancton")
Peces_demersales<-filter(datos, Expertise == "Peces demersales")

#Guardo cada lista filtrada
write.csv(Bentos, "Bentos.csv")
write.csv(Mamiferos_marinos, "Mamiferos_marinos.csv")
write.csv(Aves_marinas, "Aves_marinas.csv")
write.csv(Ascidias, "Ascidias.csv")
write.csv(Peces, "Peces.csv")
write.csv(Condrictios, "Condrictios.csv")
write.csv(Cnidarios, "Cnidarios.csv")
write.csv(Porifera, "Porifera.csv")
write.csv(Poliplacoforos, "Poliplacoforos.csv")
write.csv(Braquiopodos, "Braquiopodos.csv")
write.csv(Gasteropodos, "Gasteropodos.csv")
write.csv(Cefalopodos, "Cefalopodos.csv")
write.csv(Erizos_de_mar, "Erizos_de_mar.csv")
write.csv(Poliquetos, "Poliquetos.csv")
write.csv(Crustaceos, "Crustaceos.csv")
write.csv(Crustaceos_isopodos, "Crustaceos_isopodos.csv")
write.csv(Crustaceos_peracaridos, "Crustaceos_peracaridos.csv")
write.csv(Holoturoideos, "Holoturoideos.csv")
write.csv(Ofiuroideos, "Ofiuroideos.csv")
write.csv(Asteroideos, "Asteroideos.csv")
write.csv(Sardinas, "Sardinas.csv")
write.csv(Fitoplancton, "Fitoplancton.csv")
write.csv(Zooplancton, "Zooplancton.csv")
write.csv(Ictioplancton, "Ictioplancton.csv")
write.csv(Peces_demersales, "Peces_demersales.csv")
=======
datos <- read.csv("listaespecies_TIM.csv")
>>>>>>> 3da45d414887e6245854a5f18fb20ab2f359b14f


#Filtro lista de interacciones por expertise
Bentos_presa<-filter(datos2, Expertise.presa =="Bentos")
Bentos_depredador<-filter(datos2, Expertise.depredador =="Bentos")
write.csv(Bentos_presa, "Bentos_presa.csv")
write.csv(Bentos_depredador, "Bentos_depredador.csv")

Mamiferos_marinos_presa<-filter(datos2, Expertise.presa == "Mamiferos marinos")
Mamiferos_marinos_depredador<-filter(datos2, Expertise.depredador == "Mamiferos marinos")
write.csv(Mamiferos_marinos_presa, "Mamiferos_marinos_presa.csv")
write.csv(Mamiferos_marinos_depredador, "Mamiferos_marinos_depredador.csv")

Aves_marinas_presa<-filter(datos2, Expertise.presa == "Aves marinas")
Aves_marinas_depredador<-filter(datos2, Expertise.depredador == "Aves marinas")
write.csv(Aves_marinas_presa, "Aves_marinas_presa.csv")
write.csv(Aves_marinas_depredador, "Aves_marinas_depredador.csv")

Ascidias_presa<-filter(datos2, Expertise.presa == "Ascidias")
Ascidias_depredador<-filter(datos2, Expertise.depredador == "Ascidias")
write.csv(Ascidias_presa, "Ascidias_presa.csv")
write.csv(Ascidias_depredador, "Ascidias_depredador.csv")

Peces_presa<-filter(datos2, Expertise.presa == "Peces")
Peces_depredador<-filter(datos2, Expertise.depredador == "Peces")
write.csv(Peces_presa, "Peces_presa.csv")
write.csv(Peces_depredador, "Peces_depredador.csv")

Condrictios_presa<-filter(datos2, Expertise.presa == "Condrictios")
Condrictios_depredador<-filter(datos2, Expertise.depredador == "Condrictios")
write.csv(Condrictios_presa, "Condrictios_presa.csv")
write.csv(Condrictios_depredador, "Condrictios_depredador.csv")

Cnidarios_presa<-filter(datos2, Expertise.presa == "Cnidarios")
Cnidarios_depredador<-filter(datos2, Expertise.depredador == "Cnidarios")
write.csv(Cnidarios_presa, "Cnidarios_presa.csv")
write.csv(Cnidarios_depredador, "Cnidarios_depredador.csv")

Porifera_presa<-filter(datos2, Expertise.presa == "Porifera")
Porifera_depredador<-filter(datos2, Expertise.depredador == "Porifera")
write.csv(Porifera_presa, "Porifera_presa.csv")
write.csv(Porifera_depredador, "Porifera_depredador.csv")

Poliplacoforos_presa<-filter(datos2, Expertise.presa == "Poliplacoforos")
Poliplacoforos_depredador<-filter(datos2, Expertise.depredador == "Poliplacoforos")
write.csv(Poliplacoforos_presa, "Poliplacoforos_presa.csv")
write.csv(Poliplacoforos_depredador, "Poliplacoforos_depredador.csv")

Braquiopodos_presa<-filter(datos2, Expertise.presa == "Braquiopodos")
Braquiopodos_depredador<-filter(datos2, Expertise.depredador == "Braquiopodos")
write.csv(Braquiopodos_presa, "Braquiopodos_presa.csv")
write.csv(Braquiopodos_depredador, "Braquiopodos_depredador.csv")

Gasteropodos_presa<-filter(datos2, Expertise.presa == "Gasteropodos")
Gasteropodos_depredador<-filter(datos2, Expertise.depredador == "Gasteropodos")
write.csv(Gasteropodos_presa, "Gasteropodos_presa.csv")
write.csv(Gasteropodos_depredador, "Gasteropodos_depredador.csv")

Cefalopodos_presa<-filter(datos2, Expertise.presa == "Cefalopodos")
Cefalopodos_depredador<-filter(datos2, Expertise.depredador == "Cefalopodos")
write.csv(Cefalopodos_presa, "Cefalopodos_presa.csv")
write.csv(Cefalopodos_depredador, "Cefalopodos_depredador.csv")

Erizos_de_mar_presa<-filter(datos2, Expertise.presa == "Erizos de mar")
Erizos_de_mar_depredador<-filter(datos2, Expertise.depredador == "Erizos de mar")
write.csv(Erizos_de_mar_presa, "Erizos_de_mar_presa.csv")
write.csv(Erizos_de_mar_depredador, "Erizos_de_mar_depredador.csv")

Poliquetos_presa<-filter(datos2, Expertise.presa == "Poliquetos")
Poliquetos_depredador<-filter(datos2, Expertise.depredador == "Poliquetos")
write.csv(Poliquetos_presa, "Poliquetos_presa.csv")
write.csv(Poliquetos_depredador, "Poliquetos_depredador.csv")

Crustaceos_presa<-filter(datos2, Expertise.presa == "Crustaceos")
Crustaceos_depredador<-filter(datos2, Expertise.depredador == "Crustaceos")
write.csv(Crustaceos_presa, "Crustaceos_presa.csv")
write.csv(Crustaceos_depredador, "Crustaceos_depredador.csv")

Crustaceos_isopodos_presa<-filter(datos2, Expertise.presa == "Crustaceos isopodos")
Crustaceos_isopodos_depredador<-filter(datos2, Expertise.depredador == "Crustaceos isopodos")
write.csv(Crustaceos_isopodos_presa, "Crustaceos_isopodos_presa.csv")
write.csv(Crustaceos_isopodos_depredador, "Crustaceos_isopodos_depredador.csv")

Crustaceos_peracaridos_presa<-filter(datos2, Expertise.presa == "Crustaceos peracaridos")
Crustaceos_peracaridos_depredador<-filter(datos2, Expertise.depredador == "Crustaceos peracaridos")
write.csv(Crustaceos_peracaridos_presa, "Crustaceos_peracaridos_presa.csv")
write.csv(Crustaceos_peracaridos_depredador, "Crustaceos_peracaridos_depredador.csv")

Holoturoideos_presa<-filter(datos2, Expertise.presa == "Holoturoideos")
Holoturoideos_depredador<-filter(datos2, Expertise.depredador == "Holoturoideos")
write.csv(Holoturoideos_presa, "Holoturoideos_presa.csv")
write.csv(Holoturoideos_depredador, "Holoturoideos_depredador.csv")

Ofiuroideos_presa<-filter(datos2, Expertise.presa == "Ofiuroideos")
Ofiuroideos_depredador<-filter(datos2, Expertise.depredador == "Ofiuroideos")
write.csv(Ofiuroideos_presa, "Ofiuroideos_presa.csv")
write.csv(Ofiuroideos_depredador, "Ofiuroideos_depredador.csv")

Asteroideos_presa<-filter(datos2, Expertise.presa == "Asteroideos")
Asteroideos_depredador<-filter(datos2, Expertise.depredador == "Asteroideos")
write.csv(Asteroideos_presa, "Asteroideos_presa.csv")
write.csv(Asteroideos_depredador, "Asteroideos_depredador.csv")

Sardinas_presa<-filter(datos2, Expertise.presa == "Sardinas")
Sardinas_depredador<-filter(datos2, Expertise.depredador == "Sardinas")
write.csv(Sardinas_presa, "Sardinas_presa.csv")
write.csv(Sardinas_depredador, "Sardinas_depredador.csv")

Fitoplancton_presa<-filter(datos2, Expertise.presa == "Fitoplancton")
Fitoplancton_depredador<-filter(datos2, Expertise.depredador == "Fitoplancton")
write.csv(Fitoplancton_presa, "Fitoplancton_presa.csv")
write.csv(Fitoplancton_depredador, "Fitoplancton_depredador.csv")

Zooplancton_presa<-filter(datos2, Expertise.presa == "Zooplancton")
Zooplancton_depredador<-filter(datos2, Expertise.depredador == "Zooplancton")
write.csv(Zooplancton_presa, "Zooplancton_presa.csv")
write.csv(Zooplancton_depredador, "Zooplancton_depredador.csv")

Ictioplancton_presa<-filter(datos2, Expertise.presa == "Ictioplancton")
Ictioplancton_depredador<-filter(datos2, Expertise.depredador == "Ictioplancton")
write.csv(Ictioplancton_presa, "Ictioplancton_presa.csv")
write.csv(Ictioplancton_depredador, "Ictioplancton_depredador.csv")

Peces_demersales_presa<-filter(datos2, Expertise.presa == "Peces demersales")
Peces_demersales_depredador<-filter(datos2, Expertise.depredador == "Peces demersales")
write.csv(Peces_demersales_presa, "Peces_demersales_presa.csv")
write.csv(Peces_demersales_depredador, "Peces_demersales_depredador.csv")


