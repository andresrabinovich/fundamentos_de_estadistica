#Función que simula el llenado de un album de tamano_del_album figuritas suponiendo que cada paquete 
#tiene tamano_del_paquete figuritas.
#Devuelve la cantidad de figuritas que se tuvieron que comprar para llenar el album
#Parámetros:
#tamano_del_album = 6
#tamano_del_paquete = 1
#Devuelve:
#paquetes_comprados*tamano_del_paquete
cuantasFigus <- function(tamano_del_album = 6, tamano_del_paquete = 1){
  album_lleno        <- FALSE
  album              <- c()
  paquetes_comprados <- 0
  while(album_lleno == FALSE){
    paquete            <- sample(1:tamano_del_album, tamano_del_paquete, replace = TRUE)
    album              <- union(album, paquete)
    paquetes_comprados <- paquetes_comprados + 1
    if(length(album) == tamano_del_album) album_lleno <- TRUE
  }
  return(paquetes_comprados*tamano_del_paquete)
}

#Simulamos 1000 álbumes de 6 figuritas cada uno con paquetes de 1 figurita
cantidad_iteraciones <- 1000
tamano_del_album     <- 6
tamano_del_paquete   <- 1
figus_totales        <- rep(0, cantidad_iteraciones)
for(i in 1:cantidad_iteraciones){
  figus_totales[i] = cuantasFigus(tamano_del_album, tamano_del_paquete)
}

#Guardamos la simulacion
setwd("~/fundamentos_de_estadistica/practica_1/")
save(figus_totales, file=paste0("cantidad_de_paquetes_", tamano_del_album, "x", tamano_del_paquete, ".RData"))

#Histograma de la distribución
hist(figus_totales, breaks=30)

#Cuantas figuritas en media tenemos que comprar para llenar el album
mean(figus_totales)

#Probabilidad de completar el album con exactamente 16 figuritas
sum(figus_totales == 16)/cantidad_iteraciones

#Cuantas figuritas hay que comprar para tener una probabilidad de 90% de completar el álbum
sort(figus_totales)[0.9*cantidad_iteraciones]


