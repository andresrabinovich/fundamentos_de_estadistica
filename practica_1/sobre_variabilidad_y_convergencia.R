#Nos paramos en la práctica
setwd("~/fundamentos_de_estadistica/practica_1/")

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
  album              <- rep(0, tamano_del_album)
  paquetes_comprados <- 0
  while(album_lleno == FALSE){
    paquete            <- sample(1:tamano_del_album, tamano_del_paquete, replace = TRUE)
    album[paquete]     <- 1
    paquetes_comprados <- paquetes_comprados + 1
    if(sum(album) == tamano_del_album) album_lleno <- TRUE
  }
  return(paquetes_comprados*tamano_del_paquete)
}

#Simulamos 10000 álbumes de 6 figuritas cada uno con paquetes de 1 figurita
cantidad_iteraciones <- 10000
tamano_del_album     <- 6
tamano_del_paquete   <- 1
figus_totales = replicate(cantidad_iteraciones, cuantasFigus(tamano_del_album, tamano_del_paquete))


#Guardamos la simulacion
save(figus_totales, file=paste0("corridas/cantidad_de_paquetes_", tamano_del_album, "x", tamano_del_paquete, "x", 
                                cantidad_iteraciones, ".RData"))

#Histograma de la distribución
hist(figus_totales, breaks=30)

#Cuantas figuritas en media tenemos que comprar para llenar el album en función de la cantidad de iteraciones
colores          <- c("black", "red", "green", "blue", "orange")
for(color in colores){
  media            <- rep(0, 5)
  tamanos_muestras <- c(200, 500, 1000, 5000, 10000)
  for(i in 1:length(tamanos_muestras)){
    media[i] <- mean(sample(figus_totales, i, replace = FALSE))
  }
  
  #Graficamos la cantidad media de figuritas que tenemos que comprar en función de la cantidad de iteraciones
  if(color == "black"){
    plot(tamanos_muestras, media, col=color, ylim=c(5, 25))
  }else{
    points(tamanos_muestras, media, col=color)
  }
}
