#VALOR CONSTANTES A LO LARGO DEL CODIGO
cantJugadas <- 100
Piedra = "PIEDRA"
Papel  = "PAPEL"
Tijera = "TIJERA"
#install.packages("ggplot2")
library(ggplot2)

#matriz de probabilidades resultante
matResultados <- matrix(0, nrow = 3, ncol = 3, byrow = FALSE)
#PIEDRA=1, PAPEL=2, TIJERA=3

#definimos vectores para las jugadas PIEDRA-PAPEL-TIJERA
jugadas1 <- vector(mode = "character", length = cantJugadas)
jugadas2 <- vector(mode = "character", length = cantJugadas)

#inicializamos dos vectores j1y j2 con valores aleatorios entre 0 y 1
j1 <- runif(cantJugadas, min=0, max=1)
j2 <- runif(cantJugadas, min=0, max=1)

#funcion que carga el vJR(vector de jugadas reales) con piedra papel o tijera
#basado en el vJA(vector de  jugadas aleatorias)
cargarVectorJugadas <- function(vJA, vJR){
  for(i in 1:cantJugadas){
    if ((vJA[i] >= 0) & (vJA[i] <= .33)){
      vJR[i] <- Piedra
    }
    else 
      if ((vJA[i] > 0.33) & (vJA[i] <= 0.66)){
        vJR[i] <- Papel
      }
      else 
        if ((vJA[i] > 0.66) & (vJA[i] <= 1)){
          vJR[i] <- Tijera
        }
  }
  return(vJR)
}

#funcion que simular el juego en si
jugar <- function (jug1,jug2,matResul){
  for(i in 1:cantJugadas){
    if (jug1[i] == jug2[i]){
      if (jug1[i] == Piedra){
        matResul[1,1] <- matResul[1,1] + 1
      } else if (jug1[i] == Papel){
          matResul[2,2] <- matResul[2,2] + 1
      } else if (jug1[i] == Tijera){
          matResul[3,3] <- matResul[3,3] + 1
      } 
      print(paste("Jugador1 y Jugador2 sacaron lo mismo:",jug1[1],", entonces es empate"))
    } else if ((jug1[i] == Piedra) & (jug2[i] == Tijera) ){
        matResul[1,3] <- matResul[1,3]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador1"))
    } else if ((jug1[i] == Papel) & (jug2[i] == Piedra) ){
        matResul[2,1] <- matResul[2,1]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador1"))
    } else if ((jug1[i] == Tijera) & (jug2[i] == Papel) ){
        matResul[3,2] <- matResul[3,2]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador1"))
    } else if ((jug2[i] == Piedra) & (jug1[i] == Tijera) ){
        matResul[3,1] <- matResul[3,1]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador2"))
    } else if ((jug2[i] == Papel) & (jug1[i] == Piedra) ){
        matResul[1,2] <- matResul[1,2]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador2"))
    } else if ((jug2[i] == Tijera) & (jug1[i] == Papel) ){
        matResul[2,3] <- matResul[2,3]+1
        print(paste("Jugador1:" ,jug1[i],", Jugador2:",jug2[i], ", ganador: jugador2"))
    }
    print("---------------------------------------------------------------------------")
  }
  matResul <- matResul/cantJugadas #genera matris de probabilidades
  return(matResul)
}

# print(j1)    se carga bien
# print(j2)    se carga bien
jugadas1 <- cargarVectorJugadas(j1,jugadas1)
jugadas2 <- cargarVectorJugadas(j2,jugadas2)
# print(jugadas1)   se carga bien
# print(jugadas2)   se carga bien
matResultados <- jugar(jugadas1, jugadas2, matResultados)
print(matResultados)

# matResultadosDf <- as.data.frame(t(matResultados))  #transforma una matriz en un dataFrame de R