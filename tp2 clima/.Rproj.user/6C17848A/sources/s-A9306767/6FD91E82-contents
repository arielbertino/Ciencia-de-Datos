# Librerías usadas

# Instalar librerías
#install.packages("readxl")       
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("caret")

# Cargar librerías
library(readxl)
library(tidyverse)
library(ggplot2)
library(caret)

# Definición de variables

cantAnios <- 10
anioBase  <- 2010
rutaAndres  <- "~/Documentos/Cientifico de Datos/Practicos/datos-clima-tandil.xlsx"
rutaAriel <- "C:/Users/Ariel/Desktop/datos-clima-tandil.xlsx"
dfCompleto <- data.frame()
listaAnios <- list()
meses<- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
ordenMeses <- factor(meses)
options(digits=5,default=0)
# Definición de funciones
#-----------------------------------------------------------------------------------------------------------------

# 1. PREPARACION DE DATOS

# Función que convierte un dataframe a numeric
convertirDF <- function(df){
  dfConvertido <- apply(df,c(1,2),as.numeric)
  return(dfConvertido)
}

# Función que limpia un dataframe de ruido
# aclaración: las celdas sin valor las completamos con 0 (cero)
limpiarDataframe <- function(datgram){
  datgram <- as.data.frame(datgram)
  # Elimino las unidades dejando solo los numeros pero sigue siendo tipo string o character es lo mismo en R
  for (j in 1:nrow(datgram)){
    for (k in 1:ncol(datgram)) {
      if (class(datgram[j,k]) == "character")
        if (str_detect(datgram[j,k], "°C") | str_detect(datgram[j,k], "mm"))
          datgram[j,k] <- substr(datgram[j,k], 0, (nchar(datgram[j,k])-3 ))
        if (str_detect(datgram[j,k], "km"))
          datgram[j,k] <- substr(datgram[j,k], 0, (nchar(datgram[j,k])-5 ))
        if (str_detect(datgram[j,k], "hPa"))
          datgram[j,k] <- substr(datgram[j,k], 0, (nchar(datgram[j,k])-4 ))
        if (str_detect(datgram[j,k], "--"))
          datgram[j,k] <- "0"      # O nulo en su defecto
    }
  }
  #convierto todo el dataFrame a numerico
  datgram <- convertirDF(datgram)
  return(datgram)
}

# Función que genera una lista donde cada posicion es un datagrama con los datos de cada año
generarlistaAnios <- function(pat){
  auxL <- list()
  for (i in 0:cantAnios) {
    tablaExcel <- read_excel(pat, sheet=i+1)
    anio <- anioBase+i
    auxL[[paste(anio)]] <- limpiarDataframe(tablaExcel)
    auxL[[paste(anio)]] <- cbind(auxL[[paste(anio)]], "AÑO"=anio)
  }
  return(auxL)
}

# Funcion que unifica 
unificarDatos <- function(pat){
  aux <- data.frame()
  resul <- data.frame()
  for (i in 0:cantAnios) {
    tablaExcel <- read_excel(rutaAriel, sheet=i+1)
    anio <- anioBase+i
    aux <- data.frame(tablaExcel, "AÑO"=anio)   #esta linea esta agregando una columna más al final
    aux <- limpiarDataframe(aux)
    resul <- rbind(resul, aux)
  }
  return(resul)
}

# Funcion que muestra los primeros "CantFilas" registros de un datagrama
obtenerPrimeros <- function(df, cantFilas){
  if (class(df) == "data.frame" && cantFilas <= nrow(df) )  {
    resul <- head(df, cantFilas)
  } else if (class(df) == "list") {
    for (i in 1:length(df)){
      dfaux <-as.data.frame(df[[paste(anioBase+i)]])
      resul <- head(dfaux, cantFilas) 
    }
  }
  return(resul)
}

# Funcion que muestra los ultimos "cantFilas" registros de un datagrama
obtenerUltimos <- function(df, cantFilas){
  if (class(df) == "data.frame" && cantFilas <= nrow(df) )  {
    resul <- tail(df, cantFilas)
  } else if (class(df) == "list") {
    for (i in 0:length(df)){
      dfaux <-as.data.frame(df[[paste(anioBase+i)]])
      resul <- tail(dfaux, cantFilas)  
    }
  }
  return(resul)
}

# 2. ANALISIS PRELIMINAR DE LOS DATOS

# Tuvimos muchos problemas a la hora de generar los gráficos
# También quisimos realizar la parte de pasarle por parámetro
# el tipo de gráfico pero con ggplot se nos hizo muy complicado
# optamos por la siguiente función que directamente te genera
# el grafico con las tres líneas  inciso 2.1
generarGraficoLineaMultiple <- function(lista, anio){
  b <- lista[[paste(anio)]][,"T. MÍN"]
  c <- lista[[paste(anio)]][,"T. MÁX"]
  d <- lista[[paste(anio)]][,"T. MEDIA"]
  datu <- data.frame(meses,b,c,d)
  p <- ggplot(datu, aes(x=ordered(meses,ordenMeses), y=b, group=NA)) + geom_point(colour="red", size = 3) + geom_line(colour="red", size=1 )
  p <- p + labs(title = "TEMPERATIRA MÁXIMA(AZUL), MÍNIMA(NARANJA) Y MEDIA(ROJA)", subtitle = anio) + theme_minimal() + ylab("Temperatura")
  p <- p + theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) + xlab("Mes") 
  p <- p + geom_line(aes(y = c), color = "blue", size=1) + geom_point(aes(y = c),color="blue", size=3)  #azul
  p <- p + geom_line(aes(y = d), color = "orange", size=1) + geom_point(aes(y = d),color="orange", size=3)  #naranja
  return(p)
}

# Función que genera el grafico de barras inciso 2.2 
generarGraficoBarras <- function(lista, anio){
  b <- lista[[paste(anio)]][,"LLUVIA"]
  for (i in 2:length(b)){
    b[i] <- b[i] + b[i-1]
  }
  frape <- data.frame(meses,b)
  p <- ggplot(frape, aes(x=reorder(meses,b), y=b)) + theme_minimal() + ggtitle("PRECIPITACIÓN ACUMULADA (MM)")
  p <- p + theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1))
  p <- p + labs(subtitle=paste("La precipitación total de ", anio, "es: ", b[length(b)] ))+ xlab("Mes") + ylab("Precipitación")
  p <- p + geom_bar(stat="identity", position="stack",color="lightblue" , fill="lightblue")
  return(p)
}

# Función que genera el grafico de barras inciso 2.3
generarGraficoLinea <- function(lista, anio){
  b <- lista[[paste(anio)]][,"RACHAS MÁX"]
  df <- data.frame(meses, b)
  p <- ggplot(df, aes(x=ordered(meses,ordenMeses), y=b, group=NA)) + geom_line(colour="blue", size=2, alpha=0.3 )
  p <- p + labs(title = "VELOCIDAD MÁXIMA DEL VIENTO (KH/H)", subtitle = anio) + theme_minimal() + ylab("Velocidad Máxima")
  p <- p + theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) + xlab("Mes") 
  return(p)
}

# Función que  extrea un subdataframe inciso 2.4
generarFrame <- function(datos, desde, hasta){
  datos <- as.data.frame(datos)
  df <- datos[datos$AÑO>=desde&datos$AÑO<=hasta, c("MES","T..MÁX","T..MÍN","T..MEDIA","AÑO")]
  return(df)
}

# Función que grafica un subdataframe inciso 2.5
generarGraficoBarras2 <- function(df){
  #no puedo hacer que me ponga las tres temperauras
  #en su defecto explicar que hay que cambiar y=T..MÁX O y=T..MEDIA
  #y lo que está mostrando es la temp minima y maxima de la T..MÍN
  df <- as.data.frame(df)
  df[,"AÑO"] <- as.character(df[ ,"AÑO"])
  p <- ggplot(data=df, aes(x=AÑO, y=T..MÍN, fill=AÑO)) + geom_bar(stat="identity", position="dodge") 
  p <- p +theme(text = element_text(size=9), axis.text.x = element_text(angle=45, hjust=1))
  p <- p + labs(title = paste("TEMPERATURAS "), subtitle = paste("DESDE",min(df$AÑO),"HASTA",max(df$AÑO))) + theme_grey()
  return(p)
}

# Función que apatir del año, el conjunto de datos, el tipo de gráfico y el título del gráfico
# genera el grafico de los incisos 2.3 y 2.4
# Función muy precaria  inciso 2.6
generarGrafico <- function(datos, tipo, titulo){
  #datos <- as.data.frame(datos)
  #df <- datos[datos$AÑO==anio, c("MES","T..MÁX","T..MÍN","T..MEDIA","RACHAS.MÁX","AÑO")]
  #pFinal <- ggplot(data=df, aes(x=AÑO, y=T..MÍN, fill=AÑO))
  a <- numeric()
  if (tipo == "barras"){
    print("Usted está intentando relizar un gráfico de barras, introdusca:")
    print("Año de inicio: ")
    a <- readline(a)
    print("Año de fin: ")
    b <- numeric()
    b <- readline(b)
    df2 <- generarFrame(datos, a, b)
    grafico <- generarGraficoBarras2(df2) + labs(title=paste(titulo))
  }
  if (tipo == "lineas"){
    datos <- as.data.frame(datos)
    print("Usted está intentando relizar un gráfico de líneas, introdusca:")
    print("Año: ")
    a <- readline(a)
    b <- datos[datos$AÑO==a,c("RACHAS.MÁX")]
    df <- data.frame(meses, b)
    grafico <- ggplot(df, aes(x=ordered(meses,ordenMeses), y=b, group=NA)) + geom_line(colour="blue", size=2, alpha=0.4)
    grafico <- grafico + labs(title = paste(titulo), subtitle = a) + theme_minimal() + ylab("Temperatura")
    grafico <- grafico + theme(text = element_text(size=12), axis.text.x = element_text(angle=45, hjust=1)) + xlab("Mes") 
  }
  return(grafico)
}

# CREACIÓN DE UN MODELO PARA EL PRONÓSTICO DEL CLIMA

# Función que apartir del dataFrame con los datos de todo el pronóstico
# genera una prediccion del mes siguiente. Inciso 3.1
# Los resultados que muestra la siguiente función se 
# interpreta de la siguiente manera
#                 2.5 % 97.5 %
#(Intercept)       A      B
#datosDF[, "MES"]  C      D 
# Con un 97% de certeza la cuenta que define el valor a predecir es:
# valAPredecir = B + D*(diaSig)     
# donde diaSig para este caso es 6
predecirConRegresionLineal <- function(datosDF, datoAPredecir){
  datosDF <- as.data.frame(datosDF)
  modelo <- lm(datosDF[,paste(datoAPredecir)] ~ datosDF[,"MES"], datosDF)
  predicho <- confint(modelo)
  scatter.smooth(
    x = datosDF[,"MES"], xlab="MES",
    y = datosDF[,paste(datoAPredecir)], ylab=paste(datoAPredecir),
    main = paste("Estimación de", datoAPredecir," para el siguiente mes"),
    pch=1, cex=0.7, col="darkblue"
  )
  print(predicho)
}

#-----------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------

# Parte de Código que simula el main donde se llaman a todas las funciones

# Prueba inciso 1.1, 1.3 y 1.4 

dfCompleto <- unificarDatos(rutaAriel)
listaAnios <- generarlistaAnios(rutaAriel)

#print(listaAnios)                            #mostrar estructuras
#print(dfCompleto)

# Prueba inciso 1.2
#primeros <- obtenerPrimeros(dfCompleto,10)        #obtener primeros 10 registro      
#print(primeros)                                   #mostrar primeros 10 registro
#ultimos <- obtenerUltimos(dfCompleto,10)          #obtener últimos  10 registro    
#print(ultimos)                                    #mostrar últimos  10 registro

# Prueba inciso 2.1
#grafMulLines <- generarGraficoLineaMultiple(listaAnios,2019)  #generar graficos
#print(grafMulLines)                                           #mostrar graficos

# Prueba inciso 2.2
#grafBars <- generarGraficoBarras(listaAnios)
#print(grafBars)

# Prueba inciso 2.3
#grafLinea <- generarGraficoLinea(listaAnios, 2017)
#print(grafLinea)

# Prueba inciso 2.4
#subdf <- generarFrame(dfCompleto, 2015, 2019)                  #extraer subdataframe
#print(subdf)                                                   #mostrar subdataframe

# Prueba inciso 2.5
#grafBars2 <- generarGraficoBarras2(subdf)                      #generar el grafico del subdataframe
#print(grafBars2)                                               #mostrar el grafico del subdataframe

# Prueba inciso 2.6
#graficoX <- generarGrafico(dfCompleto,"lineas","Titulo Linea")  # Solo funcionar con tipo="barras" o tipo="linea
#print(graficoX)

# Prueba inciso 3.1
predecirConRegresionLineal(dfCompleto, "T..MÍN")