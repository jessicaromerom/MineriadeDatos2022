library(readr)
library(tidyr)
library(dplyr)

data <- readRDS("../Taller2_JessicaRomero/data/data_raw/data_agvAPI.rds") #data anidada con todas las estaciones
metadata <- readRDS("../Taller2_JessicaRomero/data/data_raw/metadata_estaciones_agvAPI(1).rds")
station_agv_jessica <- readRDS("../Taller2_JessicaRomero/data/data_raw/station_id_agv_Jesica.rds")

#filtrar la data con las estaciones correspondientes (jessica)
ind <- metadata$serial %in% station_agv_jessica
data <- data[ind] 


col <- sapply(data,ncol) #todas las filas tienen las mismas columnas (2)
col
vars <- sapply (data, nrow)
vars


#variables unicas
vars_un <- unique(vars)
vars_un#seleccionar primero las estaciones que tienen un mismo comportamiento
#de estas hay que ver que tienen humedad de suelo
table(vars)#frecuencia de las estaciones y cuantas variables



#separar para cada caso
data_sep <- lapply(vars_un, function(un){data[un == vars]}) #entrega lista de datos anidados todavia
#agrupar los casos que solo tienen la misma cantidad de variable

#como ver si x estacion tiene variables d ehumedad de suelo
data_hum <- data_sep[c(1,2,5)]



#nombre de variables
names <- sapply(data_hum[[1]], purrr::pluck,1)#se le pide a data_hum  los nombres de la primera columna
#pluck me extrae el nombre de la primera columna 
##funcion que extrae elementos de la lista, en este caso los nombre
#sapply toma lista y luego funcion


##primer caso
#seleccionar las tres primeras filas
ind_7 <- lapply(data_hum[[1]], function(l){
  
  out <- l |>
  
  slice(1:3)#selecciona las filas variables de profundiad
  out$profundidad <- c(30,60,90)
  
  return(out)})

ind_8 <- lapply(data_hum[[2]], function(l){
  
  out <- l |>
    
    slice(1:3)#selecciona las filas variables de profundiad
  out$profundidad <- c(30,60,90)
  
  return(out)})


ind_9 <- lapply(data_hum[[3]], function(l){
  
  out <- l |>
    
    slice(2:4)#selecciona las filas variables de profundiad
  out$profundidad <- c(30,60,90)
  
  return(out)})


#desanidar datos (extraer los datos (las listas 120x2 listas) de la profundidad 30,60,90)

r7 <- lapply(seq_along(ind_7), function(i){ #seq_along(a) vector con dimension de a
  out <-  tryCatch(
    ind_7[[i]] |>
      tidyr::unnest("data") |>
      tidyr::hoist(3,"value") |>
      dplyr::select(1,2,3) ,
    error = function(e) NULL)
  
    if (!is.null(out)) out$serial <- ind[i]
    if(is.list(out)) out <- out |> unnest("value")#para desanidar la columna con filas aun anidadas (en esta caso tiene valur int)

  
  
  return(out)
  
})


r8 <- lapply(seq_along(ind_8), function(i){ #seq_along(a) vector con dimension de a
  out <-  tryCatch(
    ind_8[[i]] |>
      tidyr::unnest("data") |>
      tidyr::hoist(3,"value") |>
      dplyr::select(1,2,3) ,
    error = function(e) NULL)
  
  if (!is.null(out)) out$serial <- ind[i]
  if(is.list(out)) out <- out |> unnest("value")

  
  return(out)
  
})



r9 <- lapply(seq_along(ind_9), function(i){ #seq_along(a) vector con dimension de a
    out <-  tryCatch(
      ind_9[[i]] |>
        tidyr::unnest("data") |>
        tidyr::hoist(3,"value") |>
        dplyr::select(1,2,3) ,
      error = function(e) NULL)
    
    if (!is.null(out)) out$serial <-ind[i]
    if(is.list(out)) out <- out |> unnest("value")
    return(out)
    
})
  
  


#cuales son los nulos
nuls7<- sapply(r7, is.null)
nuls8<- sapply(r8, is.null)
nuls9<- sapply(r8, is.null)


#no nulos
data1 <- r7[!nuls7]
data2 <- r8[!nuls8]
data3 <- r9[!nuls9]
#une con rbind todo lo que esta en data1 #aqui ya se elimanron los nulos

data1_df <- do.call(rbind,data1)
data2_df <- do.call(rbind,data2)
data3_df <- do.call(rbind,data3)

data_final <- rbind(data1_df, data2_df, data3_df)


#ordenar la data_final de mayor a menor
#crear una columna de fecha y hora 
data_final_ordenada <- data_final |> arrange(desc(value)) %>% 
  separate(timestamp, sep=10, into=c("fecha", "hora"))
data_final_ordenada

#ver si hay valores nulos explicitos e implicitos

data_final_ordenada|> complete(fecha, hora, fill = list(value = NA))

data_final_ordenada |> count(value = NA)


