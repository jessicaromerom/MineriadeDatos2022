library(readr)
library(tidyr)
library(dplyr)

meta_agromet <- readRDS("../Taller_JessicaRomero/data/data_raw/metadata_estaciones_agrometAPI.rds")
meta_agv <- readRDS("../Taller_JessicaRomero/data/data_raw/metadata_estaciones_agvAPI(1).rds")
station_agromet_jessica <- readRDS("../Taller_JessicaRomero/data/data_raw/station_id_agromet_Jesica.rds")
station_agv_jessica <- readRDS("../Taller_JessicaRomero/data/data_raw/station_id_agv_Jesica.rds")
data_estaciones_agvAPI <- readRDS("../Taller2_JessicaRomero/data/data_raw/data_agvAPI.rds")
data_estaciones_agrometAPI <- readRDS("../Taller2_JessicaRomero/data/data_raw/data_estaciones_agrometAPI.rds")


meta_agromet_jess <- meta_agromet[meta_agromet$ema %in% station_agromet_jessica,]
meta_agv_jess <- meta_agv[meta_agv$serial %in% station_agv_jessica,]


data_agromet_jess <- data_estaciones_agrometAPI[data_estaciones_agrometAPI$station_id %in% station_agromet_jessica,]

data_agro <- data_estaciones_agrometAPI


#1.
##data_estaciones_agrometAPI
data_nest <- data_agromet_jess |>
  nest(data=2:13)
data_nest |> glimpse()
##guarda todos los datos en data y solo deja por fuera la station id que es po rla que agrupa

##otra opcion
data_nest2 <- data_agromet_jess |>
  group_by (station_id)|>
  nest()

data_nest2 |> glimpse()

#2.

data_nest |>
  slice(5) |>
  hoist(data, "humed_rel_promedio") |>
  select(1:2) |>
  unnest('humed_rel_promedio') |>
  slice(1)


#3

data_nest |> 
  hoist(data, hoisted_col ='precipitacion_horaria') |>
  unnest(hoisted_col) |> 
  #4
  select(-data)#elimina la columna data pero puede ser seleccionando todo menos esa (1:2)
##entra a ala columna data


#5

data_agro |>
  select(station_id, fecha_hora, precipitacion_horaria) |>
  complete(fecha_hora, station_id) |>
  glimpse() #mi NA sera de la precipitacion porque me esta haciendo el complete sobre feha y hpora y station_id


#6

data_agro |>
  fill(precipitacion_horaria, .direction = "down") #down es anterio sy up es el valor siguiente

##2
#2

library(lubridate)
data_agromet_jess |>
  mutate(fecha_hora = as_date(fecha_hora)) |>
  filter(fecha_hora >= ymd(20210501) & fecha_hora < ymd (20210801)) |>
  summarize(range(fecha_hora)) 

#3

data_agromet_jess |> 
  slice_sample(n=1000)

#4
data_agromet_jess |> 
  group_by(station_id) |>
  slice_max(precipitacion_horaria,n=100) 

#5

data_agromet_jess |>
  select(station_id, fecha_hora, contains("temp"))|>
  glimpse()
  
  
#6
data_agromet_jess %>%  
  select_if(., ~ !is.numeric(.))



##3
#1
data_agro |> 
  group_by(station_id)

#2
data_agro_prom <-
data_agro |> 
  group_by(station_id) |>
  summarise(across(contains("temp"),\(x) mean( x, na.rm = T), .names = "{.col}_avg"))

data_agro_prom
  
#3
data_agro_prom_names <- data_agro_prom |> 
  rename_with(contains("temp"),.fn = \(x) substr(x, start =1,stop =8))

data_agro_prom_names

data_agro_prom_names |>
  relocate(temp_max, .after = temp_pro) |>
  relocate(temp_min, .after = last_col())


#4
data_pivot_longer <- data_agro_prom_names|>
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) |>
#5
  arrange(desc(temp))


#6 
data_pivot_wider <- data_pivot_longer |>
  pivot_wider(names_from = var_temp, values_from = temp)  


#7
data_agro |>
  select(station_id, contains('temp')) |>
  pivot_longer(2:4, names_to = 'var_temp', values_to = 'temp',) |>
  group_by(station_id, var_temp) |>
  summarize(nas = sum(is.na(temp)))|>
  glimpse()


##4
#1
data_agro_prom_4 <-
  data_agro |>
  group_by(fecha_hora, station_id) |>
  summarise(across(contains("temp"),\(x) mean( x, na.rm = T), .names = "{.col}_avg"))
data_agro_prom_4

#2
data_agro_prom_4 |>
  group_by(station_id) |>
  summarise(across(everything(),\(x) mean( x, na.rm = T)))


#4 
data_agro_prom_4|>
  distinct(station_id)

#6 
data_agro_prom_4 |>
  right_join(meta_agromet_jess) |>
  glimpse()
