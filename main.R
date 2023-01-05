#HEAD ----


setwd("/home/augusto/Documents/tesis/")

library(ggplot2)
library(tidyverse)
library(data.table)
library(ndjson)
library (pander)
library(sf)
library(sp)
library(geosphere)


# #PROPERATI ----
# 
# #Creamos lista de los nombres de los archivos que queremos importar
# #all_files <- list.files(path ="data/properati/", full.names = T, include.dirs = F)
# 
# #Función que descomprime los archivos y transforma de json a data.table
# #json_to_frame <- function(json){
# #  ndjson::stream_in(json)
# #}
# 
# #Aplicamos json_to_frame a all_files y unimos todo en un sólo data.table
# #l <- lapply(all_files, json_to_frame)
# #df <- rbindlist(l, use.names = TRUE)
# 
# #rm(l)
# 
#mdf <- fread("data/properati_clean.csv", sep=",", showProgress =T, nThread =  8 )

df <- fread("data/properati_completo.csv", sep=",", showProgress =T, nThread =  8 )
 
 # 
# #Borramos observaciones sin coordenadas, precio o superficie
 df <- na.omit(df, cols=c("place.lat", 
                          "place.lon", 
                          "property.price", 
                          "property.surface_total")
               )
# 
# #Borramos las columnas country, property.description y property.title
 df <- df[, c("country", "property.description", "property.title"):=NULL]
# 
# #Borramos las columnas que cominezan con development
 drop_cols <- grep("development*", colnames(df))
# df <-df[, (drop_cols) := NULL]
# 
# #Filtramos que las propiedades sean tipo "Propiedad" y de Capital Federal
 df <- df[type_i18n == "Propiedad"]
 df <- df[place.l2 == "Capital Federal"]
 df <- df[property.operation != "Alquiler temporal"]
# 
# # Eliminamos los datos de ubicación de la propiedad. Vamos a usar las coordenadas
 drop_cols <- grep("place.l[1-6]{1}", colnames(df))
 df <- df[, (drop_cols) := NULL]
# 
 rm(drop_cols)
# 
 df <- df[, c("created_on", 
              "id", 
              "end_date", 
              "property.type", 
              "property.operation",  
              "property.price", 
              "property.currency", 
              "property.surface_total", 
              "place.lat", 
              "place.lon")
          ]
 
# # Eliminamos propiedades con precio 0 y superficie 0
 df <- df[property.price > 0]
 df <- df[property.surface_total > 0]

#Agregarmos serie de CCL a la base para sacar el precio del metro cuadrado en usd 
ccl <- 
  fread("https://apis.datos.gob.ar/series/api/series?ids=168.1_T_CAMBIDRS_D_0_0_29&collapse=day&start_date=2015-01-01&end_date=2020-11-01&limit=5000&format=csv", 
        sep=",", 
        header = T)

ccl[, tipo_cambio_implicito_en_adrs2 := approx(.I, tipo_cambio_implicito_en_adrs, .I)$y]

setkey(df, end_date)
setkey(ccl, indice_tiempo)

# Agregarmos la serie de CCL a la base
df <- ccl[df]
rm(ccl)

# Trasformamos todos los precios a dólares
df[, precio_usd := property.price][property.currency == "ARS", 
                                   precio_usd:= property.price/tipo_cambio_implicito_en_adrs]

#Calculamos el precio del metro cuadrado en dólares
df[, precio_m2 := precio_usd/property.surface_total]

#Importamos los polígonos de las comunas 
comunas <- st_read("/home/augusto/Documents/tesis/data/comunas.csv")
comunas <- comunas[,c('comunas')]
df <- st_as_sf(df, coords = c('place.lon', 'place.lat'))
df <- st_join(df, comunas, join=st_intersects)
df <- na.omit(df, cols=c('comunas'))
rm(comunas)
#ESPACIO PÚBLICO ----



#Importamos el shapefile
espacio_publico <- st_read("/home/augusto/Documents/tesis/data/espacio_verde_publico/espacio_verde_publico_WGS84.shp")


rm <- c("CANTERO CENTRAL", "BARRIO/COMPLEJO")
espacio_publico <-  espacio_publico %>% filter( !clasificac %in% rm)
write.csv(df, "properati_clean.csv", sep=",")
st_write(espacio_publico, 'espacio_publico_clean.shp')
  #write.csv(espacio_publico, "espacio_publico_clean.csv", sep=",")

#plot(st_geometry(espacio_publico), col = espacio_publico$SUP_TOTAL)
#plot(espacio_publico["area"], nbreaks = 30 )

#plot(df["property.type"])


#ggplot(data = "df") +
#  geom_point(ae)
#  )

# TRATAMIENTO Y CONTROL ----
df <- data.frame(df)
venta <- df[df$property.operation == "Venta",]
alquiler <- df[df$property.operation == "Alquiler",]


# coordenadas <- alquiler[,'geometry']
# 
# df_sf <- SpatialPointsDataFrame(coords = coordenadas,
#                                 data = venta)
# rm(coordenadas)
# rm(venta)
# rm(df)
# rm(alquiler)

# espacio_publico <- as_Spatial(espacio_publico)
espacio_publico <- data.frame(st_centroid(espacio_publico,))  
espacio_publico <- espacio_publico %>% mutate(lat = unlist(map(espacio_publico$geometry,1)),
         lon = unlist(map(espacio_publico$geometry,2)))

alquiler <- alquiler %>% mutate(lat = unlist(map(alquiler$geometry,1)),
                                      lon = unlist(map(alquiler$geometry,2)))


point_alquiler <- as.vector(alquiler[c('lon', 'lat')])
point_ep <- as.vector(espacio_publico[c("lon", "lat")])


# create distance matrix
mat <- distm(point_alquiler, point_ep, fun=distHaversine)
alquiler$distancia_minima <- max.col(-mat)

# https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
#dist.mat <- geosphere::dist2Line(p = df_sf, line = espacio_publico)
coordenas_pub <- espacio_p[c("lat", "lon")]



#points being the spatialpointsdataframe, and lines being your spatiallinesdataframe
# require(parallel)
# library(geosphere)
# distm (c(alquiler_p$lat, alquiler_p$lon), c(espacio_p$lat, espacio_p$lon), fun = distHaversine)
# 
# fun<- function(i) data.frame(dist2Line(p = df_sf, line = espacio_publico)) #some function like this
# 
# cl <- makeCluster(detectCores())
# clusterEvalQ(cl, { library("geosphere") }) #don't know what this does, but it's how i learned this. 
# clusterExport(cl, c("dist2Line", "df_sf", "espacio_publico")) #here you have to include all your objects and functions you want to use, and export them to a cluster, whatever that is.
results <- parLapply(cl,1:length(df_sf),fun=fun) #use parLapply to 'loop' through the points and return a list of dataframes. should be a list. 
# https://automating-gis-processes.github.io/site/notebooks/L3/nearest-neighbor-faster.html
# https://www.machinelearningplus.com/python/parallel-processing-python/#:~:text=In%20python%2C%20the%20multiprocessing%20module,in%20completely%20separate%20memory%20locations.