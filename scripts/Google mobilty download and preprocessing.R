rm(list=ls())
gc()
library(tidyverse)
library(data.table)
library(sgat)
#Borrar los csv que no sirven:

codigos <- c("AR", "PE", "BR", "MX", "CO")

datos <- mobility_var(codigos)

setDT(datos)

dir.create("data/mobility", showWarnings = FALSE)

dir.create("data/mobility/countries", showWarnings = FALSE)

for(i in codigos){
  nuevos <- datos[country_region_code == i,]
  nombre <- paste0("data/mobility/countries/movilidad_", i, ".csv")
  fwrite(nuevos, nombre)
}

subregiones <- c("Buenos Aires",
                 "Lima Province", 
                 "State of São Paulo",
                 "State of Rio de Janeiro",
                 "Mexico City",
                 "Bogota")

dir.create("data/mobility/cities", showWarnings = FALSE)

for(i in subregiones){
  nuevos <- datos[sub_region_1 == i,]
  nombre <- paste0("data/mobility/cities/movilidad_", i, ".csv")
  fwrite(nuevos, nombre)
}

subregiones.bis <- c("State of São Paulo", "State of Rio de Janeiro")
subregiones.2 <- c("São Paulo", "Rio de Janeiro")

for(i in 1:2){
  nuevos <- datos[sub_region_1 == subregiones.bis[i] & sub_region_2 == subregiones.2[i],]
  nombre <- paste0("data/mobility/cities/movilidad_", subregiones.2[i], ".csv")
  fwrite(nuevos, nombre)
}

#Agrupacion y formato long:
cities <- c("Buenos Aires",
              "Lima Province", 
              "São Paulo", "Rio de Janeiro",
              "Mexico City",
              "Bogota")

datos <- data.table()
for(i in cities){
  nombre <- paste0("data/mobility/cities/movilidad_", i, ".csv")
  nueva <- fread(nombre, encoding = "UTF-8", na.strings = c(''))
  if(sum(is.na(nueva$sub_region_2)) > 0){
    nueva <- nueva[is.na(sub_region_2),]
  }else if(min(nchar(nueva$sub_region_2)) == 0){
    nueva <- nueva[sub_region_2 == "",]
    nueva$sub_region_2 = NA
  }
  datos <- rbind(datos, nueva)
}

datos <- datos %>%
  mutate(region = if_else(is.na(sub_region_2), sub_region_1, sub_region_2),
         date = as.Date(date)) %>%
  select(-sub_region_1, -sub_region_2, -metro_area, -iso_3166_2_code, -census_fips_code, -place_id)

names(datos) <- gsub("_percent_change_from_baseline", "", names(datos))


datos <- melt(datos, id.vars = c("country_region_code","country_region", "region", "date"), variable.name = "tipo")

fwrite(datos, "data/mobility/movilidad_ciudades_long.csv")
