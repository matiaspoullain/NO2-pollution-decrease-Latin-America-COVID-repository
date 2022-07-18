# Lectura de datos de NO2 a tidy:
library(tidyverse)
library(data.table)
library(lubridate)

lectura.no2.ciudades <- function(satelite = "TROPOMI", mensual = FALSE){
  
  if(satelite == "TROPOMI"){
    camino <- "data/NO2/"
  }else if(satelite == "OMI"){
    camino <- "data/NO2/OMI/"
  }
  
  nombres.csv <- list.files(camino)
  
  nombres.csv <- nombres.csv[grepl("csv", nombres.csv)]
  
  if(mensual & satelite == "TROPOMI"){
    nombres.csv.diarios <- nombres.csv[grepl("mensual", fixed = TRUE, x = nombres.csv)]  
    #nombres.ciudades <- gsub("NO2_trop_mensual.csv","", fixed = TRUE, x = nombres.csv.diarios)
  }else{
    nombres.csv.diarios <- nombres.csv[grepl("diario", fixed = TRUE, x = nombres.csv)]
    #nombres.ciudades <- gsub("NO2_trop_diario.csv","", fixed = TRUE, x = nombres.csv.diarios)
  }
  

  nombres.ciudades.real <- c("Bogota", "Buenos Aires", "Lima Province", 
                             "Mexico City", "Rio de Janeiro", "São Paulo") 
  
  no2.ciudades.diario <- data.table()
  for(i in 1:length(nombres.ciudades.real)){
    df.it <- fread(paste0(camino, nombres.csv.diarios[i]), encoding = "UTF-8")
    
    
    
    if(mensual){
      df.it <- df.it[, .(Fecha_datetime, NO2_trop_mean, SEM)]
    }else{
      if(satelite == "TROPOMI"){
        df.it <- df.it[, .(Fecha_datetime, NO2_trop_mean, NO2_trop_std)]
        }
      }
    
    df.it[, ciudad := nombres.ciudades.real[i]]
    no2.ciudades.diario <- rbind(no2.ciudades.diario, df.it)
  }
  no2.ciudades.diario
}

