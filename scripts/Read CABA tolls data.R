#Lectura peajes CABA:
library(tidyverse)
library(data.table)
library(lubridate)

read_peajes_caba_diario <- function(ano.inicial = 2020, ano.final = 2020){
  agrupado <- data.table()
  
  for(i in ano.inicial:ano.final){
    print(i)
    autopistas <- fread(paste0("data/Tolls/flujo-vehicular-", i, ".csv"), encoding = "UTF-8")
    agrupado <- rbind(agrupado, autopistas %>%
                        arrange(fecha))
  }
  
  #feriados 2019-2020:
  feriados <- as.Date(c("2019-01-01", "2019-03-04", "2019-03-05", "2019-03-24", "2019-04-02", "2019-04-18", "2019-04-19", "2019-04-20", "2019-04-21", "2019-04-25", "2019-04-26", "2019-04-27", "2019-04-24", "2019-05-01", "2019-05-25", "2019-06-17", "2019-06-20", "2019-07-08", "2019-07-09", "2019-08-11", "2019-08-17", "2019-08-19", "2019-10-12", "2019-10-14", "2019-11-18", "2019-12-08", "2019-12-25", "2020-01-01", "2020-02-24", "2020-02-25", "2020-03-23", "2020-03-24", "2020-03-31", "2020-04-09", "2020-04-10", "2020-04-15", "2020-04-16", "2020-04-24", "2020-05-01", "2020-05-25", "2020-06-15", "2020-06-20", "2020-07-09", "2020-07-10", "2020-08-17", "2020-10-12", "2020-11-23", "2020-12-07", "2020-12-08", "2020-12-25"))
  agrupado[, feriado := as.Date(agrupado$fecha) %in% feriados]
  return(agrupado)
}