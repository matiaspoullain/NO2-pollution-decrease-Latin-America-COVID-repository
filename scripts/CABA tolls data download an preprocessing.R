rm(list = ls())
gc()

library(tidyverse)
library(data.table)
library(lubridate)

datos <- fread("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ausa/flujo-vehicular-por-unidades-peaje-ausa/flujo-vehicular-2020.csv")

datos[, fecha_hora := as_datetime(paste0(substr(fecha, 1, 10), " ", str_pad(as.character(hora_inicio), width = 2, pad = "0"), ":00:00"))]
datos <- datos[, .(cantidad = round(sum(cantidad_pasos))), by = .(fecha_hora)]
datos[, fecha := as.Date(fecha_hora)]
datos <- datos[, .(cantidad = sum(cantidad)), by = fecha]

fwrite(datos, "data/Tolls/flujo-vehicular-2020.csv")
