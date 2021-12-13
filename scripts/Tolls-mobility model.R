## Movilidad y peajes en CABA:
rm(list=ls())
gc()

library(tidyverse)
library(data.table)
library(lubridate)
library(chron)


#Leo los datos
source("scripts/Read CABA tolls data.R")
peajes <- read_peajes_caba_diario()

movilidad <- fread("data/mobility/movilidad_ciudades_long.csv", encoding = "UTF-8")
#Solo caba y residencial:
residencial.caba <- movilidad[region == "Buenos Aires" & tipo == "residential", .(date, value)]

#Los uno:
datos <- merge(peajes, residencial.caba, by.x = "fecha", by.y = "date")

#variable finde y cuarentena:
datos[, c("es_finde", "es_cuarentena") := .(is.weekend(fecha), fecha >= as.Date("2020-03-20"))]

#Solo para cuarentena:
datos.cuarentena <- datos[es_cuarentena == TRUE,]
datos.cuarentena[, tipo_dia := fcase(es_finde & feriado == FALSE, "finde",
                                     !es_finde & feriado == FALSE, "semana",
                                     feriado, "feriado")]

modelo.peajes.residencial.cuarentena <- lm(cantidad ~ value + tipo_dia, data = datos.cuarentena)

summary(modelo.peajes.residencial.cuarentena)

residuos <- rstandard(modelo.peajes.residencial.cuarentena)

datos.cuarentena.pred <- datos.cuarentena
datos.cuarentena.pred$pred <- predict(modelo.peajes.residencial.cuarentena)

#saco outliers:
outliers <- abs(residuos) >= 2.5

datos.cuarentena.sin.out <- datos.cuarentena[!outliers]

modelo.peajes.residencial.cuarentena.sin.out <- lm(cantidad ~ value + tipo_dia, data = datos.cuarentena.sin.out)

summary(modelo.peajes.residencial.cuarentena.sin.out)

