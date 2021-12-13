#### Reemplazo de conteo vehicular por datos de google en prediccion de NO2:
rm(list=ls())
gc()
library(tidyverse)
library(data.table)
library(lubridate)
library(chron)

source("scripts/Read NO2 data.R", encoding = "UTF-8")

#Leo datos de google:
movilidad <- fread("data/mobility/movilidad_ciudades_long.csv", encoding = "UTF-8")
#Solo caba y residencial:
residencial.caba <- movilidad[region == "Buenos Aires" & tipo == "residential", .(date, value)]#movilidad[region == "Buenos Aires" & tipo == "residential" & date >= as.Date("2020-03-20"), .(date, value)]
#Agrego variable de tipo día:
feriados <- c("2019-01-01", "2019-03-04", "2019-03-05", "2019-03-24", "2019-04-02", "2019-04-18", "2019-04-19", "2019-04-20", "2019-04-21", "2019-04-25", "2019-04-26", "2019-04-27", "2019-04-24", "2019-05-01", "2019-05-25", "2019-06-17", "2019-06-20", "2019-07-08", "2019-07-09", "2019-08-11", "2019-08-17", "2019-08-19", "2019-10-12", "2019-10-14", "2019-11-18", "2019-12-08", "2019-12-25", "2020-01-01", "2020-02-24", "2020-02-25", "2020-03-23", "2020-03-24", "2020-03-31", "2020-04-09", "2020-04-10", "2020-04-15", "2020-04-16", "2020-04-24", "2020-05-01", "2020-05-25", "2020-06-15", "2020-06-20", "2020-07-09", "2020-07-10", "2020-08-17", "2020-10-12", "2020-11-23", "2020-12-07", "2020-12-08", "2020-12-25")
residencial.caba[, tipo_dia := fcase(as.character(date) %chin% feriados, "feriado",
                                     !(as.character(date) %chin% feriados) & is.weekend(date) , "finde",
                                     default = "semana")]

#Leo los datos de NO2 y clima:
no2 <- fread("data/NO2/Buenos AiresNO2_trop_diario.csv", encoding = "UTF-8")
no2 <- no2[, fecha := as.Date(paste0(Year, "-", Month, "-", Day))][, .(fecha, NO2_trop_mean)]

datos.smn <- fread("data/Weather/ortuzar_diario.csv")
datos.smn <- datos.smn %>%
  rename(pp = pp24,
         temperatura_media = T,
         temperatura_rocio = Td)
datos.smn[, fecha := as.Date(paste(Y, str_pad(M, 2, "left", "0"), str_pad(D, 2, "left", "0"), sep = "-"))]

datos.smn <- datos.smn[, .(fecha, V_mag_vectorial, temperatura_media)]

datos.smn.lag2 <- as.data.frame(datos.smn)
datos.smn.lag2 <- data.table(datos.smn.lag2)[, fecha := fecha + 2] %>%
  rename(temperatura_media_lag2 = temperatura_media) %>%
  select(fecha, temperatura_media_lag2)


#Leo los datos de peajes:
source("scripts/Read CABA tolls data.R")
peajes <- read_peajes_caba_diario()

#Junto todas las tablas:
datos <- residencial.caba %>%
  rename(fecha = date) %>%
  merge(no2) %>%
  merge(datos.smn[, -c("temperatura_media")]) %>%
  merge(datos.smn.lag2)




#Comparacion de modelos:
datos.cantidad.real <- datos %>%
  merge(peajes[, -c("feriado")])

datos[, tipo_dia := factor(tipo_dia, levels = c("semana", "finde", "feriado"))]

lm.vehiculos <- lm(log(NO2_trop_mean) ~ cantidad  + temperatura_media_lag2 + V_mag_vectorial, data = datos.cantidad.real)
lm.google <- lm(log(NO2_trop_mean) ~ value + tipo_dia  + temperatura_media_lag2 + V_mag_vectorial, data = datos)
lm.control <- lm(log(NO2_trop_mean) ~ temperatura_media_lag2 + V_mag_vectorial, data = datos)

anova(lm.vehiculos, lm.google, lm.control, test="Chisq")


#Hago la tabla par latex:
library(tidymodels)

tidy.vehiculos <- lm.vehiculos %>%
  tidy(conf.int = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, function(x){signif(x, digits = 2)}) %>%
  mutate(conf.vehiculos = paste0("[", conf.low, ";", conf.high, "]")) %>%
  select(term, conf.vehiculos)

tidy.google <- lm.google %>%
  tidy(conf.int = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, function(x){signif(x, digits = 2)}) %>%
  mutate(conf.google = paste0("[", conf.low, ";", conf.high, "]")) %>%
  select(term, conf.google)

tidy.control <- lm.control %>%
  tidy(conf.int = TRUE) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, function(x){signif(x, digits = 2)}) %>%
  mutate(conf.control = paste0("[", conf.low, ";", conf.high, "]")) %>%
  select(term, conf.control)


tabla.final <- merge(tidy.vehiculos, tidy.google, all = TRUE) %>%
  merge(tidy.control, all = TRUE) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                          term == "cantidad" ~ "V",
                          term == "temperatura_media_lag2" ~ "T_{lag2}",
                          term == "V_mag_vectorial" ~ "U",
                          term == "tipo_diafinde" ~ "W_e",
                          term == "tipo_diaferiado" ~ "W_h",
                          term == "value" ~ "R")) %>%
  t() %>%
  as.data.frame()

names(tabla.final) <- tabla.final[1,]

tabla.final <- tabla.final[-1,] 

rownames(tabla.final) <- c("Model (2)", "Model (3)", "Model (4)")

library(xtable)

print(xtable(tabla.final, type = "latex"), file = "figures and tables/Models table.tex")
