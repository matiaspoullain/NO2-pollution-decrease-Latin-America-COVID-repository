#Tabla resumen:

library(tidyverse)
library(data.table)
library(scales)

source("scripts/Read NO2 data.R", encoding = "UTF-8")
#no2
Sys.setlocale("LC_TIME", "English")

no2.ciudades <- lectura.no2.ciudades(mensual = TRUE)

no2.ciudades <- no2.ciudades[between(as.Date(Fecha_datetime), as.IDate("2020-02-01"), as.IDate("2020-12-31")),]

no2.ciudades[, mes := month(Fecha_datetime, label = TRUE, abbr = TRUE)]

no2.ciudades[, c("NO2_trop_mean", "SEM") := .(1000000 * NO2_trop_mean, 1000000 * SEM)]


#Movilidad
movilidad <- fread("data/mobility/movilidad_ciudades_long.csv", encoding = "UTF-8")

movilidad[, mes := month(date, label = TRUE, abbr = TRUE)]
movilidad <- movilidad[tipo == "residential", .(media = mean(value),
                                                desvio = sd(value)/.N), by = .(region, mes)]

#union:
union <- merge(movilidad, no2.ciudades[, -c("Fecha_datetime")], by.x = c("region", "mes"), by.y = c("ciudad", "mes"))

variables <- c("media", "desvio", "NO2_trop_mean", "SEM")

union[, c("media", "desvio", "NO2_trop_mean", "SEM") := .(format(round(media, 2), nsmall = 2),
                                                          format(round(desvio, 2), nsmall = 2),
                                                          format(round(NO2_trop_mean, 2), nsmall = 2),
                                                          format(round(SEM, 2), nsmall = 2))]

union[, c("label_movi", "label_no2") := .(paste0(media, " (", trimws(desvio),")"),
                                          paste0(NO2_trop_mean, " (", trimws(SEM), ")"))]

union <- union[, .(region, mes, label_movi, label_no2)]

union <- union %>%
  mutate(region = case_when(region == "Bogota" ~ "Bogotá",
                            region == "Lima Province" ~ "Lima",
                            region == "Mexico City" ~ "México DF",
                            TRUE ~ region))

formateado <- union %>%
  melt(id = c("region", "mes")) %>%
  dcast(mes + variable ~ region, value.var = "value") %>%
  mutate(variable = case_when(variable == "label_movi" ~ "Residential variation", 
                              variable == "label_no2" ~ "NO2 concentration")) %>%
  arrange(variable, mes)

names(formateado)[1:2] <- c("Month", "Variable")


tabla.no2 <- formateado[Variable == "NO2 concentration",]
tabla.no2$Variable <- NULL
nombres.ciudades <- names(tabla.no2)[names(tabla.no2) != "Month"]

enes <- c(367, 259, 1986, 1165, 750, 633)

#names(tabla.no2)[names(tabla.no2) != "Month"] <- paste0(nombres.ciudades, " (n = ", enes, ")")
tabla.no2[2:(1+nrow(tabla.no2))] <- tabla.no2[1:nrow(tabla.no2)]

for(i in nombres.ciudades){
  tabla.no2[1, i] <- paste0("(n=", enes[which(i == nombres.ciudades)], ")")
}

tabla.no2[1, 1] <- NA

tabla.movi <- formateado[Variable == "Residential variation",]
tabla.movi$Variable <- NULL



library(xtable)
print(xtable(tabla.no2, type = "latex"), file = "figures and tables/NO2 summary table.tex", include.rownames=FALSE)
print(xtable(tabla.movi, type = "latex"), file = "figures and tables/Mobility summary table.tex", include.rownames=FALSE)

