#google y clima:
rm(list=ls())
gc()
library(tidyverse)
library(data.table)
library(lubridate)
library(viridis)
library(ggh4x)
library(grid)
library(egg)
library(magick)

source("scripts/Read NO2 data.R", encoding = "UTF-8")

#Movilidad:
movilidad <- fread("data/mobility/movilidad_ciudades_long.csv", encoding = "UTF-8")
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.IDate(x)
}
movilidad.mes <- movilidad %>% as.data.frame %>% setDT
movilidad.mes <- movilidad.mes[tipo == "residential"][,date := monthStart(date)][, .(valor = mean(value), std = sd(value)), by = .(region, date)]

#clima:
noaa <- fread("data/Weather/datos_noaa.csv", encoding = "UTF-8")

unique(noaa$NAME)

noaa <- noaa[NAME %chin% c("GALEAO ANTONIO CARLOS JOBIM, BR",
                   "JORGE CHAVEZ INTERNATIONAL, PE",
                   "MEXICO CITY, MX",
                   "BUENOS AIRES OBSERV, AR",
                   "BOGOTA ELDORADO, CO",
                   "SAO PAULO CUMBICA, BR"),]


region <- c("Bogota", "Buenos Aires", "Lima Province", 
            "Mexico City", "Rio de Janeiro", "São Paulo")


noaa[, region := fcase(NAME == "GALEAO ANTONIO CARLOS JOBIM, BR", "Rio de Janeiro",
                      NAME == "JORGE CHAVEZ INTERNATIONAL, PE", "Lima Province",
                      NAME == "MEXICO CITY, MX", "Mexico City",
                      NAME == "BUENOS AIRES OBSERV, AR", "Buenos Aires",
                      NAME == "BOGOTA ELDORADO, CO", "Bogota",
                      NAME == "SAO PAULO CUMBICA, BR", "São Paulo")]

noaa <- noaa[,date := monthStart(DATE)][, .(temperatura = mean(TAVG, na.rm = TRUE),
                                    temperatura_std = sd(TAVG, na.rm = TRUE),
                                    precipitaciones = sum(PRCP, na.rm = TRUE),
                                    elevacion = mean(ELEVATION, na.rm = TRUE)), by = .(region, date)]

#noaa <- noaa[year(date) %in% c(2020, 2019)]

#clima y no2:
no2.ciudades.mes <- lectura.no2.ciudades(mensual = TRUE)

noaa.no2 <- merge(noaa, no2.ciudades.mes, by.x = c("date", "region"), by.y = c("Fecha_datetime", "ciudad"))

noaa.no2[, c("NO2_trop_mean") := .(NO2_trop_mean * 1000000)]

#Con bogota:
(meses.flechas.centradas.long <-noaa.no2 %>%
    mutate(mes = month(date),
           anio = year(date),
           region_anio = interaction(region, anio),
           region = factor(region, levels = c("Buenos Aires", "Lima Province",
                                              "Mexico City", "Rio de Janeiro", "São Paulo", "Bogota"))) %>%
    filter(mes %in% c(2, 4)) %>%
    group_by(region, anio) %>%
    mutate(temp_feb = temperatura[mes == 2],
           dif_temp_feb = temperatura - temp_feb,
           no2_feb = NO2_trop_mean[mes == 2],
           dif_no2_feb = NO2_trop_mean - no2_feb) %>%
    ungroup() %>%
    ggplot(aes(x = dif_temp_feb, y = dif_no2_feb, label = mes, group = region_anio, color = as.factor(anio))) +#, alpha = if_else(grepl("Janeiro", region), 0.8, 0))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    #geom_text(aes(x = dif_temp_feb * 1.05, y = dif_no2_feb * 1.05), show.legend = FALSE) +
    geom_path(lwd = 2, arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches")), alpha = 0.8) +
    labs(x = "Temperature difference to February (C°)", y = expression(paste("April-February difference of monthly ", NO[2], " concentration average (", mu, "mol.", m^-2, ")", sep = "")), color = "Year") +
    scale_alpha(guide = 'none') +
    scale_color_viridis(discrete=TRUE) +
    facet_wrap(region~., ncol = 1) +
    theme_bw() +
    scale_y_continuous( position = "right") +
    theme(strip.background = element_blank(), strip.text.x = element_blank(),
          panel.spacing = unit(6, "mm"), legend.position = "top",
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", color = NA),
          axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10),
          plot.margin=unit(c(41,0,23.3*2, 54.1629 * 2 + (63 - 54.1629) * 2),"mm")))



meses.flechas.centradas.long <- set_panel_size(meses.flechas.centradas.long,
                                               width  = unit(54.1629, "mm"),
                                               height = unit(40, "mm"))

ggsave("figures and tables/NO2-T intermediate.png", meses.flechas.centradas.long, width = 210, height = 297, units = "mm", bg = "transparent")


#Pego mi imagen sobre la de manu:
imagen.manu <- image_read("figures and tables/NO2_art_300dpi.jpg")
imagen.mia <- image_read("figures and tables/NO2-T intermediate.png")
unidas <- image_composite(imagen.manu, imagen.mia)
image_write(unidas, 'figures and tables/NO2-T maps.png','png')
