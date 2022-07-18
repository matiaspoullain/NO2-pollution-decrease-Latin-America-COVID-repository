rm(list=ls())
gc()
#Comparacion NO2 y google ciudades:

library(tidyverse)
library(data.table)
library(lubridate)
library(viridis)
library(ggh4x)
library(grid)
library(egg)
library(magick)

source("scripts/Read NO2 data.R", encoding = "UTF-8")

no2.bsas.tropomi <- lectura.no2.ciudades(mensual = TRUE)[, .(anio_mes = Fecha_datetime, NO2_trop_mean, ciudad)]

no2.bsas.tropomi[, satelite := "TROPOMI"]

fecha.max <- max(no2.bsas.tropomi$anio_mes)

no2.bsas.omi <- lectura.no2.ciudades(satelite = "OMI", mensual = FALSE)[, .(Fecha_datetime, NO2_trop_mean = no2_mean, ciudad)]

no2.bsas.omi[, satelite := "OMI"]

no2.bsas <- rbind(no2.bsas.tropomi, no2.bsas.omi[Fecha_datetime <= fecha.max, .(NO2_trop_mean = mean(NO2_trop_mean, na.rm = TRUE)), by = .(satelite, anio_mes = as.IDate(floor_date(Fecha_datetime, "month")), ciudad)])

no2.bsas.original <- no2.bsas

no2.bsas[, c("mes", "anio") := .(month(anio_mes), year(anio_mes))]

wide.df <- dcast(no2.bsas, anio_mes + ciudad + mes + anio ~ satelite, value.var = "NO2_trop_mean")

no2.bsas <- no2.bsas %>%
  arrange(anio_mes)

grilla <- expand.grid(anio_mes = seq(min(no2.bsas$anio_mes), max(no2.bsas$anio_mes), by = "month"),
                      ciudad = unique(no2.bsas$ciudad),
                      satelite = unique(no2.bsas$satelite)) %>%
  as.data.table

no2.bsas <- merge(grilla, no2.bsas, by = c("anio_mes", "ciudad", "satelite"), all.x = TRUE) %>%
  arrange(anio_mes)

no2.bsas[, dif_porcentual_mes_anterior := NO2_trop_mean / shift(NO2_trop_mean), by = .(ciudad, satelite)]
             

no2.bsas[, no2_febrero := NO2_trop_mean[mes == 2], by = .(anio, ciudad, satelite)]

no2.bsas[, diff_febrero := NO2_trop_mean - no2_febrero]

#con temperatura:
noaa <- fread("data/Weather/3013600.csv")

region <- c("Bogota", "Buenos Aires", "Lima Province", 
            "Mexico City", "Rio de Janeiro", "São Paulo")


noaa[, region := fcase(NAME == "GALEAO ANTONIO CARLOS JOBIM, BR", "Rio de Janeiro",
                       NAME == "JORGE CHAVEZ INTERNATIONAL, PE", "Lima Province",
                       NAME == "MEXICO CITY, MX", "Mexico City",
                       NAME == "BUENOS AIRES OBSERV, AR", "Buenos Aires",
                       NAME == "BOGOTA ELDORADO, CO", "Bogota",
                       NAME == "SAO PAULO CUMBICA, BR", "São Paulo")]

noaa[,date := floor_date(DATE, "month")]

noaa <- noaa[, .(temperatura = mean(TAVG, na.rm = TRUE),
             temperatura_std = sd(TAVG, na.rm = TRUE)), by = .(region, date)]

noaa.no2 <- merge(noaa, no2.bsas.original, by.x = c("date", "region"), by.y = c("anio_mes", "ciudad"))

noaa.no2[, c("NO2_trop_mean") := .(NO2_trop_mean * 1000000)]

#Por promedio de los años:

(meses.flechas.centradas.long <-noaa.no2 %>%
    mutate(mes = month(date),
           anio = year(date),
           region_anio = interaction(region, anio, satelite),
           region = factor(region, levels = c("Buenos Aires", "Lima Province",
                                              "Mexico City", "Rio de Janeiro", "São Paulo", "Bogota"))) %>%
    filter(mes %in% c(2, 4)) %>%
    group_by(region, anio, satelite) %>%
    mutate(temp_feb = temperatura[mes == 2],
           dif_temp_feb = temperatura - temp_feb,
           no2_feb = NO2_trop_mean[mes == 2],
           dif_no2_feb = NO2_trop_mean - no2_feb,
           pintado = if_else(anio <= 2019, "2004-2019", as.character(anio))) %>%
    ungroup() %>%
    group_by(region, pintado, satelite, mes) %>%
    summarise(dif_temp_feb = mean(dif_temp_feb), dif_no2_feb = mean(dif_no2_feb)) %>%
    ggplot(aes(x = dif_temp_feb, y = dif_no2_feb, group = interaction(region, pintado, satelite), color = pintado, linetype = satelite)) +#, alpha = if_else(grepl("Janeiro", region), 0.8, 0))) +
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
          legend.margin=margin(0,10,0,-10),
          legend.box.margin=margin(-10,-10,-10,-10),
          plot.margin=unit(c(41,0,23.3*2, 54.1629 * 2 + (63 - 54.1629) * 2),"mm")))


meses.flechas.centradas.long <- set_panel_size(meses.flechas.centradas.long,
                                               width  = unit(54.1629, "mm"),
                                               height = unit(40, "mm"))

ggsave("figures and tables/NO2-O intermediate.png", meses.flechas.centradas.long, width = 210, height = 297, units = "mm", bg = "transparent")

#Pego mi imagen sobre la de manu:
imagen.manu <- image_read("figures and tables/OMI.jpg")
imagen.mia <- image_read("figures and tables/NO2-O intermediate.png")
unidas <- image_composite(imagen.manu, imagen.mia)
image_write(unidas, 'figures and tables/NO2-O maps.png','png')
