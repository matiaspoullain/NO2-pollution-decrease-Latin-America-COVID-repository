rm(list=ls())
gc()
#Comparacion NO2 y google ciudades:

library(tidyverse)
library(data.table)
library(lubridate)
source("scripts/Read NO2 data.R", encoding = "UTF-8")

#Mensual:
no2.ciudades.mes <- lectura.no2.ciudades(mensual = TRUE)
movilidad <- fread("data/mobility/movilidad_ciudades_long.csv", encoding = "UTF-8")


#Diferencias con 2019:
no2.ciudades.abril <- no2.ciudades.mes[month(Fecha_datetime) == 4,]

no2.ciudades.abril <- dcast(no2.ciudades.abril, ciudad ~ Fecha_datetime, value.var = "NO2_trop_mean")

no2.ciudades.abril[, dif := `2019-04-01` - `2020-04-01`]

no2.ciudades.abril[dif == min(dif)]

no2.ciudades.abril[dif == max(dif)]

no2.ciudades.abril[, dif.porcentual := ((`2019-04-01` - `2020-04-01`)/`2019-04-01`) * 100]

no2.ciudades.abril[dif.porcentual == min(dif.porcentual)]

no2.ciudades.abril[dif.porcentual == max(dif.porcentual)]

names(no2.ciudades.mes) <- c("date", "valor", "std", "region")

no2.ciudades.mes$base <- "no2"

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.IDate(x)
}

movilidad.mes <- movilidad %>% as.data.frame %>% setDT

movilidad.mes <- movilidad.mes[tipo == "residential"][,date := monthStart(date)][, .(valor = mean(value), std = sd(value)), by = .(region, date)]

movilidad.mes$base <- "google"

movilidad.no2.mes <- rbind(movilidad.mes, no2.ciudades.mes)


#Scatter con saltos marzo-abril:
#Primero calculo el salto de marzo-abril:
marzo.abril <- movilidad.no2.mes[as.character(month(date)) %chin% c("2", "4") & year(date) == 2020,]

marzo.abril.mean <- marzo.abril %>%
  dcast(region + base ~ date, value.var = "valor") %>%
  mutate(salto_mean_no2 = `2020-04-01` - `2020-02-01`,
         salto_mean_no2_porcentual = 100 * salto_mean_no2 / `2020-02-01`) %>%
  select(region, base, salto_mean_no2, salto_mean_no2_porcentual) %>%
  arrange(region, base)

marzo.abril.std <- marzo.abril %>%
  dcast(region + base ~ date, value.var = "std") %>%
  mutate(salto_std_no2 = sqrt(`2020-04-01`^2 + `2020-02-01`^2)) %>%
  select(region, base, salto_std_no2) %>%
  arrange(region, base)

marzo.abril.salto <- cbind(marzo.abril.mean, marzo.abril.std %>%
        select(salto_std_no2)) %>%
  mutate(salto_std_no2_porcentual = salto_std_no2 * salto_mean_no2_porcentual / salto_mean_no2)


#Con poblacion:
region <- c("Bogota", "Buenos Aires", "Lima Province", 
  "Mexico City", "Rio de Janeiro", "São Paulo")

poblacion <- c(11.7*1000000, 13641973, 10.8 *1000000 , 	21828944 , 11902701, 12.33*1000000)/ 1000000

region.poblacion <- data.table(region, poblacion)


(scatter.saltos.poblacion <- marzo.abril.salto %>%
  merge(region.poblacion)%>%
  mutate(region = case_when(region == "Bogota" ~ "Bogotá",
                            region == "Lima Province" ~ "Lima",
                            region == "Mexico City" ~ "México DF",
                            TRUE ~ region),
    region = fct_rev(fct_reorder(region, salto_mean_no2, max)),
         salto_mean_no2 = if_else(base == "no2", salto_mean_no2_porcentual/poblacion, salto_mean_no2)) %>%
  dcast(region ~ base, value.var = "salto_mean_no2") %>%
  ggplot(aes(x = no2, y = google))+#, col = region)) +
  geom_point(size = 10) +
  #ggtitle("Diferencias abril-febrero") +
  labs(x = bquote(April-February~NO[2]~concentration~percentage~difference~.("(% per million inhabitants)")),
       y = "April-February residential variation percentage difference (%)",
       color = "City") +
    theme_bw() +
   # scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "top") +
  geom_text(aes(label = region), vjust = 2.5))

ggsave("figures and tables/scatter.png", scatter.saltos.poblacion, width = 10, height = 7.5)


df.cor.pob <- marzo.abril.salto %>%
  merge(region.poblacion)%>%
  mutate(region = fct_rev(fct_reorder(region, salto_mean_no2, max)),
         salto_mean_no2 = if_else(base == "no2", salto_mean_no2_porcentual/poblacion, salto_mean_no2)) %>%
  dcast(region ~ base, value.var = "salto_mean_no2")

cor.test(df.cor.pob$google, df.cor.pob$no2)


