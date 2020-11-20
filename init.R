library(tidyverse)
library(gdata)
library(rvest)
options(scipen = 999)

url1 <- "http://www.bcv.org.ve/sites/default/files/indicadores_sector_monetario/liquidez_monetaria_semanal.xls"

data_bcv  <- read.xls(url1, sheet = "LIQUIDEZ_2019-2020", pattern = "al")

data_bcv <- data_bcv[, c(7, 8)]

names(data_bcv) <- c("liquidez_monetaria_semanal_bs", "variacion")

data_bcv$liquidez_monetaria_semanal_bs <- gsub(",", "",
                                data_bcv$liquidez_monetaria_semanal_bs)

data_bcv$variacion <- gsub("\\(", "", data_bcv$variacion)
data_bcv$variacion <- gsub("\\)", "", data_bcv$variacion)

data_bcv$liquidez_monetaria_semanal_bs  <-
    as.numeric(data_bcv$liquidez_monetaria_semanal_bs)

data_bcv$variacion <- as.numeric(data_bcv$variacion)

data_bcv <- data_bcv[!is.na(data_bcv$liquidez_monetaria_semanal_bs), ]

data_bcv$fecha <- rev(seq(as.Date("2019-1-4"),
                          length.out = length(data_bcv$variacion),
                          by = "week"))

scrappedurl <-
    read_html("http://www.bcv.org.ve/estadisticas/indice-de-inversion")

tabla_idi <- html_table(html_node(scrappedurl, "table"), dec = ",")

names(tabla_idi) <- c("fecha", "tipo_de_cambio", "idi")

for (val in c(1:5)) {

scrappedurl <-
    read_html(paste
              ("http://www.bcv.org.ve/estadisticas/indice-de-inversion?page=",
               as.character(val), sep = ""))

tabla_idi_1 <- html_table(html_node(scrappedurl, "table"), dec = ",")

names(tabla_idi_1) <- c("fecha", "tipo_de_cambio", "idi")

tabla_idi <- rbind(tabla_idi, tabla_idi_1)

rm(tabla_idi_1)

}

tabla_idi$tipo_de_cambio <- gsub("\\.", "",
                                       tabla_idi$tipo_de_cambio)
tabla_idi$tipo_de_cambio <- gsub(",", ".",
                                       tabla_idi$tipo_de_cambio)

tabla_idi$idi <- gsub("\\.", "", tabla_idi$idi)
tabla_idi$idi <- gsub(",", ".", tabla_idi$idi)

tabla_idi$tipo_de_cambio <- as.numeric(tabla_idi$tipo_de_cambio)
tabla_idi$idi <- as.numeric(tabla_idi$idi)

tabla_idi$fecha <- as.Date(tabla_idi$fecha, "%d-%m-%Y")

data_bcv <- merge(data_bcv, tabla_idi, all.x = TRUE, by = "fecha")

data_bcv <- data_bcv[!is.na(data_bcv$tipo_de_cambio), ]

data_bcv <- data_bcv %>%
    mutate(pct_change = (tipo_de_cambio / lag(tipo_de_cambio) - 1) * 100)

ggplot() +
    geom_line(data = data_bcv, aes(x = fecha, y = variacion,
                                 color = "Liquidez Monetaria"), size = 1.5) +
    geom_line(data = data_bcv, aes(x = fecha, y = pct_change,
                                 color = "Dolar BCV"), size = 1.5) +
    geom_hline(yintercept = 10, size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    ggtitle("Variación % Liquidez Monetaria vs Dolar BCV (hasta el 6/11/2020)") +
    labs(caption = "Fuente: BCV y calculos propios") +
    scale_color_manual(values = c("Liquidez Monetaria" = "#065FB4",
                                  "Dolar BCV" = "#57780B"),
                                  labs(color = "Variación %"))
