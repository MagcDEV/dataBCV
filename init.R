library(tidyverse)
library(gdata)
library(rvest)
options(scipen = 999)

url1 <- "http://www.bcv.org.ve/sites/default/files/indicadores_sector_monetario/liquidez_monetaria_semanal.xls"

data_bcv  <- read.xls(url1, sheet = "LIQUIDEZ_2019-2020", pattern = "al")

data_bcv <- data_bcv[3:length(data_bcv$X.5), c("X.5", "X.6")]

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

tabla_idi <- tabla_idi %>%
    mutate(pct_change = (tipo_de_cambio / lead(tipo_de_cambio) - 1) * 100)

data_bcv <- merge(data_bcv, tabla_idi, all.x = TRUE, by = "fecha")

data_bcv <- data_bcv[!is.na(data_bcv$pct_change), ]

data_bcv_1 <- data_bcv[, c("fecha", "variacion")]
data_bcv_2 <- data_bcv[, c("fecha", "pct_change")]

names(data_bcv_2) <- c("fecha", "variacion")

data_bcv_1$variable <- 

data_bcv <- rbind(data_bcv_1, data_bcv_2)

ggplot(data_bcv, aes(x = fecha, y = variacion)) +
    geom_line(aes(y = variacion), color = "darkred", size = 1.5) +
    geom_line(aes(y = pct_change), color = "steelblue", size = 1.5) +
    geom_hline(yintercept = 10, size = 1) +
    geom_hline(yintercept = 0, size = 1)
