library(tidyverse)
library(gdata)

url1  <- "http://www.bcv.org.ve/sites/default/files/indicadores_sector_monetario/liquidez_monetaria_semanal.xls"

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

rev(seq(as.Date("2019-1-4"), length.out = length(data_bcv$variacion),
        by = "week"))
