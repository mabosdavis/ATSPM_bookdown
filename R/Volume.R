library(tidyverse)

Volume<- read_csv("data/TrafficVolume.csv")
Vol_PC <- filter(Volume, BinStartTime < "2020-01-01")
Vol_C <- filter(Volume, BinStartTime >= "2020-01-01")
