# Create Volume df
library(tidyverse)
library(lubridate)

Volume_pre<- read_csv("data/TrafficVolume.csv")

a <- Volume_pre %>% mutate(hour = lubridate::hour(BinStartTime))

# Write Peak identifier    
b <- a %>% mutate(timeperiod = case_when(
      hour >= 7 & hour < 9 ~ "AMPeak",
      hour >= 12 & hour < 14 ~ "MidDay",
      hour >= 16 & hour < 18 ~ "PMPeak",
      TRUE ~ "OffPeak"))

c <- b %>% filter(PhaseNumber%in% c(2,6), timeperiod %in% c("AMPeak", "PMPeak"))

# Write rds file
write_rds(c, "data/Volume.rds")



# Start HERE unless rebuilding Volume file
library(tidyverse)

# Read Volume file in
Volume <- read_rds("data/Volume.rds")
Vol_PC <- filter(Volume, BinStartTime < "2020-01-01")
Vol_C <- filter(Volume, BinStartTime >= "2020-01-01")
