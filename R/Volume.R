# Create Volume df
library(tidyverse)
library(lubridate)
library(data.table)

Volume_pre<- read_csv("data/TrafficVolume.csv")

vol <- Volume_pre %>%
  select(-Movement, -LaneNumber) %>% data.table()

b <- vol[, lapply(.SD, sum), by=list(BinStartTime, SignalId, ApproachId, PhaseNumber)] %>%
  as_tibble()

# Write Peak identifier    
c <- b %>% mutate(Volume_hour = Volume*4,
                  hour = lubridate::hour(BinStartTime),
                  timeperiod = case_when(
                    hour >= 7 & hour <= 9 ~ "AMPeak",
                    hour >= 12 & hour <= 14 ~ "MidDay",
                    hour >= 16 & hour <= 18 ~ "PMPeak",
                    TRUE ~ "OffPeak"))
d <- c %>% 
  filter(PhaseNumber%in% c(2,6), 
         timeperiod %in% c("AMPeak", "PMPeak"),
         BinStartTime >= "2017-11-12")

# Write rds file
write_rds(d, "data/Volume.rds")



# Start HERE unless rebuilding Volume file
library(tidyverse)

# Read Volume file in
Volume <- read_rds("data/Volume.rds")

# Build the Pre-Covid df
Vol_PC <- filter(Volume, BinStartTime < "2020-01-01")

#Graph Signal Specific Volumes
Volume%>%
  filter(
    timeperiod == "AMPeak",
    SignalId == 6303,
    PhaseNumber ==2,
    BinStartTime) %>%
  ggplot() +
  geom_point(mapping = aes(x = BinStartTime, y = Volume, color = PhaseNumber))

# Build the Covid df
Vol_C <- filter(Volume, BinStartTime >= "2020-01-01")


ggplot(data = Vol_PC) +
  geom_point(mapping = aes(x = BinStartTime, y = Volume))
