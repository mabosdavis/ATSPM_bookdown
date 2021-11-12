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
Vol_PC <- filter(Volume, BinStartTime < "2020-03-12")

#Graph Signal Specific Volumes
Vol_PC%>%
  filter(
    timeperiod == "PMPeak",
    SignalId == 6304,
    PhaseNumber ==2,) %>%
  ggplot(aes(x= BinStartTime, y = Volume_hour, color = hour)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = 'lm')


# Build the Covid df
Vol_C <- filter(Volume, BinStartTime >= "2020-03-12")

#Graph Signal Specific Volumes
Vol_C%>%
  filter(
    timeperiod == "PMPeak",
    SignalId == 6304,
    PhaseNumber ==2,) %>%
  ggplot(aes(x= BinStartTime, y = Volume_hour, color = hour)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = 'lm')


ggplot(data = Vol_PC) +
  geom_point(mapping = aes(x = BinStartTime, y = Volume))

#Testing
Volume_year <- Volume %>% mutate(Time = lubridate::md_hms
                                 
                                 #year = lubridate::year(BinStartTime),
                                 # month = lubridate::month(BinStartTime),
                                 # day = lubridate::day(BinStartTime),
                                 # hour = lubridate::hour(BinStartTime),
                                 # minute = lubridate::minute(BinStartTime)
                                 # Time = )
                                 
                                 Volume_year%>%
                                   filter(
                                     timeperiod == "PMPeak",
                                     SignalId == 6304,
                                     PhaseNumber ==2,) %>%
                                   ggplot(aes(x= BinStartTime, y = Volume_hour, color = hour)) +
                                   geom_point() +
                                   geom_smooth() +
                                   geom_smooth(method = 'lm') +
                                   facet_wrap(~year)
