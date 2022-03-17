library(dplyr)
library(geosphere)

# Listing all files that you want to analyse
files <- data.frame(input = list.files("GPSdata2021/", full.name = TRUE),
                    output = paste0("Summarised", list.files("GPSdata2021/", full.name = TRUE)))

# For loop
for(i in 1:nrow(files)){
  df <- read.csv(files$input[i], fileEncoding="UTF-16LE")
  
  # Clean data
  clean <- df %>%
    mutate(timestamp = as.POSIXct(paste0(Date, Time), format = "%Y/%m/%d %H:%M:%S")) %>%
    arrange(timestamp) %>%
    filter(timestamp >= as.POSIXct("2018-01-01 00:00:00"),
           Latitude != 0, Longitude != 0) %>%
    mutate(distance = distHaversine(cbind(Longitude, Latitude), cbind(lag(Longitude), lag(Latitude))),
           speed = distance/as.numeric(timestamp - lag(timestamp))) %>%
    filter(speed <= 5.39) %>%
    mutate(distance = distHaversine(cbind(Longitude, Latitude), cbind(lag(Longitude), lag(Latitude))),
           speed = distance/as.numeric(timestamp - lag(timestamp)),
           hour = as.POSIXct(paste0(strftime(timestamp, format = "%Y-%m-%d %H"), ":00:00"))) %>%
    group_by(hour) %>%
    summarise(meandist = mean(distance, na.rm = TRUE),
              mindist = min(distance, na.rm = TRUE),
              maxdist = max(distance, na.rm = TRUE),
              sddist = sd(distance, na.rm = TRUE),
              meanspeed = mean(speed, na.rm = TRUE),
              minspeed = min(speed, na.rm = TRUE),
              maxspeed = max(speed, na.rm = TRUE),
              sdspeed = sd(speed, na.rm = TRUE))
  
  write.csv(clean, files$output[i], row.names = FALSE)
}