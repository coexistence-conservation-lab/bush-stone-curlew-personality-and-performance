# Read in clean data
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, 
                               "%Y-%m-%d %H:%M:%S",tz = "Australia/Melbourne"),
         date = as.Date(DateTime, tz = "Australia/Melbourne"))

# Calculate if time is pre/post dawn/dusk
suntime <- getSunlightTimes(date = unique(data$date),
                            lat = -37.90,
                            lon = 144.43,
                            keep = c("sunrise", "sunset"),
                            tz = "Australia/Melbourne") %>%
  subset(select = -c(lat, lon)) 

# Append to data frame
data_sun <- left_join(data, suntime) %>%
  mutate(tod = ifelse(DateTime>sunrise & DateTime<sunset, "day", "night")) %>%
  relocate(DateTime, .after = tod) %>%
  na.omit()

# Plot to check
ggplot(data_sun)+
  geom_point(aes(lon, lat, colour = tod))

# Marmalade issues?
marma <- filter(data_sun, id == "Marmalade")

marmasp <- vect(marma, geom = c("lon", "lat"), crs = "EPSG:4326")

marma_out <- marma %>%
  mutate(infence = is.related(marmasp, fence, "intersects")) %>%
  filter(infence == FALSE)

# Save day data
data_day <- data_sun %>%
  filter(tod == "day") %>%
  relocate(DateTime, .after = tod)

write.csv(data_day, "Processed/GPSdata_clean_day.csv", row.names = FALSE)

# Save night data
data_night <- data_sun %>%
  filter(tod == "night")

write.csv(data_night, "Processed/GPSdata_clean_night.csv", row.names = FALSE)