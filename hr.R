## Home range

#Core 50% KUD and full 90% KUD home range using kernel utilisation density. 

# Read in clean data
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(datetime = as.POSIXct(datetime)); beep()

# Convert GPS fixes to spatial points data frame
locs <- SpatialPointsDataFrame(coordinates(
  cbind(data$utm_easting, data$utm_northing)),
  data = dplyr::select(data, id))

# calculate home range 
## 90% kud
hr_90 <-  kernelUD(locs, h="href", grid=200) %>% 
  getverticeshr(percent = 90); beep()

## 50% kud
hr_50 <-  kernelUD(locs, h="href", grid=200) %>% 
  getverticeshr(percent = 50); beep()

# transform to latlon
## 90% kud
proj4string(hr_90) <- CRS("EPSG:32755") #set crs
hr_90ll <- spTransform(hr_90, CRS("EPSG:4326")) %>% #transform to lat lon
  st_as_sf()

## 50% kud
proj4string(hr_50) <- CRS("EPSG:32755") #set crs
hr_50ll <- spTransform(hr_50, CRS("EPSG:4326")) %>% #transform to lat lon
  st_as_sf()

# plot map
## 90% kud
ggmap(map_z15)+
  geom_sf(data=hr_90ll, aes(fill=id), alpha = .7, inherit.aes = FALSE) +
  scale_fill_viridis_d()+
  theme_void()

## 50% kud
ggmap(map_z15)+
  geom_sf(data=hr_50ll, aes(fill=id), alpha = .7, inherit.aes = FALSE) +
  scale_fill_viridis_d()+
  theme_void()

# save results
hr <- data.frame(bird = hr_90@data$id,
                 kud90 = hr_90@data$area,
                 kud50 = hr_50@data$area)

write.csv(hr, "Output/hr_area.csv", row.names = FALSE)

#And daily 90% home range

# Read in clean data
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(datetime = as.POSIXct(datetime)); beep()

# convert GPS fixes to spatial points data frame
locs <- SpatialPointsDataFrame(coordinates(
  cbind(data$utm_easting, data$utm_northing)),
  data = data)

# loop through birds and days for 90% KUD 🐢🐢🐢 expect 5-8 hours
dates <- unique(locs$date)

hr_daily <- data.frame()

for (i in 1:length(birds)){
  for (j in 1:length(dates)){
    points <- subset(locs, id == birds[i] & date == dates[j],
                     select = id)
    if (length(points)<5){
      next
    }
    kud <- kernelUD(points[,1], h="href", grid=1000, extent = 5) %>% 
      getverticeshr(percent = 90) %>%
      st_as_sf() %>%
      mutate(date = as_date(dates[j])) %>%
      st_drop_geometry()
    
    print(head(kud, n= 1L))
    
    hr_daily <- rbind.data.frame(hr_daily, kud)
  }}; beep()

# Save to file
write.csv(hr_daily, "Output/hr_daily_area.csv", row.names = FALSE)

# plot home ranges

# read data
hr_daily <- read.csv("Output/hr_daily_area.csv" )%>%
  mutate(date = as_date(date))

# Plot daily home ranges, smoothed and coloured by individual
ggplot(hr_daily)+
  geom_smooth(aes(date, area, group = id, color=id), method = "lm")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_labels = "%b %Y", breaks = "1 month")+
  labs(x= element_blank(), y = "Daily 90% KDE home range (ha)")+
  scale_color_viridis_d()+
  theme(axis.text.x = element_text(angle = 90))

# And individual box plots
ggplot(hr_daily)+
  geom_boxplot(aes(id, area, color = id))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= element_blank(), y = "Daily 90% KDE home range (ha)")+
  theme(axis.text.x = element_text(angle = 50))+
  scale_color_viridis_d()+
  theme(legend.position="none")

## Calculate the change in home range over time (as a slope)

# Read in daily hr data
daily_hr <- read.csv("Output/hr_daily_area.csv") %>%
  mutate(date = as.numeric(as_date(date)))

# Extract slope coefficient from lm per bird
hr_slope <- data.frame()
for(i in 1:length(birds)){
  subset <- subset(daily_hr, id == birds[i])
  slope <- lm(area ~ date, data = subset) %>%
    coef() 
  out <- data.frame(id = birds[i], 
                    slope = slope[2])
  hr_slope <- rbind(hr_slope, out)
}

# Save to file
write.csv(hr_slope, "Output/hr_slope.csv", row.names = FALSE)