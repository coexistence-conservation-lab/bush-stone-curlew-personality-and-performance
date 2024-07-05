## Distance
### Distance from release location
#This gives an indication of the bird's propensity to explore the release site and exploit new resources. 

# Read in clean data
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(datetime = as.POSIXct(datetime)); beep()

# Specify the coords of each GPS fix
coords <- as.matrix(cbind(data$utm_easting, data$utm_northing))

# Set coords for the release location 
release  <- matrix(c(274423.45, 5801912.85), ncol=2) #(-37.902374, 144.434323)

# Calculate distance between the release location and all fixes
dist <- mutate(data, dist_release = distance(coords, as.matrix(release), lonlat=FALSE))

# Calculate daily summary statistics
dist_day <- dist %>% group_by(date, id) %>% 
  dplyr::summarise(mean = mean(dist_release),
                   max = max(dist_release),
                   min = min(dist_release)) %>%
  mutate(date = as_date(date))

# Plot average distance from release site per day, smoothed & coloured by individual
ggplot(dist_day)+
  geom_smooth(aes(date, mean, group = id, color = id))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_labels = "%b %Y", breaks = "1 month")+
  labs(x= element_blank(), y = "Mean daily distance from release location (m)")+
  scale_colour_viridis_d()

# Add release group information
group <- data.frame(
  id = c("Aurora", "Robin", "Briar", "Nutmeg", 
         "Iona", "Sage", "Koda",
         "Prem", "Rove", "Brook", "Clover", "Wobbles", "Valentine", "Marmalade"),
  release = c(rep("October",4), rep("December",3), rep("January",7)))

dist_day <- left_join(dist_day, group)        

# And summary stats for whole period
dist_summary <- dist_day %>%
  group_by(id) %>%
  summarise(mean_daily_dist = mean(mean))

# Save results
write.csv(dist_day, "Output/distance_release.csv", row.names = FALSE)
write.csv(dist_summary, "Output/distance_release_mean.csv", row.names = FALSE)
```

```{r plot distance moved release}
# Read data
dist_day <- read.csv("Output/distance_release.csv") %>%
  mutate(date = as_date(date))

# Plot average distance from release site per day, smoothed & coloured by release group
ggplot(dist_day)+
  geom_smooth(aes(date, mean, group = id, color = release))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_labels = "%b %Y", breaks = "1 month")+
  labs(x= element_blank(), y = "Mean daily distance from release location (m)")+
  scale_color_manual(values = c("#881C00FF","#1BB6AFFF","#172869FF"))+
  theme(axis.text.x = element_text(angle = 90))


### Distance moved per day
#Total distance moved daily gives an indication of the activity levels of the bird.

# Read in clean data
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(datetime = as.POSIXct(datetime)); beep()

# Convert to track (AMT format)
track <- make_track(data, .x = utm_easting,.y= utm_northing,
                    .t= datetime, id = id) %>%
  nest(data = -"id")

# Loop through distance per day per bird 🐢
dates <- unique(data$date)
dist <- data.frame()

for (i in 1:length(birds)){ 
  for (j in 1:length(dates)){
    
    t <-  track$data[[i]] %>%
      mutate(date = date(t_)) %>%
      filter(date==dates[j])
    
    if (nrow(t)<2){
      next
    }
    
    out <- data.frame(dist= cum_dist(t)/1000,
                      bird = track$id[[i]],
                      date = as_date(dates[j]))
    
    print(out)
    
    dist <- rbind.data.frame(dist, out)
    
  }}; beep()

# Plot distance moved per day, smoothed and coloured by individual
ggplot(dist)+
  geom_smooth(aes(date, dist, group = bird, color=bird))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_labels = "%b %Y", breaks = "1 month")+
  labs(x= element_blank(), y = "Total daily distance moved (km)")+
  scale_color_viridis_d()

# Add release group information
group <- data.frame(
  bird = c("Aurora", "Robin", "Briar", "Nutmeg",
         "Iona", "Sage", "Koda",
         "Prem", "Rove", "Brook", "Clover", "Wobbles", "Valentine", "Marmalade"),
  release = c(rep("October",4), rep("December",3), rep("January",7)))

dist <- left_join(dist, group)  

# and as an average over the study period
dist_summary <- dist %>% 
  group_by(bird) %>%
  summarise(mean_daily_dist = mean(dist))

# save results
write.csv(dist, "Output/distance_daily.csv", row.names = FALSE)
write.csv(dist_summary, "Output/distance_daily_mean.csv", row.names = FALSE)

# Read data
dist <- read.csv("Output/distance_daily.csv") %>%
  mutate(date = as_date(date))

# Plot distance moved per day, smoothed & coloured by release group
ggplot(dist)+
  geom_smooth(aes(date, dist, group = bird, color = release))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_labels = "%b %Y", breaks = "1 month")+
  labs(x= element_blank(), y = "Total daily distance moved (km)")+
  scale_color_manual(values = c("#881C00FF","#1BB6AFFF","#172869FF"))+
  theme(axis.text.x = element_text(angle = 90))

### Calculate the change in distance over time (as a slope)

# Read in daily dist data
dist_daily <- read.csv("Output/distance_daily.csv") %>%
  mutate(date = as.numeric(as_date(date)))

dist_release <- read.csv("Output/distance_release.csv") %>%
  mutate(date = as.numeric(as_date(date))) %>%
  rename(bird = id)

# Extract slope coefficient from lm per bird
dist_slope <- data.frame()

for(i in 1:length(birds)){
  subset1 <- subset(dist_daily, bird == birds[i])
  slope1 <- lm(dist ~ date, data = subset1) %>%
  coef()
  
  subset2 <- subset(dist_release, bird == birds[i])
  slope2 <- lm(mean ~ date, data = subset2) %>%
  coef()
  
  out <- data.frame(bird = birds[i], 
                    slope_dist_moved = slope1[2],
                    slope_dist_release = slope2[2])
  dist_slope <- rbind(dist_slope, out)
}

# Save to file
write.csv(dist_slope, "Output/dist_slope.csv", row.names = FALSE)