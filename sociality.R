# Read in clean data in move2 format for wildlifeDI
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(datetime = as.POSIXct(study_local_timestamp, 
                               format = "%Y-%m-%d %H:%M:%S")) %>%
  select(c(datetime, utm_easting, utm_northing, id)) %>%
  na.omit() %>%
  mt_as_move2(time_column = "datetime",
              track_id_column = "id",
              coords =  c("utm_easting", "utm_northing")) %>%
  sf::st_set_crs("EPSG:32755"); beep()

# Check all birds present
unique(mt_track_id(data))

# Check temporal overlap
checkTO(data); beep()

# Issue with Nutmeg?
nutmeg <- subset(data, id=="Nutmeg")

# All bird combinations
list <-combinations(n = 14, r = 2, v = 1:14, repeats.allowed = FALSE)
list1 <- list[,1]
list2 <- list[,2]

# Calculate interactions between all birds 🐢🐢
interact <- data.frame()

for(i in 1:length(list1)) {
  
  iab <- tryCatch({
    data.frame(
      iab = IAB(data_move[list1[i]], data_move[list2[i]], tc=120, dc=50),
      bird1 = attr(data_move[[list1[i]]], "id"),
      bird2 = attr(data_move[[list2[i]]], "id"))
  }, error = function(e) data.frame(iab = NA, bird1 = NA, bird2 = NA))  
  
  print(paste("Finished",i))
  
  interact <- rbind(interact, iab)
}

# save results
write.csv(interact, "global_interactions.csv", row.names = FALSE)