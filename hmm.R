# Read in cleaned data prepare moveHMM object
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, 
                               "%Y-%m-%d %H:%M:%S",tz = "Australia/Melbourne"),
         date = as.Date(DateTime, tz = "Australia/Melbourne")) %>%
  prepData(type = "UTM", coordNames = c("utm_easting", "utm_northing"))

# Test subset, 1 month for Robin
robin <- filter(data, id == "Robin" & date %in% as_date(c(as_date("2023-03-01"):as_date("2023-03-30")))) %>%
  mutate(vedba = ((sqrt(acceleration_raw_x^2 + acceleration_raw_y^2 + acceleration_raw_x^2))/1000))

# 3 state model
## Initial parameters for gamma and von Mises distributions
### Step lengths
mu0 <- c(0,5, 20, 100) # step mean 
sigma0 <- c(1, 5, 20) # step SD 
stepPar0 <- c(mu0, sigma0) 
#### Angle concentrations
angleMean0 <- c(pi, (pi/2), 0) # angle mean 
kappa0 <-c(0.3, 0.1, 0.2) # angle concentration 
anglePar0 <- c(angleMean0, kappa0)

## call to fitting function 
robin_hmm3 <-fitHMM(robin,
                    nbStates = 3,
                    stepPar0 = stepPar0,
                    anglePar0 = anglePar0);beep()

# 4 state model
## Initial parameters for gamma and von Mises distributions
### Step lengths
mu0 <- c(1, 5, 20, 100) # step mean 
sigma0 <- c(1, 1, 5, 20) # step SD 
stepPar0 <- c(mu0, sigma0) 
#### Angle concentrations
angleMean0 <- c(pi, pi, (pi/2), 0) # angle mean 
kappa0 <-c(0.2, 0.2, 0.1, 0.2) # angle concentration 
anglePar0 <- c(angleMean0, kappa0)

## call to fitting function 
robin_hmm4 <-fitHMM(robin,
                    nbStates = 4,
                    stepPar0 = stepPar0,
                    anglePar0 = anglePar0);beep()

# Run on all birds and compare a 3 and 4 state model

# Read in cleaned data prepare moveHMM object
data <- read.csv("Processed/GPSdata_clean.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, 
                               "%Y-%m-%d %H:%M:%S",tz = "Australia/Melbourne"),
         date = as.Date(DateTime, tz = "Australia/Melbourne")) %>%
  rename(ID = id) %>%
  prepData(type = "UTM", coordNames = c("utm_easting", "utm_northing")) %>%
  filter(step < 500); beep()

# Plot to check
plot(data, compact = TRUE)

# 3 state model
## Initial parameters for gamma and von Mises distributions
### Step lengths
mu0 <- c(0.5, 20, 100) # step mean 
sigma0 <- c(1, 5, 20) # step SD 
zeromass0 <- c(0.1, 0.1, 0.05) # step zero mass
stepPar0 <- c(mu0, sigma0, zeromass0) 
#### Angle concentrations
angleMean0 <- c(pi, (pi/2), 0) # angle mean 
kappa0 <-c(0.3, 0.1, 0.2) # angle concentration 
anglePar0 <- c(angleMean0, kappa0)

## call to fitting function 
hmm3 <-fitHMM(data,
              nbStates = 3,
              stepPar0 = stepPar0,
              anglePar0 = anglePar0);beep()

# 4 state model
## Initial parameters for gamma and von Mises distributions
### Step lengths
mu0 <- c(1, 5, 20, 100) # step mean 
sigma0 <- c(1, 1, 5, 20) # step SD 
stepPar0 <- c(mu0, sigma0) 
#### Angle concentrations
angleMean0 <- c(pi, pi, (pi/2), 0) # angle mean 
kappa0 <-c(0.2, 0.2, 0.1, 0.2) # angle concentration 
anglePar0 <- c(angleMean0, kappa0)

## call to fitting function 
hmm4 <-fitHMM(data,
              nbStates = 4,
              stepPar0 = stepPar0,
              anglePar0 = anglePar0);beep()