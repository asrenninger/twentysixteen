library(readxl)
library(tidyverse)
library(janitor)

rallies <- 
  read_xlsx("data-in/appc/rallies.xlsx", sheet = 3, skip = 1) %>%
  clean_names()

##

trump   <- rallies[, 1:5]
clinton <- rallies[, 14:18]

names(clinton) <- names(trump)

##

rallies <- 
  bind_rows(mutate(trump, candidate = "trump"),
            mutate(clinton, candidate = "clinton")) %>%
  drop_na(date_of_rally)

##

geocoderesults <- tibble()

##

library(googleway)

##

for (i in 1:nrow(rallies)) {
  
  index <- i
  
  rally <- paste(rallies$venue[index],
                 rallies$city[index],
                 rallies$state[index],
                 sep = ", ")
  
  location <- google_geocode(rally, key = "YOURKEY")
  
  located  <- tibble(rally = rally, 
                     name = c(location$results$formatted_address),
                     lat = c(location$results$geometry$location[1]),
                     lon = c(location$results$geometry$location[2]))
  
  geocoderesults <- bind_rows(geocoderesults, located)
  
  Sys.sleep(1)
  
}

##

latlon <- tibble()

for (i in 1:nrow(geocoderesults)) {
  
  index <- i 
  
  Y <- geocoderesults$lat[[index]][1]
  X <- geocoderesults$lon[[index]][1]
  
  iteration <- tibble(lat = Y,
                      lon = X)
  
  latlon <- bind_rows(latlon, iteration)
  
}

##

locations <-
  geocoderesults %>%
  select(-lat, -lon) %>%
  bind_cols(latlon)

##

locations <- 
  locations %>%
  group_by(rally) %>%
  slice(1)

##

rallies_located <- 
  rallies %>%
  mutate(rally = paste(venue, city, state, sep = ", ")) %>%
  left_join(locations)

##

write_csv(rallies_located, "rallies.csv")

##

read_csv("data-out/rallies.csv")
