########################################################
## Section 1: Nielson markets
## ## 
## ## 
########################################################

dma <- read_delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/IVXEHT/A56RIW", 
                  "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(STATEFP = if_else(STATEFP < 10, paste("0", STATEFP, sep = ""), paste(STATEFP)),
         CNTYFP = if_else(CNTYFP < 100 & CNTYFP > 9, paste("0", CNTYFP, sep = ""),
                          if_else(CNTYFP < 10, paste("00", CNTYFP, sep = ""), paste(CNTYFP)))) %>%
  mutate(GEOID = paste(STATEFP, CNTYFP, sep = "")) %>%
  mutate(GEOID = if_else(GEOID == "12025", "12086", GEOID)) %>%
  select(DMA, GEOID)

##

principals <-
  dma %>%
  mutate(principal_city = str_extract(DMA, "[^-]+")) %>%
  mutate(principal_city = str_replace_all(principal_city, pattern = " \\(", replacement = ", ")) %>%
  mutate(principal_city = str_remove_all(principal_city, pattern = "\\) ")) %>%
  mutate(principal_city = str_remove_all(principal_city, pattern = "\\)")) %>%
  mutate(principal_city = str_to_lower(principal_city)) %>%
  group_by(DMA) %>%
  slice(1)

##

geocoderesults <- tibble()

##

library(googleway)

##

for (i in 1:nrow(principals)) {
  
  index <- i
  
  city <- principals$principal_city[i]
  
  location <- google_geocode(city, key = "YOURKEY")
  
  located  <- tibble(city = city,
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
  group_by(city) %>%
  slice(1)

##

principals %>%
  rename(city = principal_city) %>%
  left_join(locations) %>%
  write_csv("principals.csv")
