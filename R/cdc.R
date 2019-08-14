########################################################
## Section 1: Wonder
## ## 
## ## 
########################################################

library(tidyverse)
library(janitor)

##
## https://wonder.cdc.gov/
##

despair <- 
  read_delim("data-in/cdc/Underlying Cause of Death, 1999-2017.txt", 
             "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  clean_names() %>%
  rename(GEOID = county_code) %>%
  mutate(rate = (deaths / population)) %>%
  select(GEOID, year, rate) %>%
  filter(year > 2012) %>%
  group_by(GEOID) %>%
  summarise(rate = mean(rate))

##

library(rgdal)
library(sf)

##

pills <- 
  readOGR("https://www.washingtonpost.com/graphics/2019/investigations/dea-pain-pill-database/json/us_counties.json") %>%
  st_as_sf() %>%
  select(GEOID, pills:year_2012) %>%
  st_drop_geometry() %>%
  gather(year, yearly, year_2006:year_2012) %>%
  mutate(year = str_replace_all(year, "year_", "")) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(GEOID) %>%
  mutate(change = case_when(lag(yearly) == 0 & yearly == 0 ~ 0,
                            lag(yearly) == 0 & yearly != 0 ~ 1,
                            TRUE ~ (yearly / lag(yearly)) - 1))

##

pills <-
  pills %>%
  group_by(GEOID) %>%
  summarise(total = mean(pills, na.rm = TRUE),
            annually = mean(pills_per_county_per_year, na.rm = TRUE),
            change = mean(change, na.rm = TRUE)) %>%
  replace_na(list(total = NA, annually = NA, change = NA))

##

despair <-
  despair %>%
  left_join(pills)

##

write_csv(despair, "despair.csv")

##


