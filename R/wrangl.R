library(tidyverse)
library(janitor)

########################################################
## Section 1: Census data
## ## Convert into demography into rates and ratios
## ## Summarise employment situation
########################################################

acs <- read_csv("data-out/batch.csv")

glimpse(census)

population <- 
  counties %>%
  mutate(area = st_area(geometry)) %>%
  mutate(area = set_units(area, km^2)) %>%
  st_drop_geometry() %>%
  left_join(acs) %>%
  mutate(pct_white = (white / peeps) * 100,
         pct_black = (black / peeps) * 100,
         household_size = peeps / holds,
         median_age = age,
         education = (edu / peeps) * 100,
         pct_foreign = (foreign / peeps) * 100,
         pct_foreign_lat = (latinos / peeps) * 100,
         pct_moved_dom = (nation / peeps) * 100,
         pct_moved_int = (abroad / peeps) * 100,
         unemployment = (unemployed / force) * 100,
         population = peeps,
         density = peeps / area) %>%
  select(-unemployed, -employed, -force, -nation, -abroad, -foreign, -latinos, -edu, -age,
         -holds, -peeps, -black, -white, -area) %>%
  select(GEOID, year, everything()) %>%
  as_tibble()

##

write_csv(population, "population.csv")

##

loquo <- read_csv("data-out/loquo.csv")

##

pop_ind_mix <- 
  population %>%
  left_join(loquo)

##

change_vars <- 
  population %>%
  select(pct_moved_dom, pct_moved_int, 
         pct_foreign, pct_foreign_lat,
         earn, unemployment) %>%
  names()

##

test <- 
  population %>%
  select(GEOID, year, pct_foreign_lat) %>%
  group_by(GEOID) %>% 
  mutate(change = (1 - (lag(pct_foreign_lat) / pct_foreign_lat)) * 100) %>%
  summarise(change_for_lat = mean(change, na.rm = TRUE)) %>%
  left_join(crosswalk)

##

library(glue)

##

changes <- crosswalk

for (i in 1:length(change_vars)) {
  
  iteration <- change_vars[i]
  
  temp <-
    population %>%
    select(GEOID, year, iteration) %>%
    set_names(c("GEOID", "year", "variable")) %>%
    group_by(GEOID) %>% 
    mutate(variable_lag = lag(variable)) %>%
    mutate(change = case_when(variable != 0 & variable_lag != 0 ~ ((variable / variable_lag) - 1) * 100,
                              variable != 0 & variable_lag == 0 ~ 100,
                              variable == 0 & variable_lag != 0 ~ -100,
                              variable == 0 & variable_lag == 0 ~ 0)) %>%
    replace_na(list(change = NA)) %>%
    summarise(change = mean(change, na.rm = TRUE)) %>%
    set_names(c("GEOID", glue("{iteration}_ch")))
  
  changes <- left_join(changes, temp)
  
}

write_csv(changes, "changes.csv")

########################################################
## Section 2: BLS data
## ## Create unemployment rate
## ## Measure change
########################################################



########################################################
## Section 3: RWJ data
## ## Look at changes over time
## ## Hybridise mental and physical health
########################################################

health <- read_csv("data-out/health.csv")

########################################################
## Section 4: CDC data
## ## Weight by population
## ## 
########################################################



########################################################
## Section 4: Nielson data
## ## Add in rallies
## ## Add in advertising
########################################################

library(sf)

##

dma <- read_delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/IVXEHT/A56RIW", 
                  "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(STATEFP = if_else(STATEFP < 10, paste("0", STATEFP, sep = ""), paste(STATEFP)),
         CNTYFP = if_else(CNTYFP < 100 & CNTYFP > 9, paste("0", CNTYFP, sep = ""),
                          if_else(CNTYFP < 10, paste("00", CNTYFP, sep = ""), paste(CNTYFP)))) %>%
  mutate(GEOID = paste(STATEFP, CNTYFP, sep = "")) %>%
  mutate(GEOID = if_else(GEOID == "12025", "12086", GEOID)) %>%
  select(DMA, GEOID)

counties <- 
  st_read("data-out/counties.geojson") %>%
  st_set_crs(102003)

##

markets <- 
  counties %>%
  left_join(dma) %>%
  group_by(DMA) %>%
  summarise()

##

rallies <- 
  read_csv("data-out/rallies.csv") %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(102003)

##

coverage <-
  rallies %>%
  st_join(markets) %>%
  st_join(counties) %>%
  st_as_sf()

##

library(lubridate)
library(glue)

##

dated <- 
  coverage %>%
  separate(date_of_rally, sep = ", ", into = c("day", "date")) %>%
  separate(date, sep = " ", into = c("month", "date")) %>%
  mutate(date = glue("{month} {date}th, 2016")) %>%
  select(-month, -day) %>%
  mutate(date = mdy(date)) %>%
  drop_na(date)

coverage %>%
  group_by(DMA, candidate) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  group_by(DMA) %>%
  slice(1) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  left_join(markets) %>%
  st_as_sf() %>%
  select(candidate) %>%
  plot(pal = c('blue', 'red'))

dated %>%
  select(-geometry) %>%
  write_csv("rallies.csv")

########################################################
## Section 5: Leip data
## ## Line up the left hand of the regression
## ## 
########################################################

independent <- 
  health %>%
  left_join(changes) %>%
  left_join(population) %>%
  left_join(loquo)
  
########################################################
## Section 6: Leip data
## ## Line up the left hand of the regression
## ## 
########################################################

dependent <- read_csv("data-out/left.csv")

########################################################
## Section 6: Leip data
## ## Line up the left hand of the regression
## ## 
########################################################

regression <-
  crosswalk %>%
  left_join(dependent) %>%
  left_join(independent)

##

library(spdep)

counties %>% poly2nb()

?poly2nb

primed <- counties[-c(1931, 2003, 2639), ] %>% as('Spatial')
neighb <- poly2nb(primed, queen = TRUE)
weight <- nb2listw(neighb, style = 'W', zero.policy = FALSE)

plot(weight, coordinates(primed))

coords <- coordinates(primed)

relations <- tibble()

new <- counties[-c(1931, 2003, 2639), ]

for (i in 1:nrow(new)) {
 
  iteration <- new %>% slice(i)
  
  relation <- mutate(iteration, neighbours = list(weight$neighbours[[i]]))
  
  relations <- bind_rows(relations, relations)
  
}


rome_shape %>%
  filter(str_detect(TITLE, paste(rome$birth_cty, collapse="|"))) %>%
  mapview() +
  geom_point(data = rome_shape,
             aes(REPRLONG, REPRLAT), alpha = 0.25) +
  geom_point(aes(colour = GEOCONTEXT), show.legend = FALSE)

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

?st_relate

coords <- 
  counties %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neighbours = rowname)
           
neighbours <- 
  neighborpoints %>% 
  as_tibble() %>%
  set_names(c("rowname", "neighbours")) %>%
  mutate(rowname = as.character(rowname),
         neighbours = as.character(neighbours)) %>%
  left_join(rownames_to_column(counties)) %>%
  filter(str_detect(str_sub(GEOID, 1, 2), "10|42|34|09|07|05|47")) %>%
  st_as_sf() %>%
  st_centroid() %>%
  left_join(coords)

states <- crosswalk %>% 
  filter(!str_detect(state_code, "15|02|60|66|69|72|78|74")) %>% 
  pull(state_code) %>%
  unique()

test <- 
  neighbours %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(neighbours) %>%
  select(-geometry) %>%
  mutate(start = glue("{X}, {Y}"),
         end = glue("{X1}, {Y1}")) %>%
  mutate(path = glue("{X},{Y} - {X1}, {Y1}")) %>%
  select(GEOID, path, start, end) %>%
  gather(position, coordinates, start:end) %>%
  separate(coordinates, into = c("X", "Y"), sep = ", ")

ggplot(data = test) +
  geom_line(aes(x = X, y = Y, group = path),
            colour = 'grey40') +
  geom_point(aes(x = X, y =Y), 
             colour = 'grey70')

