library(tidyverse)
library(tidycensus)

##

counties <- 
  fips_codes %>%
  mutate(GEOID = paste(state_code, county_code, sep = "")) %>%
  select(GEOID, state, county)

##

write_csv(counties, "counties.csv")

##

move <- get_estimates(geography = "county",
                      variables = "RNETMIG", 
                      year = 2016) %>%
  rename(migration = value) %>%
  select(-variable)

##

vars <- load_variables(year = 2016, dataset = "acs5", cache = TRUE)

test <- filter(vars, str_detect(concept, "EMPLOYMENT STATUS"))
test <- filter(vars, str_detect(label, "Unemployed"))
test <- filter(vars, str_detect(name, "B23001_001"))

read_csv("https://www.bls.gov/lau/laucnty16.txt", skip = 3) %>% glimpse()

##

area <- 
  get_acs(geography = "county", 
          variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001"), 
          year = 2016, output = "wide") %>% 
  rename(gini = B19083_001E,
         earn = B06011_001E,
         home = B25077_001E,
         rent = B25113_001E) %>%
  select(GEOID, gini, earn, home, rent)

##

race <- 
  get_acs(geography = "county", 
          variables = c("B02001_002", "B02001_003"), 
          year = 2016, output = "wide") %>% 
  rename(white = B02001_002E,
         black = B02001_003E) %>%
  select(GEOID, white, black)

##

work <-
  get_acs(geography = "county", variables = c("C18120_002E", "C18120_006E", "C18120_003E"),
          year = 2016, output = "wide") %>%
  rename(force = C18120_002E,
         unemployed = C18120_006E,
         employed = C18120_003E) %>%
  select(GEOID, force, unemployed, employed)

##

live <- 
  get_acs(geography = "county", variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"),
          year = 2016, output = "wide") %>%
  rename(peeps = B01001_001E,
         holds = B11016_001E,
         age = B01002_001E,
         edu = B06009_005E) %>%
  select(GEOID, peeps, holds, age, edu)

##

fors <- get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
                year = 2016, output = "wide") %>%
  rename(latinos = B05006_124E,
         foreign = B05006_001E) %>%
  select(GEOID, latinos, foreign)

##

outs <- 
  mobility <- get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
                      year = 2016, output = "wide") %>%
  rename(nation = B07001_033E,
         abroad = B07001_081E) %>%
  select(GEOID, nation, abroad)

##

pres <-
  move %>%
  left_join(area) %>%
  left_join(live) %>%
  left_join(work) %>%
  left_join(race) %>%
  left_join(fors) %>%
  left_join(outs)

##

area <- 
  get_acs(geography = "county", 
          variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001"), 
          year = 2010, output = "wide") %>% 
  rename(gini = B19083_001E,
         earn = B06011_001E,
         home = B25077_001E,
         rent = B25113_001E) %>%
  select(GEOID, gini, earn, home, rent)

##

race <- 
  get_acs(geography = "county", 
          variables = c("B02001_002", "B02001_003"), 
          year = 2010, output = "wide") %>% 
  rename(white = B02001_002E,
         black = B02001_003E) %>%
  select(GEOID, white, black)

##

vars <- load_variables(year = 2010, dataset = "acs5", cache = TRUE)

test <- filter(vars, str_detect(concept, "EMPLOYMENT"))

work <-
  get_acs(geography = "county", variables = c("C18120_002E", "C18120_006E", "C18120_003E"),
          year = 2010, output = "wide") %>%
  rename(force = C18120_002E,
         unemployed = C18120_006E,
         employed = C18120_003E) %>%
  select(GEOID, force, unemployed, employed)

##

live <- 
  get_acs(geography = "county", variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"),
          year = 2010, output = "wide") %>%
  rename(peeps = B01001_001E,
         holds = B11016_001E,
         age = B01002_001E,
         edu = B06009_005E) %>%
  select(GEOID, peeps, holds, age, edu)

##

fors <- get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
                year = 2010, output = "wide") %>%
  rename(latinos = B05006_124E,
         foreign = B05006_001E) %>%
  select(GEOID, latinos, foreign)

##

outs <- 
  get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
                      year = 2010, output = "wide") %>%
  rename(nation = B07001_033E,
         abroad = B07001_081E) %>%
  select(GEOID, nation, abroad)

##

past <-
  move %>%
  left_join(area) %>%
  left_join(live) %>%
  left_join(work) %>%
  left_join(race) %>%
  left_join(fors) %>%
  left_join(outs)

##

move <- 
  get_estimates(geography = "county", product = "components", time_series = TRUE) %>%
  spread(variable, value) %>%
  mutate(PERIOD = PERIOD + 2009) %>%
  clean_names() %>%
  rename(GEOID = geoid) %>%
  select(GEOID, everything(), -name) 

##

write_csv(move, "move.csv")

##
## (https://factfinder.census.gov/)
## 




