########################################################
## Section 1: American Community Survey
## ## Set up a crosswalk for all geographies
## ## Download variables with TidyCensus
########################################################

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

########################################################
## Section 2: County business Patterns
## ## Unzip and bind
## ## Clean and select industries
########################################################

##
## (https://factfinder.census.gov/)
## 

business <- 
  reduce(
  map(2006:2016, function(x) {
    glue("~/Desktop/R/git/twentysixteen/data-in/census/BP_{x}_00A1.zip") %>%
      read_csv(skip = 1) %>%
      clean_names() %>%
      select_if(!str_detect(names(.), "noise|geographic")) %>%
      set_names(c("GEOID", "NAICS", "meaning", "year", "establishments", "employees", "q1", "annual")) %>%
      mutate(year = x)
  }), 
  bind_rows
)

##

write_csv(business, "business.csv")

##

test <- 
  bind_cols(business %>%
              filter(GEOID == "01001" & year == 2016) %>%
              select(meaning, NAICS, year) %>%
              rename_all(tolower),
            business %>%
              filter(GEOID == "01001" & year == 2006) %>%
              select(meaning, NAICS, year) %>%
              rename_all(toupper)) %>%
  select(naics, NAICS, meaning, MEANING, year, YEAR)

##

mix <-
  business %>%
  mutate(class = case_when(NAICS == "00" ~ "all",
                           NAICS == "11" ~ "blue",
                           NAICS == "21" ~ "blue",
                           NAICS == "31-33" ~ "blue", 
                           NAICS == "51" ~ "white",
                           NAICS == "52" ~ "white",
                           NAICS == "53" ~ "white",
                           NAICS == "54" ~ "white",
                           NAICS == "55" ~ "white",
                           NAICS == "61" ~ "pink",
                           NAICS == "62" ~ "pink")) %>%
  drop_na(class) %>%
  mutate(employees = case_when(employees == "a" ~ 10,
                               employees == "b" ~ 60,
                               employees == "c" ~ 175,
                               employees == "e" ~ 375,
                               employees == "f" ~ 750,
                               employees == "g" ~ 1750,
                               employees == "h" ~ 3760,
                               employees == "i" ~ 7500,
                               employees == "j" ~ 17500,
                               employees == "k" ~ 37500,
                               employees == "l" ~ 75000,
                               TRUE ~ as.numeric(employees))) %>%
  mutate(employees = as.numeric(employees))

##

write_csv(mix, "mix.csv")