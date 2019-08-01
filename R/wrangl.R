library(tidyverse)
library(tidycensus)

##

move <- get_estimates(geography = "county",
                      variables = "RNETMIG", 
                      year = 2015) %>%
  rename(migration = value) %>%
  select(-variable)

##

vars <- load_variables(year = 2015, dataset = "acs5", cache = TRUE)

test <- filter(vars, str_detect(concept, "MEDIAN INCOME"))
test <- filter(vars, str_detect(label, "Median income"))
test <- filter(vars, str_detect(name, "B06011_001"))

##

area <- 
  get_acs(geography = "county", 
          variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001"), 
          year = 2015, output = "wide") %>% 
  rename(gini = B19083_001E,
         earn = B06011_001E,
         home = B25077_001E,
         rent = B25113_001E) %>%
  select(GEOID, gini, earn, home, rent)

##

race <- 
  get_acs(geography = "county", 
          variables = c("B02001_002", "B02001_003"), 
          year = 2015, output = "wide") %>% 
  rename(white = B02001_002E,
         black = B02001_003E) %>%
  select(GEOID, white, black)

##

work <-
  get_acs(geography = "county", variables = c("C18120_002E", "C18120_006E", "C18120_003E"),
          year = 2015, output = "wide") %>%
  rename(force = C18120_002E,
         unemployed = C18120_006E,
         employed = C18120_003E) %>%
  select(GEOID, force, unemployed, employed)

##

live <- 
  get_acs(geography = "county", variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"),
          year = 2015, output = "wide") %>%
  rename(peeps = B01001_001E,
         holds = B11016_001E,
         age = B01002_001E,
         edu = B06009_005E) %>%
  select(GEOID, peeps, holds, age, edu)

##

fors <- get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
                year = 2015, output = "wide") %>%
  rename(latinos = B05006_124E,
         foreign = B05006_001E) %>%
  select(GEOID, latinos, foreign)

##

outs <- 
  mobility <- get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
                      year = 2015, output = "wide") %>%
  rename(nation = B07001_033E,
         abroad = B07001_081E) %>%
  select(GEOID, nation, abroad)

##

present <-
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
          year = 2015, output = "wide") %>% 
  rename(gini = B19083_001E,
         earn = B06011_001E,
         home = B25077_001E,
         rent = B25113_001E) %>%
  select(GEOID, gini, earn, home, rent)

##

race <- 
  get_acs(geography = "county", 
          variables = c("B02001_002", "B02001_003"), 
          year = 2015, output = "wide") %>% 
  rename(white = B02001_002E,
         black = B02001_003E) %>%
  select(GEOID, white, black)

##

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
  mobility <- get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
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

migration <- reduce(
  map(2015:2017, function(x) {
    get_estimates(geography = "county",
                  variables = "RNETMIG", 
                  year = x) %>%
      rename(migration = value) %>%
      select(-variable) %>%
      mutate(year = x)
  }), 
  bind_rows
)

##




