########################################################
## Section 1: American Community Survey
## ## Set up a crosswalk for all geographies
## ## Download variables with TidyCensus
########################################################

library(tidyverse)
library(tidycensus)

##

crosswalk <- 
  fips_codes %>%
  mutate(GEOID = paste(state_code, county_code, sep = "")) %>%
  select(GEOID, state, county, state_code, county_code)

##

write_csv(counties, "crosswalk.csv")

##

library(glue)

##

batch <- 
  reduce(
    map(2012:2016, function(x) {
      get_acs(geography = "county", 
              variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001",
                            "B02001_002", "B02001_003", 
                            "C18120_002", "C18120_006", "C18120_003",
                            "B01001_001", "B11016_001", "B01002_001", "B06009_005",
                            "B05006_124", "B05006_001",
                            "B07001_033", "B07001_081"), 
              year = x, output = "wide") %>% 
        rename(gini = B19083_001E,
               earn = B06011_001E,
               home = B25077_001E,
               rent = B25113_001E,
               white = B02001_002E,
               black = B02001_003E,
               force = C18120_002E,
               unemployed = C18120_006E,
               employed = C18120_003E,
               peeps = B01001_001E,
               holds = B11016_001E,
               age = B01002_001E,
               edu = B06009_005E,
               latinos = B05006_124E,
               foreign = B05006_001E,
               nation = B07001_033E,
               abroad = B07001_081E) %>%
        select(GEOID, gini, earn, home, rent,
               white, black, 
               force, unemployed, employed, 
               peeps, holds, age, edu,
               latinos, foreign,
               nation, abroad) %>%
        mutate(year = glue("{x}"))
    }), 
    bind_rows
  )

##

write_csv(batch, "batch.csv")

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
  get_acs(geography = "county", variables = c("C18120_002", "C18120_006", "C18120_003"),
          year = 2012, output = "wide") %>%
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

fors <- 
  get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
                year = 2016, output = "wide") %>%
  rename(latinos = B05006_124E,
         foreign = B05006_001E) %>%
  select(GEOID, latinos, foreign)

##

outs <- 
  get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
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
  get_acs(geography = "county", variables = c("C18120_002", "C18120_006", "C18120_003"),
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

fors <- 
  get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
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
                      #     NAICS == "61" ~ "pink",
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

########################################################
## Section 3: Tiger files
## ## Download counties with Tigris
## ## Add in Media Markets
########################################################

library(tigris)
library(sf)

##

options(tigris_use_cache = TRUE)

##

counties <- 
  counties(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 

##

states <- 
  states(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(102003) 

##

library(rmapshaper)

##

simplified <- st_simplify(states, dTolerance = 50000, preserveTopology = TRUE)
simplified <- ms_simplify(states, keep = 0.001)

##

read_csv("~/Desktop/R/git/twentysixteen/data-in/leip/2016.csv") %>%
  clean_names() %>%
  mutate(fips = if_else(fips < 10000, paste("0", fips, sep = ""), paste(fips))) %>%
  transmute(GEOID = str_sub(fips, 1, 2),
            clinton_trump = clinton_trump) %>%
  group_by(GEOID) %>%
  summarise(clinton_trump = sum(clinton_trump)) %>%
  mutate(result = case_when(clinton_trump < 0 ~ "trump",
                            clinton_trump > 0 ~ "clinton")) %>%
  left_join(simplified) %>%
  st_as_sf() %>%
  ggplot(aes(fill = result)) +
  geom_sf(size = 0.5, colour = '#ffffff', show.legend = FALSE) +
  scale_fill_manual(values = c(pal[2], pal[8])) +
  theme_map() +
  ggsave("logo.png", height = 4, width = 6, dpi = 300)

##

st_write(states, "states.geojson")
st_write(counties, "counties.geojson")

########################################################
## Section 4: Location Quotients
## ## Use tiger files to create regions
## ## Computer location quotients
########################################################

library(tidyverse)
library(sf)

##

county_points <- 
  counties %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble()

##

library(FNN)

##

nn <- get.knnx(county_points, county_points, k = 10)

##

k <- 10

bases <- tibble()

##

for (i in 1:nrow(counties)) {
  
  iteration <-
    counties %>%
    slice(i) %>%
    pull(GEOID)
  
  index <- 
    nn$nn.index %>%
    as_tibble() %>%
    bind_cols(counties) %>%
    select(-geometry) %>%
    gather(county, index, V1:glue("V{k}")) %>%
    filter(GEOID == iteration) %>%
    pull(index)
  
  region <- 
    counties %>%
    slice(index) %>%
    st_drop_geometry() %>%
    mutate(region = 1)
  
  reference <- 
    mix %>%
    left_join(region) %>%
    drop_na() %>%
    group_by(NAICS, year) %>%
    summarise(employment = sum(employees)) %>%
    mutate(change = (employment - lag(employment)) / employment) %>%
    ungroup()
  
  base <- 
    reference %>%
    filter(NAICS == "00") %>%
    select(-NAICS) %>%
    rename(total_employment = employment,
           total_change = change) %>%
    right_join(reference) %>%
    mutate(base_share = employment / total_employment,
           GEOID = iteration) %>%
    select(GEOID, year, NAICS, base_share)
  
  bases <- bind_rows(bases, base)
  
}

##

basics <- 
  mix %>%
  filter(NAICS == "00") %>%
  select(-NAICS) %>%
  rename(total_employment = employees) %>%
  select(GEOID, year, total_employment) %>%
  right_join(mix) %>%
  mutate(area_share = employees / total_employment) %>%
  mutate(area_share = if_else(area_share > 1, 1, area_share)) %>%
  left_join(bases) %>%
  mutate(loquo = area_share / base_share) %>%
  drop_na()

##

basics %>%
  filter(year == 2006) %>%
  group_by(GEOID) %>%
  arrange(desc(loquo)) %>%
  slice(1)

##

collar <-
  basics %>%
  filter(year == 2006) %>%
  filter(class != "all") %>%
  group_by(GEOID) %>%
  arrange(desc(loquo)) %>%
  slice(1) %>%
  ungroup() %>%
  select(GEOID, class) %>%
  rename(collar = class)

##

loquos <-
  basics %>%
  group_by(GEOID, class) %>%
  summarise(colour = mean(loquo, na.rm = TRUE)) %>%
  spread(class, colour)

##

growth <-
  basics %>%
  group_by(GEOID, class, year) %>%
  summarise(employment = mean(employees, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(GEOID, class) %>%
  mutate(change = 1 - (employment / lag(employment))) %>%
  ungroup() %>%
  group_by(GEOID, class) %>%
  summarise(growth = mean(change, na.rm = TRUE)) %>%
  spread(class, growth)

##

jobs <- 
  loquos %>%
  select(-all) %>%
  rename(blue_share = blue,
         pink_share = pink,
         white_share = white) %>%
  left_join(growth) %>%
  left_join(collar)
