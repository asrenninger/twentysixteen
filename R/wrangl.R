library(tidyverse)
library(janitor)

########################################################
## Section 1: Census data
## ## Convert into demography into rates and ratios
## ## Summarise employment situation
########################################################

acs <- read_csv("data-out/batch.csv")

##

library(units)

##

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

##

write_csv(changes, "changes.csv")

##

census <-
  population %>%
  filter(year == "2016") %>%
  select(-year) %>%
  left_join(changes) %>%
  select(-state, -county, -state_code, -county_code)

########################################################
## Section 2: Finishing up the economy
## ## Create unemployment rate
## ## Measure change
########################################################

unemployment <- read_csv("data-out/jobs.csv")
ssi <- read_csv("data-out/ssi.csv")
foreclosures <- read_csv("data-out/foreclosures.csv")

economy <-
  unemployment %>%
  left_join(ssi) %>%
  left_join(foreclosures)

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

despair <- read_csv("data-out/despair.csv")

########################################################
## Section 4: Nielson data
## ## Add in rallies
## ## Add in advertising
########################################################

rallies_counties <- read_csv("data-out/rallies_counties.csv")
rallies_markets <- read_csv("data-out/rallies_DMA.csv")
rallies_distances <- read_csv("data-out/rallies_distances.csv")

##

rallies <-
  rallies_distances %>%
  left_join(rallies_markets) %>%
  left_join(rallies_counties) %>%
  replace_na(list(trump_rallies_dma_post_convention = 0,
                  clinton_rallies_dma_post_convention = 0,
                  trump_rallies_county_post_convention = 0,
                  clinton_rallies_county_post_convention = 0))

########################################################
## Section 5: aggregated data
## ## Line up the right hand of the regression
## ## 
########################################################

independent <- 
  health %>%
  left_join(despair) %>%
  left_join(census) %>%
  left_join(loquo) %>%
  left_join(economy) %>%
  left_join(rallies) %>%
  drop_na(GEOID)

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

regression %>%
  tabyl(state, flips) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  formattable::formattable()

##

names(regression)

regression <- 
  counties %>%
  mutate(continental = 1) %>%
  right_join(regression) %>%
  drop_na(continental) %>%
  select(-continental) %>%
  st_drop_geometry() %>%
  as_tibble()

##

regression %>%
  write_csv("regression.csv")

##

touching <- 
  st_touches(counties) %>%
  as_tibble()

coords <- 
  counties %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(neighbours = rowname)

##

library(glue)

##

ordest <- 
  touching %>%
  mutate(neighbours = as.character(row.id)) %>%
  left_join(coords) %>%
  rename(X1 = X,
         Y1 = Y) %>%
  mutate(neighbours = as.character(col.id)) %>%
  left_join(coords) %>%
  rename(X2 = X,
         Y2 = Y) %>%
  select(X1:Y2) %>%
  mutate(path = glue("{X1} {Y1} - {X2} {Y2}")) %>%
  mutate(start = glue("{X1} - {Y1}"),
         finish = glue("{X2} - {Y2}")) %>%
  select(path, start, finish) %>%
  group_by(path) %>%
  gather(position, coordinates, start:finish) %>%
  separate(coordinates, sep = " - ", into = c("X", "Y"), remove = FALSE) %>%
  mutate(X = as.numeric(X),
         Y = as.numeric(Y)) %>%
  ungroup()

ordest <- 
  touching %>%
  mutate(neighbours = as.character(row.id)) %>%
  left_join(coords) %>%
  rename(X1 = X,
         Y1 = Y) %>%
  mutate(neighbours = as.character(col.id)) %>%
  left_join(coords) %>%
  rename(X2 = X,
         Y2 = Y) %>%
  select(X1:Y2) %>%
  mutate(start = glue("{X1} {Y1}"),
         finish = glue("{X2} {Y2}")) %>%
  mutate(finish = sample_n(., nrow(.))$finish) %>%
  mutate(path = glue("{X1} {Y1} - {X2} {Y2}")) %>%
  select(path, start, finish) %>%
  group_by(path) %>%
  gather(position, coordinates, start:finish) %>%
  separate(coordinates, sep = " ", into = c("X", "Y"), remove = FALSE) %>%
  mutate(X = as.numeric(X),
         Y = as.numeric(Y)) %>%
  ungroup()

p <- 
  ggplot(data = ordest) +
  geom_point(aes(x = X, y =Y), 
             size = 0.1,
             colour = 'grey70', 
             alpha = 0.5) +
  geom_path(aes(x = X, y = Y, group = path),
            colour = 'grey40') +
  theme_void()

ggsave(p, filename = "test.png", height = 6, width = 8)


