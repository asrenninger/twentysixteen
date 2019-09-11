library(tidyverse)
library(janitor)

########################################################
## Section 1: Census data
## ## Convert into demography into rates and ratios
## ## Summarise employment situation
########################################################

acs <- read_csv("data-out/batch.csv")

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

regression <- 
  regression %>%
  mutate(collar = if_else(collar == "blue", 1, 0))

lm(change_2012 ~ 
     annual_pills + ssi_rate + dod_rate +
     density + home + foreclosure_rate +
     unemployment + earn + gini +
     percent_fair_poor +
     pcp_rate + dentist_rate +
     collar + 
     trump_rallies_dma_post_convention,
   data = regression) %>%
  summary()

##

unique(regression$collar)

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



