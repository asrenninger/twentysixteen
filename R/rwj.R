########################################################
## Section 1: Community Health Rankings
## ## Import all files
## ## allign columns
########################################################

library(tidyverse)
library(janitor)

vars <- c("FIPS",
          "Years of Potential Life Lost Rate", "YPLL Rate", "% Fair/Poor",
          "Physically Unhealthy Days", "Mentally Unhealthy Days",
          "PCP Rate", "Dentist Rate", "MHP Rate")

##

library(glue)

##

aggregate <- 
  reduce(
    map(2010:2016, function(x) {
      glue("~/Desktop/R/git/twentysixteen/data-in/rwj/{x}.csv") %>%
        read_csv() %>%
        mutate(year = x)
    }), 
    bind_rows
  ) 

##

names(aggregate)[str_detect(names(aggregate), "PCP Rate|MHP Rate|Dentist Rate")]
names(aggregate)[str_detect(names(aggregate), "MHP")]
names(aggregate)[str_detect(names(aggregate), "Dentist Rate")]

aggregate %>%
  select(names(aggregate)[str_detect(names(aggregate), "PCP Rate|MHP Rate|Dentist Rate")], "year") %>%
  group_by(year) %>%
  summarise(s = sum(is.na(`Dentist Rate`)))

aggregate %>%
  select(names(aggregate)[str_detect(names(aggregate), "PCP Rate|MHP Rate|Dentist Rate")], "year") %>%
  group_by(year) %>%
  summarise(s = sum(is.na(`MHP Rate`)))


##

aggregate <-
  aggregate %>%
  select(vars, "year") %>%
  clean_names() %>%
  replace_na(list(ypll_rate = 0, years_of_potential_life_lost_rate = 0)) %>%
  mutate(years_of_potential_life_lost_rate = years_of_potential_life_lost_rate + ypll_rate,
         GEOID = fips) %>%
  select(GEOID, everything(), -ypll_rate, -fips)

change <- function(x) {((x / lag(x)) - 1) * 100}

##

rates <- 
  aggregate %>% 
  filter(year > 2012) %>%
  group_by(GEOID) %>%
  mutate_if(is.numeric, change) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  select(-year) %>%
  set_names(c(names(select(aggregate, -year)) %>% str_c("_ch", sep = ""))) %>%
  rename(GEOID = GEOID_ch)

##

rates[is.na(rates)] <- NA

##

aggregate %>%
  filter(year == 2016) %>%
  select(-year) %>%
  left_join(rates) %>%
  write_csv("health.csv")

##




