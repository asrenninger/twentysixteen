library(tidyverse)
library(janitor)

########################################################
## Section 1: Community Health Rankings
## ## Import all files
## ## allign columns
########################################################

vars <- c("FIPS",
          "Years of Potential Life Lost Rate", "YPLL Rate", "% Fair/Poor",
          "Physically Unhealthy Days", "Mentally Unhealthy Days")

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

aggregate <-
  aggregate %>%
  select(vars, "year") %>%
  clean_names() %>%
  replace_na(list(ypll_rate = 0, years_of_potential_life_lost_rate = 0)) %>%
  mutate(years_of_potential_life_lost_rate = years_of_potential_life_lost_rate + ypll_rate,
         GEOID = fips) %>%
  select(GEOID, everything(), -ypll_rate, -fips)


