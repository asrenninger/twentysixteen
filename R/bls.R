########################################################
## Section 1: Employment estimates
## ## 
## ## 
########################################################

library(tidyverse)
library(glue)
library(readxl)
library(janitor)

##
## (https://www.bls.gov/lau/#cntyaa)
##

jobs <-
  reduce(
    map(12:16, function(x) {
      glue("~/Desktop/R/git/twentysixteen/data-in/bls/laucnty{x}.xlsx") %>%
        read_xlsx(skip = 4) %>%
        clean_names() %>%
        select(-code, -county_name_state_abbreviation, -x_1, -year) %>%
        rename(statefp = code_1,
               countyfp = code_2,
               rate = percent) %>%
        mutate(year = glue("20{x}"))
    }), 
    bind_rows
  )

##

glimpse(jobs)

##

clean <-
  jobs %>%
  mutate(employed = as.numeric(str_replace(employed, pattern = ",", replacement = "")),
         unemployed = as.numeric(str_replace(unemployed, pattern = ",", replacement = "")),
         force = as.numeric(str_replace(force, pattern = ",", replacement = ""))) %>%
  mutate(GEOID = paste(statefp, countyfp, sep = "")) %>%
  select(GEOID, year, force, employed, unemployed, rate) %>%
  filter(GEOID != "NANA")

##

write_csv(clean, "jobs.csv")

##