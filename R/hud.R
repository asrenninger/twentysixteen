library(tidyverse)
library(janitor)

##

foreclosed <- read_csv("data-in/hud/nsp_foreclosures.csv")

##

foreclosed %>%
  group_by(countycode) %>%
  slice(1) %>%
  transmute(GEOID = countycode,
            foreclosure_rate = (estimated_number_foreclosures / estimated_number_mortgages) * 100,
            predatory_loans =  (total_hicost_2004_to_2006_HMDA_loans / total_2004_to_2006_HMDA_loans) * 100) %>%
  write_csv("foreclosures.csv")
