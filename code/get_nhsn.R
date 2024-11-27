library(RSocrata)
require(dplyr)

get_nhsn <- function() {
  url <- "https://data.cdc.gov/resource/mpgq-jmmr.json"
  nhsn <- read.socrata(url)

  ## Read in data for states
  fips <- read.csv("data/fips_mappings.csv")
  
  # Clean data totalconfflunewadm
  hhs <- nhsn %>%
    mutate(date = as.Date(weekendingdate)) %>%
    left_join(fips, by = c("jurisdiction" = "abbreviation")) %>%
    mutate(total_flu_new_adm = as.numeric(totalconfflunewadm),
           total_flu_new_adm = replace_na(total_flu_new_adm, 0)) %>%
    select(date, jurisdiction,location, location_name, hhs_region, total_flu_new_adm) %>%
    filter(!(jurisdiction %in% c("AS", "GU", "MP", "PR", "UM", "VI")),
           date > "2021-07-01") %>%
    arrange(date)
    
  return(hhs)
}
   
  