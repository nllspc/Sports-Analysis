# JAWS splash visual



library(tidyverse)
library(gghighlight)

# average HOF values
other_summary <- read_rds("data/07b - otherGroupSummary.rds")
hofWJ <- read_rds("data/16 - Average HOF WAR and JAWS.rds")
wtHofWJ <- read_rds("data/16 - Weighted Average HOF WAR and JAWS.rds")


# Player War and Jaws

nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      bind_rows(nWandJ)

inWar <- inWandJ %>% 
      mutate(name_whole = if_else(fangraphs_id == 327, "Ken Griffey Jr", name_whole)) %>% 
      select(name_whole, redsWAR, redsWAR4, POS) %>%
      rename(Name = name_whole)




wtWarAvg <- wtHofWJ %>% 
      select(POS, wtWAR_avg) %>% 
      rename(group = POS)
