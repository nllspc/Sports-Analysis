# Numbers pg table prep



library(tidyverse)



# Season ===========================================

# No names, no fg_id, and need to wittle down some of these stats and maybe add others

# Traditional
inBatStats <- read_rds("data/10 - inRedsBatStats.rds") %>% 
      filter(teamID == "CIN")
inPitStats <- read_rds("data/10 - inRedsPitStats.rds")

# Advanced
inAdvBat <- read_rds("data/10 - seasAdvancedBatStats.rds")
inAdvPit <- read_rds("data/10 - seasAdvancedPitStats.rds")

# 53 Batters which I think is correct but there should be 29 pitchers and there ain't. Just 27.  O'toole is present and accounted for so it's some other persons. Guess it's time to re-run script 10.
n_distinct(inAdvBat$playerId)
n_distinct(inBatStats$playerId)
n_distinct(inPitStats$playerId)
n_distinct(inAdvPit$playerId)
