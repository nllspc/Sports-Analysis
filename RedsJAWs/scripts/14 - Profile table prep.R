# Profile table prep


library(tidyverse)
library(clusterSim)



franchise_batting <- read_rds("data/13 - Franchise Batting.rds")
franchise_pitching <- read_rds("data/13 - Franchise Pitching.rds")

hof_batting <- read_rds("data/13 - HOF Batting.rds")
hof_pitching <- read_rds("data/13 - HOF Pitching.rds")



# Get only the numeric vars
fr_bat_num <- franchise_batting[-c(1,2)]
fr_pit_num <- franchise_pitching[-c(1,2,3,5,6)]
hof_bat_num <- hof_batting[-c(1,2)]
hof_pit_num <- hof_pitching[-c(1,2,3,5,6)]

# Positional Normalization
pnorm_fr_bat <- data.Normalization(fr_bat_num, type = "n1", normalization = "column")










