# Getting WAR data





library(tidyverse)
library(openWARData)

# Creating inductee (aka member) list
indIdList <- list(indId$playerId)
indIdList <- as.character(indIdList[[1]])

# Filtering from the rWAR dataset
warFilter <- function(x) filter(rWAR, playerId == x)
indWarDat <- map_dfr(indIdList, warFilter)

# Grouping together player stats together; selecting 4 vars; changing playerId class
# FOR THE LOVE OF GOD ALWAYS REMEMBER TO UNGROUP. Wouldn't let me change class therefore couldn't
# create a usable table. It was HELL to figure out why it wouldn't work.
indWarDatT <- indWarDat %>% group_by(playerId) %>%
  select(playerId, yearId, teamId, rWAR) %>% ungroup() %>%
  mutate(playerId = as.character(playerId))

# Only ONE player didn't have seven years (needed for JAWs calculation) in their career, Harry Craft.
# Makes you wonder why? Injury? What did he do in only 6 of being in a MLB uniform to deserve 
# Reds HOF status? Something in his bio? Postseason heroics?
table(indWarDatT$playerId)

# Filter and leave just the Reds years
indWarDatR <- filter(indWarDatT, teamId == "CIN")

# Now for the nominees
nomIdList <- list(nomId$playerId)
nomIdList <- as.character(nomIdList[[1]])
nomWarDat <- map_dfr(nomIdList, warFilter)
nomWarDatT <- nomWarDat %>% group_by(playerId) %>%
  select(playerId, yearId, teamId, rWAR) %>% ungroup() %>%
  mutate(playerId = as.character(playerId)) %>% 
      

# Just the Reds years
nomWarDatR <- filter(nomWarDatT, teamId == "CIN")

# Lets make some files to save this progress
write_rds(members, "Data/memberScrape.rds")
write_rds(indId, "Data/inducteeIds.rds")
write_rds(nomId, "Data/nomineeIds.rds")
write_rds(indWarDatT, "Data/inducteeWARtotal.rds")
write_rds(indWarDatR, "Data/inducteeWARreds.rds")
write_rds(nomWarDatT, "Data/nomineeWARtotal.rds")
write_rds(nomWarDatR, "Data/nomineeWARreds.rds")

# Load the files
members <- read_rds("Data/memberScrape.rds")
nomId <- read_rds("Data/nomineeIds.rds")
indId <- read_rds("Data/inducteeIds.rds")
nomWarT <- read_rds("Data/nomineeWARtotal.rds")
nomWarR <- read_rds("Data/nomineeWARreds.rds")
indWarT <- read_rds("Data/inducteeWARtotal.rds")
indWarR <- read_rds("Data/inducteeWARreds.rds")


# Ready for some damn analysis

