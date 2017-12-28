# Reds WAR positions
# Need to figure out what the primary position was during inductees' top 4 WAR years so I can create position distributions


library(tidyverse)
library(openWARData)
library(Lahman)


redsWarJaws <- read_rds("data/indRedsWARandJAWS.rds")
indWarR <- read_rds("data/inducteeWARreds.rds")

# Getting player IDs and their top 4 WAR years
war4YearsDat <- indWarR %>%
      group_by(playerId) %>%
      top_n(4, rWAR)

# Making lists for map functions
idList <- list(war4YearsDat$playerId)
idList <- idList[[1]]
yearList <- list(war4YearsDat$yearId)
yearList <- yearList[[1]]

# Using lists to filter Fielding data set
aFilter <- function(x,y) filter(Fielding, playerID == x & yearID == y)
posDat2 <- map2_dfr(idList, yearList, aFilter)

# Getting the position where each player played the most games for each of the 4 yrs
posDat3 <- posDat2 %>% select(playerID, yearID, POS, G) %>% 
      group_by(playerID, yearID) %>% 
      filter(G == max(G)) %>% 
      ungroup()

# Calculating position each player played the most over the WAR4 years
posDat4 <- posDat3 %>% 
      select(playerID, POS) %>% 
      group_by(playerID, POS) %>% 
      summarize(nPOS = n()) %>% 
      filter(nPOS == max(nPOS)) %>% 
      ungroup() %>% 
      select(playerID, POS)

# Jim O'Toole's ID isn't showing up in the Fielding data set but he was a pitcher his whole career. Double-checked. Adding him into the df.
indPosList <- list(indPos$playerID)
indPosList <- indPosList[[1]]
setdiff(idList, indPosList)
posDat4 <- posDat4 %>% 
      add_row(playerID = "o'tooji01", POS = "P") %>% 
      rename(playerId = playerID)

# Removing the old POS column which I evidently didn't need to go to the trouble to add in the first place. Joining both dataframes to add new POS. Ugh could've just done an add_column but I'm tired. 
redsWarJaws <- redsWarJaws %>% select(playerId, fangraphs_id, name_whole, totalYrs, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, name_first, name_last)

redsWarJaws <- inner_join(redsWarJaws, posDat4, by = "playerId")

# Reordering columns and renaming totalYrs to tenure because I think it's more accurate.
redsWarJaws <- redsWarJaws %>% select(playerId, fangraphs_id, name_whole, totalYrs, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, POS, name_first, name_last) %>% 
      rename(tenure = totalYrs)

write_rds(redsWarJaws, "data/indRedsWARandJAWS.rds")


# Nominees=======================================

nomWarR <- read_rds("data/nomineeWARreds.rds")
nRedsWarJaws <- read_rds("data/nomRedsWARandJAWS.rds")

# Getting player IDs and their top 4 WAR years
nRedsWar4YearsDat <- nomWarR %>%
      group_by(playerId) %>%
      top_n(4, rWAR)

# Making lists for map functions
nIdList <- list(nRedsWar4YearsDat$playerId)
nIdList <- nIdList[[1]]
nYearList <- list(nRedsWar4YearsDat$yearId)
nYearList <- nYearList[[1]]

# Using lists to filter Fielding data set
aFilter <- function(x,y) filter(Fielding, playerID == x & yearID == y)
nPosDat2 <- map2_dfr(nIdList, nYearList, aFilter)

# Getting the position where each player played the most games for each of the 4 yrs
nPosDat3 <- nPosDat2 %>% select(playerID, yearID, POS, G) %>% 
      group_by(playerID, yearID) %>% 
      filter(G == max(G)) %>% 
      ungroup()

# Calculating with position each player played the most over the WAR4 years
nPosDat4 <- nPosDat3 %>% 
      select(playerID, POS) %>% 
      group_by(playerID, POS) %>% 
      summarize(nPOS = n()) %>% 
      filter(nPOS == max(nPOS)) %>% 
      ungroup() %>% 
      select(playerID, POS) %>% 
      rename(playerId = playerID)

# Removing the old POS column Joining both dataframes to add new POS.
nRedsWarJaws <- nRedsWarJaws %>% select(playerId, fangraphs_id, name_whole, tenure, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, name_first, name_last)

nRedsWarJaws <- inner_join(nRedsWarJaws, nPosDat4, by = "playerId")

# Reordering columns
nRedsWarJaws <- nRedsWarJaws %>% select(playerId, fangraphs_id, name_whole, tenure, redsWAR, redsPeakWAR, redsWAR4, redsJAWS, POS, name_first, name_last)

write_rds(nRedsWarJaws, "data/nomRedsWARandJAWS.rds")

