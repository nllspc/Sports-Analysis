# Franchise data
# sections: Batting, Pitching
# Note: CSVs created from Baseball Reference website. Rate stat thresholds obtained from BBref franchise leader board page.


library(tidyverse)


# Batting =======================================

redsFranBat <- read_csv("./redsFranchiseBattingJan2018.csv")


# Splitting Name column into name_whole and playerId. Removing "HOF" from name_whole players who are in MLB HOF.
nameId <- strsplit(redsFranBat$Name, "\\", fixed = TRUE)
firstElt <- function(x) {x[1]}
secondElt <- function(y) {y[2]}
redsFranBat$playerId <- sapply(nameId, secondElt)
redsFranBat$name_whole <- sapply(nameId, firstElt)
redsFranBat <- redsFranBat %>%
      select(-Name, -Rk) %>%
      select(playerId, name_whole, everything())
name_whole <- strsplit(redsFranBat$name_whole, " HOF", fixed = TRUE)
redsFranBat <- redsFranBat %>% 
      select(-name_whole) %>% 
      add_column(name_whole) %>% 
      select(playerId, name_whole, everything())

# Position players that are below the 1500 PA threshold used to qualify for rate statistic ranking yet are in Reds HOF or are a nominee. Assuming top ranked players in counting stats have enough PAs.
fewAB <- redsFranBat %>% 
      filter(name_whole == "Scott Rolen" | name_whole == "Mike McCormick")

redsFranBat <- redsFranBat %>% 
      filter(PA > 1499) %>% 
      bind_rows(fewAB)

write_rds(redsFranBat, "data/09 - redsFranchiseBatting.rds")



# Pitching =====================================


redsFranPit <- read_csv("./redsFranchisePitchingJan2018.csv")


# Same as with Batting
nameId <- strsplit(redsFranPit$Name, "\\", fixed = TRUE)
firstElt <- function(x) {x[1]}
secondElt <- function(y) {y[2]}
redsFranPit$playerId <- sapply(nameId, secondElt)
redsFranPit$name_whole <- sapply(nameId, firstElt)
redsFranPit <- redsFranPit %>%
      select(-Name, -Rk) %>%
      select(playerId, name_whole, everything())
name_whole <- strsplit(redsFranPit$name_whole, " HOF", fixed = TRUE)
redsFranPit <- redsFranPit %>% 
      select(-name_whole) %>% 
      add_column(name_whole) %>% 
      select(playerId, name_whole, everything())

# Filtering nominee and inductee dfs for pitchers and their playerIds
iRedsWandJp <- iRedsWandJ %>% 
      filter(POS == "P")
nRedsWandJp <- nRedsWandJ %>% 
      filter(POS == "P")
inRedsWandJp <- iRedsWandJp %>%
      bind_rows(nRedsWandJp)

# Using those playerIds to filter Francise df to see if any pitchers fall below the 500 IP threshold for rate stat qualification. Everybody made it.
inRedsFranPit <- map_dfr(inRedsWandJp$playerId, function(x) {redsFranPit %>% filter(playerId == x)})

# Filtering Franchise df for pitchers with at least 500 innings pitched
redsFranPit <- redsFranPit %>% 
      filter(IP > 499)

write_rds(redsFranPit, "data/09 - redsFranchisePitching.rds")


