# Postseason and Awards




library(tidyverse)
library(Lahman)
library(rlang)

inRedsBatStats <- read_rds("data/10 - inRedsBatStats.rds")
inRedsPitStats <- read_rds("data/10 - inRedsPitStats.rds")

# Combined inductee and nominee lists

playerBatList <- list(inRedsBatStats$playerId)
playerBatList <- playerBatList[[1]]
yearBatList <- list(inRedsBatStats$yearId)
yearBatList <- yearBatList[[1]]

playerPitList <- list(inRedsPitStats$playerId)
playerPitList <- playerPitList[[1]]
yearPitList <- list(inRedsPitStats$yearId)
yearPitList <- yearPitList[[1]]


# Postseason
filter_BatPost <- function(x, y) {
      filter(battingStats(data = Lahman::BattingPost), playerID == x, yearID == y)
}
inRedsBatPost <- map2_dfr(playerBatList, yearBatList, filter_BatPost)

filter_PitPost <- function(x, y) {
      filter(PitchingPost, playerID == x, yearID == y)
}
inRedsPitPost <- map2_dfr(playerPitList, yearPitList, filter_PitPost)


# Awards
filter_BatAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x, yearID == y)
}
inRedsBatAwa <- map2_dfr(playerBatList, yearBatList, filter_BatAwa)


filter_PitAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x, yearID == y)
}
inRedsPitPost <- map2_dfr(playerPitList, yearPitList, filter_PitAwa)


# Award Shares
filter_BatSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x, yearID == y)
}
inRedsBatSha <- map2_dfr(playerBatList, yearBatList, filter_BatSha)


filter_PitSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x, yearID == y)
}
inRedsPitSha <- map2_dfr(playerPitList, yearPitList, filter_PitSha)


inRedsBatAwa <- inRedsBatAwa %>% 
      full_join(inRedsBatSha, by = c("playerID", "yearID")) %>% 
      rename(playerId = playerID, yearId = yearID, awardId.x = awardID.x, awardId.y = awardID.y) %>%
      select(playerId, yearId, awardId.x, awardId.y, tie, pointsWon, pointsMax, votesFirst)









