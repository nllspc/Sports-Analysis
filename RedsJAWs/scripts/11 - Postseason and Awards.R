# Postseason and Awards
# sections: Postseason (Per Season, Reds Tenure), Awards, Award Shares



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


# Postseason ===================================================

# Per Season ======================


# Batting

postBat_data <- battingStats(data = Lahman::BattingPost)
filter_BatPost <- function(x, y) {
      filter(postBat_data, playerID == x, yearID == y)
}
inRedsBatPost <- map2_dfr(playerBatList, yearBatList, filter_BatPost) %>% filter(teamID == "CIN")


# Pitching

filter_PitPost <- function(x, y) {
      filter(PitchingPost, playerID == x, yearID == y)
}
inRedsPitPost <- map2_dfr(playerPitList, yearPitList, filter_PitPost)



# Reds Tenure =====================


# Batting

# Counting Stats

stat_Batlabels <- names(inRedsBatPost)
stat_Batlabels <- stat_Batlabels[-c(1,2,3,4,5,23,26,27,28,29)]

sum_Batfill <- function(x) {
      inRedsBatPost %>% 
            filter(playerID == x) %>% 
            select(stat_Batlabels) %>% 
            colSums() %>% 
            t() %>% 
            as.tibble()
}

postBatList <- unique(playerBatList)

sum_postBatStats <- map_dfr(postBatList, sum_Batfill) %>% 
      add_column(playerId = postBatList) %>% 
      select(playerId, everything())

# Adding in some of the traditional rate stats      
sum_postBatStats <- battingStats(data = sum_postBatStats)

write_rds(sum_postBatStats, "data/11 - careerRedsPostseasonBat.rds")


# Pitching

stat_Pitlabels <- names(inRedsPitPost)
stat_Pitlabels <- stat_Pitlabels[-c(1,2,3,4,5,19,20)]

sum_Pitfill <- function(x) {
      inRedsPitPost %>% 
            filter(playerID == x) %>% 
            select(stat_Pitlabels) %>% 
            colSums() %>% 
            t() %>% 
            as.tibble()
}

postPitList <- unique(playerPitList)

sum_postPitStats <- map_dfr(postPitList, sum_Pitfill) %>% 
      add_column(playerId = postPitList) %>% 
      select(playerId, everything()) %>% 
      mutate(IP = round(IPouts/3, 2),
             ERA = round((9*ER)/IP, 2),
             WHIP = round((H+BB)/IP, 2),
             BAopp = round(H/(BFP - BB - HBP - SH - SF), 3)
      )
             

write_rds(sum_postPitStats, "data/11 - careerRedsPostseasonPit.rds")



# Awards =====================================================

# Batting

filter_BatAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x, yearID == y)
}
inRedsBatAwa <- map2_dfr(playerBatList, yearBatList, filter_BatAwa) %>% 
      select(playerID, yearID, awardID)
      
sum_BatAwa <- inRedsBatAwa %>% 
      select(playerID, awardID) %>% 
      group_by(playerID, awardID) %>% 
      summarize(number = n())

write_rds(sum_BatAwa, "data/11 - summaryBattingAwards.rds")


# Pitching

filter_PitAwa <- function(x, y) {
      filter(AwardsPlayers, playerID == x, yearID == y)
}
inRedsPitAwa <- map2_dfr(playerPitList, yearPitList, filter_PitAwa) %>% 
      select(playerID, yearID, awardID)

sum_PitAwa <- inRedsPitAwa %>% 
      select(playerID, awardID) %>% 
      group_by(playerID, awardID) %>% 
      summarize(number = n())

write_rds(sum_PitAwa, "data/11 - summaryPitchingAwards.rds")



# Award Shares =================================================

# Batting

filter_BatSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x, yearID == y)
}
inRedsBatSha <- map2_dfr(playerBatList, yearBatList, filter_BatSha) %>% 
      mutate(vote_percentage = round((pointsWon/pointsMax)*100, 0))


# Pitching

filter_PitSha <- function(x, y) {
      filter(AwardsSharePlayers, playerID == x, yearID == y)
}
inRedsPitSha <- map2_dfr(playerPitList, yearPitList, filter_PitSha) %>% 
      mutate(vote_percentage = round((pointsWon/pointsMax)*100, 0))

mvp_roy_bat <- inRedsBatAwa %>% 
      filter(awardID == "Most Valuable Player" | awardID == "Rookie of the Year") %>% 
      select(playerID, yearID)

mrList_player <- list(mvp_roy_bat$playerID)
mrList_player <- mrList_player[[1]]
mrList_year <- list(mvp_roy_bat$yearID)
mrList_year <- mrList_year[[1]]



no_win_BatShares <- map2_dfr(mrList_player, mrList_year, function(x, y) {filter(inRedsBatSha, playerID == x & yearID == y)})   
      

