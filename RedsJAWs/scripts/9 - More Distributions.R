# More distributions


library(tidyverse)
library(Lahman)


iRedsWandJ <- read_rds("data/indRedsWARandJAWS.rds")
iRedsYrs <- read_rds("data/inducteeWARreds.rds")
# Seeing how many OF'ers I have.
table(iRedsWandJ$POS)

# Subsetting df with players with position = outfielder
of <- iRedsWandJ %>% 
      select(playerId, POS) %>% 
      filter(POS == "OF")
idList <- list(of$playerId)
idList <- idList[[1]]
yrFilter <- function(x, y) {
      filter(iRedsYrs, playerId == x)
}
ofYears <- map_dfr(idList, yrFilter)


# Making lists to feed to map
idList2 <- list(ofYears$playerId)
idList2 <- idList2[[1]]
yrsList <- list(ofYears$yearId)
yrsList <- yrsList[[1]]

# Getting number of games played at each OF position for each season
ofFilter <- function(x,y) {
      filter(Appearances, playerID == x, yearID == y)
}
ofSplit <- map2_dfr(idList2, yrsList, ofFilter)
ofSplit_gathered <- gather(ofSplit, 'G_lf', 'G_cf', 'G_rf', key = "of_pos", value = "G")

# For each player, summing number of games played at each position for entire Reds tenure and getting the most played position.
ofSplitSum <- ofSplit_gathered %>% 
      select(playerID, of_pos, G) %>% 
      group_by(playerID, of_pos) %>% 
      summarize(sumG = sum(G)) %>% 
      filter(sumG == max(sumG)) %>% 
      ungroup()

# renaming columns and values, so I can eventually join to another df
# Yes, I know I should've just changed the column names before I gathered earlier.
ofPos <- ofSplitSum %>% 
      select(playerID, of_pos) %>% 
      mutate(POS = plyr::mapvalues(of_pos, from = c("G_lf", "G_cf", "G_rf"), to = c("LF", "CF", "RF"))) %>% 
      rename(playerId = playerID) %>% 
      select(playerId, POS)
table(ofPos$POS)

# Replacing the POS column values that used to be "OF" with our new values.
ofWandJ <- iRedsWandJ %>% 
      filter(POS == "OF") %>% 
      select(-POS) %>% 
      inner_join(ofPos, by = "playerId")
iRedsWandJ <- iRedsWandJ %>% 
      filter(POS != "OF") %>%
      bind_rows(ofWandJ)

write_rds(iRedsWandJ, "data/indRedsWARandJAWS.rds")



# Other groups =============================


cornerIF <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B") %>% 
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "CI")

middleIF <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "MI")

outField <- iRedsWandJ %>% 
      filter(POS == "LF" | POS == "CF" | POS == "RF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "OF")

corners <- iRedsWandJ %>% 
      filter(POS == "1B" | POS == "3B" | POS == "LF" | POS == "RF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "CO")

middle <- iRedsWandJ %>% 
      filter(POS == "2B" | POS == "SS" | POS == "C" | POS == "CF") %>%
      summarize(number = n(), redsWAR = round(mean(redsWAR), 2), redsWAR4 = round(mean(redsWAR4), 2), redsJAWS = round(mean(redsJAWS), 2)) %>% 
      add_column(Group = "Md")

other_Groups <- cornerIF %>% 
      bind_rows(middleIF, outField, corners, middle)

write_rds(other_Groups, "data/otherGroupDistributions.rds")


      