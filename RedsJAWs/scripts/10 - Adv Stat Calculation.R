# Adv Stat Calculation
# Sections: Create Constants tibbles, Pitching(gather stats, calculation), Batting(gather stats, calculation)
# Note: Links to averages, factors, and constants (aka 'guts') can be found on the fangraphs glossary pages for each of the adv stats calc'd





library(tidyverse)
library(Lahman)


# Various constants
fConstants <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsConstants.csv")
# Park Factors
fFactors <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsParkFactors.csv")
# Handedness Park Factors
fHand <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsHandedness.csv")
# NL league Avgs for Pitching and Batting stats
fPitAvgs <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsNLPitAvgs.csv")
fBatAvgs <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsNLBatAvgs.csv")


iRedsWar <- read_rds("data/inducteeWARreds.rds") %>% 
      select(playerId, yearId)
nRedsWar <- read_rds("data/nomineeWARreds.rds") %>%
      select(playerId, yearId)




# Synching playerIds, yearIds to constants, etc =====================


# Creating inductee, nominee tibbles by filtering by yearId and binding playerId and yearId cols

iBatAvgs <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fBatAvgs, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nBatAvgs <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fBatAvgs, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inBatAvgs <- iBatAvgs %>% 
      bind_rows(nBatAvgs)


iPitAvgs <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fPitAvgs, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nPitAvgs <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fPitAvgs, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inPitAvgs <- iPitAvgs %>% 
      bind_rows(nPitAvgs)


iConstants <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fConstants, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>% 
      select(playerId, yearId, everything())

nConstants <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fConstants, Season == x)
}) %>% 
      bind_cols(nRedsWar) %>% 
      select(playerId, yearId, everything())
inConstants <- iConstants %>% 
      bind_rows(nConstants)

# 1878, 1879, 1880 not included. Opened csv in Excel and added season, team values so I could bind playerId and yearId cols (nrows needs to be equal).
# Starting at 2001, cols 10:13 are all missing values
# Starting at 1973, cols 4:14 are all missing values (leaving basic, team, and year)
# Starting at 1881, cols 3:14 are all missing values (leaving team and year)
iFactors <- map_dfr(iRedsWar$yearId, function(x) {
      filter(fFactors, Season == x)
}) %>% 
      bind_cols(iRedsWar) %>%
      select(playerId, yearId, everything())

nFactors <- map_dfr(nRedsWar$yearId, function(x) {
      filter(fFactors, Season == x)
}) %>%  
      bind_cols(nRedsWar) %>%
      select(playerId, yearId, everything())
inFactors <- iFactors %>% 
      bind_rows(nFactors)

# Handedness Park Factors only available from 2002 to 2015
iRedsWarx <- iRedsWar %>% 
      filter(between(yearId, 2002, 2015))
nRedsWarx <- nRedsWar %>%
      filter(between(yearId, 2002, 2015))

iHand <- map_dfr(iRedsWarx$yearId, function(x) {
      filter(fHand, Season == x)
}) %>% 
      bind_cols(iRedsWarx) %>% 
      select(playerId, yearId, everything())

nHand <- map_dfr(nRedsWarx$yearId, function(x) {
      filter(fHand, Season == x)
}) %>% 
      bind_cols(nRedsWarx) %>% 
      select(playerId, yearId, everything())
inHand <- iHand %>% 
      bind_rows(nHand)


# Pitching  ===========================================================


# Gathering needed stats ========================

iRedsWandJ <- read_rds("data/indRedsWARandJAWS.rds") %>% 
      filter(POS == "P")
redsYrsPit <- map_dfr(iRedsWandJ$playerId, function(x) {
      filter(iRedsWar, playerId == x)
})

redsPitStats <- map2_dfr(redsYrsPit$playerId, redsYrsPit$yearId, function(x, y) {
      filter(Pitching, playerID == x & yearID == y) %>% 
            mutate(IP = IPouts/3) %>% 
            select(playerID, yearID, H, HR, BB, HBP, SO, IP)
})

# Something is up with Jim O'Toole's ID. Pitching db in Lahman has otoolji01 while openWARdat doesn't have the "l" but has "'". Don't know if it's me or them. Think it's them though. Need to double-check in openWARdat and then the bbref site figure out which one is real.
otoole <- Pitching %>%
      filter(playerID == "otoolji01" & teamID == "CIN") %>%
      mutate(IP = IPouts/3) %>%
      select(-playerID) %>% 
      add_column(playerID = rep("o'tooji01", 9))
      select(playerID, yearID, H, HR, BB, HBP, SO, IP)
redsPitStats <- redsPitStats %>%
      bind_rows(otoole)


nRedsWandJ <- read_rds("data/nomRedsWARandJAWS.rds") %>% 
      filter(POS == "P")
nRedsYrsPit <- map_dfr(nRedsWandJ$playerId, function(x) {
      filter(nRedsWar, playerId == x)
})

nRedsPitStats <- map2_dfr(nRedsYrsPit$playerId, nRedsYrsPit$yearId, function(x, y) {
      filter(Pitching, playerID == x & yearID == y) %>% 
            mutate(IP = IPouts/3) %>% 
            select(playerID, yearID, H, HR, BB, HBP, SO, IP)
})

inRedsPitStats <- redsPitStats %>% 
      bind_rows(nRedsPitStats) %>% 
      rename(playerId = playerID, yearId = yearID)


# Calculation ===================================


fip <- function(HR,BB,HBP,K,IP,constant) {
            fip <- ((13*HR)+(3*(BB+HBP))-(2*K))/IP + constant
            return(fip)
      }


fip_minus <- function(FIP, ParkFactor, LeagueFIP, Basic) {
      if(is.na(ParkFactor)) {
            fipminus <- (FIP+(FIP-(FIP*(Basic/100))))/(LeagueFIP)*100 
      } else {
            fipminus <- (FIP+(FIP-(FIP*(ParkFactor/100))))/(LeagueFIP)*100
      }
}

playerList <- list(redsPitStats$playerID)
playerList <- playerList[[1]]
yearList <- list(redsPitStats$yearID)
yearList <- yearList[[1]]

# Empty tibble
tempTib <- tibble(
      playerId = character(),
      yearId = numeric(),
      K_per_BB = numeric(),
      K_per_nine = numeric(),
      WHIP = numeric(),
      FIP_minus = numeric()
)

# input: playerId and yearId; function calcs adv stats for each player

fillAdv <- function(player, year) {
      fipFactor <- inFactors %>% 
            filter(playerId == player & yearId == year)
      pF <- fipFactor$FIP[1]
      bsc <- fipFactor$Basic[1]
      
      cfipConstant <- inConstants %>%
            filter(playerId == player & yearId == year)
      cFIP <- cfipConstant$cFIP[1]
      
      lgFIPavg <- inPitAvgs %>%
            filter(playerId == player & yearId == year)
      lgFIP <- lgFIPavg$FIP[1]
      
      stat <- inRedsPitStats %>% 
            filter(playerId == player & yearId == year)
      
      FIP <- fip(stat$HR, stat$BB, stat$HBP, stat$SO, stat$IP, cFIP)
      
      tempTib <- tempTib %>%
            add_row(
                  playerId = player,
                  yearId = year,
                  K_per_BB = round(stat$SO/stat$BB, 2),
                  K_per_nine = round(stat$SO/(stat$IP/9), 2),
                  WHIP = round((stat$H + stat$BB)/stat$BB, 2),
                  FIP_minus = round(fip_minus(FIP, pF, lgFIP, bsc), 0)
            )
}

# Got everything except Will White's FIP- from 1878 to 1883 because there weren't HBP stats for him those years
seasRedsPitAdv <- map2_dfr(playerList, yearList, fillAdv)

write_rds(seasRedsPitAdv, "data/seasAdvancedPitStats.rds")


tenRedsPitAdv <- seasRedsPitAdv %>%
      select(-yearId) %>% 
      group_by(playerId) %>% 
      summarize(reds_K_per_BB = round(mean(K_per_BB), 2),
             reds_K_per_nine = round(mean(K_per_nine), 2),
             reds_WHIP = round(mean(WHIP), 2),
             reds_FIP_minus = round(mean(FIP_minus), 0)
      )

write_rds(tenRedsPitAdv, "data/tenureRedsAdvancedPitStats.rds")



# Batting ====================================================


# Gathering needed stats ========================

# inductees
iRedsWandJ <- read_rds("data/indRedsWARandJAWS.rds") %>% 
      filter(POS != "P")
iRedsYrsBat <- map_dfr(iRedsWandJ$playerId, function(x) {
      filter(iRedsWar, playerId == x)
})

# nominees
nRedsWandJ <- read_rds("data/nomRedsWARandJAWS.rds") %>% 
      filter(POS != "P")
nRedsYrsBat <- map_dfr(nRedsWandJ$playerId, function(x) {
      filter(nRedsWar, playerId == x)
})

# Both
inRedsYrsBat <- iRedsYrsBat %>% 
      bind_rows(nRedsYrsBat)

# Batting db augmented with other stats
# Provided in Lahman pkg
augBatStats <- battingStats()
inRedsBatStats <- map2_dfr(inRedsYrsBat$playerId, inRedsYrsBat$yearId, function(x, y) {
      filter(augBatStats, playerID == x & yearID == y)
})
inRedsBatStats <- inRedsBatStats %>% 
      rename(playerId = playerID, yearId = yearID)

      


# Calculation ===================================


fip <- function(HR,BB,HBP,K,IP,constant) {
            fip <- ((13*HR)+(3*(BB+HBP))-(2*K))/IP + constant
            return(fip)
      }



fip_minus <- function(FIP, ParkFactor, LeagueFIP, Basic) {
      if(is.na(ParkFactor)) {
            fipminus <- (FIP+(FIP-(FIP*(Basic/100))))/(LeagueFIP)*100 
      } else {
            fipminus <- (FIP+(FIP-(FIP*(ParkFactor/100))))/(LeagueFIP)*100
      }
}

playerList <- list(redsPitStats$playerID)
playerList <- playerList[[1]]
yearList <- list(redsPitStats$yearID)
yearList <- yearList[[1]]

# Empty tibble
tempTib <- tibble(
      playerId = character(),
      yearId = numeric(),
      K_per_BB = numeric(),
      K_per_nine = numeric(),
      WHIP = numeric(),
      FIP_minus = numeric()
)

# input: playerId and yearId; function calcs adv stats for each player

fillAdv <- function(player, year) {
      fipFactor <- inFactors %>% 
            filter(playerId == player & yearId == year)
      pF <- fipFactor$FIP[1]
      bsc <- fipFactor$Basic[1]
      
      cfipConstant <- inConstants %>%
            filter(playerId == player & yearId == year)
      cFIP <- cfipConstant$cFIP[1]
      
      lgFIPavg <- inPitAvgs %>%
            filter(playerId == player & yearId == year)
      lgFIP <- lgFIPavg$FIP[1]
      
      stat <- inRedsPitStats %>% 
            filter(playerId == player & yearId == year)
      
      FIP <- fip(stat$HR, stat$BB, stat$HBP, stat$SO, stat$IP, cFIP)
      
      tempTib <- tempTib %>%
            add_row(
                  playerId = player,
                  yearId = year,
                  K_per_BB = round(stat$SO/stat$BB, 2),
                  K_per_nine = round(stat$SO/(stat$IP/9), 2),
                  WHIP = round((stat$H + stat$BB)/stat$BB, 2),
                  FIP_minus = round(fip_minus(FIP, pF, lgFIP, bsc), 0)
            )
}

# Got everything except Will White's FIP- from 1878 to 1883 because there weren't HBP stats for him those years
seasRedsPitAdv <- map2_dfr(playerList, yearList, fillAdv)

write_rds(seasRedsPitAdv, "data/seasAdvancedPitStats.rds")


tenRedsPitAdv <- seasRedsPitAdv %>%
      select(-yearId) %>% 
      group_by(playerId) %>% 
      summarize(reds_K_per_BB = round(mean(K_per_BB), 2),
                reds_K_per_nine = round(mean(K_per_nine), 2),
                reds_WHIP = round(mean(WHIP), 2),
                reds_FIP_minus = round(mean(FIP_minus), 0)
      )

write_rds(tenRedsPitAdv, "data/tenureRedsAdvancedPitStats.rds")



#wOBA
woba <- function(year,AB,BB,IBB,HBP,single,double,triple,HR,SF) {
      wBB <- linearWeights$wBB[which(linearWeights$Season == year)]
      wHBP <- linearWeights$wHBP[which(linearWeights$Season == year)]
      w1B <- linearWeights$w1B[which(linearWeights$Season == year)]
      w2B <- linearWeights$w2B[which(linearWeights$Season == year)]
      w3B <- linearWeights$w3B[which(linearWeights$Season == year)]
      wHR <- linearWeights$wHR[which(linearWeights$Season == year)]
      woba <- ((wBB*BB)+(wHBP*HBP)+(w1B*single)+(w2B*double)+(w3B*triple)+(wHR*HR))/(AB+BB-IBB+SF+HBP)
      return(woba)
}

#wRAA
wraa <- function(woba,year,PA) {
      wraa <- ((woba-linearWeights$wOBA[which(linearWeights$Season == year)])/linearWeights$wOBAScale[which(linearWeights$Season == year)])*PA
      return(wraa)
}

#wRC
wrc <- function(wOBA,PA,year) {
      wrc <- (((wOBA-linearWeights$wOBA[which(linearWeights$Season == year)])/linearWeights$wOBAScale[which(linearWeights$Season == year)])+linearWeights$RPerPA[which(linearWeights$Season == year)])*PA
      return(wrc)
}

#wRC+
wrcplus <- function(wRAA,PA,year,parkfactor,leaguewRC) {
      leaguerpa <- linearWeights$RPerPA[which(linearWeights$Season == year)]
      wrcplus <- (((wRAA/PA + leaguerpa)+(leaguerpa-parkfactor*leaguerpa))/leaguewRC)*100
}

