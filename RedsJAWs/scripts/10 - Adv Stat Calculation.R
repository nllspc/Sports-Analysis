# Adv Stat Calculation
# Sections: Create Constants tibbles, Pitching(gather stats, calculation), Batting(gather stats, calculation)
# Note: Links to averages, factors, and constants (aka 'guts') can be found on the fangraphs glossary pages for each of the adv stats calc'd





library(tidyverse)
library(Lahman)
library(rlang)

# Various constants
fConstants <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsConstants.csv")
# Park Factors
fFactors <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsParkFactors.csv")
# Handedness Park Factors
fHand <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsHandedness.csv")
# NL league Avgs for Pitching and Batting stats
fPitAvgs <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsNLPitAvgs.csv")
fBatAvgs <- read_csv("C:/Users/Kevin/Documents/R/Data/Fangraphs/FanGraphsNLBatAvgs.csv")


iRedsWar <- read_rds("data/03 - inducteeWARreds.rds") %>% 
      select(playerId, yearId)
nRedsWar <- read_rds("data/03 - nomineeWARreds.rds") %>%
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
      bind_rows(nConstants) %>% 
      rename(R_per_PA = "R/PA", R_per_W = "R/W")

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

iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
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
      add_column(playerID = rep("o'tooji01", 9)) %>% 
      select(playerID, yearID, H, HR, BB, HBP, SO, IP)
redsPitStats <- redsPitStats %>%
      bind_rows(otoole)


nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds") %>% 
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


fip <- function(HR,BB,HBP,K,IP,cFIP) {
            fip <- ((13*HR)+(3*(BB+HBP))-(2*K))/IP + cFIP
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
tempTibPit <- tibble(
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
      
      tempTibPit <- tempTibPit %>%
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

missPitAdv <- naniar::miss_var_summary(seasRedsPitAdv)

write_rds(seasRedsPitAdv, "data/seasAdvancedPitStats.rds")


tenRedsPitAdv <- seasRedsPitAdv %>%
      select(-yearId) %>% 
      group_by(playerId) %>% 
      summarize(reds_K_per_BB = round(mean(K_per_BB), 2),
             reds_K_per_nine = round(mean(K_per_nine), 2),
             reds_WHIP = round(mean(WHIP), 2),
             reds_FIP_minus = round(mean(FIP_minus), 0)
      )

write_rds(tenRedsPitAdv, "data/tenAdvancedPitStats.rds")



# Batting ====================================================


# Gathering needed stats ========================

# inductees
iRedsWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      filter(POS != "P")
iRedsYrsBat <- map_dfr(iRedsWandJ$playerId, function(x) {
      filter(iRedsWar, playerId == x)
})

# nominees
nRedsWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds") %>% 
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
      rename(playerId = playerID, yearId = yearID) %>%
      mutate(one_B = H - (X2B + X3B + HR))

write_rds(inRedsBatStats, "data/inRedsBatStats.rds")


# Calculation ===================================

inRedsBatStats <- read_rds("data/inRedsBatStats.rds")



benchBatAdv <- seasRedsBatAdv %>% 
      filter(playerId == "benchjo01" & yearId == "1969")
benchFactor <- inFactors %>% 
      filter(playerId == "benchjo01" & yearId == "1969")
benchAvg <- inBatAvgs %>%
      filter(playerId == "benchjo01" & yearId == "1969")
benchStat <- inRedsBatStats %>% 
      filter(playerId == "benchjo01" & yearId == "1969")
benchCon <- inConstants %>% 
      filter(playerId == "benchjo01" & yearId == "1969")
woba <- ((wBB*BB)+(wHBP*HBP)+(w1B*one_B)+(w2B*X2B)+(w3B*X3B)+(wHR*HR))/(AB+BB-IBB+SF+HBP)

#  Bench 1969 wOBA
#  0.382 using constants pulled from website, 0.373 listed at website
#  ((wBB*BB)+(wHBP*HBP)+(w1B*one_B)+(w2B*X2B)+(w3B*X3B)+(wHR*HR))/(AB+BB-IBB+SF+HBP)
#  ((0.705*49) + (0.739*4) + (0.913*106) + (1.316*23) + (1.679*1) + (2.195*26)) / (532+49-7+7+4)
# 
#  Bench 1969 wRAA
#  22.02 using website's wOBA 0.373, 25.9 using my wOBA 0.382, wRAA of 26.4 listed at website
#  ((wOBA_val-lgwOBA)/wOBAScale)*PA
#  ((0.373 - 0.323) / 1.344) * 592
# 
#  Bench 1969 wRC+ using website's 26.4 wRAA
#  126 using website's wRAA 26.4, 125 using my wRAA, 126 wRC+ listed at website
# ((((26.4 / 592) + 0.107) + (0.107 - ((107/100) * 0.107))) / (7822 / 68383)) * 100
# 
#  While this one is close, the rest are not. 1970 - 1975 - mine: 141, 106, 163, 122, 148, 145; FG's: 144, 106, 156, 115, 142, 140


# Example on wRC+ webpage, works
# ((((wRAA_val/PA) + r_per_PA) + (r_per_PA - (pF *r_per_PA)))/(lgwRC/lgPA))*100
((((48.2 / 639) + 0.114) + (0.114 - (0.95 * 0.114))) / (10032 / 85797)) * 100

# wOBA
woba <- function(player, year) {
      
      obaStat <- inRedsBatStats %>%
            filter(playerId == !!player & yearId == !!year)
      BB <- obaStat$BB[1]
      HBP <- obaStat$HBP[1]
      one_B <- obaStat$one_B[1]
      X2B <- obaStat$X2B[1]
      X3B <- obaStat$X3B[1]
      HR <- obaStat$HR[1]
      AB <- obaStat$AB[1]
      IBB <- obaStat$IBB[1]
      SF <- obaStat$SF[1]

      obaCon <- inConstants %>%
            filter(playerId == !!player & yearId == !!year)
      wBB <- obaCon$wBB[1]
      wHBP <- obaCon$wHBP[1]
      w1B <- obaCon$w1B[1]
      w2B <- obaCon$w2B[1]
      w3B <- obaCon$w3B[1]
      wHR <- obaCon$wHR[1]

      woba <- ((wBB*BB)+(wHBP*HBP)+(w1B*one_B)+(w2B*X2B)+(w3B*X3B)+(wHR*HR))/(AB+BB-IBB+SF+HBP)

}

# wRAA
wraa <- function(wOBA_val, player, year) {

      raaStat <- inRedsBatStats %>%
            filter(playerId == !!player & yearId == !!year)
      PA <- raaStat$PA[1]

      raaCon <- inConstants %>%
            filter(playerId == !!player & yearId == !!year)
      wOBAScale <- raaCon$wOBAScale[1]

      raaAvg <- inBatAvgs %>%
            filter(playerId == !!player & yearId == !!year)
      lgwOBA <- raaAvg$wOBA[1]
      
      wraa <- ((wOBA_val-lgwOBA)/wOBAScale)*PA

}

# wRC+
wrcplus <- function(wRAA_val, player, year) {
      
      rcStat <- inRedsBatStats %>% 
            filter(playerId == !!player & yearId == !!year)
      PA <- rcStat$PA[1]
      
      rcCon <- inConstants %>%
            filter(playerId == !!player & yearId == !!year)
      r_per_PA <- rcCon$R_per_PA[1]
      
      rcAvg <- inBatAvgs %>% 
            filter(playerId == !!player & yearId == !!year)
      lgwRC <- rcAvg$wRC[1]
      lgPA <- rcAvg$PA[1]
      
      rcFactor <- inFactors %>% 
            filter(playerId == !!player & yearId == !!year)
      pF <- rcFactor$Basic[1]
      
      wrcplus <- ((((wRAA_val / PA) + r_per_PA) + (r_per_PA - ((pF/100) * r_per_PA))) / (lgwRC/lgPA)) * 100
}

tempTibBat <- tibble(
      playerId = character(),
      yearId = numeric(),
      wOBA = numeric(),
      wRAA = numeric(),
      wRC_plus = numeric()
)

fillBatAdv <- function(player, year) {
      
      wOBA_val <- woba(quo(player), quo(year))
      wRAA_val <- wraa(wOBA_val, quo(player), quo(year))
      wRC_plus_val <- wrcplus(wRAA_val, quo(player), quo(year))
      
      tempTibBat <- tempTibBat %>%
            add_row(
                  playerId = player,
                  yearId = year,
                  wOBA = round(wOBA_val, 3),
                  wRAA = round(wRAA_val, 1),
                  wRC_plus = round(wRC_plus_val, 0)
            )
      
}

playerBatList <- list(inRedsBatStats$playerId)
playerBatList <- playerBatList[[1]]
yearBatList <- list(inRedsBatStats$yearId)
yearBatList <- yearBatList[[1]]

seasRedsBatAdv <- map2_dfr(playerBatList, yearBatList, fillBatAdv)

missBatAdv <- naniar::miss_var_summary(seasRedsBatAdv)


write_rds(seasRedsBatAdv, "data/seasAdvancedBatStats.rds")






