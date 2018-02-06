# Numbers pg table prep



library(tidyverse)



# Tenure ==============================================

# Batting has no fg_id, etc. Plus need to wittle down some of these stats and maybe add others. Basically a re-do of script 13 which is pretty much a re-do of script 10. UGH!


# Reds HOF inductee and nominee war and jaws (career) stats
iWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJp <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS == "P")
inWandJb <- iWandJ %>% bind_rows(nWandJ) %>% filter(POS != "P")

# franchise advanced stats for entire Reds career
advFranBat <- read_rds("data/09 - franchiseAdvBatting.rds")

# franchise traditional stats for entire Reds career
tradFranBat <- read_rds("data/09 - franchiseTradBatting.rds")



# Batting =============

fgFranBat <- read_csv("data/csv/09 - FanGraphs Franchise Batting.csv")
rolen <- read_csv("data/csv/09 - FanGraphs Rolen.csv") %>% 
      filter(Name == "Scott Rolen")
mcc <- read_csv("data/csv/09 - FanGraphs McCormick.csv") %>% 
      filter(Name == "Mike McCormick")
smk <- read.csv("data/csv/09 - FanGraphs Burgess.csv") %>% 
      rename(Name = ï..Name) %>% 
      mutate(Name = as.character(Name)) %>% 
      filter(Name == "Smoky Burgess")

left_out <- rolen %>% 
      bind_rows(mcc) %>% 
      bind_rows(smk) %>%
      select(playerid, Name, WAR)
fgFranBat <- fgFranBat %>% 
      select(playerid, Name, WAR) %>% 
      bind_rows(left_out) %>% 
      mutate(Name = if_else(Name == "Ken Griffey Jr.", "Ken Griffey", Name), Name = if_else(Name == "Eddie Taubensee", "Ed Taubensee", Name), Name = if_else(Name == "George Kelly", "High Pockets Kelly", Name), Name = if_else(Name =="Dick Hoblitzel", "Dick Hoblitzell", Name), Name = if_else(Name == "Elmer Smith", "Mike Smith", Name))
missing_fg <- setdiff(inWandJb$fangraphs_id, fgFranBat$playerid)
# Not applicable. Its franchise so it's supposed to have player not in HOF. Just leaving this here. Had players missing in this process and everything got confused.
#missing_wj <- setdiff(fgFranBat$playerid, inWandJb$fangraphs_id)

# Adding WAR col
advFranBat <- advFranBat %>%
      inner_join(fgFranBat, by = c("name_whole" = "Name", "fg_playerId" = "playerid")) %>%
      rename(Name = name_whole)
missing_fg <- setdiff(inWandJb$fangraphs_id, advFranBat$fg_playerId)

# Combine traditional and advanced tables; 
franchise_batting <- tradFranBat %>% 
      inner_join(advFranBat[, -c(2,3,4,5,6,7,8)], by = "bbref_playerId")
missing_fran <- setdiff(inWandJb$playerId, franchise_batting$bbref_playerId)

# add Jr to Griff; remove some extraneous columns; rename name_whole, WAR
jr <- franchise_batting %>% 
      filter(fg_playerId == "327") %>% 
      mutate(name_whole = if_else(name_whole == "Ken Griffey", "Ken Griffey Jr", name_whole))


fran_num <- franchise_batting[-54,] %>% 
      bind_rows(jr) %>%
      rename(Name = name_whole, fWAR = WAR) %>% 
      arrange(bbref_playerId) %>% 
      select(-CS)

K_num <- strsplit(fran_num$`K%`, " ", fixed = TRUE)
BB_num <- strsplit(fran_num$`BB%`, " ", fixed = TRUE)

fran_num <- fran_num[-c(19,20)]

firstElt <- function(x) {x[1]}
fran_num$K_num <- sapply(K_num, firstElt)
fran_num$BB_num <- sapply(BB_num, firstElt)

fran_num <- fran_num %>% 
      mutate('K%' = as.numeric(K_num), 'BB%' = as.numeric(BB_num)) %>% 
      select(-K_num, -BB_num)

fran_num <- fran_num %>% 
      select(bbref_playerId:BB, `BB%`, SO, `K%`, everything())

hof_num <- map_dfr(inWandJb$playerId, function(x) {filter(fran_num, bbref_playerId == x)})


write_rds(hof_num, "data/20 - Numbers pg HOF Batting.rds")

# Pitching table looks good as is I think.
num_hof_pitching <- read_rds("data/13 - HOF Pitching.rds")



# Season ======================================================


# Traditional

sTradBat <- read_rds("data/10 - seasTraditionalBatStats.rds") %>% 
      rename(`BBRef Id` = bbref_id)
sTradPit <- read_rds("data/10 - seasTraditionalPitStats.rds") %>% 
      mutate(IP = plyr::round_any(IP, accuracy = 0.1, f = floor)) %>% 
      rename(`BBRef Id` = bbref_id)

# Advanced
sAdvBat <- read_rds("data/10 - seasAdvancedBatStats.rds") %>% 
      rename(`BBRef Id` = playerId) %>% 
      select(-yearId)
sAdvPit <- read_rds("data/10 - seasAdvancedPitStats.rds") %>% 
      rename(`BBRef Id` = playerId, Year = yearId)

# 53 Batters and 29 pitchers. Good
sAdvBat %>% summarize(n = n_distinct(`BBRef Id`))
sTradBat %>% summarize(n = n_distinct(`BBRef Id`))
sTradPit %>% summarize(n = n_distinct(`BBRef Id`))
sAdvPit %>% summarize(n = n_distinct(`BBRef Id`))

# Get Names and FG Ids
num_hof_batting <- read_rds("data/20 - Numbers pg HOF Batting.rds") 
num_hof_pitching <- read_rds("data/13 - HOF Pitching.rds")

seasBat <- sTradBat %>%
      mutate(`FG Id` = plyr::mapvalues(`BBRef Id`, from = num_hof_batting$bbref_playerId, to = num_hof_batting$fg_playerId), Name = plyr::mapvalues(`BBRef Id`, from = num_hof_batting$bbref_playerId, to = num_hof_batting$Name)) %>% 
      select(`BBRef Id`, `FG Id`, Name, everything()) %>% 
      inner_join(sAdvBat, by = "BBRef Id")

