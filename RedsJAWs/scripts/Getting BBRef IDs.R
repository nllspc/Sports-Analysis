# Getting BBRef IDs for members and nominees



library(tidyverse)
library(openWARData)

# loading dataset with appropriate IDs
str(idTT)
head(idTT)

# Selecting ID and name columns; combining first, last names; renaming column
idTTa <- idTT %>% select(key_bbref, name_last, name_first) %>% 
  mutate(name_whole= paste(name_first, name_last)) %>% 
  rename(playerId = key_bbref)
str(idTTa)
head(idTTa)

# Taking column from other tibble and coercing into a list
inductees <- list(members[,"Inductee"])
inductees <- inductees[[1]]
str(inductees)
head(inductees)

# Taking that list and filtering the tibble to get inductee names only
indFilter <- function(x) filter(idTTa, name_whole == x)
indID <- map_dfr(inductees, indFilter)
# Only 85 obs. so we're missing one.
str(indID)
head(indID)

# Seeing missing ID values but they're duplicate names
View(indID)

# 86 distinct values which is the original count so duplicates aren't coming from 
# the original list.
indTib <- tibble(inductees)
count(distinct(indTib))

# Seven missing values
sum(indID$playerId=="")


# Take 2: This time filtering out those missing values.
indFilter2 <- function(x) filter(idTTa, name_whole == x & playerId != "")
indID <- map_dfr(inductees, indFilter2)

# Lets compare the two player lists and see what the filter loop missed
## Column names have to match.
indTib <- indTib %>% rename(name_whole = 'inductees')
indIDnam <- indID %>% select(name_whole)
# Some vowel marks, etc in the names are large part of the problem
# Don't see anything wrong w/Giles, Howsam, Herrmann though. Need to do some research on those birds.
missNames <- setdiff(indTib,indIDnam)

# From members list, Giles was president/GM; Howsam was a GM; Herrmann was president, so they can 
# remain removed.
# Create list with corrected names. There's a Griffey Jr and Sr but both should pop up.
missNamList <- list("Dolf Luque", "Leo Cardenas", "Tony Perez", "Dave Concepcion", "Ken Griffey",
                    "Jose Rijo", "Cesar Geronimo", "Pedro Borbon")

# Pass new list to the function and assign to a tibble
missId <- map_dfr(missNamList, indFilter2)
str(missId)

# Got everything expected except an extra Borbon. One has Felix as
# a middle name. Googled him and he didn't play for the Reds so Borbope02 we can drop.
missId
missId <- filter(missId, playerId != "borbope02")

# Combine with previous tibble
indId <- bind_rows(indID, missId)
View(indId)
# Have extra Pete Rose, Joe Morgan, Mike McCormick as well. Figured which ones to drop  
# by filtering rWAR data set and looking at the years played. Plus Sparky and Hutchinson  
# are managers. Want to try and make this apples to apples. The cutoff will be having Reds stats 
# after the dead ball era so Wright bros, Dummy Hoy, Bill McKechnie, Noodles Hahn, Sam Crawford,
# Cy Seymour, Bob Ewing, Bid McPhee, Will White, Tony Mullane, John Reilly, Jake Beckley 
# are out. Dropping Harry Craft because he only has 6yrs while everyone else has at least 7 and it
# makes the JAWS scores inconsistent with bbref. Final tally is 67 members worthy of JAWs calculation.
indId <- filter(indId, playerId != "rosepe02" & playerId != "morgajo01"
                & playerId != "andersp01" & playerId != "wrighge01" &
                  playerId != "wrighge03" & playerId != "wrighha01" &
                  playerId != "hoydu01" & playerId != "hahnno01" &
                  playerId != "mckecbi01" & playerId != "crawfsa01" &
                  playerId != "seymocy01" & playerId != "ewingbo01" &
                  playerId != "mcphebi01" & playerId != "whitewi01" &
                  playerId != "mullato01" & playerId != "reilljo01" &
                  playerId != "becklja01" & playerId != "mccormi03" &
                        playerId != "hutchfr01" & playerId != "craftha01")
                


# Now need the nominees
nomNamList <- list("Aaron Boone", "Adam Dunn", "John Franco", "Danny Graves", "Scott Rolen",
                   "Reggie Sanders")
nomId <- map_dfr(nomNamList, indFilter2)

# Snagged an extra Sanders. Filtered rWAR again. Dropping the older one.
nomId <- filter(nomId, playerId != "sandere01")

membersP <- tibble(members$Inductee, members$Position)
membersP <- rename(membersP, name_whole = members$Inductee, Position = members$Position)
filterP <- function(x) filter(membersP, name_whole == x)
indList <- list(indId$name_whole)
members2 <- map_dfr(indList, filterP )
filterM <- function(x) filter(indId, name_whole != x)
indId2 <- map_dfr(missNamList, filterM)
missNamList
indId3 <- transmute(indId2, name_whole = unique(name_whole))


