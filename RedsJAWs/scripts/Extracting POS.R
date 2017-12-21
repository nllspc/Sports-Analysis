# Adding POS column

library(tidyverse)
library(openWARData)
library(Lahman)

indIdFinal <- read_rds("./data/inducteeIdsFinal.rds")
nomId <- read_rds("./data/nomineeIds.rds")
members <- read_rds("./data/memberScrape.rds")

posList <- list(members[,"POS"])
posList <- posList[[1]]
inductees <- list(members[,"Inductee"])
inductees <- inductees[[1]]

indPlusPos  <- tibble(name_whole = inductees, POS = posList)

# Getting rid of the undesirables in one fell swoop
indPlusPosA <- indPlusPos[-c(29, 45, 52, 55, 62, 66, 72, 76, 82, 33, 54, 63, 73, 21, 46, 30, 11, 67, 68), ]

# Some of the names have accent marks so I to get rid of them and now I'm replacing them. Griffeys had Jr and Sr which aren't in the data sets we're dealing with.
namAdd <- tribble(
      ~name_whole, ~POS,
      "Dolf Luque", "P",
      "Leo Cardenas", "SS",
      "Tony Perez", "1B",
      "Dave Concepcion", "SS",
      "Ken Griffey", "OF",
      "Jose Rijo", "P",
      "Cesar Geronimo", "CF",
      "Pedro Borbon", "P",
      "Ken Griffey", "CF"
      )
indPlusPosB <- bind_rows(indPlusPosA, namAdd)
indPlusPosFinal <- inner_join(indIdFinal, indPlusPosB, by = "name_whole")
# The Griffeys got double entered
indIdFinal <- indPlusPosFinal[-c(73, 74), ]

write_rds(indIdFinal, "data/inducteeIdsFinal.rds")



# Add position column for nominees

# Boone played 3 pos (2B, 3B, SS) so going to have to see when his top WAR years are.
boonePos <- Fielding %>% filter(playerID == "booneaa01" & teamID == "CIN") %>% select(POS, yearID)
# Dunn played 1B and OF
dunnPos <- Fielding %>% filter(playerID == "dunnad01" & teamID == "CIN") %>% select(POS, yearID)
# Franco and Graves are P; Rolen played 3B; Sanders OF

nomPos <- c("SS/2B/3B", "1B/OF", "P", "P", "3B", "OF")
nomId <- nomId %>% mutate(POS = nomPos)

write_rds(nomId, "data/nomineeIds.rds")
