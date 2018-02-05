# 18 - JAWS tab table prep
# Need a table with the player's WAR for each year; distinguishes which years are the WAR4 years; has a position column


library(tidyverse)


# Line Chart ===============================================

# Data 

# All the WAR for all the Reds years
nomWarR <- read_rds("data/03 - nomineeWARreds.rds")
inWar <- read_rds("data/03 - inducteeWARreds.rds") %>% 
      bind_rows(nomWarR)

# Need player ids and positions
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
inWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      bind_rows(nWandJ) %>% 
      select(playerId, name_whole, POS)


# Minor manipulation

# WAR4 + years; add type column
inWar4 <- inWar %>%
      group_by(playerId) %>%
      top_n(4, rWAR) %>% 
      ungroup() %>% 
      select(-teamId) %>% 
      add_column(type = rep("WAR4", 328))

# Not WAR4 + years; add type column
notWar4 <- inWar %>% 
      anti_join(inWar4, by = c("playerId", "yearId")) %>% 
      select(-teamId) %>% 
      add_column(type = rep("WAR", 427))


# Add position column

add_pos_FUN <- function(df) {
      
      FUN <- function(x){
            position <- inWandJ %>% 
                  filter(playerId == x) %>% 
                  select(name_whole, POS)
            
            psn <- position[[2]]
            nm <- position[[1]]
            
            df %>% 
                  filter(playerId == x) %>% 
                  mutate(POS = rep(psn, n()), Name = rep(nm, n()))
      } 
      
      df_pos <- map_dfr(unique(df$playerId), FUN)
}

inWar4_pos <- add_pos_FUN(inWar4)
notWar4_pos <- add_pos_FUN(notWar4)

war_combined <- notWar4_pos %>% 
      bind_rows(inWar4_pos)


# Map weighted averages

#  Positional Average seasonal WAR values

pitMedWar <- war_combined %>% 
      filter(POS == "P") %>% 
      summarize(`Median Pitcher WAR` = median(rWAR))

posMedWAR <- war_combined %>% 
      filter(POS != "P") %>% 
      summarize(`Median Position WAR` = median(rWAR))


war_combo_avg <- war_combined %>% 
      mutate(`Median WAR` = if_else(POS == "P", pitMedWar$`Median Pitcher WAR`[[1]], posMedWAR$`Median Position WAR`[[1]])) %>% 
      rename(bbref_id = playerId, WAR = rWAR) %>% 
      select(bbref_id, Name, everything()) %>% 
      mutate(Name = if_else(bbref_id == "griffke02", "Ken Griffey Jr", Name))

write_rds(war_combo_avg, "data/18 - JAWS pg line chart table.rds")



# Cleveland Dot Chart ======================================

# Average Data
groupSummary <- read_rds("data/07b - otherGroupSummary.rds") %>% 
      rename(JAWS_avg = redsJAWS) %>% 
      select(Group, JAWS_avg)
wtJAWS_avg <- wtAvgWar %>% 
      select(POS, wtJAWS_avg) %>%
      rename(Group = POS, JAWS_avg = wtJAWS_avg)
      
group_pos_sum <- wtJAWS_avg %>% 
      bind_rows(groupSummary)


# Player JAWS data

nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")
playaWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds") %>% 
      bind_rows(nWandJ) %>% 
      select(playerId, name_whole, redsJAWS, POS) %>% 
      rename(bbref_id = playerId, Name = name_whole, JAWS = redsJAWS, Group = POS) %>% 
      mutate(Name = if_else(bbref_id == "griffke02", "Ken Griffey Jr", Name))


# Classify Players into other groups

cornerIF <- playaWandJ %>% 
      filter(Group == "1B" | Group == "3B") %>%
      mutate(Group = "CI")

middleIF <- playaWandJ %>% 
      filter(Group == "2B" | Group == "SS") %>%
      mutate(Group = "MI")

outField <- playaWandJ %>% 
      filter(Group == "LF" | Group == "CF" | Group == "RF") %>%
      mutate(Group = "OF")

corners <- playaWandJ %>% 
      filter(Group == "1B" | Group == "3B" | Group == "LF" | Group == "RF") %>%
      mutate(Group = "CO")

middle <- playaWandJ %>% 
      filter(Group == "2B" | Group == "SS" | Group == "C" | Group == "CF") %>%
      mutate(Group = "Md")

pwj_complete <- playaWandJ %>% 
      bind_rows(cornerIF, middleIF, outField, corners, middle)


# Map JAWS averages
jaws_group <- pwj_complete %>% 
      mutate(`Avg HOF` = plyr::mapvalues(Group, from = group_pos_sum$Group, to = group_pos_sum$JAWS_avg))


write_rds(jaws_group, "data/18 - JAWS pg dot chart table.rds")


