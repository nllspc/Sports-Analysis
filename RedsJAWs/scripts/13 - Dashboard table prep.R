# Table preparation for the dashboard




library(tidyverse)
library(broom)


iWandJ <- read_rds("data/05 06 07b - indRedsWARandJAWS.rds")
nWandJ <- read_rds("data/05 06 07b - nomRedsWARandJAWS.rds")

advFranBat <- read_rds("data/09 - advFranchiseBatting.rds")
advFranPit <- read_rds("data/09 - advFranchisePitching.rds")

tradFranPit <- read_rds("data/09 - tradFranchisePitching.rds")
tradFranBat <- read_rds("data/09 - tradFranchiseBatting.rds")

advFranFld <- read_rds("data/10 - careerAdvancedFieldingStats.rds")

seasTradHofBat <- read_rds("data/10 - inRedsBatStats.rds")
seasTradHofPit <- read_rds("data/10 - inRedsPitStats.rds")

seasAdvHofBat <- read_rds("data/10 - seasAdvancedBatStats.rds")
seasAdvHofPit <- read_rds("data/10 - seasAdvancedPitStats.rds")



# Stat Rank ======================================================


# Probably want 2 sets of tables with each set having two tables. 1 set with ranks and percentages for HOF and Franchise and the other with stats for HOF and Franchise.


trad_rank <- tradFranBat %>%
      mutate(rank_HR = min_rank(desc(HR)), perc_HR = round(percent_rank(HR) * 100, 0)) 


rank_perc_FUN <- function(x) {
      
      rank_var <- paste0("rank_", x)
      perc_var <- paste0("perc_", x)
      
      tradFranBat %>%
            mutate(!!rank_var := min_rank(desc(!! rlang::sym(x))), !!perc_var := round(percent_rank(!! rlang::sym(x)) * 100, 0)) %>% 
            select(rank_var, perc_var)
}

id_cols <- tradFranBat %>% 
      select(bbref_playerId, fg_playerId, name_whole)

col_names <- names(tradFranBat)
col_names <- col_names[-c(1:6)]
trad_rank_perc <- map_dfc(col_names, rank_perc_FUN) %>% 
      bind_cols(id_cols) %>% 
      select(bbref_playerId, fg_playerId, name_whole, everything())
      
