# Profile table prep


library(tidyverse)
library(caret)

franchise_batting <- read_rds("data/13 - Franchise Batting.rds")
franchise_pitching <- read_rds("data/13 - Franchise Pitching.rds")

hof_batting <- read_rds("data/13 - HOF Batting.rds")
hof_pitching <- read_rds("data/13 - HOF Pitching.rds")



# Get only the numeric vars
fr_bat_num <- franchise_batting[-c(1,2)]
fr_pit_num <- franchise_pitching[-c(1,2,3,5,6)]
hof_bat_num <- hof_batting[-c(1,2)]
hof_pit_num <- hof_pitching[-c(1,2,3,5,6)]


# Reg standardization

mean_sd <- preProcess(fr_bat_num, method = c("center", "scale"))
mean_sd2 <- preProcess(fr_pit_num, method = c("center", "scale"))
mean_sd3 <- preProcess(hof_bat_num, method = c("center", "scale"))
mean_sd4 <- preProcess(hof_pit_num, method = c("center", "scale"))

rstd_fr_bat <- predict(mean_sd, fr_bat_num)
rstd_fr_pit <- predict(mean_sd2, fr_pit_num)
rstd_hof_bat <- predict(mean_sd3, hof_bat_num)
rstd_hof_pit <- predict(mean_sd4, hof_pit_num)



# Positional Normalization

# Other functions in various pkgs I found wouldn't deal with columns that had NAs. This will though.

mad_median_FUN <- function(df){
      df_nested <- df %>% 
            gather() %>%
            group_by(key) %>% 
            nest()
      
      pos_norm_FUN <- function(col) {
            col_n <- col[[1]]
            med <- median(col_n, na.rm = TRUE)
            mad <- mad(col_n, na.rm = TRUE)
            value <- map_dbl(col_n, function(x) {(x - med)/mad}) %>% 
                  t() %>% 
                  as.tibble()
      }
      
      # outputs matrix (couldn't get spread() to work)
      norm_matrix <- df_nested %>% 
            mutate(stats = map(data, pos_norm_FUN)) %>% 
            select(key, stats) %>%
            unnest() %>%
            t()
      
      # First row has column names but I need to get rid of it for now
      norm_pre <- norm_matrix[-1,]
      # Now add the column names and convert to tibble.
      colnames(norm_pre) <- names(df)
      norm_df <- as.tibble(norm_pre)
      norm_df_num <- norm_df %>% 
            mutate_if(is.character, as.numeric)
}

posn_fr_bat <- mad_median_FUN(fr_bat_num)
posn_fr_pit <- mad_median_FUN(fr_pit_num)
posn_hof_bat <- mad_median_FUN(hof_bat_num)
posn_hof_pit <- mad_median_FUN(hof_pit_num)



factor_FUN <- function(df) {
      
      col_list <- names(df)
      
      create_col_FUN <- function(x) {
            factor_var <- paste0("factor_", x)
            
            df %>% 
                  mutate(!!factor_var := factor(if_else((!! rlang::sym(x)) < 0, "negative", "positive"), levels = c("negative", "positive"))) %>% 
                  select(factor_var)
      }
      
      factor_df <- map_dfc(col_list, create_col_FUN) %>% 
            bind_cols(df)
      
}

fact_rstd_fr_bat <- factor_FUN(rstd_fr_bat)
fact_rstd_fr_pit <- factor_FUN(rstd_fr_pit)
fact_rstd_hof_bat <- factor_FUN(rstd_hof_bat)
fact_rstd_hof_pit <- factor_FUN(rstd_hof_pit)

fact_posn_fr_bat <- factor_FUN(posn_fr_bat)
fact_posn_fr_pit <- factor_FUN(posn_fr_pit)
fact_posn_hof_bat <- factor_FUN(posn_hof_bat)
fact_posn_hof_pit <- factor_FUN(posn_hof_pit)



# add Id and Name columns back
fin_rstd_fr_bat <-  fact_rstd_fr_bat %>%
      bind_cols(franchise_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_posn_fr_bat <-  fact_posn_fr_bat %>%
      bind_cols(franchise_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_rstd_hof_bat <-  fact_rstd_hof_bat %>%
      bind_cols(hof_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_posn_hof_bat <-  fact_posn_hof_bat %>%
      bind_cols(hof_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)


fin_rstd_fr_pit <-  fact_rstd_fr_pit %>%
      bind_cols(franchise_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_posn_fr_pit <-  fact_posn_fr_pit %>%
      bind_cols(franchise_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_rstd_hof_pit <- fact_rstd_hof_pit %>%
      bind_cols(hof_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_posn_hof_pit <- fact_posn_hof_pit %>%
      bind_cols(hof_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 




write_rds(fin_rstd_fr_bat, "data/14 - reg standardization fran batting.rds")
write_rds(fin_rstd_fr_pit, "data/14 - reg standardization fran pitching.rds")
write_rds(fin_rstd_hof_bat, "data/14 - reg standardization hof batting.rds")
write_rds(fin_rstd_hof_pit, "data/14 - reg standardization hof pitching.rds")

write_rds(fin_posn_fr_bat, "data/14 - pos normalization fran batting.rds")
write_rds(fin_posn_fr_pit, "data/14 - pos normalization fran pitching.rds")
write_rds(fin_posn_hof_bat, "data/14 - pos normalization hof batting.rds")
write_rds(fin_posn_hof_pit, "data/14 - pos normalization hof pitching.rds")


