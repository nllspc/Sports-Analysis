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


# add Id and Name columns back
fin_rstd_fr_bat <- rstd_fr_bat %>%
      bind_cols(franchise_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_posn_fr_bat <- posn_fr_bat %>%
      bind_cols(franchise_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_rstd_hof_bat <- rstd_hof_bat %>%
      bind_cols(hof_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)
fin_posn_hof_bat <- posn_hof_bat %>%
      bind_cols(hof_batting[1:2]) %>% 
      select(bbref_playerId, Name, everything()) %>% 
      rename(bbref_id = bbref_playerId)

fin_rstd_fr_pit <-  rstd_fr_pit %>%
      bind_cols(franchise_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_posn_fr_pit <-  posn_fr_pit %>%
      bind_cols(franchise_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_rstd_hof_pit <- rstd_hof_pit %>%
      bind_cols(hof_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 
fin_posn_hof_pit <- posn_hof_pit %>%
      bind_cols(hof_pitching[c(1,3)]) %>% 
      select(bbref_id, Name, everything()) 


sign_FUN <- function(df) {
      col_df <- as.character(names(df[-c(1,2)]))
      df_g <- df %>% 
            gather(col_df, key = "stat", value = "score") %>% 
            mutate(score = if_else(stat == "K%", score * -1, score), score = if_else(stat == "SO", score * -1, score)) 
      df_g$sign <- factor(ifelse(df_g$score < 0, "negative", "positive"), levels = c("negative", "positive"))
      return(df_g)
}

final_rfb <- sign_FUN(fin_rstd_fr_bat)
final_rfp <- sign_FUN(fin_rstd_fr_pit)
final_rhb <- sign_FUN(fin_rstd_hof_bat)
final_rhp <- sign_FUN(fin_rstd_hof_pit)

final_pfb <- sign_FUN(fin_posn_fr_bat)
final_pfp <- sign_FUN(fin_posn_fr_pit)
final_phb <- sign_FUN(fin_posn_hof_bat)
final_php <- sign_FUN(fin_posn_hof_pit)



write_rds(final_rfb, "data/14 - reg standardization fran batting.rds")
write_rds(final_rfp, "data/14 - reg standardization fran pitching.rds")
write_rds(final_rhb, "data/14 - reg standardization hof batting.rds")
write_rds(final_rhp, "data/14 - reg standardization hof pitching.rds")

write_rds(final_pfb, "data/14 - pos normalization fran batting.rds")
write_rds(final_pfp, "data/14 - pos normalization fran pitching.rds")
write_rds(final_phb, "data/14 - pos normalization hof batting.rds")
write_rds(final_php, "data/14 - pos normalization hof pitching.rds")


