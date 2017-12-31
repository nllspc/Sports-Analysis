# POS distributions




library(tidyverse)

iRedsWandJ <- read_rds("data/indRedsWARandJAWS.rds")

# When we compare nominees we'll compare pitchers to pitchers and regular position players to regular position players
iPitRedsWandJ <- iRedsWandJ %>% 
      filter(POS == "P")
iBatRedsWandJ <- iRedsWandJ %>% 
      filter(POS != "P")

# When we compute the "average" HOF WAR and JAWS to fill our distributions, do we use the mean or median?
# EDA ==============================================


# For now I'm not splitting these into P and RP so not immediately useful but I'm going to take a look at the pitcher distributions all the same.
# Pitchers ===============

# redsWAR mean = 26.08, median = 26.02
# redsWAR4 mean = 18.95, median = 18.21
# redsJAWS mean = 22.52, median = 23.12
summary(iPitRedsWandJ)


# Not the prettiest graphs but given the that the mean and median are nearly identical, using the mean is fine.
hist <- ggplot(iPitRedsWandJ, aes(x = redsWAR)) +
      geom_histogram(binwidth = 5)
hist
density <- ggplot(iPitRedsWandJ, aes(x=redsWAR)) +
      geom_density()
density

# mean looks acceptable here as well
hist <- ggplot(iPitRedsWandJ, aes(x = redsWAR4)) +
      geom_histogram(binwidth = 3)
hist
density <- ggplot(iPitRedsWandJ, aes(x=redsWAR4)) +
      geom_density()
density

# Mean is fine.
hist <- ggplot(iPitRedsWandJ, aes(x = redsJAWS)) +
      geom_histogram(binwidth = 3)
hist
density <- ggplot(iPitRedsWandJ, aes(x=redsJAWS)) +
      geom_density()
density

# Boxplots show some skewness but nothing concerning.
iPitRedsWandJ_gathered <- iPitRedsWandJ %>% 
      gather("redsWAR", "redsWAR4", key = "Metric", value = "Value")

iPitRedsWandJ_gathered %>% ggplot(aes(x = Metric, y = Value)) +
      geom_boxplot() + geom_jitter(width = 0.15)



# Hitters ==================

# redsWAR mean = 25.07, median = 18.25
# redsWAR4 mean = 15.63, median = 13.71
# redsJAWS mean = 20.35, median = 15.39
summary(iBatRedsWandJ)


# Skewed. Use median
hist <- ggplot(iBatRedsWandJ, aes(x = redsWAR)) +
      geom_histogram(binwidth = 5)
hist
density <- ggplot(iBatRedsWandJ, aes(x=redsWAR)) +
      geom_density()
density

# Skewed but not as bad. Use median
hist <- ggplot(iBatRedsWandJ, aes(x = redsWAR4)) +
      geom_histogram(binwidth = 4)
hist
density <- ggplot(iBatRedsWandJ, aes(x=redsWAR4)) +
      geom_density()
density

# Skewed. Use median
hist <- ggplot(iBatRedsWandJ, aes(x = redsJAWS)) +
      geom_histogram(binwidth = 4)
hist
density <- ggplot(iBatRedsWandJ, aes(x=redsJAWS)) +
      geom_density()
density

# Boxplots similar to pitchers. I'll trust the other indicators and use the median for this group.
iBatRedsWandJ_gathered <- iBatRedsWandJ %>% 
      gather("redsWAR", "redsWAR4", key = "Metric", value = "Value")

iBatRedsWandJ_gathered %>% ggplot(aes(x = Metric, y = Value)) +
      geom_boxplot() + geom_jitter(width = 0.15)


# Median went up a little bit. Since we're using both medians to calculate our average HOF hitter, guessing it's more mathematically correct to use the summary value of 15.39
avgHOFJAWS <- round((median(iBatRedsWandJ$redsWAR) + median(iBatRedsWandJ$redsWAR4))/2, 2)
# 15.98




# Create Tibble of filler players =========================================


# OF has the most inductees with 19. 1B is second with 10. In the future I'll want to split OF into LF/CF/RF but it's not convenient right now.
table(iBatRedsWandJ$POS)

# columns needed: name_whole, redsWAR, redsWAR4, redsJAWS, POS
nPOS <- iBatRedsWandJ %>%
      group_by(POS) %>%
      summarize(n = n()) %>% 
      ungroup()
      
# Number of filler players needed at each position
neededPOS <- nPOS %>%
      mutate(remPOS = max(n) - n) %>%
      filter(POS != "OF") %>%
      select(-n)

# Create list of lists of each position
posList <- list(neededPOS$POS)
posList <- posList[[1]]
remList <- list(neededPOS$remPOS)
remList <- remList[[1]]

pos_vector_fun <- function(POS, n) {
      POS <- rep(POS, n)
}
posLL <- map2(posList, remList, pos_vector_fun)



# Create tibble with all the filler players for each position

# Empty tibble
posFillTib <- tibble(
      name_whole = character(),
      redsWAR = numeric(),
      redsWAR4 = numeric(),
      redsJAWS = numeric(),
      POS = character()
)

# input: Position; function creates one filler player with avgHOF stats
fillPOS <- function(POS) {
      posFillTib <- posFillTib %>%
            add_row(name_whole = "avgHOFplayer",
                    redsWAR = median(iBatRedsWandJ$redsWAR),
                    redsWAR4 = median(iBatRedsWandJ$redsWAR4),
                    redsJAWS = median(iBatRedsWandJ$redsJAWS),
                    POS = POS
            )
}
# List of lists fed to function; outputs tibble of filler players
posFillTibFinal <- map_dfr(posLL, fillPOS)



# Combining table of inductees with table of filler players
iBatRedsWandJ <- iBatRedsWandJ %>%
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

batPosDstrbWt <- bind_rows(iBatRedsWandJ, posFillTibFinal)
pitPosDstrb <- iPitRedsWandJ %>% 
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

write_rds(pitPosDstrb, "data/pitcherPositionDistribution.rds")
write_rds(batPosDstrbWt, "data/weightedPositionDistributions.rds")
