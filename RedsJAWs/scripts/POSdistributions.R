# POS distributions




library(tidyverse)

iRedsWandJ <- read_rds("data/indRedsWARandJAWS.rds")



# EDA ==============================================

# redsWAR mean = 25.43, median = 21.03
# redsWAR4 mean = 16.81, median = 16.64
# redsJAWS mean = 21.12, median = 19.21
summary(iRedsWandJ)

# Skewed. Should use the median here
hist <- ggplot(iRedsWandJ, aes(x = redsWAR)) +
      geom_histogram(binwidth = 5)
hist
density <- ggplot(iRedsWandJ, aes(x=redsWAR)) +
      geom_density()
density

# Pretty nice Gaussian
hist <- ggplot(iRedsWandJ, aes(x = redsWAR4)) +
      geom_histogram(binwidth = 2)
hist
density <- ggplot(iRedsWandJ, aes(x=redsWAR4)) +
      geom_density()
density

# Definitely some skewness which is expected given this is the avg of the previous two metrics
hist <- ggplot(iRedsWandJ, aes(x = redsJAWS)) +
      geom_histogram(binwidth = 3)
hist
density <- ggplot(iRedsWandJ, aes(x=redsJAWS)) +
      geom_density()
density

# Boxplots confirms histogram and density observations
iRedsWandJ_gathered <- iRedsWandJ %>% 
      gather("redsWAR", "redsWAR4", key = "Metric", value = "Value")

iRedsWandJ_gathered %>% ggplot(aes(x = Metric, y = Value)) +
      geom_boxplot() + geom_jitter(width = 0.15)


# Significantly lowered from redsJAWS mean, median values
avgHOFJAWS <- round((median(iRedsWandJ$redsWAR) + mean(iRedsWandJ$redsWAR4))/2, 2)
# 18.92
avgHOFJAWS



# Create Tibble of filler players =========================================


# Pitcher has the most inductees with 27. OF is second with 19. Hopefully in the future I'll be able to split this groups into P/RP and LF/CF/RF
table(iRedsWandJ$POS)

# columns needed: name_whole, redsWAR, redsWAR4, redsJAWS, POS
nPOS <- iRedsWandJ %>%
      group_by(POS) %>%
      summarize(n = n()) %>% 
      ungroup()
      
# Number of filler players needed at each position
neededPOS <- nPOS %>%
      mutate(remPOS = max(n) - n) %>%
      filter(POS != "P") %>%
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
                    redsWAR = median(iRedsWandJ$redsWAR),
                    redsWAR4 = mean(iRedsWandJ$redsWAR4),
                    redsJAWS = round((median(iRedsWandJ$redsWAR) + mean(iRedsWandJ$redsWAR4))/2, 2),
                    POS = POS
            )
      
}
# List of lists fed to function; outputs tibble of filler players
posFillTibFinal <- map_dfr(posLL, fillPOS)



# Combining table of inductees with table of filler players
iRedsWandJ <- iRedsWandJ %>%
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

wtPosDstrb <- bind_rows(iRedsWandJ, posFillTibFinal)

write_rds(wtPosDstrb, "data/weightedPositionDistributions.rds")