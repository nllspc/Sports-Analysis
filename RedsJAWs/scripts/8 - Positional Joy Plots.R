# Positional Joy Plots


library(tidyverse)
library(ggridges)
library(viridis)
library(cowplot)
library(ggbeeswarm)



# Weighted Positional Distributions =======================

# Combining Pitcher and regular position player distributions
batPosDistrbWt <- read_rds("data/weightedPositionDistributions.rds")
pitPosDistrb <- read_rds("data/pitcherPositionDistribution.rds") %>% 
      select(name_whole, redsWAR, redsWAR4, redsJAWS, POS)

compPosDistrb <- bind_rows(batPosDistrbWt,pitPosDistrb)
compPosDistrb$POSf <- as.factor(compPosDistrb$POS)


# Adding HOF distribution
compPosDistrbA <- compPosDistrb %>% 
      select(-POS, -POSf) %>% 
      add_column(POS = rep("Reds HOF", 141))

compPosDistrB <- compPosDistrb %>% 
      select(-POSf)

wtCompPosDistrbHOF <- bind_rows(compPosDistrbA, compPosDistrB) %>%
      mutate(POSf = as.factor(POS))

write_rds(wtCompPosDistrbHOF, "data/weightedPositionJoyPlot.rds")

# Less of a joy than I was imagining. It should've been expected though. All the filler players have drowned out the variation in most of the position plots.
wtJoyPOS <- ggplot(data = wtCompPosDistrbHOF,
                 aes(y = forcats::fct_relevel(POSf, "OF", "SS", "3B", "2B", "1B", "C", "P", "Reds HOF"),
                     x = redsJAWS,
                     color = redsJAWS,
                     fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      geom_quasirandom(color = "white",
                       alpha = 0.5,
                       shape = 21,
                       size = 1.1,
                       groupOnX = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Position",
           title = "Weighted Reds HOF JAWS Distributions")

wtJoyPOS


# Without the fillers =====================================


# Removing filler players and creating HOF distribution
compPosDistrbAlt <- compPosDistrb %>% 
      filter(name_whole != "avgHOFplayer") %>% 
      select(-POS, -POSf) %>% 
      add_column(POS = rep("Reds HOF", 76))

# Removing filler players and factor position column
compPosDistr2 <- compPosDistrb %>% 
      filter(name_whole != "avgHOFplayer") %>% 
      select(-POSf)

# Combining to form unweighted distribution with HOF distribution included
compPosDistrbHOF <- bind_rows(compPosDistrbAlt, compPosDistr2) %>%
      mutate(POSf = as.factor(POS))

write_rds(compPosDistrbHOF, "data/positionJoyPlot.rds")



# Much mo'better. Some variation!
# Also removed jittered points. Usually in favor but I think it possibly subtracts from the visual here.
joyPOS <- ggplot(data = compPosDistrbHOF,
                   aes(y = forcats::fct_relevel(POSf, "OF", "SS", "3B", "2B", "1B", "C", "P", "Reds HOF"),
                       x = redsJAWS,
                       color = redsJAWS,
                       fill = ..x..)) +
      geom_density_ridges_gradient(rel_min_height = 0.01,
                                   alpha = 0.75)  +
      scale_fill_viridis(option = "C",
                         name = "Reds JAWS") +
      scale_color_viridis(option = "C") +
      guides(color = F) +
      theme_ridges() +
      theme(legend.position = "top",
            plot.caption = element_text(hjust=0),
            legend.key.width = unit(1.25, "cm")) +
      labs(x = "Reds JAWS", y = "Position",
           title = "Reds HOF JAWS Distributions")

joyPOS



