# Calculations





library(tidyverse)
library(openWARData)

# Career WAR
carWar <- indWarT %>% group_by(playerId) %>%
        summarize(careerWAR = sum(rWAR), PeakWAR = max(rWAR)) %>%
        ungroup()

WAR6 <- indWarT %>% group_by(playerId) %>% top_n(6, rWAR) %>% tally(rWAR) %>% rename(WAR6 = n)
 
carWar <- carWar %>% add_column(WAR6 = WAR6$WAR6)
carWar <- mutate(carWar, JAWS = round((careerWAR + WAR6)/2, 2))
        
       
# Reds career WAR
carWarR <- indWarR %>% group_by(playerId) %>%
        summarize(RedsWAR = sum(rWAR), PeakRedsWAR = max(rWAR), AvgRedsWAR = round(mean(rWAR),2)) %>% 
        ungroup()

