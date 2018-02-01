# statrank density plots



library(tidyverse)
library(broom)

ggplot2::theme_set(
      theme_bw(base_family = 'TT Arial', base_size = 12) +
            theme(
                  plot.title = element_text(face = 'bold', hjust = 0),
                  text = element_text(colour = '#4e5c65'),
                  panel.background = element_rect('#ffffff'),
                  strip.background = element_rect('#ffffff', colour = 'white'),
                  plot.background = element_rect('#ffffff'),
                  panel.border = element_rect(colour = '#ffffff'),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  legend.background = element_rect('#ffffff'),
                  legend.title = element_blank(),
                  legend.position = 'right',
                  legend.direction = 'vertical',
                  legend.key = element_blank(),
                  strip.text = element_text(face = 'bold', size = 10),
                  axis.text = element_text(face = 'bold', size = 9),
                  axis.title = element_blank(),
                  axis.ticks = element_blank()
            )
)

hof_batting <- read_rds("data/13 - HOF Batting.rds")
inBatStats <- read_rds("data/10 - inRedsBatStats.rds")
tradFranBat <- read_rds("./Projects/Sports-Analysis/RedsJAWS/data/09 - tradFranchiseBatting.rds")
tradFranPit <- read_rds("./Projects/Sports-Analysis/RedsJAWS/data/09 - tradFranchisePitching.rds")
pitWandJ <- read_rds("./Projects/Sports-Analysis/RedsJAWS/data/07a - pitcherPositionDistribution.rds")
batWandJ <- read_rds("./Projects/Sports-Analysis/RedsJAWS/data/08 - positionJoyPlot.rds")

# Left Fielders
lfWandJ <- batWandJ %>% 
      filter(POS == "LF")

# Just the graph, maam
a1 <- ggplot(data = pitWandJ, aes(x = redsJAWS)) +
      geom_density(fill = "#000000", alpha = 0.7)

d1 <- ggplot_build(a1)$data[[1]]

p1 <- a1 + geom_area(data = subset(d1, x < 3.5), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75)

p1


# The control
df <- tradFranBat %>%
      select(bbref_playerId, OBP) %>% 
      group_by(bbref_playerId) %>%
      do(tidy(density(tradFranBat$OBP, bw = "nrd0"))) %>% 
      group_by() %>% 
      mutate(ymin = max(y) / 1.5, 
             ymax = y + ymin,
             ylabel = ymin + min(ymin)/2,
             xlabel = min(x) - mean(range(x))/2)

labels <- tradFranBat %>% 
      select(bbref_playerId, OBP) %>% 
      group_by(bbref_playerId) %>% 
      mutate(q1 = quantile(OBP)[2],
             median = quantile(OBP)[3],
             q3 = quantile(OBP)[4]) %>%
      filter(row_number() == 1) %>% 
      select(-OBP) %>% 
      left_join(df) %>% 
      mutate(xmed = x[which.min(abs(x - median))],
             yminmed = ymin[which.min(abs(x - median))],
             ymaxmed = ymax[which.min(abs(x - median))]) %>% 
      filter(row_number() == 1)     


a <- ggplot(data = tradFranBat, aes(x = OBP)) +
      geom_density(fill = "#000000", alpha = 0.7) +
      theme(axis.text.y = element_blank())

d <- ggplot_build(a)$data[[1]]

p <- a + geom_area(data = subset(d, x < .300), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
      geom_segment(data = labels[labels$bbref_playerId == "adamsbo03",], aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed), colour = "#F0F0F0", linetype = "dashed") +
      geom_text(data = labels[labels$bbref_playerId == "adamsbo03",], aes(xmed - xlabel/50, ylabel), 
                label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4)

p



# density() not working with wRC+ right now

# The experimental

df2 <-  hof_batting %>%
      select(Name, `wRC+`) %>% 
      group_by(Name) %>%
      do(tidy(density(hof_batting$`wRC+`, bw = "nrd0"))) %>% 
      group_by() %>% 
      mutate(ymin = max(y) / 1.5, 
             ymax = y + ymin,
             ylabel = ymin + min(ymin)/2,
             xlabel = min(x) - mean(range(x))/2)

labels2 <- lfWandJ %>% 
      select(name_whole, redsJAWS) %>% 
      mutate(median = quantile(redsJAWS)[3]) %>%
      filter(row_number() == 1) %>% 
      select(-redsJAWS) %>% 
      left_join(df2) %>% 
      mutate(xmed = x[which.min(abs(x - median))],
             yminmed = ymin[which.min(abs(x - median))],
             ymaxmed = ymax[which.min(abs(x - median))]) %>% 
      filter(row_number() == 1)     

shade_bdy <- lfWandJ %>% 
      filter(name_whole == "Frank Robinson")
shade_bdy <- shade_bdy$redsJAWS[1]

# 3 decimal places needed for stats like OBP
scaleFUN <- function(x) {sprintf("%.3f", x)}

a <- ggplot(data = lfWandJ, aes(x = redsJAWS)) +
      geom_density(fill = "#000000", alpha = 0.7) +
      # scale_x_continuous(labels = scaleFUN) +
      scale_x_continuous() +
      theme(axis.text.y = element_blank())

d <- ggplot_build(a)$data[[1]]

p <- a + geom_area(data = subset(d, x < shade_bdy), aes(x = x, y = y), fill = "#C6011F", alpha = 0.75) +
      
      geom_segment(data = labels2, aes(x = xmed, xend = xmed, y = 0, yend = ymaxmed/1.77), colour = "#F0F0F0", linetype = "dashed") +
      
      geom_text(data = labels2, aes(xmed - xlabel/50, ylabel), 
                label = "Median", colour = "#000000", hjust = 0, fontface = "italic", size = 4) +
      geom_rug()

p


