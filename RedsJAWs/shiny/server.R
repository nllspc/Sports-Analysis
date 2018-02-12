


library(shiny)
library(tidyverse)
library(DT)

# JAWS Reference Table
library(knitr)
library(kableExtra)

# Ridge Plots
library(ggridges)
library(viridis)
library(cowplot)
library(ggbeeswarm)

# Interactive Line Chart
library(ggiraph)

# Cleveland Dot Plots
library(ggpubr)

# Data =========================================================

# JAWS Page ====================

# Table
wj_display <- read_rds("data/18 - JAWS pg display table.rds")

# Cleveland Dot Plots
jaws_group <- read_rds("data/18 - JAWS pg JAWS dot chart table.rds")
war_group <- read_rds("data/18 - JAWS pg WAR dot chart table.rds")

# Line Chart
war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")



# Server =======================================================

shinyServer(function(input, output, session){
      
      # JAWs Pg
      
      output$jTable <- renderDT({
            datatable(
                  data = wj_display,
                  rownames = FALSE,
                  extensions = c("Responsive", "Buttons"),
                  filter = list(position = "top"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = I("colvis"),
                                 dom = "Bfrtip")
            )
      })
      
      
      output$warCleve <- renderPlot({
            
            war_dot <- war_group %>% 
                  filter(Name == input$jplayer)
            
            
            war_right_label <- war_dot %>% 
                  group_by(Group) %>% 
                  arrange(desc(Value)) %>% 
                  top_n(1)
            
            war_left_label <- war_dot %>% 
                  group_by(Group) %>% 
                  arrange(desc(Value)) %>% 
                  slice(2)
            
            ggplot(war_dot, aes(x = Value, y = Group)) +
                  geom_line(aes(group = Group)) +
                  geom_point(aes(color = Stat), size = 3) +
                  geom_text(data = war_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
                  geom_text(data = war_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
                  scale_x_continuous(limits = c(min(war_dot$Value)-10, max(war_dot$Value)+10)) + 
                  scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
                  labs(title = "WARtenure") +
                  theme_minimal() +
                  theme(axis.title = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        legend.justification = c(0,1),
                        legend.position = c(.1, 1),
                        legend.background = element_blank(),
                        legend.direction = "horizontal",
                        # text = element_text(family = "TT Arial"),
                        plot.title = element_text(size = 20, margin = margin(b = 10))
                  )
      })
      
      
      
      
      
      output$jawsCleve <- renderPlot({
            
            jaws_dot <- jaws_group %>% 
                  filter(Name == input$jplayer)
            
            jaws_right_label <- jaws_dot %>% 
                  group_by(Group) %>%
                  arrange(desc(Value)) %>% 
                  top_n(1)
            
            jaws_left_label <- jaws_dot %>% 
                  group_by(Group) %>% 
                  arrange(desc(Value)) %>% 
                  slice(2)
            
            ggplot(jaws_dot, aes(x = Value, y = Group)) +
                  geom_line(aes(group = Group)) +
                  geom_point(aes(color = Stat), size = 3) +
                  geom_text(data = jaws_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
                  geom_text(data = jaws_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
                  scale_x_continuous(limits = c(min(jaws_dot$Value)-10, max(jaws_dot$Value)+10)) + 
                  scale_color_manual(labels = c("Typical HOFer (weighted)", "Player"), values = c("#000000", "#C6011F")) +
                  labs(title = "JAWS-4") +
                  theme_minimal() +
                  theme(axis.title = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        legend.justification = c(0,1),
                        legend.position = c(.1, 1),
                        legend.background = element_blank(),
                        legend.direction = "horizontal",
                        # text = element_text(family = "TT Arial"),
                        plot.title = element_text(size = 20, margin = margin(b = 10))
                  )
      })
      
      
      
      output$lineChart <- renderggiraph({
            
            war_line <- war_combo_avg %>% 
                  filter(Name == input$jplayer)
            line_filtered <- war_line %>% 
                  filter(type == "WAR4")
            
            p <- ggplot(data = war_line) + 
                  geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
                  geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
                  geom_line(aes(x = yearId, y = WAR)) +
                  geom_hline(aes(yintercept = mean(`Median WAR`), linetype = "Typical HOFer (weighted)"), color = alpha("#C6011F", 0.5), size = 1.25) +
                  scale_linetype_manual(values = 2, guide = guide_legend(override.aes = list(color = "#C6011F"))) +
                  scale_y_continuous(limits = c(min(war_line$WAR)-5, max(war_line$WAR)+5)) +
                  labs(title = "WAR") +
                  theme_minimal() +
                  theme(axis.title = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        legend.justification = c(0,1),
                        legend.position = c(.1, 1),
                        legend.box = "horizontal",
                        legend.background = element_blank(),
                        legend.direction = "horizontal",
                        # text = element_text(family = "Georgia"),
                        plot.title = element_text(size = 20, margin = margin(b = 10))
                  )
            ggiraph(code = print(p))
            
      })
})