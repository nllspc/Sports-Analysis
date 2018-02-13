


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

# Home Page ====================

jaws_ref_table <- read_rds("data/17 - JAWS Reference Table.rds")
bothDistrb <- read_rds("data/08 - unweightedandGroupJoyplot.rds")

# JAWS Page ====================

# Table
wj_display <- read_rds("data/18 - JAWS pg display table.rds")

# Cleveland Dot Plots
jaws_group <- read_rds("data/18 - JAWS pg JAWS dot chart table.rds")
war_group <- read_rds("data/18 - JAWS pg WAR dot chart table.rds")

# Line Chart
war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")

# Profile Page =================

# Tables
# Batting
prof_batting <- read_rds("data/13 - HOF Batting.rds") %>% 
      filter(Name == input$prof_bat_player)
prof_fielding <- read_rds("data/20 - Numbers pg HOF Fielding.rds") %>% 
      select(-`BBRef Id`, -`FG Id`) %>% 
      select(Name, everything()) %>% 
      filter(Name == input$prof_bat_player)
prof_postseas_b <- read_rds("data/20 - Numbers pg HOF Postseason Batting.rds") %>% 
      select(-`BBRef Id`, -`FG Id`) %>% 
      filter(Name == input$prof_bat_player)
prof_awards_b <- read_rds("data/20 - Numbers pg HOF Awards.rds") %>%
      select(-`BBRef Id`, -`FG Id`) %>% 
      filter(Name == input$prof_bat_player)
# Pitching
prof_pitching <- read_rds("data/13 - HOF Pitching.rds") %>% 
      filter(Name == input$prof_pit_player)
prof_postseas_p <- read_rds("data/20 - Numbers pg HOF Postseason Pitching.rds") %>% 
      select(-`BBRef Id`, -`FG Id`) %>% 
      filter(Name == input$prof_pit_player)
prof_awards_p <- read_rds("data/20 - Numbers pg HOF Awards.rds") %>%
      select(-`BBRef Id`, -`FG Id`) %>% 
      filter(Name == input$prof_pit_player)

# Deviation Chart
# Batting
phb <- read_rds("data/14 - pos normalization hof batting.rds") %>% 
      filter(Name == input$prof_bat_player)
# Pitching
php <- read_rds("data/14 - pos normalization hof pitching.rds") %>% 
      filter(Name == input$prof_pit_player)


# Server =======================================================

shinyServer(function(input, output, session){
      
      # Home Pg =================
      
      output$hTable <- renderDT({
            datatable(
                  data = jaws_ref_table,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtip")
            )
      })
      
      
      output$ridge <- renderPlot({
            ggplot(data = bothDistrb,
                   aes(y = forcats::fct_relevel(POSf, "HOF", "Md", "CO", "OF", "MI", "CI", "RF", "CF", "LF", "SS", "3B", "2B", "1B", "C", "P"),
                       x = redsJAWS,
                       color = redsJAWS,
                       fill = ..x..)) +
                  geom_density_ridges_gradient(rel_min_height = 0.01,
                                               alpha = 0.75)  +
                  scale_fill_viridis(option = "C") +
                  scale_color_viridis(option = "C") +
                  guides(fill = F) +
                  theme_ridges() +
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank()) +
                  labs(title = "Reds HOF JAWS-4 Distributions")
            
      })
      
      # JAWs Pg =================
      
      output$jTable <- renderDT({
            datatable(
                  data = wj_display,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
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
      
      
      
      # Profile Pg ===============
      
      # Batting
      output$prof_bat_Table <- renderDT({
            datatable(
                  data = prof_batting,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtip")
            )
      })
      
      output$prof_field_Table <- renderDT({
            datatable(
                  data = prof_fielding,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtip")
            )
      })
      
      output$prof_psb_Table <- renderDT({
            datatable(
                  data = prof_postseas_b,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtip")
            )
      })
      
      output$prof_awa_Table <- renderDT({
            datatable(
                  data = prof_awards_b,
                  rownames = FALSE,
                  extensions = c("FixedColumns","Buttons"),
                  options = list(language = list(sSearch = "Filter:"),
                                 buttons = c("colvis", "csv"),
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 dom = "Bfrtip")
            )
      })
})






