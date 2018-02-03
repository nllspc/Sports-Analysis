# JAWS tab visuals




library(tidyverse)



jaws_group <- read_rds("data/18 - JAWS pg dot chart table.rds")
war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")



# Cleveland Dot ==============================================

bench_jaws <- jaws_group %>% 
      filter(Name == "Johnny Bench") %>% 
      gather(key = "Stat", value = "Value", -c(bbref_id, Name, Group)) %>% 
      mutate(Value = as.numeric(Value))

boone_jaws <- jaws_group %>% 
      filter(Name == "Aaron Boone") %>% 
      gather(key = "Stat", value = "Value", -c(bbref_id, Name, Group)) %>% 
      mutate(Value = as.numeric(Value))


# Bench


bench_right_label <- bench_jaws %>% 
      group_by(Group) %>%
      arrange(desc(Value)) %>% 
      top_n(1)

bench_left_label <- bench_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

ggplot(bench_jaws, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = bench_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = bench_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_x_continuous(limits = c(min(bench_jaws$Value)-10, max(bench_jaws$Value)+10)) + 
      scale_color_manual(values = c("#000000", "#C6011F")) +
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
            text = element_text(family = "TT Arial"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )



# Boone

boone_right_label <- boone_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      top_n(1)

boone_left_label <- boone_jaws %>% 
      group_by(Group) %>% 
      arrange(desc(Value)) %>% 
      slice(2)

ggplot(boone_jaws, aes(x = Value, y = Group)) +
      geom_line(aes(group = Group)) +
      geom_point(aes(color = Stat), size = 3) +
      geom_text(data = boone_right_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = -0.5) +
      geom_text(data = boone_left_label, aes(color = Stat, label = round(Value, 1)), size = 5, hjust = 1.5) +
      scale_x_continuous(limits = c(min(boone_jaws$Value)-10, max(boone_jaws$Value)+10)) + 
      scale_color_manual(values = c("#000000", "#C6011F")) +
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
            text = element_text(family = "Georgia"),
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )



# Line Chart



