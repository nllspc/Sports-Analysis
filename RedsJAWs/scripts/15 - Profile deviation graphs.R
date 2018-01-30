# Profile deviation graphs






library(tidyverse)
library(ggpubr)

rstd_fr_bat <- read_rds("data/14 - reg standardization fran batting.rds")
rstd_fr_pit <- read_rds("data/14 - reg standardization fran pitching.rds")
rstd_hof_bat <- read_rds("data/14 - reg standardization hof batting.rds")
rstd_hof_pit <- read_rds("data/14 - reg standardization hof pitching.rds")

posn_fr_bat <- read_rds("data/14 - pos normalization fran batting.rds")
posn_fr_pit <- read_rds("data/14 - pos normalization fran pitching.rds")
posn_hof_bat <- read_rds("data/14 - pos normalization hof batting.rds")
posn_hof_pit <- read_rds("data/14 - pos normalization hof pitching.rds")


