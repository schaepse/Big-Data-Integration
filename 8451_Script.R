library(readr)
library(dplyr)
library(tidyverse)
install.packages("tibble")
library(tibble)

remove.packages("rlang")
setwd("~/8451_Carbo-Loading/Carbo-Loading CSV")

#Read datasets#
transaction <- read_csv("transactions.csv")
causal <- read_csv("causal_lookup.csv")
#I might want to join these by store and upc for these too#
transaction_filter <- transaction %>%
  filter(week > 42)

causal_edit <- causal %>%
  dplyr::select(upc, store, week, feature_desc, display_desc)

remove.packages("dplyr")
install.packages("dplyr")
library(dplyr)
install.packages("pillar")
library(pillar)

update.packages()
