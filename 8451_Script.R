library(readr)
library(dplyr)
library(tidyverse)

setwd("~/8451_Carbo-Loading/Carbo-Loading CSV")

#Read datasets#
transaction <- read_csv("transactions.csv")
causal <- read_csv("causal_lookup.csv")
#I might want to join these by store and upc for these too#

