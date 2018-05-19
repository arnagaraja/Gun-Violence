# Classify each state based on strictness of gun laws
library(readr)
library(dplyr)

lawsdf <- read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/48_states_1991_data.csv")

lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Hawaii_1991_data.csv"))

lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Alaska_1991_data.csv"))