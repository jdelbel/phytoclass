#Script to do initial testing on phytoclass 

#Load packages.
library(phytoclass)
library(readxl)
library(dplyr)
library(ggplot2)
library(here)

#Load data - eventually download using API?
h <- read.csv(here("files", "2025-06-23_HakaiData_hplc.csv"))

#function to select data and columns and rename for phytoclass


