#Script to do initial testing on phytoclass 

#Load packages.
library(phytoclass)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

#Load data - eventually download using API?
h <- read.csv(here("files", "2025-06-23_HakaiData_hplc.csv"))

#function to select data and columns and rename for phytoclass
h2 <- h %>%
  filter(line_out_depth == 5 & site_id == "QU39" & analyzing_lab == "USC") %>% 
  select(date,
         site_id,
         depth = line_out_depth,
         # chlc1c2 = chl_c1_c2, #
         Peri = peri, #
         X19but = X_19_but,
         Fuco = fuco,
         X19hex = X_19_hex,
         Pra = prasinoxanthin,
         Allo = alloxanthin,
         # lut = lutein,
         Zea = zeaxanthin,
         Chl_b = chl_b,
         Tchla = all_chl_a) %>% 
  drop_na()
  
s_matrix <- h2 %>% 
  select(Peri:Tchla)

Fu <- data.frame(
  Peri = c(0,  0, 0, 1, 0, 0, 0),
  X19but = c(0,  0, 0, 0, 1, 1, 0),
  Fuco = c(0,  0, 1, 0, 1, 1, 0),
  Pra = c(1,  0, 0, 0, 0, 0, 0),
  X19hex = c(0,  0, 0, 0, 1, 0, 0),
  Allo = c(0,  1, 0, 0, 0, 0, 0),
  Zea = c(1,  0, 0, 0, 0, 0, 1),
  Chl_b = c(1,  0, 0, 0, 0, 0, 0),
  Tchla = c(1,  1, 1, 1, 1, 1, 1)
)

rownames(Fu) <- c(
  "Prasinophytes",
  # "Chlorophytes",
  "Cryptophytes",
  "Diatoms-2", 
  "Dinoflagellates-1",
  "Haptophytes",
  "Pelagophytes",
  "Syn"
)

Min_max <- data.frame(
  Class = c(
    "Syn", "Chlorophytes", "Chlorophytes", "Prasinophytes", "Prasinophytes",
    "Prasinophytes", "Cryptophytes", "Diatoms-2", "Diatoms-2", "Pelagophytes",
    "Pelagophytes", "Pelagophytes", "Dinoflagellates-1", "Haptophytes",
    "Haptophytes", "Haptophytes", "Haptophytes", "Diatoms-2", "Cryptophytes",
    "Prasinophytes", "Chlorophytes", "Syn", "Dinoflagellates-1", "Pelagophytes"
  ),
  Pig_Abbrev = c(
    "Zea", "Zea", "Chl_b", "Pra", "Zea", "Chl_b", "Allo", "Chl_c3",
    "Fuco", "Chl_c3", "X19but", "Fuco", "Per", "X19but", "X19hex",
    "Fuco", "Tchla", "Tchla", "Tchla", "Tchla", "Tchla", "Tchla", "Tchla",
    "Tchla"
  ),
  min = as.numeric(c(
    0.0800, 0.0063, 0.1666, 0.0642, 0.0151, 0.4993, 0.2118, 0.0189,
    0.3315, 0.1471, 0.2457, 0.3092, 0.3421, 0.0819, 0.2107, 0.0090,
    1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000
  )),
  max = as.numeric(c(
    1.2123, 0.0722, 0.9254, 0.4369, 0.1396, 0.9072, 0.5479, 0.1840,
    0.9332, 0.2967, 1.0339, 1.2366, 0.8650, 0.2872, 1.3766, 0.4689,
    1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000
  ))
)

set.seed("7683")
Results <- simulated_annealing(
  S = s_matrix, 
  F = Fu,
  user_defined_min_max = Min_max,
  do_matrix_checks = TRUE,
  niter = 1,
  step = 0.01,
  weight.upper.bound = 30
)

Results$Figure
