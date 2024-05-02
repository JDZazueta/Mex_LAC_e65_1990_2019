###############################################################################
# Título:   De líderes a rezagados: el deterioro de la esperanza de vida en las 
#           edades avanzadas en México frente a otros países de América Latina y 
#           el Caribe, 1990-2019
# Revista:  Revista Latinoamericana de Población
# Data:     UN WPP 2022 Life Tables
# Autor:    J.Daniel Zazueta Borboa
################################################################################


#---------------------------------------------------------------------------- #
#     0. Packages and functions
# ---------------------------------------------------------------------------- #

# To clear everything in R, before start the analysis
rm(list = ls())

pacman::p_load(here)

source(here::here("R code/00.Packages and functions.R"))

#---------------------------------------------------------------------------- #
#    1. Open Life tables UN WPP 2022
# ---------------------------------------------------------------------------- #

# - Life tables females
UN_WPP_LT_Females <- read.csv("Data/UNWPP/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.csv",
                              header = T, sep = ",")


# - Life tables males
UN_WPP_LT_Males <- read.csv("Data/UNWPP/WPP2022_Life_Table_Complete_Medium_Male_1950-2021.csv",
                            header = T, sep = ",")


# Combine
UN_WPP_LT <- rbind(UN_WPP_LT_Females,UN_WPP_LT_Males)

#---------------------------------------------------------------------------- #
#    2. Create subdatasets
# ---------------------------------------------------------------------------- #

# Latin America countries
UN_WPP_LT_LAC <- UN_WPP_LT %>% 
  filter(ISO3_code=="ARG" | ISO3_code=="BOL" | ISO3_code=="BRA" | ISO3_code=="BLZ" |
          ISO3_code=="CHL" | ISO3_code=="COL" | ISO3_code=="CRI" | 
           ISO3_code=="CUB" | ISO3_code=="DOM" | ISO3_code=="ECU" | ISO3_code=="SLV" |
           ISO3_code=="GTM" | ISO3_code=="HTI" | ISO3_code=="HND" | ISO3_code=="MEX" | 
           ISO3_code=="NIC" | ISO3_code=="PAN" | ISO3_code=="PRY" | ISO3_code=="PER" | 
           ISO3_code=="URY" | ISO3_code=="VEN")

# 26 countries 
write.csv(UN_WPP_LT_LAC, file = "Data/Final/Life_tables_LAC.csv", row.names = F)

# The overall value of LAC
UN_WPP_LT_LAC_overall <- UN_WPP_LT %>% 
  filter(Location=="Latin America and the Caribbean")

write.csv(UN_WPP_LT_LAC_overall, file = "Data/Final/UN_WPP_LT_LAC_overall.csv", row.names = F)

