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


# ---------------------------------------------------------------------------- #
#     1. Open life tables
# ---------------------------------------------------------------------------- #

# ---------------
#  Life tables
# ---------------
UN_WPP_LT_LAC <- read.csv("Data/Final/Life_tables_LAC.csv",
                          header = T,
                          sep = ",")

UN_WPP_LT_overall <- read.csv("Data/Final/UN_WPP_LT_LAC_overall.csv",
                              header = T,
                              sep = ",")


table(UN_WPP_LT_LAC$ISO3_code, UN_WPP_LT_LAC$Location)

#---------------------------------------------------------------------------- #
#    2. Create subdatasets
# ---------------------------------------------------------------------------- #

# -- Subdataset with e65
LAC_dx65_countries <- UN_WPP_LT_LAC %>% 
  #filter(Location!="Latin America and the Caribbean") %>% 
  filter(AgeGrpStart>=65) %>% 
  filter(Time==1990 | Time==2019) %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, dx) %>% 
  rename(Year = Time,
         Age = AgeGrpStart) %>% 
  mutate(Region = 1) %>% 
  group_by(Year,Sex) %>% 
  ungroup() %>% 
  mutate(Pais = case_when(ISO3_code=="ARG" ~ "Argentina",
                          ISO3_code=="BLZ" ~ "Belice",
                          ISO3_code=="BOL" ~ "Bolivia",
                          ISO3_code=="BRA" ~ "Brasil",
                          ISO3_code=="CHL" ~ "Chile",
                          ISO3_code=="COL" ~ "Colombia",
                          ISO3_code=="CRI" ~ "Costa Rica",
                          ISO3_code=="CUB" ~ "Cuba",
                          ISO3_code=="DOM" ~ "República Dominicana",
                          ISO3_code=="ECU" ~ "Ecuador",
                          ISO3_code=="GTM" ~ "Guatemala",
                          ISO3_code=="HND" ~ "Honduras",
                          ISO3_code=="HTI" ~ "Haití",
                          ISO3_code=="MEX" ~ "México",
                          ISO3_code=="NIC" ~ "Nicaragua",
                          ISO3_code=="PAN" ~ "Panamá",
                          ISO3_code=="PER" ~ "Perú",
                          ISO3_code=="PRY" ~ "Paraguay",
                          ISO3_code=="SLV" ~ "El Salvador",
                          ISO3_code=="URY" ~ "Uruguay",
                          ISO3_code=="VEN" ~ "Venezuela")) %>% 
  filter(!is.na(Pais))




LAC_dx65_countries_males <- LAC_dx65_countries %>% 
  filter(Sex=="Male")

LAC_dx65_countries_females <- LAC_dx65_countries %>% 
  filter(Sex=="Female")


#---------------------------------------------------------------------------- #
#    3. Figures
# ---------------------------------------------------------------------------- #


Fig_A1 <- ggplot(LAC_dx65_countries_males, mapping = aes(x=Age, y=dx,
                                                         color=factor(Year)))+
  geom_line() +
  facet_wrap(.~Pais, ncol=5) +
  scale_x_continuous(breaks = c(seq(65,100,5))) +
  scale_color_manual(values = c("grey40","lightblue3")) +
  theme_bw() +
  theme_minimal( )+
  theme(text = element_text(size = 14), 
        legend.position = "bottom",
        #legend.position=c(.30, 0.0),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title ="A) Hombres",
    #subtitle = "England & Wales and Finland MCD & Italy (Turin) Enhanced wholly",
    color = "Año",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Edad a la muerte" ~d[x]~ " tabla de vida"),
    x="Edad")
Fig_A1
ggsave(filename = "Figure A1.png",
       path= "Figures/Appendix/",
       dpi = 320, width = 9, height = 8,
       bg = "transparent")



Fig_A2 <- ggplot(LAC_dx65_countries_females, mapping = aes(x=Age, y=dx,
                                                         color=factor(Year)))+
  geom_line() +
  facet_wrap(.~Pais, ncol=5) +
  scale_x_continuous(breaks = c(seq(65,100,5))) +
  scale_color_manual(values = c("grey40","lightblue3")) +
  theme_bw() +
  theme_minimal( )+
  theme(text = element_text(size = 14), 
        legend.position = "bottom",
        #legend.position=c(.30, 0.0),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title ="A) Mujeres",
       #subtitle = "England & Wales and Finland MCD & Italy (Turin) Enhanced wholly",
       color = "Año",
       #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
       y=bquote("Edad a la muerte" ~d[x]~ " tabla de vida"),
       x="Edad")
Fig_A2
ggsave(filename = "Figure A2.png",
       path= "Figures/Appendix/",
       dpi = 320, width = 9, height = 8,
       bg = "transparent")

