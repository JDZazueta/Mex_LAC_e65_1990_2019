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
LAC_e65_countries <- UN_WPP_LT_LAC %>% 
  #filter(Location!="Latin America and the Caribbean") %>% 
  filter(AgeGrpStart==65) %>% 
  filter(Time>=1990 & Time<=2019) %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, ex) %>% 
  rename(Year = Time,
         Age = AgeGrpStart) %>% 
  mutate(Region = 1) %>% 
  group_by(Year,Sex) %>% 
  mutate(rank = dense_rank(desc(ex))) %>% 
  ungroup()

# -- Subdataset with e65
Mex_e65 <- LAC_e65_countries %>% 
  filter(Location=="Mexico") %>% 
  mutate(sexo = case_when(Sex=="Male"~ 1,
                          Sex=="Female" ~ 2),
         order = rank)  

save(Mex_e65, file = "Data/Final/Mex_e65_UNWPP.RData")

Mex_e65_years <- Mex_e65 %>% 
  filter(Year==1990 | Year==2000 | Year==2010 | Year==2019) 


# -- Latin american leader
LAC_e65_leaders <- LAC_e65_countries %>% 
  mutate(Mexico = case_when(Location=="Mexico" ~ 1,
                            Location!="Mexico" ~ 0),
         sexo = case_when(Sex=="Male"~ 1,
                          Sex=="Female" ~ 2)) %>% 
  filter(rank<=10 | Location=="Mexico") 



Panama_e65 <- LAC_e65_countries %>% 
  filter(Location=="Panama")  %>% 
  filter(Year==1990 | Year==2000 | Year==2010 | Year==2019) 

LAC_e65_leaders_years <- LAC_e65_leaders %>% 
  filter(Year==1990) 


#---------------------------------------------------------------------------- #
#   3. Figures
# ---------------------------------------------------------------------------- #

# -------------------------------
#. Gráfica 1.
# -------------------------------

LAC_e65_leaders$sexo <- factor(LAC_e65_leaders$sexo, 
                               levels = c(1,2),
                               labels = c("Hombres", "Mujeres"))

LAC_e65_leaders$Mexico <- factor(LAC_e65_leaders$Mexico, 
                                 levels = c(0,1),
                                 labels = c("Otros paises", "Mexico"))


LAC_e65_leaders <- LAC_e65_leaders %>% 
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
                          ISO3_code=="VEN" ~ "Venezuela")) 


Figure_1 <- ggplot(LAC_e65_leaders, mapping = aes(x=Year, y=ex,
                                                  color=Pais)) +
  #geom_line(aes(shape=Mexico, color=Location), size=1.2) +
  geom_line(size=1.2) +
  facet_grid(.~sexo) +
  scale_x_continuous(breaks = c(seq(1990,2020,5))) +
  #scale_y_continuous(breaks = c(seq(1,20,1))) +
  scale_color_manual(values = Color_fig2) +
  theme_bw() +
  #theme_minimal( )+
  #theme_classic() +
  theme(text = element_text(size = 14), 
        legend.position = "bottom",
        #legend.position=c(.10, 0.50),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(color=guide_legend(ncol=4)) +
  labs(#title ="A) Latin American countries (n=22)",
    #subtitle = "England & Wales and Finland MCD & Italy (Turin) Enhanced wholly",
    color = "País",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Esperanza de vida " ~e[65]~ " "),
    x="Año")
Figure_1
ggsave(filename = "Figure 1.png",
       path= "Figures/Main figures/",
       dpi = 320, width = 10, height = 8,
       bg = "transparent")


# -------------------------------
#. Gráfica 2
# -------------------------------

Mex_e65$sexo <- factor(Mex_e65$sexo, 
                       levels = c(1,2),
                       labels = c("Hombres", "Mujeres"))



Figure_2 <- ggplot(Mex_e65, mapping = aes(x=Year, y=rank)) +
  geom_line(size=1.2) +
  facet_grid(.~sexo) +
  scale_x_continuous(breaks = c(seq(1990,2020,5))) +
  #scale_y_continuous(breaks = c(seq(20,1,1))) +
  scale_y_reverse(limits = c(15, 1,1),
                  breaks = c(seq(1,15,1))) +
  theme_bw() +
  theme_minimal( )+
  theme(text = element_text(size = 14), 
        legend.position = "bottom",
        #legend.position=c(.10, 0.50),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title ="A) Latin American countries (n=22)",
    #subtitle = "England & Wales and Finland MCD & Italy (Turin) Enhanced wholly",
    color = "Estimate",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Posición del ranking en " ~e[65]~ ""),
    x="Año")
Figure_2
ggsave(filename = "Figure 2.png",
       path= "Figures/Main figures/",
       dpi = 320, width = 8, height = 6,
       bg = "transparent")

