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

#---------------------------------------------------------------------------- #
#    2. Prepare data
# ---------------------------------------------------------------------------- #

# Mexico
Mex_mx_fordecomp <- UN_WPP_LT_LAC %>% 
  filter(AgeGrpStart>=65) %>% 
  filter(Time>=1990 & Time<=2019) %>% 
  filter(Location=="Mexico") %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, mx) %>% 
  filter(Time==1990 | Time==1999 | 
         Time==2000 | Time==2009 |
         Time==2010 | Time==2019) %>% 
  rename(Year = Time,
         Age = AgeGrpStart) %>% 
  dplyr::select(Year, Age, Sex, mx)  %>% 
  pivot_wider(names_from = Year,
              values_from = mx,
              names_prefix = "mx_") %>% 
  arrange(Sex, Age) 

Mex_mx_fordecomp <- data.table(Mex_mx_fordecomp)

#---------------------------------------------------------------------------- #
#    3. Decomposition
# ---------------------------------------------------------------------------- #

# ---------
# Mexico 1990-1999
# ---------

Decomposition_Mex_90_99 <- Mex_mx_fordecomp[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                         pars1 = mx_1990, 
                                                                                         pars2 = mx_1999,
                                                                                         symmetrical = TRUE, direction = "up")),
                                              by = list(Sex)]
Decomposition_Mex_90_99 <- data.frame(Decomposition_Mex_90_99)


Decomposition_Mex_90_99_resutls <- Decomposition_Mex_90_99 %>% 
  rename(Contribution = V1) %>% 
  group_by(Sex) %>% 
  mutate(age = seq(65,100,1),
         Age_groups = case_when(#age>=50 & age<=54 ~ 50,
                                #age>=55 & age<=59 ~ 55,
                                #age>=60 & age<=64 ~ 60,
                                age>=65 & age<=69 ~ 65,
                                age>=70 & age<=74 ~ 70,
                                age>=75 & age<=79 ~ 75,
                                age>=80 & age<=84 ~ 80,
                                age>=85 & age<=89 ~ 85,
                                age>=90 & age<=94 ~ 90,
                                age>=95  ~ 95),
         Period = 1,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 


# ---------
# Mexico 2000-2009
# ---------

Decomposition_Mex_00_09 <- Mex_mx_fordecomp[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                       pars1 = mx_2000, 
                                                                                       pars2 = mx_2009,
                                                                                       symmetrical = TRUE, direction = "up")),
                                            by = list(Sex)]
Decomposition_Mex_00_09 <- data.frame(Decomposition_Mex_00_09)


Decomposition_Mex_00_09_resutls <- Decomposition_Mex_00_09 %>% 
  rename(Contribution = V1) %>% 
  group_by(Sex) %>% 
  mutate(age = seq(65,100,1),
         Age_groups = case_when(#age>=50 & age<=54 ~ 50,
           #age>=55 & age<=59 ~ 55,
           #age>=60 & age<=64 ~ 60,
           age>=65 & age<=69 ~ 65,
           age>=70 & age<=74 ~ 70,
           age>=75 & age<=79 ~ 75,
           age>=80 & age<=84 ~ 80,
           age>=85 & age<=89 ~ 85,
           age>=90 & age<=94 ~ 90,
           age>=95  ~ 95),
         Period = 2,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 

# ---------
# Mexico 2010-2019
# ---------

Decomposition_Mex_10_19 <- Mex_mx_fordecomp[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                       pars1 = mx_2010, 
                                                                                       pars2 = mx_2019,
                                                                                       symmetrical = TRUE, direction = "up")),
                                            by = list(Sex)]
Decomposition_Mex_10_19 <- data.frame(Decomposition_Mex_10_19)


Decomposition_Mex_10_19_resutls <- Decomposition_Mex_10_19 %>% 
  rename(Contribution = V1) %>% 
  group_by(Sex) %>% 
  mutate(age = seq(65,100,1),
         Age_groups = case_when(#age>=50 & age<=54 ~ 50,
           #age>=55 & age<=59 ~ 55,
           #age>=60 & age<=64 ~ 60,
           age>=65 & age<=69 ~ 65,
           age>=70 & age<=74 ~ 70,
           age>=75 & age<=79 ~ 75,
           age>=80 & age<=84 ~ 80,
           age>=85 & age<=89 ~ 85,
           age>=90 & age<=94 ~ 90,
           age>=95  ~ 95),
         Period = 3,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 


# ---------
# Combined
# ---------

Data_Mex_decomp_1990_2019 <- rbind(Decomposition_Mex_90_99_resutls,
                                   Decomposition_Mex_00_09_resutls,
                                   Decomposition_Mex_10_19_resutls)




#---------------------------------------------------------------------------- #
#    4. Decomposition Figures
# ---------------------------------------------------------------------------- #



Data_Mex_decomp_1990_2019_aggregated <- Data_Mex_decomp_1990_2019 %>% 
  group_by(Period, Sex, Age_groups ) %>% 
  summarize(Contribution = sum(Contribution)) %>% 
  group_by(Period, Sex) %>% 
  mutate(Total = sum(Contribution),
         sexo = case_when(Sex=="Male" ~ 1,
                          Sex=="Female" ~ 2),
         gains = case_when(Contribution<0 ~ 0,
                           Contribution>=0 ~ 1))


Data_Mex_decomp_1990_2019_aggregated$gains <- factor(Data_Mex_decomp_1990_2019_aggregated$gains,
                                                     levels = c(0,1),
                                                     labels = c("Decrecer", "Incrementar"))


Data_Mex_decomp_1990_2019_aggregated$sexo <- factor(Data_Mex_decomp_1990_2019_aggregated$sexo,
                                                       levels = c(1,2),
                                                       labels = c("Hombres", "Mujeres"))

Data_Mex_decomp_1990_2019_aggregated$Period <- factor(Data_Mex_decomp_1990_2019_aggregated$Period,
                                                   levels = c(1,2,3),
                                                   labels = c("1990-1999", "2000-2009", "2010-2019"))

Data_Mex_decomp_1990_2019_aggregated$Age_groups <- factor(Data_Mex_decomp_1990_2019_aggregated$Age_groups,
                                                             levels = c(seq(65,95,5)),
                                                             labels = c(#"50-54", "55-59",
                                                                        #"60-64",
                                                                        "65-69", 
                                                                        "70-74", "75-79",
                                                                        "80-84", "85-89",
                                                                        "90-94", "95+"))


Fig_5 <- ggplot(Data_Mex_decomp_1990_2019_aggregated,
                mapping = aes(x=Age_groups,
                              y=Contribution,
                              color=gains,
                              fill=gains)) +
  geom_bar(position = "stack", 
           stat = "identity", 
           alpha = 1) +
  facet_grid(sexo~Period) +
  coord_flip() +
  #scale_x_continuous(breaks = c(seq(1990,2020,5))) +
  #scale_y_continuous(breaks = c(seq(1,20,1))) +
  #scale_color_manual(values = Color_fig2) +
  scale_color_manual(values = c("black","black")) +
  scale_fill_manual(values = c("grey40","lightblue4")) +
  theme_bw() +
  theme_minimal( )+
  theme_classic() +
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
    color = "Contribución",
    fill = "Contribución",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Contribución a los cambios en " ~e[65]~ " (en años)"),
    x="Grupos de edad")
Fig_5
ggsave(filename = "Figure 5.png",
       path= "Figures/Main figures/",
       dpi = 320, width = 8, height = 6,
       bg = "transparent")








