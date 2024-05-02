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
  filter(Time==1990 | Time==2000 | Time==2010 | Time==2019) %>% 
  rename(Year = Time,
         Age = AgeGrpStart,
         mx_Mex = mx) %>% 
  dplyr::select(Year, Age, Sex, mx_Mex)  %>% 
  arrange(Year, Sex, Age) 


# --- Latin America
Countries_Lac_fordecomp <- UN_WPP_LT_LAC %>% 
  #filter(Location!="Latin America and the Caribbean") %>% 
  filter(AgeGrpStart==65) %>% 
  filter(Time>=1990 & Time<=2019) %>% 
  filter(Location!="Mexico") %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, mx, ex) %>% 
  mutate(Region = 1) %>% 
  group_by(Time,Sex) %>% 
  mutate(rank = dense_rank(desc(ex))) %>% 
  ungroup() %>% 
  filter(rank<=10) %>% 
  group_by(Time, Sex) %>% 
  filter(rank==1 | rank==5  | rank==10) %>% 
  filter(Time==1990 | Time==2000 | Time==2010 | Time==2019) %>% 
  arrange(Time, rank, Location) %>% 
  dplyr::select(ISO3_code, Location, Time,Sex, rank) %>% 
  mutate(decomposition=1) 

Table_1 <- UN_WPP_LT_LAC %>% 
  #filter(Location!="Latin America and the Caribbean") %>% 
  filter(AgeGrpStart==65) %>% 
  filter(Time>=1990 & Time<=2019) %>% 
  filter(Location!="Mexico") %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, mx, ex) %>% 
  mutate(Region = 1) %>% 
  group_by(Time,Sex) %>% 
  mutate(rank = dense_rank(desc(ex))) %>% 
  ungroup() %>% 
  filter(rank<=10) %>% 
  group_by(Time, Sex) %>% 
  filter(rank==1 | rank==5  | rank==10) %>% 
  filter(Time==1990 | Time==2000 | Time==2010 | Time==2019) %>% 
  arrange(Time, rank, Location) %>% 
  dplyr::select(ISO3_code, Location, Time,Sex, rank, ex) %>% 
  mutate(decomposition=1) %>% 
  arrange(Sex, Time, rank)

write.xlsx(Table_1, file = "Excel/Table_1.xlsx")  


UN_WPP_LT_LAC_selected <- merge(UN_WPP_LT_LAC, 
                                Countries_Lac_fordecomp,
                                by=c("ISO3_code", "Location", "Time", "Sex"))


# Data from decop LAC
UN_WPP_LT_LAC_fordecomp <- UN_WPP_LT_LAC_selected %>% 
  dplyr::select(ISO3_code, Location, Time, AgeGrpStart, Sex, rank, mx) %>% 
  rename(Year = Time,
         Age = AgeGrpStart) %>% 
  filter(Age>=65) %>% 
  dplyr::select(Year, Age, Sex, rank, mx) %>% 
  arrange(rank, Year, Sex, Age) %>% 
  pivot_wider(names_from = rank,
              values_from = mx,
              names_prefix = "mx_")


###############################
# Combine
###############################


Data_predecomop <- merge(Mex_mx_fordecomp, 
                         UN_WPP_LT_LAC_fordecomp,
                         by=c("Year", "Sex", "Age"))

Data_predecomop_2 <- Data_predecomop %>% 
  arrange(Year, Sex, Age)


#---------------------------------------------------------------------------- #
#    3. Decomposition
# ---------------------------------------------------------------------------- #



# ---------
# Mex version rank 1
# ---------

Data_predecomop_2 <- data.table(Data_predecomop_2)
Decomposition_LAC_rank_1 <- Data_predecomop_2[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                         pars1 = mx_1, 
                                                                                         pars2 = mx_Mex,
                                                                                         symmetrical = TRUE, direction = "up")),
                                              by = list(Year, Sex)]
Decomposition_LAC_rank_1 <- data.frame(Decomposition_LAC_rank_1)


Decomposition_LAC_rank_1_resutls <- Decomposition_LAC_rank_1 %>% 
  rename(Contribution = V1) %>% 
  group_by(Year, Sex) %>% 
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
         Rank = 1,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 




# ---------
# Mex version rank 5
# ---------

Decomposition_LAC_rank_5 <- Data_predecomop_2[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                         pars1 = mx_5, 
                                                                                         pars2 = mx_Mex,
                                                                                         symmetrical = TRUE, direction = "up")),
                                              by = list(Year, Sex)]
Decomposition_LAC_rank_5 <- data.frame(Decomposition_LAC_rank_5)


Decomposition_LAC_rank_5_resutls <- Decomposition_LAC_rank_5 %>% 
  rename(Contribution = V1) %>% 
  group_by(Year, Sex) %>% 
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
         Rank = 5,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 




# ---------
# Mex version rank 10
# ---------

Decomposition_LAC_rank_10 <- Data_predecomop_2[,cbind(Contribution = stepwise_replacement(func= e0,
                                                                                          pars1 = mx_10, 
                                                                                          pars2 = mx_Mex,
                                                                                         symmetrical = TRUE, direction = "up")),
                                              by = list(Year, Sex)]
Decomposition_LAC_rank_10 <- data.frame(Decomposition_LAC_rank_10)


Decomposition_LAC_rank_10_resutls <- Decomposition_LAC_rank_10 %>% 
  rename(Contribution = V1) %>% 
  group_by(Year, Sex) %>% 
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
         Rank = 10,
         e65_gap = sum(Contribution),
         Rela_ctb_1x1 = Contribution/e65_gap*100) 


# ---------
# Combin resutls
# ---------



Decomposition_results_e65_Mex_Lac <- rbind(Decomposition_LAC_rank_1_resutls,
                                           Decomposition_LAC_rank_5_resutls,
                                           Decomposition_LAC_rank_10_resutls)


write.csv(Decomposition_results_e65_Mex_Lac, file = "Data/Final/Decomposition_results_e65_Mex_Lac.csv", row.names = F)




#---------------------------------------------------------------------------- #
#    4. Decomposition Figures
# ---------------------------------------------------------------------------- #


########################
# Males
########################

Decomposition_results_e65_Mex_Lac_males <- Decomposition_results_e65_Mex_Lac %>% 
  filter(Sex=="Male") %>%
  group_by(Year, Sex, Rank, Age_groups ) %>% 
  summarize(Contribution = sum(Contribution)) %>% 
  group_by(Year, Sex, Rank) %>% 
  mutate(Total = sum(Contribution),
         gains = case_when(Contribution<0 ~ 0,
                           Contribution>=0 ~ 1))

Decomposition_results_e65_Mex_Lac_males$gains <- factor(Decomposition_results_e65_Mex_Lac_males$gains,
                                                       levels = c(0,1),
                                                       labels = c("Desventaja", "Ventaja"))


Decomposition_results_e65_Mex_Lac_males$Rank <- factor(Decomposition_results_e65_Mex_Lac_males$Rank,
                                                       levels = c(1,5,10),
                                                       labels = c("Ranking 1", "Ranking 5", "Ranking 10"))

Decomposition_results_e65_Mex_Lac_males$Age_groups <- factor(Decomposition_results_e65_Mex_Lac_males$Age_groups,
                                                       levels = c(seq(65,95,5)),
                                                       labels = c(#"50-54", "55-59",
                                                                  #"60-64",
                                                                  "65-69", 
                                                                  "70-74", "75-79",
                                                                  "80-84", "85-89",
                                                                  "90-94", "95+"))

Fig_3 <- ggplot(Decomposition_results_e65_Mex_Lac_males,
                     mapping = aes(x=Age_groups,
                                   y=Contribution,
                                   color=gains,
                                   fill=gains)) +
  geom_bar(position = "stack", 
           stat = "identity", 
           alpha = 1) +
  facet_grid(Rank~Year) +
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
    color = "México",
    fill = "México",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Contribución a las differencias en " ~e[65]~ " (en años)"),
    x="Grupos de edad")
Fig_3
ggsave(filename = "Figure 3.png",
       path= "Figures/Main figures/",
       dpi = 320, width = 8, height = 6,
       bg = "transparent")





########################
# Females
########################


Decomposition_results_e65_Mex_Lac_females <- Decomposition_results_e65_Mex_Lac %>% 
  filter(Sex=="Female") %>%
  group_by(Year, Sex, Rank, Age_groups ) %>% 
  summarize(Contribution = sum(Contribution)) %>% 
  group_by(Year, Sex, Rank) %>% 
  mutate(Total = sum(Contribution),
         gains = case_when(Contribution<0 ~ 0,
                           Contribution>=0 ~ 1))


Decomposition_results_e65_Mex_Lac_females$gains <- factor(Decomposition_results_e65_Mex_Lac_females$gains,
                                                        levels = c(0,1),
                                                        labels = c("Desventaja", "Ventaja"))


Decomposition_results_e65_Mex_Lac_females$Rank <- factor(Decomposition_results_e65_Mex_Lac_females$Rank,
                                                       levels = c(1,5,10),
                                                       labels = c("Ranking 1", "Ranking 5", "Ranking 10"))

Decomposition_results_e65_Mex_Lac_females$Age_groups <- factor(Decomposition_results_e65_Mex_Lac_females$Age_groups,
                                                               levels = c(seq(65,95,5)),
                                                               labels = c(#"50-54", "55-59",
                                                                 #"60-64",
                                                                 "65-69", 
                                                                 "70-74", "75-79",
                                                                 "80-84", "85-89",
                                                                 "90-94", "95+"))


Fig_4 <- ggplot(Decomposition_results_e65_Mex_Lac_females,
                     mapping = aes(x=Age_groups,
                                   y=Contribution,
                                   color=gains,
                                   fill=gains)) +
  geom_bar(position = "stack", 
           stat = "identity", 
           alpha = 1) +
  facet_grid(Rank~Year) +
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
        axis.text.x = element_text(angle = 0, vjust = -0.50, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(color=guide_legend(ncol=4)) +
  labs(#title ="A) Latin American countries (n=22)",
    #subtitle = "England & Wales and Finland MCD & Italy (Turin) Enhanced wholly",
    color = "México",
    fill = "México",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    y=bquote("Contribución a las differencias en " ~e[65]~ " (en años)"),
    x="Grupos de edad")
Fig_4
ggsave(filename = "Figure 4.png",
       path= "Figures/Main figures/",
       dpi = 320, width = 9, height = 6,
       bg = "transparent")





















