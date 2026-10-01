###### load library and functions
library(ggplot2)
library(patchwork)
library(tidyverse)
library(reshape2)
library(nlme)
library(dplyr)
library(purrr)
library(tidyr)
library(broom.mixed)
library(emmeans)
library(HIEv)
library(pander)
library(mgcv)
library(visreg)
library(ggpubr)
library(plotrix) ##std.error	
Sys.setlocale("LC_TIME", "C")

source("functions_of_data_processing_and_visualizing_data.R")

###### load data
##### LAI (Leaf area index) data
lai <- readTOA5("cache/FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat")
lai$LAI <- lai$LAI - 0.8
laisub <- subset(lai, Date > as.Date("2015-10-1") & Date < as.Date("2016-1-1"))
laisub <- merge(laisub, eucface())
#### convert unit to cm2 g-1 C
### get carbon concentrations of foliage and nsc
leafC <- read.csv("cache/FACE_P0020_RA_NPleaf_2012-2018-L2.csv")
leafC$Cm <- with(leafC, Nm * CN)
CperDM_leaf <- mean(leafC$Cm,na.rm=T)/1000
CperDM_NSC <- 12/(12 + 2 + 16)
eucface_sla <- 52.6/CperDM_leaf  # See Duursma et al. 2016
#### Net increase in LAI from 1 Nov to 1 Dec for each ring
lai_prod <- group_by(laisub, Ring) %>%
  dplyr::do(lai_pre = approx(.$Date, .$LAI, as.Date("2015-11-5"))$y,
            lai_post = approx(.$Date, .$LAI, as.Date("2015-12-16"))$y) %>%
  unnest(cols = c(lai_pre, lai_post))  %>% 
  mutate(lai_net_inc = lai_post - lai_pre,
         mf_net_inc = (10000 / eucface_sla) * lai_net_inc)




##### gcc
gcc <- readTOA5("cache/FACE_P0037_RA_CANOPYGREENNESS-ZOOMED_OPEN_L2.dat")





##### wood mass
#### read
d15 <-  read.csv("cache/FACE_AUX_RA_STEM_DIAMETERS_RAW_20150525.csv")
d15$Diameter <- as.numeric(d15$Diameter)  #"MISSING" in file
d15$wood_mass <- allom_agb(d15$Diameter)  # kg DM tree-1
#### calculate stem and branch weight using proportion (0.744/0.256)
d15$Woodmass_stem <- 0.744 * with(d15,wood_mass)
d15$Woodmass_branch <- 0.256 * with(d15,wood_mass)
### bark
d15$bark_frac <- with(d15,(0.8/Diameter) / (1 + 0.8/Diameter))
d15$woodmass_stem_bark <- 0.744 * with(d15,wood_mass * bark_frac)
d15$woodmass_branch_bark <- 0.256 * with(d15,wood_mass * bark_frac)
### sapwood (Gimeno equation)
d15$depth_stem_sapwood <- d15$Diameter * 0.022 + 1.2
d15$woodmass_stem_sapwood <- (1 - ((d15$Diameter - 2 * d15$depth_stem_sapwood)/d15$Diameter)^2) * d15$Woodmass_stem
d15$woodmass_branch_sapwood <- (d15$Woodmass_branch - d15$woodmass_branch_bark) * 0.8
#### combine bark and sapwood
wood_mass <- doBy::summaryBy(woodmass_stem_sapwood + woodmass_branch_sapwood +
                         woodmass_stem_bark + woodmass_branch_bark ~ Ring,
                       FUN=sum, na.rm = T, keep.names = T, dat = d15)
wood_mass$CO2 <- c("ele","amb","amb","ele","ele","amb")





##### litter data
litter <-  read.csv("cache/FACE_P0017_RA_Litter_20150101-20151217-L1-V2.csv")
litter$CO2 <- "Ambient"
litter$CO2[litter$Ring %in% c(1,4,5)] <- "Elevated"
litter$DateCorr <- parse_date_time(litter$Date,"d m y")





##### NSC data
#### read
wood <- read.csv("cache/FACE_P0078_RA_WOODCARB_L1_20151101-20151201.csv") %>% 
  add_flush_fac %>%
  mutate(component = "wood", Ring = paste0("R", Ring))
bark <- read.csv("cache/FACE_P0078_RA_BARKCARB_L1_20151115-20151215.csv") %>%
  filter(treenr != "116_2") %>%
  mutate(treenr = as.integer(treenr),
         component = "bark", Ring = paste0("R", Ring)) %>% 
  add_flush_fac
leaf <- read.csv("cache/FACE_P0078_RA_LEAFCARB_L1_20151115-20151215.csv") %>% 
  mutate(component = "leaf", Ring = paste0("R", Ring)) %>%
  add_flush_fac
#### combine all nsc data
ssall <- bind_rows(wood, bark, leaf) %>%
  filter(!is.na(sugar))  # 6 rows where sugar and starch missing
ssall$position[ssall$component == "leaf"] <- ssall$leafage[ssall$component == "leaf"]
### averaged by component/position/flush/treatment
# --> use for figures
ssall_a <- group_by(ssall, component, position, flush, treatment) %>%
  summarize(tnc_se = sefun(tnc),
            tnc = mean(tnc),
            starch_se=sefun(starch),
            starch=mean(starch),
            sugar_se = sefun(sugar),
            sugar = mean(sugar)) %>%
  ungroup %>%
  mutate(position = factor(position, levels=c("DBH","DBH + 50","BoC","Branch","Twig","old","new"))) %>%
  arrange(component, position, flush)
levels(ssall_a$position) <- c("Base","Base","BoC","Branch","Twig", "Old","New")



a <- subset(ssall, component == "wood" & position == "Branch" & flush == "pre-flush")
mean(a$tnc, na.rm = TRUE) 
str(a)
library(dplyr)

a %>%
  group_by(treatment) %>%
  summarise(
    mean_tnc = mean(tnc, na.rm = TRUE),
    SE = sd(tnc, na.rm = TRUE) / sqrt(n())
  )

b <- subset(ssall, component == "leaf" & position == "new" & flush == "pre-flush")
mean(b$tnc, na.rm = TRUE) 
str(b)
library(dplyr)

b %>%
  group_by(treatment) %>%
  summarise(
    mean_tnc = mean(tnc, na.rm = TRUE),
    SE = sd(tnc, na.rm = TRUE) / sqrt(n())
  )

