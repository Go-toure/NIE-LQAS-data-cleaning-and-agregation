
setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_raw/")
library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)
library(qs)

# Define input and output paths
input_file <- "C:/Users/TOURE/Documents/PADACORD/LQAS/272.rds"
# Read the RDS file
AB <- qread(input_file) |> 
  mutate(country = "NIE")

#read 
AB <- read_csv("C:/Users/TOURE/Downloads/Nigeria_LQAS_rev2_2025_05_20_03_49_34_888616_April_May_2025.csv") |>
  mutate(country = "NIE")

# AB <- read_excel("Final Raw Data Nigeria_LQAS_rev2_2025_01_30_05_57_28_045445.xlsx") |> 
#   mutate(country = "NIE")
# AB<- AB |> 
  #drop_na(states) 
AB$Cluster[!is.na(AB$Cluster)] <- 1
# AB$states<- toupper(AB$states)
# AB[,7] = toupper(AB[,7])
# AB[,8] = toupper(AB[,8])
# 
#remove rows with NA in x or y column
#AB <- AB[!(is.na(AC$Country)) & !(is.na(AC$roundNumber)), ]
AB<- AB |> 
  mutate(across((starts_with("Children_Seen_")),
                as.numeric)) |> 
  # filter(Children_seen_h1 >0, Children_seen_h2 >0, Children_seen_h3 >0, Children_Seen_h4 >0, Children_Seen_h5 >0, Children_Seen_h6 >0, Children_Seen_h7 >0, Children_Seen_h8 >0, Children_Seen_h9 >0, Children_Seen_h10 >0) |> 
  select(country, Region = states, District = lgas, Cluster, today, Children_seen_h1,Children_seen_h2, Children_seen_h3, Children_Seen_h4, Children_Seen_h5, Children_Seen_h6, Children_Seen_h7, Children_Seen_h8, Children_Seen_h9, Children_Seen_h10, Sex_Child1, Sex_Child2, Sex_Child3, Sex_Child4, Sex_Child5, Sex_Child6, Sex_Child7, Sex_Child8, Sex_Child9, Sex_Child10, FM_Child1, FM_Child2, FM_Child3, FM_Child4, FM_Child5, FM_Child6, FM_Child7, FM_Child8, FM_Child9, FM_Child10, Reason_Not_FM1, Reason_Not_FM2, Reason_Not_FM3, Reason_Not_FM4, Reason_Not_FM5, Reason_Not_FM6, Reason_Not_FM7, Reason_Not_FM8, Reason_Not_FM9, Reason_Not_FM10, Caregiver_Aware_h1, Caregiver_Aware_h2, Caregiver_Aware_h3, Caregiver_Aware_h4, Caregiver_Aware_h5, Caregiver_Aware_h6, Caregiver_Aware_h7, Caregiver_Aware_h8, Caregiver_Aware_h9, Caregiver_Aware_h10, Total_OPVdoses_Child1, Total_OPVdoses_Child2, Total_OPVdoses_Child3 , Total_OPVdoses_Child4, Total_OPVdoses_Child5, Total_OPVdoses_Child6 , Total_OPVdoses_Child7, Total_OPVdoses_Child8, Total_OPVdoses_Child9 , Total_OPVdoses_Child10) #Reason_Underimm_Child1 , Reason_Underimm_Child2, Reason_Underimm_Child3, reason_underimm_child4 , Reason_Underimm_Child5, Reason_Underimm_Child6, Reason_Underimm_Child7, Reason_Underimm_Child8, Reason_Underimm_Child9, Reason_Underimm_Child10 , PrevOPV_Child1, PrevOPV_Child2, PrevOPV_Child3, PrevOPV_child4, PrevOPV_Child5 , PrevOPV_Child6, PrevOPV_Child7, PrevOPV_Child8, PrevOPV_Child9, PrevOPV_Child10

AD<-AB |> 
  mutate(today = as_date(today)) |>
  filter(Children_seen_h1 != "NA", Children_seen_h2 != "NA", Children_seen_h3 != "NA", Children_Seen_h4 != "NA", Children_Seen_h5 != "NA", Children_Seen_h6 != "NA", Children_Seen_h7 != "NA", Children_Seen_h8 != "NA", Children_Seen_h9 != "NA", Children_Seen_h10 != "NA") |> 
  mutate(
    h1 = case_when(
      Children_seen_h1 != "NA" ~ 1),
    h2 = case_when(
      Children_seen_h2 != "NA" ~ 1),
    h3 = case_when(
      Children_seen_h3 != "NA" ~ 1),
    h4 = case_when(
      Children_Seen_h4 != "NA" ~ 1),
    h5 = case_when(
      Children_Seen_h5 != "NA" ~ 1),
    h6 = case_when(
      Children_Seen_h6 != "NA" ~ 1),
    h7 = case_when(
      Children_Seen_h7 != "NA" ~ 1),
    h8 = case_when(
      Children_Seen_h8 != "NA" ~ 1),
    h9 = case_when(
      Children_Seen_h9 != "NA" ~ 1),
    h10 = case_when(
      Children_Seen_h10 != "NA" ~ 1),
    tot_hh_visited = (h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10)) |> 
  select(country, Region, District , Cluster, today, tot_hh_visited, Children_seen_h1,Children_seen_h2, Children_seen_h3, Children_Seen_h4, Children_Seen_h5, Children_Seen_h6, Children_Seen_h7, Children_Seen_h8, Children_Seen_h9, Children_Seen_h10, Sex_Child1, Sex_Child2, Sex_Child3, Sex_Child4, Sex_Child5, Sex_Child6, Sex_Child7, Sex_Child8, Sex_Child9, Sex_Child10, FM_Child1, FM_Child2, FM_Child3, FM_Child4, FM_Child5, FM_Child6, FM_Child7, FM_Child8, FM_Child9, FM_Child10, Reason_Not_FM1, Reason_Not_FM2, Reason_Not_FM3, Reason_Not_FM4, Reason_Not_FM5, Reason_Not_FM6, Reason_Not_FM7, Reason_Not_FM8, Reason_Not_FM9, Reason_Not_FM10, Caregiver_Aware_h1, Caregiver_Aware_h2, Caregiver_Aware_h3, Caregiver_Aware_h4, Caregiver_Aware_h5, Caregiver_Aware_h6, Caregiver_Aware_h7, Caregiver_Aware_h8, Caregiver_Aware_h9, Caregiver_Aware_h10)

#Female sampled_structure
AD$`Sex_Child1`[AD$`Sex_Child1` == "F"] <- 1
AD$`Sex_Child2`[AD$`Sex_Child2` == "F"] <- 1
AD$`Sex_Child3`[AD$`Sex_Child3` == "F"] <- 1
AD$`Sex_Child4`[AD$`Sex_Child4` == "F"] <- 1
AD$`Sex_Child5`[AD$`Sex_Child5` == "F"] <- 1
AD$`Sex_Child6`[AD$`Sex_Child6` == "F"] <- 1
AD$`Sex_Child7`[AD$`Sex_Child7` == "F"] <- 1
AD$`Sex_Child8`[AD$`Sex_Child8` == "F"] <- 1
AD$`Sex_Child9`[AD$`Sex_Child9` == "F"] <- 1
AD$`Sex_Child10`[AD$`Sex_Child10` == "F"] <- 1
AD$`Sex_Child1`[AD$`Sex_Child1` == "M"] <- 0
AD$`Sex_Child2`[AD$`Sex_Child2` == "M"] <- 0
AD$`Sex_Child3`[AD$`Sex_Child3` == "M"] <- 0
AD$`Sex_Child4`[AD$`Sex_Child4` == "M"] <- 0
AD$`Sex_Child5`[AD$`Sex_Child5` == "M"] <- 0
AD$`Sex_Child6`[AD$`Sex_Child6` == "M"] <- 0
AD$`Sex_Child7`[AD$`Sex_Child7` == "M"] <- 0
AD$`Sex_Child8`[AD$`Sex_Child8` == "M"] <- 0
AD$`Sex_Child9`[AD$`Sex_Child9` == "M"] <- 0
AD$`Sex_Child10`[AD$`Sex_Child10` == "M"] <- 0

#CHIL VACCINATED
AD$`FM_Child1`[AD$`FM_Child1` == "Yes"] <- 1
AD$`FM_Child1`[AD$`FM_Child1` == "YES"] <- 1
AD$`FM_Child1`[AD$`FM_Child1` == "yes"] <- 1
AD$`FM_Child1`[AD$`FM_Child1` == "Y"] <- 1
AD$`FM_Child2`[AD$`FM_Child2` == "Yes"] <- 1
AD$`FM_Child2`[AD$`FM_Child2` == "YES"] <- 1
AD$`FM_Child2`[AD$`FM_Child2` == "yes"] <- 1
AD$`FM_Child2`[AD$`FM_Child2` == "Y"] <- 1
AD$`FM_Child3`[AD$`FM_Child3` == "Yes"] <- 1
AD$`FM_Child3`[AD$`FM_Child3` == "YES"] <- 1
AD$`FM_Child3`[AD$`FM_Child3` == "yes"] <- 1
AD$`FM_Child3`[AD$`FM_Child3` == "Y"] <- 1
AD$`FM_Child4`[AD$`FM_Child4` == "Yes"] <- 1
AD$`FM_Child4`[AD$`FM_Child4` == "YES"] <- 1
AD$`FM_Child4`[AD$`FM_Child4` == "yes"] <- 1
AD$`FM_Child4`[AD$`FM_Child4` == "Y"] <- 1
AD$`FM_Child5`[AD$`FM_Child5` == "Yes"] <- 1
AD$`FM_Child5`[AD$`FM_Child5` == "YES"] <- 1
AD$`FM_Child5`[AD$`FM_Child5` == "yes"] <- 1
AD$`FM_Child5`[AD$`FM_Child5` == "Y"] <- 1
AD$`FM_Child6`[AD$`FM_Child6` == "Yes"] <- 1
AD$`FM_Child6`[AD$`FM_Child6` == "YES"] <- 1
AD$`FM_Child6`[AD$`FM_Child6` == "yes"] <- 1
AD$`FM_Child6`[AD$`FM_Child6` == "Y"] <- 1
AD$`FM_Child7`[AD$`FM_Child7` == "Yes"] <- 1
AD$`FM_Child7`[AD$`FM_Child7` == "YES"] <- 1
AD$`FM_Child7`[AD$`FM_Child7` == "yes"] <- 1
AD$`FM_Child7`[AD$`FM_Child7` == "Y"] <- 1
AD$`FM_Child8`[AD$`FM_Child8` == "Yes"] <- 1
AD$`FM_Child8`[AD$`FM_Child8` == "YES"] <- 1
AD$`FM_Child8`[AD$`FM_Child8` == "yes"] <- 1
AD$`FM_Child8`[AD$`FM_Child8` == "Y"] <- 1
AD$`FM_Child9`[AD$`FM_Child9` == "Yes"] <- 1
AD$`FM_Child9`[AD$`FM_Child9` == "YES"] <- 1
AD$`FM_Child9`[AD$`FM_Child9` == "yes"] <- 1
AD$`FM_Child9`[AD$`FM_Child9` == "Y"] <- 1
AD$`FM_Child10`[AD$`FM_Child10` == "Yes"] <- 1
AD$`FM_Child10`[AD$`FM_Child10` == "YES"] <- 1
AD$`FM_Child10`[AD$`FM_Child10` == "yes"] <- 1
AD$`FM_Child10`[AD$`FM_Child10` == "Y"] <- 1
AD$`FM_Child1`[AD$`FM_Child1` == "NO"] <- 0
AD$`FM_Child1`[AD$`FM_Child1` == "No"] <- 0
AD$`FM_Child1`[AD$`FM_Child1` == "no"] <- 0
AD$`FM_Child1`[AD$`FM_Child1` == "N"] <- 0
AD$`FM_Child2`[AD$`FM_Child2` == "NO"] <- 0
AD$`FM_Child2`[AD$`FM_Child2` == "No"] <- 0
AD$`FM_Child2`[AD$`FM_Child2` == "no"] <- 0
AD$`FM_Child2`[AD$`FM_Child2` == "N"] <- 0
AD$`FM_Child3`[AD$`FM_Child3` == "NO"] <- 0
AD$`FM_Child3`[AD$`FM_Child3` == "No"] <- 0
AD$`FM_Child3`[AD$`FM_Child3` == "no"] <- 0
AD$`FM_Child3`[AD$`FM_Child3` == "N"] <- 0
AD$`FM_Child4`[AD$`FM_Child4` == "NO"] <- 0
AD$`FM_Child4`[AD$`FM_Child4` == "No"] <- 0
AD$`FM_Child4`[AD$`FM_Child4` == "no"] <- 0
AD$`FM_Child4`[AD$`FM_Child4` == "N"] <- 0
AD$`FM_Child5`[AD$`FM_Child5` == "NO"] <- 0
AD$`FM_Child5`[AD$`FM_Child5` == "No"] <- 0
AD$`FM_Child5`[AD$`FM_Child5` == "no"] <- 0
AD$`FM_Child5`[AD$`FM_Child5` == "N"] <- 0
AD$`FM_Child6`[AD$`FM_Child6` == "NO"] <- 0
AD$`FM_Child6`[AD$`FM_Child6` == "No"] <- 0
AD$`FM_Child6`[AD$`FM_Child6` == "no"] <- 0
AD$`FM_Child6`[AD$`FM_Child6` == "N"] <- 0
AD$`FM_Child7`[AD$`FM_Child7` == "NO"] <- 0
AD$`FM_Child7`[AD$`FM_Child7` == "No"] <- 0
AD$`FM_Child7`[AD$`FM_Child7` == "no"] <- 0
AD$`FM_Child7`[AD$`FM_Child7` == "N"] <- 0
AD$`FM_Child8`[AD$`FM_Child8` == "NO"] <- 0
AD$`FM_Child8`[AD$`FM_Child8` == "No"] <- 0
AD$`FM_Child8`[AD$`FM_Child8` == "no"] <- 0
AD$`FM_Child8`[AD$`FM_Child8` == "N"] <- 0
AD$`FM_Child9`[AD$`FM_Child9` == "NO"] <- 0
AD$`FM_Child9`[AD$`FM_Child9` == "No"] <- 0
AD$`FM_Child9`[AD$`FM_Child9` == "no"] <- 0
AD$`FM_Child9`[AD$`FM_Child9` == "N"] <- 0
AD$`FM_Child10`[AD$`FM_Child10` == "NO"] <- 0
AD$`FM_Child10`[AD$`FM_Child10` == "No"] <- 0
AD$`FM_Child10`[AD$`FM_Child10` == "no"] <- 0
AD$`FM_Child10`[AD$`FM_Child10` == "N"] <- 0

#Care_Giver_Informed_SIA
AD$`Caregiver_Aware_h1`[AD$`Caregiver_Aware_h1` == "Y"] <- 1
AD$`Caregiver_Aware_h1`[AD$`Caregiver_Aware_h1` == "1"] <- 1
AD$`Caregiver_Aware_h1`[AD$`Caregiver_Aware_h1` == "N"] <- 0
AD$`Caregiver_Aware_h1`[AD$`Caregiver_Aware_h1` == "0"] <- 0
AD$`Caregiver_Aware_h2`[AD$`Caregiver_Aware_h2` == "Y"] <- 1
AD$`Caregiver_Aware_h2`[AD$`Caregiver_Aware_h2` == "1"] <- 1
AD$`Caregiver_Aware_h2`[AD$`Caregiver_Aware_h2` == "N"] <- 0
AD$`Caregiver_Aware_h2`[AD$`Caregiver_Aware_h2` == "0"] <- 0
AD$`Caregiver_Aware_h3`[AD$`Caregiver_Aware_h3` == "Y"] <- 1
AD$`Caregiver_Aware_h3`[AD$`Caregiver_Aware_h3` == "1"] <- 1
AD$`Caregiver_Aware_h3`[AD$`Caregiver_Aware_h3` == "N"] <- 0
AD$`Caregiver_Aware_h3`[AD$`Caregiver_Aware_h3` == "0"] <- 0
AD$`Caregiver_Aware_h4`[AD$`Caregiver_Aware_h4` == "Y"] <- 1
AD$`Caregiver_Aware_h4`[AD$`Caregiver_Aware_h4` == "1"] <- 1
AD$`Caregiver_Aware_h4`[AD$`Caregiver_Aware_h4` == "N"] <- 0
AD$`Caregiver_Aware_h4`[AD$`Caregiver_Aware_h4` == "0"] <- 0
AD$`Caregiver_Aware_h5`[AD$`Caregiver_Aware_h5` == "Y"] <- 1
AD$`Caregiver_Aware_h5`[AD$`Caregiver_Aware_h5` == "1"] <- 1
AD$`Caregiver_Aware_h5`[AD$`Caregiver_Aware_h5` == "N"] <- 0
AD$`Caregiver_Aware_h5`[AD$`Caregiver_Aware_h5` == "0"] <- 0

AD$`Caregiver_Aware_h6`[AD$`Caregiver_Aware_h6` == "Y"] <- 1
AD$`Caregiver_Aware_h6`[AD$`Caregiver_Aware_h6` == "1"] <- 1
AD$`Caregiver_Aware_h6`[AD$`Caregiver_Aware_h6` == "N"] <- 0
AD$`Caregiver_Aware_h6`[AD$`Caregiver_Aware_h6` == "0"] <- 0
AD$`Caregiver_Aware_h7`[AD$`Caregiver_Aware_h7` == "Y"] <- 1
AD$`Caregiver_Aware_h7`[AD$`Caregiver_Aware_h7` == "1"] <- 1
AD$`Caregiver_Aware_h7`[AD$`Caregiver_Aware_h7` == "N"] <- 0
AD$`Caregiver_Aware_h7`[AD$`Caregiver_Aware_h7` == "0"] <- 0
AD$`Caregiver_Aware_h8`[AD$`Caregiver_Aware_h8` == "Y"] <- 1
AD$`Caregiver_Aware_h8`[AD$`Caregiver_Aware_h8` == "1"] <- 1
AD$`Caregiver_Aware_h8`[AD$`Caregiver_Aware_h8` == "N"] <- 0
AD$`Caregiver_Aware_h8`[AD$`Caregiver_Aware_h8` == "0"] <- 0
AD$`Caregiver_Aware_h9`[AD$`Caregiver_Aware_h9` == "Y"] <- 1
AD$`Caregiver_Aware_h9`[AD$`Caregiver_Aware_h9` == "1"] <- 1
AD$`Caregiver_Aware_h9`[AD$`Caregiver_Aware_h9` == "N"] <- 0
AD$`Caregiver_Aware_h9`[AD$`Caregiver_Aware_h9` == "0"] <- 0
AD$`Caregiver_Aware_h10`[AD$`Caregiver_Aware_h10` == "Y"] <- 1
AD$`Caregiver_Aware_h10`[AD$`Caregiver_Aware_h10` == "1"] <- 1
AD$`Caregiver_Aware_h10`[AD$`Caregiver_Aware_h10` == "N"] <- 0
AD$`Caregiver_Aware_h10`[AD$`Caregiver_Aware_h10` == "0"] <- 0

#Reason for not vaccinated structure
AC<-AD |> 
  mutate(
    R_childnotborn1= case_when(
      `Reason_Not_FM1`=="childnotborn"~1),
    R_childnotborn2= case_when(
      `Reason_Not_FM2`=="childnotborn"~1),
    R_childnotborn3= case_when(
      `Reason_Not_FM3`=="childnotborn"~1),
    R_childnotborn4= case_when(
      `Reason_Not_FM4`=="childnotborn"~1),
    R_childnotborn5= case_when(
      `Reason_Not_FM5`=="childnotborn"~1),
    R_childnotborn6= case_when(
      `Reason_Not_FM6`=="childnotborn"~1),
    R_childnotborn7= case_when(
      `Reason_Not_FM7`=="childnotborn"~1),
    R_childnotborn8= case_when(
      `Reason_Not_FM8`=="childnotborn"~1),
    R_childnotborn9= case_when(
      `Reason_Not_FM9`=="childnotborn"~1),
    R_childnotborn10= case_when(
      `Reason_Not_FM10`=="childnotborn"~1),
    R_childabsent1= case_when(
      `Reason_Not_FM1`=="childabsent"~1),
    R_childabsent2= case_when(
      `Reason_Not_FM2`=="childabsent"~1),
    R_childabsent3= case_when(
      `Reason_Not_FM3`=="childabsent"~1),
    R_childabsent4= case_when(
      `Reason_Not_FM4`=="childabsent"~1),
    R_childabsent5= case_when(
      `Reason_Not_FM5`=="childabsent"~1),
    R_childabsent6= case_when(
      `Reason_Not_FM6`=="childabsent"~1),
    R_childabsent7= case_when(
      `Reason_Not_FM7`=="childabsent"~1),
    R_childabsent8= case_when(
      `Reason_Not_FM8`=="childabsent"~1),
    R_childabsent9= case_when(
      `Reason_Not_FM9`=="childabsent"~1),
    R_childabsent10= case_when(
      `Reason_Not_FM10`=="childabsent"~1),
    R_Non_Compliance1= case_when(
      `Reason_Not_FM1`=="noncompliance"~1),
    R_Non_Compliance2= case_when(
      `Reason_Not_FM2`=="noncompliance"~1),
    R_Non_Compliance3= case_when(
      `Reason_Not_FM3`=="noncompliance"~1),
    R_Non_Compliance4= case_when(
      `Reason_Not_FM4`=="noncompliance"~1),
    R_Non_Compliance5= case_when(
      `Reason_Not_FM5`=="noncompliance"~1),
    R_Non_Compliance6= case_when(
      `Reason_Not_FM6`=="noncompliance"~1),
    R_Non_Compliance7= case_when(
      `Reason_Not_FM7`=="noncompliance"~1),
    R_Non_Compliance8= case_when(
      `Reason_Not_FM8`=="noncompliance"~1),
    R_Non_Compliance9= case_when(
      `Reason_Not_FM9`=="noncompliance"~1),
    R_Non_Compliance10= case_when(
      `Reason_Not_FM10`=="noncompliance"~1),
    R_House_not_visited1= case_when(
      `Reason_Not_FM1`=="housenotvisited"~1),
    R_House_not_visited2= case_when(
      `Reason_Not_FM2`=="housenotvisited"~1),
    R_House_not_visited3= case_when(
      `Reason_Not_FM3`=="housenotvisited"~1),
    R_House_not_visited4= case_when(
      `Reason_Not_FM4`=="housenotvisited"~1),
    R_House_not_visited5= case_when(
      `Reason_Not_FM5`=="housenotvisited"~1),
    R_House_not_visited6= case_when(
      `Reason_Not_FM6`=="housenotvisited"~1),
    R_House_not_visited7= case_when(
      `Reason_Not_FM7`=="housenotvisited"~1),
    R_House_not_visited8= case_when(
      `Reason_Not_FM8`=="housenotvisited"~1),
    R_House_not_visited9= case_when(
      `Reason_Not_FM9`=="housenotvisited"~1),
    R_House_not_visited10= case_when(
      `Reason_Not_FM10`=="housenotvisited"~1),
    R_security1= case_when(
      `Reason_Not_FM1`=="security"~1),
    R_security2= case_when(
      `Reason_Not_FM2`=="security"~1),
    R_security3= case_when(
      `Reason_Not_FM3`=="security"~1),
    R_security4= case_when(
      `Reason_Not_FM4`=="security"~1),
    R_security5= case_when(
      `Reason_Not_FM5`=="security"~1),
    R_security6= case_when(
      `Reason_Not_FM6`=="security"~1),
    R_security7= case_when(
      `Reason_Not_FM7`=="security"~1),
    R_security8= case_when(
      `Reason_Not_FM8`=="security"~1),
    R_security9= case_when(
      `Reason_Not_FM9`=="security"~1),
    R_security10= case_when(
      `Reason_Not_FM10`=="security"~1))
 
AC <- AC |> 
  select(c(1:36,47:106))
#female sampled
AE <- AC |>
  # replace(is.na("."), "0") |> 
  mutate(across((starts_with("Sex_")),
                as.numeric)) |> 
  mutate(across((starts_with("FM_")),
                as.numeric)) |> 
  mutate(across((starts_with("Caregiver_")),
                as.numeric))
AF <- AE |>
  mutate(
    female_sampled = rowSums(across(
      c("Sex_Child1":"Sex_Child10"))),
    male_sampled = tot_hh_visited - female_sampled,
    total_vaccinated = rowSums(across(
      c("FM_Child1":"FM_Child10"))),
    missed_child = tot_hh_visited - total_vaccinated)
AG <- AF |> 
  rowwise() |>
  mutate(FV1 =ifelse((`Sex_Child1` + `FM_Child1`) >=2, 1, 0 ),
         FV2 =ifelse((`Sex_Child2` + `FM_Child2`) >=2, 1, 0 ),
         FV3 =ifelse((`Sex_Child3` + `FM_Child3`) >=2, 1, 0 ),
         FV4 =ifelse((`Sex_Child4` + `FM_Child4`) >=2, 1, 0 ),
         FV5 =ifelse((`Sex_Child5` + `FM_Child5`) >=2, 1, 0 ),
         FV6 =ifelse((`Sex_Child6` + `FM_Child6`) >=2, 1, 0 ),
         FV7 =ifelse((`Sex_Child7` + `FM_Child7`) >=2, 1, 0 ),
         FV8 =ifelse((`Sex_Child8` + `FM_Child8`) >=2, 1, 0 ),
         FV9 =ifelse((`Sex_Child9` + `FM_Child9`) >=2, 1, 0 ),
         FV10 =ifelse((`Sex_Child10` + `FM_Child10`) >=2, 1, 0 ),
         female_vaccinated = rowSums(across(
           c("FV1":"FV10"))),
         male_vaccinated = (total_vaccinated - female_vaccinated))
AN <- AG |> 
  mutate(across((starts_with("R_")),
                as.numeric))
AS <- AN |> 
  mutate(
    R_House_not_visited = sum(`R_House_not_visited1`,`R_House_not_visited2`,`R_House_not_visited3`,`R_House_not_visited4`,`R_House_not_visited5`,`R_House_not_visited6`,`R_House_not_visited7`,`R_House_not_visited8`,`R_House_not_visited9`,`R_House_not_visited10`, na.rm = TRUE), 
    R_childabsent = sum(`R_childabsent1`,`R_childabsent2`,`R_childabsent3`,`R_childabsent4`,`R_childabsent5`,`R_childabsent6`,`R_childabsent7`,`R_childabsent8`,`R_childabsent9`,`R_childabsent10`, na.rm = TRUE),
    R_Non_Compliance = sum(`R_Non_Compliance1`,`R_Non_Compliance2`,`R_Non_Compliance3`,`R_Non_Compliance4`,`R_Non_Compliance5`,`R_Non_Compliance6`,`R_Non_Compliance7`,`R_Non_Compliance8`,`R_Non_Compliance9`,`R_Non_Compliance10`, na.rm = TRUE),
    R_Child_is_a_visitor = sum(`R_childnotborn1`,`R_childnotborn2`,`R_childnotborn3`,`R_childnotborn4`,`R_childnotborn5`,`R_childnotborn6`,`R_childnotborn7`,`R_childnotborn8`,`R_childnotborn9`,`R_childnotborn10`, na.rm = TRUE),
    R_security = sum(`R_security1`,`R_security2`,`R_security3`,`R_security4`,`R_security5`,`R_security6`,`R_security7`,`R_security8`,`R_security9`,`R_security10`, na.rm = TRUE),
    Care_Giver_Informed_SIA = rowSums(across(
      c("Caregiver_Aware_h1":"Caregiver_Aware_h10")))) |>
  mutate(today = as_date(today)) |> 
  mutate(month = month(today, label = TRUE, abbr = TRUE))
AS <- AS |>
  mutate(roundNumber = case_when(
    str_detect(month, pattern = "janv") ~ "Rnd1",
    str_detect(month, pattern = "févr") ~ "Rnd2",
    str_detect(month, pattern = "mars") ~ "Rnd3",
    str_detect(month, pattern = "avr") ~ "Rnd4",
    str_detect(month, pattern = "mai") ~ "Rnd5",
    str_detect(month, pattern = "juin") ~ "Rnd6",
    str_detect(month, pattern = "juil") ~ "Rnd7",
    str_detect(month, pattern = "août") ~ "Rnd8",
    str_detect(month, pattern = "sept") ~ "Rnd9",
    str_detect(month, pattern = "oct") ~ "Rnd10",
    str_detect(month, pattern = "nov") ~ "Rnd11",
    str_detect(month, pattern = "déc") ~ "Rnd12"),
    response = "OBR_name")
"NIE-2024-nOPV2"
AQ<-AS |> 
  mutate(year = year(today),
         roundNumber = case_when(
           year == "2025" & month == "avr" ~ "Rnd1",
           year == "2025" & month == "janv" ~ "Rnd1",
           year == "2024" & month == "déc" ~ "Rnd5",
           year == "2024" & month == "nov" ~ "Rnd4",
           year == "2024" & month == "oct" ~ "Rnd3",
           year == "2024" & month == "sept" ~ "Rnd3",
           year == "2024" & month == "févr" ~ "Rnd1",
           year == "2024" & month == "mars" ~ "Rnd1",
           year == "2024" & month == "avr" ~ "Rnd2",
           year == "2024" & month == "mai" ~ "Rnd2",
           year == "2024" & month == "juin" ~ "Rnd2",
           year == "2023" & month == "janv" ~ "Rnd1",
           year == "2023" & month == "mai" ~ "Rnd1",
           year == "2023" & month == "juil" ~ "Rnd2",
           year == "2023" & month == "août" ~ "Rnd2",
           year == "2023" & month == "sept" ~ "Rnd3",
           year == "2023" & month == "oct" ~ "Rnd3",
           year == "2023" & month == "nov" ~ "Rnd4",
           year == "2023" & month == "déc" ~ "Rnd5",
           year == "2023" & month == "juin" ~ "Rnd2",
           year == "2023" & month == "mai" ~ "Rnd1",
           TRUE ~ roundNumber)) 
    
AQ <- AQ |> 
  mutate(vaccine.type = case_when(
    year == "2025" & month == "avr" ~ "nOPV2",
    year == "2025" & month == "janv" ~ "nOPV2",
    year == "2024" & month == "déc" ~ "nOPV2",
    year == "2024" & month == "nov" ~ "nOPV2",
    year == "2024" & month == "oct" ~ "nOPV2",
    year == "2024" & month == "sept" ~ "nOPV2",
    year == "2024" & month == "mars" ~ "nOPV2" ,
    year == "2024" & month == "févr" ~ "nOPV2",
    year == "2024" & month == "avr" ~ "nOPV2",
    year == "2023" & month == "janv" ~ "nOPV2",
    year == "2023" & month == "mai" ~ "fIPV+nOPV2",
    year == "2023" & month == "juil" ~ "fIPV+nOPV2",
    year == "2023" & month == "août" ~ "nOPV2",
    year == "2023" & month == "sept" ~ "fIPV+nOPV2",
    year == "2023" & month == "oct" ~ "nOPV2",
    year == "2023" & month == "nov" ~ "nOPV2",
    year == "2023" & month == "déc" ~ "nOPV2",
    year == "2022" & month == "juil" ~ "nOPV2" ,
    year == "2021" & month == "mars" ~ "nOPV2" ,
    year == "2021" & month == "avr" ~ "nOPV2",
    year == "2021" & month == "mai" ~ "nOPV2",
    year == "2021" & month == "juin" ~ "nOPV2",
    year == "2021" & month == "juil" ~ "nOPV2",
    year == "2021" & month == "août" ~ "nOPV2",
    year == "2021" & month == "sept" ~ "nOPV2",
    year == "2021" & month == "oct" ~ "nOPV2",
    year == "2020" & month == "janv" ~ "nOPV2",
    year == "2020" & month == "mars" ~ "nOPV2",
    year == "2021" & month == "nov" ~ "bOPV",
    year == "2021" & month == "déc" ~ "bOPV",
    year == "2022" & month == "janv" ~ "bOPV",
    year == "2022" & month == "févr" ~ "bOPV",
    year == "2022" & month == "mars" ~ "bOPV",
    year == "2022" & month == "avr" ~ "bOPV",
    year == "2022" & month == "mai" ~ "bOPV",
    year == "2022" & month == "août" ~ "bOPV",
    year == "2022" & month == "sept" ~ "bOPV",
    year == "2022" & month == "oct" ~ "bOPV",
    year == "2022" & month == "nov" ~ "bOPV",
    year == "2022" & month == "déc" ~ "bOPV",
    TRUE~""))
  
  AX <- AQ |> 
    mutate(response = case_when(
      year == "2025" & month == "avr" ~ "NIE-2025-04-01_nOPV_NIDs",
      year == "2025" & month == "janv" ~ "OBR1",
      year == "2024" & month == "déc" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "nov" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "oct" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "sept" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "févr" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "mars" ~ "NIE-2024-nOPV2",
      year == "2024" & month == "avr" ~ "NIE-2024-nOPV2",
      year	=="2020" & month == "janv" ~	"NGA-20DS-01-2020",
      year	=="2020" & month == "févr" ~	"NGA-20DS-01-2020",
      year	=="2020" & month == "mars" ~	"NGA-20DS-01-2020",
      year	=="2020" & month == "déc" ~	"NGA-5DS-10-2020",
      year	=="2021" & month == "janv" ~	"NGA-5DS-10-2020",
      year	=="2021" & month == "mars" ~	"NGA-2021-013-1",
      year	=="2021" & month == "avr" ~	"NGA-2021-013-1",
      year	=="2021" & month == "avr" ~	"NGA-2021-011-1",
      year	=="2021" & month == "mai" ~	"NGA-2021-011-1",
      year	=="2021" & month == "juin" ~	"NGA-2021-016-1",
      year	=="2021" & month == "juil" ~	"NGA-2021-014-1",
      year	=="2021" & month == "juil" ~	"NGA-2021-016-1",
      year	=="2021" & month == "août" ~	"NGA-2021-014-1",
      year	=="2021" & month == "août" ~	"NGA-2021-019",
      year	=="2021" & month == "sept" ~	"NGA-2021-020-2",
      year	=="2021" & month == "sept" ~	"NGA-2021-020-1",
      year	=="2021" & month == "sept" ~	"NGA-2021-020-4",
      year	=="2021" & month == "oct" ~	"NGA-2021-020-3",
      year	=="2021" & month == "oct" ~	"NGA-2021-020-1",
      year	=="2021" & month == "oct" ~	"NGA-2021-020-4",
      year	=="2021" & month == "oct" ~	"NGA-2021-020-2",
      year	=="2021" & month == "nov" ~	"NGA-2021-020-3",
      year	=="2022" & month == "juil" ~	"Kwara Response",
      year	=="2022" & month == "août" ~	"Kwara Response",
      year	=="2023" & month == "mai" ~	"NIE-2023-04-02_nOPV",
      year	=="2023" & month == "juin" ~	"NIE-2023-04-02_nOPV",
      year	=="2023" & month == "juil" ~	"NIE-2023-07-03_nOPV",
      year	=="2023" & month == "oct" ~	"NIE-2023-07-03_nOPV",
      year	=="2023" & month == "nov" ~	"NIE-2023-07-03_nOPV",
      year	=="2023" & month == "déc" ~	"NIE-2023-07-03_nOPV",
      year	=="2024" & month == "mars" ~	"NIE-2024-nOPV2",
      year	=="2024" & month == "avr" ~	"NIE-2024-nOPV2",
      year	=="2024" & month == "août" ~	"NIE-2024-nOPV2",
      year	=="2024" & month == "oct" ~	"NIE-2024-nOPV2",
      year == "2023" & month == "déc" ~ "NIE-2023-07-03_nOPV2",
      TRUE~response))
  AX <- AX |> 
    select(country,Region, District, Cluster, today, response, roundNumber, vaccine.type, male_sampled,female_sampled,total_sampled = tot_hh_visited, male_vaccinated, female_vaccinated, total_vaccinated, missed_child, R_Non_Compliance, R_House_not_visited,R_childabsent, R_Child_is_a_visitor, R_security, Care_Giver_Informed_SIA )
    
  
F1 <- AX |>
  mutate(Cluster = as.numeric(Cluster)) |> 
  mutate(year = year(today)) |> 
  filter(year > 2019) |> 
  group_by(country, Region, District, response, vaccine.type , roundNumber) |>
  mutate(today = as_date(today)) |> 
  arrange(today) |> 
  mutate(date.diff = c(1, diff(today))) |> 
  mutate(period = cumsum(date.diff != 1)) |> 
  ungroup() |> 
  group_by(country, Region, District, response, vaccine.type, roundNumber) |> 
  summarise(start_date = min(today),
            end_date = max(today),
            year = year(start_date),
            cluster = sum(Cluster),
            male_sampled = sum(male_sampled),
            female_sampled = sum(female_sampled),
            total_sampled = sum(total_sampled),
            male_vaccinated = sum(male_vaccinated),
            female_vaccinated = sum(female_vaccinated),
            total_vaccinated = sum(total_vaccinated),
            missed_child = sum(missed_child),
            r_Non_Compliance = sum(R_Non_Compliance),
            r_House_not_visited = sum(R_House_not_visited),
            r_childabsent = sum(R_childabsent),
            r_Child_is_a_visitor = sum(R_Child_is_a_visitor),
            r_security = sum(R_security),
            Care_Giver_Informed_SIA = sum(Care_Giver_Informed_SIA),
            percent_care_Giver_Informed_SIA = (Care_Giver_Informed_SIA/total_sampled))
  
# gsub('\\b0+','',format(as.Date(start_date),'%d/%m/%Y'))
F2 <- F1 |> 
    filter(start_date > 2019-10-1) |> 
  mutate(
    percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA, 2),
    percent_care_Giver_Informed_SIA = percent_care_Giver_Informed_SIA*100,
    total_missed = ifelse(total_sampled<60, ((60-total_sampled) + missed_child), missed_child)) |> 
  filter(cluster >= 3) |> 
  mutate(Status = case_when(
    total_missed<=3~"Pass",total_missed>3~"Fail"),
    roundNumber = case_when(
      roundNumber == "RND 1" ~ "Rnd1",
      roundNumber == "RND 2" ~ "Rnd2",
      TRUE ~ roundNumber))|> 
  mutate(Performance = case_when(
    total_missed<4~"high",
    total_missed>=4 & total_missed<9~"moderate",
    total_missed>=9 & total_missed<20~"poor",
    total_missed>=20~"very poor"))

F3<-F2 |>
  mutate(tot_r = (r_Non_Compliance + r_House_not_visited + r_childabsent + r_security + r_Child_is_a_visitor) ,
         other_r = ifelse((total_missed - tot_r)< 0, 0, (total_missed - tot_r)))  |> 
  select(country , province = Region, district = District, response, vaccine.type, roundNumber , numbercluster = cluster, start_date, end_date, year, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status = Status, performance = Performance, r_Non_Compliance, r_House_not_visited, r_childabsent, r_childnotborn = r_Child_is_a_visitor, r_security, other_r, percent_care_Giver_Informed_SIA) |>
  arrange(start_date)

# Read and transform the preparedness data
date <- read_excel("C:/Users/TOURE/Documents/REPOSITORIES/LQAS_raw_data/harmonized_date/data (5).xlsx")

# Transform the "Round Number" column
date <- date |>
  filter(Country=="NIGERIA") |> 
  mutate(`Round Number` = case_when(
    `Round Number` == "Round 0" ~ "Rnd0",
    `Round Number` == "Round 1" ~ "Rnd1",
    `Round Number` == "Round 2" ~ "Rnd2",
    `Round Number` == "Round 3" ~ "Rnd3",
    `Round Number` == "Round 4" ~ "Rnd4",
    `Round Number` == "Round 5" ~ "Rnd5",
    `Round Number` == "Round 6" ~ "Rnd6",
    TRUE ~ `Round Number`  # Leave other values unchanged
  ))

# Prepare the lookup table
data <- date |> 
  rename(
    response = `OBR Name`,        # Rename "OBR Name" to "response"
    vaccine.type = Vaccines,      # Rename "Vaccines" to "vaccine.type"
    roundNumber = `Round Number`  # Rename "Round Number" to "roundNumber"
  ) |> 
  # mutate(response = case_when(
  #   response =="NIE-2024-nOPV2"~ "NIE-2024_nOPV2",
  #   TRUE~ response)) |> 
  mutate(
    `Round Start Date` = as_date(`Round Start Date`),  # Convert to date
    start_date = `Round Start Date` + 4,               # Add 4 days to start date
    end_date = start_date                          # End date is 1 day after start_date
  )

# Convert to tibble and select relevant columns
prep_data <- data |> 
  select(response, vaccine.type, roundNumber, start_date, end_date) |> 
  as_tibble()

# Convert start_date and end_date to date format (if needed)
lookup_table <- prep_data |> 
  mutate(
    start_date = as_date(start_date),
    end_date = as_date(end_date)
  )

# Join the lookup table with the original data `G`
NIE_LQAS <- F3 |> 
  left_join(lookup_table, by = c("response", "vaccine.type", "roundNumber")) |> 
  mutate(
    start_date = coalesce(start_date.y, as_date(start_date.x)), 
    end_date = coalesce(end_date.y, as_date(end_date.x))
  ) |> 
  select(country , province, district, response, vaccine.type, roundNumber , numbercluster, start_date, end_date, year, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status, performance, r_Non_Compliance, r_House_not_visited, r_childabsent, r_childnotborn, r_security, other_r, percent_care_Giver_Informed_SIA, -start_date.x, -start_date.y, -end_date.x, -end_date.y,) |> 
  filter(!is.na(district))  # Filter out rows where district is missing

F4<-NIE_LQAS |> 
  mutate(
    prct_Non_Compliance = ifelse(total_missed ==0, 0, r_Non_Compliance/total_missed),
    prct_Non_Compliance = round(prct_Non_Compliance,2),
    prct_Non_Compliance = prct_Non_Compliance*100,
    prct_House_not_visited = ifelse(total_missed ==0, 0, (r_House_not_visited/total_missed)),
    prct_House_not_visited = round(prct_House_not_visited,2),
    prct_House_not_visited = prct_House_not_visited*100,
    prct_childabsent = ifelse(total_missed ==0, 0, (r_childabsent/total_missed)),
    prct_childabsent = round(prct_childabsent,2),
    prct_childabsent = prct_childabsent*100,
    prct_childnotborn = ifelse(total_missed ==0, 0, (r_childnotborn/total_missed)),
    prct_childnotborn = round(prct_childnotborn,2),
    prct_childnotborn = prct_childnotborn*100,
    prct_security = ifelse(total_missed ==0, 0, (r_security/total_missed)),
    prct_security = round(prct_security,2),
    prct_security = prct_security*100,
    prct_other_r = ifelse(total_missed ==0, 0, (other_r/total_missed)),
    prct_other_r = round(prct_other_r,2),
    prct_other_r = prct_other_r*100
  )

  
write_csv(F4,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_level1/272.csv")         

    
 



 






 

