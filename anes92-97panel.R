rm(list = ls())

library(tidyverse)
library(haven)
library(naniar)

setwd("~/Box Sync/dissertation thoughts/American Left/Data")

###need to go back and check directions on equality variables 
### fewer problems: lower numbers means more value on equality
### equal chance: lower numbers means more value on equality
### equal opportunity: lower numbers means more value on equality

###here I am using data from the anes 1992-1997 panel study 
###data available at:https://electionstudies.org/data-center/1992-1997-merged-file/
panel <- read_dta ("anes_92_97_panel.dta") %>% 
  select(V923316 , V940228  , V960283 , ### jackson fts
         V923306,  V940223 , V961019 , ### clinton fts
         V923309 , V940227,    V960275 , ###gore fts
         V926029 , V940916 , V961231, ## big problem equal chance 
         V926028 , V940919 , V961234 , ### fewer problems if treated more equal
         V926024, V940914 , V961229##society should make sure equal opportunity 
         ) %>%#
  zap_formats() %>% 
  rename(jacksonft_92 = V923316  , jacksonft_94 = V940228, jacksonft_96 = V960283,
         clintonft_92 = V923306, clintonft_94 = V940223, clintonft_96 = V961019,
         goreft_92 = V923309, goreft_94 = V940227, goreft_96 = V960275,
         eqchance_92 = V926029, eqchance_94 = V940916 , eqchance_96 = V961231,
         eqfewer_92 = V926028  , eqfewer_94 = V940919 , eqfewer_96 = V961234 ,
         eqopp_92 = V926024 , eqopp_94 = V940914 , eqopp_96 = V961229) %>% 
  replace_with_na(replace = list(jacksonft_92=c(997,998,999), jacksonft_94=c(997,998,999), jacksonft_96=c(997,998,999),
                                 clintonft_92=c(997,998,999), clintonft_94=c(997,998,999), clintonft_96=c(996,997,998,999),
                                 goreft_92=c(997,998,999), goreft_94=c(997,998,999), goreft_96=c(997,998,999),
                                 eqchance_92=c(0,8,9), eqchance_94=c(8,9), eqchance_96=c(0,8,9),
                                 eqfewer_92=c(0,8,9), eqfewer_94=c(8,9), eqfewer_96=c(0,8,9),
                                 eqopp_92=c(0,8,9), eqopp_94=c(8,9), eqopp_96=c(0,8,9))) %>% 
  mutate_at(vars(jacksonft_92:goreft_96) , function(x){x/100}) %>% 
  mutate_at(vars(eqchance_92:eqopp_96), function(x){case_when(x==1 ~ 1, 
                                                                x==2 ~ .75,
                                                                x==3 ~ .5,
                                                                x==4 ~ .25,
                                                                x==5 ~ 0)})%>% 
  mutate(
    eqtot_92 = (eqchance_92 +eqfewer_92 +eqopp_92)/3 ,
    eqtot_94 = (eqchance_94 +eqfewer_94 +eqopp_94)/3,
    eqtot_96 = (eqchance_96 +eqfewer_96 +eqopp_96)/3 ,
    id = row_number()) 



longpanel <- panel %>% 
  pivot_longer(
    cols = !id , 
    names_to = c("variable" , "year"),
    names_sep = "_",
    values_to = "score")%>% 
  pivot_wider(names_from = variable ,
              values_from = score)


summary(panel$eq_tot_92)
reg1 <- glm(data = panel , eq_tot_96 ~ jacksonft_92 + jacksonft_94 + jacksonft_96 +  eq_tot_92 + eq_tot_94 )

summary(reg1)




