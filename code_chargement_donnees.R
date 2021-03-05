## ----librairies---------------------------------------------------------------------------------------------------

require(tidyverse) # Manipulation de donnees
require(readxl) # Lecture de fichiers excel
require(lubridate) # Manipulation de dates

## ----gros fichier---------------------------------------------------------------------------------------------------------

load("d1.RData")
load("d2.RData")
load("d3.RData")

data_train_DF=rbind(d1,d2,d3)
rm(d1,d2,d3)
# ajout code site et date
data_train_DF<-data_train_DF %>% 
  unite(col = site, c("lon","lat"),remove = FALSE) %>% 
  mutate(site=str_replace(site,pattern = "-",replacement = "Lon")) %>% 
  mutate(site=str_replace(site,pattern = "_",replacement = "lat")) %>% 
  mutate(date=lubridate::make_datetime(year=year, month=month))