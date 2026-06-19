
# ============================================================
# Title:     GGTactical to APSIM
# Author:    Priyanath Jayasinghe
# Email:     priyanath.jayasinghe@csiro.au
# Created:   2026-04-10
#
# Description:
#   This script run GGTactical and creat .txt files to be read in read_apsiminput.R function and create excel file ready to feed into APSIMX for livestock simulation 
#Inputs - 
# Notes:
#   - Designed for multi-pasture systems (plant1, plant2, ...)
# ============================================================




library(openxlsx)
library(apsimx)
install.packages("httr")
install.packages("jsonlite")

library(jsonlite)
library(httr)

# install.packages('openxlsx')

# run GrassGro (GGTactical)

setwd("C:/alldata/automation_of_GG_to_Apsim/GGTactical/Windows/x86")  # set the ggtactical files (.exe. and all farm systems, report library files) directory

command_txt <- 'GGTactical -f "farmsystems.ggfl|Merino @ Southern Mallee_ClayLoam" -w "77028.txt" -s "Soils.soils|Clay Loam (Culgoa No730)" -b 1904-01-01 -e 2005-12-31 -r "custom_library.gglb|APSIMSheepInput" -o "ggoutput_SouthernMallee.txt"' ## -s "Soils.soils|Clay Loam (Culgoa No730)"   -p "PastureLibrary.gglb|Panicum coloratum - Bambatsi_LW_HighNV" -m "CN30_PanicGrass_NV060623.prm" 
command_txt <- 'GGTactical -f "farmsystems.ggfl|Merino @ Kellerberrin_LoamyEarth" -w "Kellerberrin.txt" -b 1904-01-01 -e 2005-12-31 -r "custom_library.gglb|APSIMSheepInput" -o "ggoutput_SouthernMallee.txt"' ## -s "Soils.soils|Clay Loam (Culgoa No730)"   -p "PastureLibrary.gglb|Panicum coloratum - Bambatsi_LW_HighNV" -m "CN30_PanicGrass_NV060623.prm" 
command_txt <- 'GGTactical -f "farmsystems.ggfl|Steers @ Chinchilla" -l -27.140,149.109 -k "MetData|SILO143103570.met" -g "SoilData|1505650-268670.xml" -b 1904-01-01 -e 2005-12-31 -r "custom_library.gglb|APSIMBeefInput" -o "ggoutput_Chinchilla.txt"' ## -s "Soils.soils|Clay Loam (Culgoa No730)"   -p "PastureLibrary.gglb|Panicum coloratum - Bambatsi_LW_HighNV" -m "CN30_PanicGrass_NV060623.prm" 

##command_txt <- 'GGTactical -f "farmsystems.ggfl|Merino @ Southern Mallee_ClayLoam" -l -31.632,117.719 -k "MetData|SILO143103570.met" -g "SoilData|1505650-268670.xml" -b 1904-01-01 -e 2005-12-31 -r "custom_library.gglb|APSIMSheepInput" -o "ggoutput_SouthernMallee.txt"' ## -s "Soils.soils|Clay Loam (Culgoa No730)"   -p "PastureLibrary.gglb|Panicum coloratum - Bambatsi_LW_HighNV" -m "CN30_PanicGrass_NV060623.prm" 

shell(command_txt, wait = TRUE) #-l -35.715,143.107

# import GrassGro output

source("C:/alldata/automation_of_GG_to_Apsim/GGTactical/Windows/x86/read_apsiminput.R") ## read  the space delimited text file generated from GGTactical.
dtout <- read_apsiminput()


write.xlsx(dtout, file = "AnimalInput_Ch.xlsx", sheetName = "AnimalInput") # this needs to be the same as the defined APSIM file and sheet name in the GrassGro manager script.

# run APSIM
apsim_exe <- "C:/Program Files/APSIM2024.2.7382.0/bin/Models.exe"
apsim_file <- "C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee/SLURP_Livestock.apsimx"

cmd <- paste0('"', apsim_exe, '" "', apsim_file, '"')

system(cmd, wait = TRUE)



setwd("C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee")

apsimcommand_txt <- '"C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee/SLURP_Livestock.apsimx"'
shell(apsimcommand_txt, wait = T)



#Access the APSIM db files

library(RSQLite)
library(tidyverse)

db_file <- "C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee/SLURP_Livestock.db"
db_file_CR<-"C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee/Southern_Mallee_CropRotation.db"

mydb <- dbConnect(SQLite(), db_file)
mydb_CR<- dbConnect(SQLite (), db_file_CR)
## retrieve simulated daily output -----------------------------------------------------------------
##
tbl_daily_pasture <- tbl(mydb, "PastureReport") |> collect()
tbl_daily_livestock <- tbl (mydb, "LivestockReport") |> collect()
tbl_annual_livestock <- tbl (mydb, "AnnualReport") |> collect()
tbl_annual_CR<- tbl(mydb_CR, "AnnualReport") |> collect()
tbl_daily_CR<- tbl (mydb_CR, "Report") |> collect()

p1<- ggplot(tbl_daily_pasture, aes(x = Date, y = TotalSoilN2Oemission)) +
  geom_col() 

p1

p2<- ggplot(tbl_daily_livestock, aes(x = tbl_daily_pasture$Soil.SoilWater.Eo, y = tbl_daily_livestock$GrassGro.Script.PET)) + 
  geom_point(shape = 16, alpha = 0.4 ) + xlab("Evapotranspiration SLURP") + ylab("Evapotranspiration GrassGro")
p2

p3<- ggplot(tbl_daily_livestock, aes(x = Date, y = tbl_daily_livestock$GrassGro.Script.NYoung)) + 
  geom_point(shape = 16, alpha = 0.4 )
p3

p4<- ggplot(tbl_annual_CR, aes(x=tbl_annual_CR$Date, y=tbl_annual_CR$ChangeInSoilCarbon, color = tbl_annual_CR$SimulationID)) + geom_bar()
p4

p5<- plot (x=tbl_annual_CR$Date, y=tbl_annual_CR$ChangeInSoilCarbon, color = tbl_annual_CR$SimulationID)
p4


dbDisconnect(mydb)

