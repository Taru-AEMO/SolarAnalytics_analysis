require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")

#Set Working Directory - have pulled this out as it will change with each data set.
folder <- "20170215"

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics/")

#Input the file name of each of the data sets
Actual_Data_file <- "data_4551_2017_02_15.csv"

Site_details_file <- "site_details_4551.csv"

Circuit_details_file <- "circuit_details_4551.csv"


EventTime <- "2017-02-15 10:34:25"


#Run through each Process
#1. Read, Format and Join Data Files
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics/")

source("Join_script.R")

#2.Clean PV Data Files
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics/")
source("Clean_script_draft.R")

#3. Process Cleaned Files to Evaluate Number of Disconnections

#Define Category thresholds 
Cat1_PL_perc <- 0.04
#Categories 2-6: Curtailment
Cat2_PL_perc <- 0.1
Cat3_PL_perc <- 0.25
Cat4_PL_perc <- 0.5
Cat5_PL_perc <- 0.75
Cat6_PL_perc <- 0.1
#As a value in kW
#Category 7: Disconnect
Cat7_Disconnect_kW=0.1
Cat8_Disconnect_kW=0

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics/")
source("Process_script.R")

