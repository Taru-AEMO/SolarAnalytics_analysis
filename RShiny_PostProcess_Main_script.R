installed.packages("xlsx")
require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")
library("gtable")
library("grid")
library("gridExtra")


################ set directory for all inputs / outputs
directory <- "~/DER_disturbance_analysis/E2"

#
setwd(paste0("",directory,""))
################# set inputs
## save underlying data from r shiny
underlying_data_file <- "E1_6_short_ud.csv"


## toggle upscaled data and save aggregated results from r shiny
upscaled_aggregated_file <- "E1_6_short_upscaled_ar.csv"

####### event data, only one t0 can be input per analysis. multiple events can be input.
###### don't deviate from formats
event_date <- "20180415"

### t0 is the interval immediately prior to event
pre_event_interval <- "07:52:25"

event_time <- c("07:52:55","07:53:55")



######
dir.create(file.path(paste0("",directory,"/PP_output_",event_date,"")))
############# monitored data analysis - tables

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")
source("RShiny_PostProcess_RawTables.R")

############# upscaled data analysis - tables

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")
source("RShiny_PostProcess_UpscaledTables.R")

############# data analysis - plots
#######

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")
source("RShiny_PostProcess_Plots.R")


#######################################################################

