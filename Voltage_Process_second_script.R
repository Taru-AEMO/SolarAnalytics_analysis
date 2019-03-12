
### run this after voltage prep script. takes dataframes as inputs
### you don't have to run voltage process first before this one

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse"))

file.name <- list.files()


for (i in file.name){}



######################################### MULTUPLE SYSTEMS, TREND ANALYSIS
#### this section... 

temp.aggregate <- temp.volt_t0
# what is looks compared to the normalised value (maybe the average of the last 10mins) maybe 3 categorised
# check this: 
# <180 , >260, must disconnect after 1 second. >265 must disconnect

