# ##########################################
# DESCRIPTION: This file loads the helper files used in the feature generation code
# ##########################################
# Creator: Clara Marquardt
# Date: 5th January 
# ##########################################
# Language: R
# ##########################################
# TO-DO-LIST 


################################################################################
#################  (A) HELPER SPECIFICATION & LOADING ##########################
################################################################################
load_helpers <- function(required_helpers) {

################################################################################
################################################################################
### (a) zip_codes - classify zip code by state and city 
### source: library(zipcode) (R package) - data(zipcode)
if ("zip_class" %in% required_helpers) {

  zip_class <<- fread("/data/zolab/methods_new/dem/zip_class.csv")
  setnames(zip_class, c("zip", "state", "state_name"), c("zip_code", "state_abbr", 
    "state"))

  # DC -> District of Columbia
  zip_class[state_abbr=="DC", state:="District of Columbia"]

  # subset to relevant columns
  zip_class[, setdiff(names(zip_class), c("state", "zip_code", "city")):=NULL]
}

################################################################################
################################################################################
### (b) flu_data
### source: library(cdcfluview) (R package) - get_state_data(years=c(1997:2015)) - 
### no data pre 1997
if ("flu_data_cdc" %in% required_helpers) {

  flu_data_cdc <<- fread("/data/zolab/methods_new/dem/flu_data.csv")

  # basic formatting
  setnames(flu_data_cdc, c("ACTIVITY.LEVEL", "WEEKEND", "STATENAME"), 
    c("activity_level", "flu_date", "state"))
  flu_data_cdc[, activity_level:=as.numeric(gsub("Level", "", activity_level))]

  # omit records in which no activity is recorded due to insufficient data
  flu_data_cdc <<- flu_data_cdc[!activity_level=="0"]

  # subset to relevant columns
  flu_data_cdc[, setdiff(names(flu_data_cdc), c("state", "activity_level", 
    "flu_date")):=NULL] 


}


################################################################################
################################################################################
### (b) gagne_cat
if ("gagne_cat" %in% required_helpers) {

  gagne_cat <<- fread("/data/zolab/methods_new/dia/gagne_codes/Gagne_codes.csv")


}
################################################################################
################################################################################
}
################################################################################
##################################  END  #######################################
################################################################################
