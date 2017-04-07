 ##########################################
# DESCRIPTION: This file defines a function which generates medication 
# features for a given cohort from a raw .ONCDRS File
# ##########################################
# Creator: Clara Marquardt
# Date: 5th January 
# ##########################################
# Language: R
# ##########################################
# TO-DO-LIST 


################################################################################
########################  CREATE DIAGNOSIS FEATURES ##########################
################################################################################

 med_oncdrs_feature_gen <- function() {

  
  ##############################################################################
  ### load the dia_feature code
  source(med_feature_code)
  med_oncdrs <- med_feature_gen(med_oncdrs_file_mod, leak_oncdrs_med_day)

  ##############################################################################
  ### rename the variables
  var_rename <- setdiff(names(med_oncdrs), cohort_key_var_merge)
  setnames(med_oncdrs, var_rename,
    gsub("^med", "med.dfci", var_rename))
  var_rename <- setdiff(names(med_oncdrs), cohort_key_var_merge)
  setnames(med_oncdrs,  var_rename , paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames(med_oncdrs, gsub("(.*)(med.dfci_time)", "med_oncdrs_time", names(med_oncdrs)))


  ### return
  return(med_oncdrs)

}


################################################################################
##################################  END  #######################################
################################################################################

