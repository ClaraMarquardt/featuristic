 ##########################################
# DESCRIPTION: This file defines a function which generates diagnosis 
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

 dia_oncdrs_feature_gen <- function() {

  ##############################################################################
  ### load the dia_feature code
  source(dia_feature_code)
  dia_oncdrs <- dia_feature_gen(dia_oncdrs_file_mod, leak_oncdrs_dia_day)

  ##############################################################################
  ### rename the variables
  var_rename <- setdiff(names(dia_oncdrs), cohort_key_var_merge)
  setnames(dia_oncdrs, var_rename,
    gsub("^dia", "dia.dfci", var_rename))
  var_rename <- setdiff(names(dia_oncdrs), cohort_key_var_merge)
  setnames(dia_oncdrs,  var_rename , paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames(dia_oncdrs, gsub("(.*)(dia.dfci_time)", "dia_oncdrs_time", names(dia_oncdrs)))

  ### return
  return(dia_oncdrs)

}


################################################################################
##################################  END  #######################################
################################################################################

