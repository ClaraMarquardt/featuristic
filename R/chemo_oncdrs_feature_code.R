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

 chemo_oncdrs_feature_gen <- function() {

  ##############################################################################
  ### load the dia_feature code
  source(med_feature_code)
  chemo_oncdrs <- med_feature_gen(chemo_oncdrs_file_mod, leak_oncdrs_chemo_day)

  ##############################################################################
  ### rename the variables
  var_rename <- setdiff(names(chemo_oncdrs), cohort_key_var_merge)
  setnames(chemo_oncdrs, var_rename,
    gsub("^med", "chemo.dfci", var_rename))
  var_rename <- setdiff(names(chemo_oncdrs), cohort_key_var_merge)
  setnames(chemo_oncdrs,  var_rename , paste0(gsub("_", "_dfci.chemo.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
  	var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
  	var_rename)))
  setnames(chemo_oncdrs, gsub("med\\.", "chemo\\.", names(chemo_oncdrs)))
  setnames(chemo_oncdrs, gsub("(.*)(chemo.dfci_time)", "chemo_oncdrs_time", names(chemo_oncdrs)))

  ### return
  return(chemo_oncdrs)

}


################################################################################
##################################  END  #######################################
################################################################################
