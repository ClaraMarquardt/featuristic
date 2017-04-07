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

 med_chemo_oncdrs_rpdr_feature_gen <- function() {

  
  ##############################################################################
  ### load the dia_feature code
  source(med_feature_code)
  med_chemo_oncdrs_rpdr <- med_feature_gen(med_oncdrs_file_mod, leak_med_day, 
  	combine=TRUE, med_file_mod, chemo_oncdrs_file_mod)

  ### return
  return(med_chemo_oncdrs_rpdr)

}


################################################################################
##################################  END  #######################################
################################################################################

