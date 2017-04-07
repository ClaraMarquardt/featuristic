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

 dia_oncdrs_rpdr_feature_gen <- function() {

  ##############################################################################
  ### load the dia_feature code
  source(dia_feature_code)
  dia_oncdrs_rpdr <- dia_feature_gen(dia_oncdrs_file_mod, 
  	leak_dia_day, combine=TRUE, dia_file_mod)

  ### return
  return(dia_oncdrs_rpdr)

}


################################################################################
##################################  END  #######################################
################################################################################



