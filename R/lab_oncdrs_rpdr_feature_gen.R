#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) lab-related features (lab_oncdrs & lab data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples


lab_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {
    
  print("launching lab_oncdrs_rpdr_feature_gen")
  
  ##############################################################################
  ### load the dia_feature code
  lab_oncdrs_rpdr <- lab_feature_gen(cohort, cohort_key_var_merge, cohort_key_var, 
  	lab_oncdrs_file_mod, leak_lab_day, combine=TRUE, lab_file_mod)

  ### return
  return(lab_oncdrs_rpdr)


}

#----------------------------------------------------------------------------#

