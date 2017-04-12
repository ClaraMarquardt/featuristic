#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) diagnosis-related features (dia_oncdrs & dia data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param cohort
#' @param cohort_key_var_merge 
#' @return
#' @examples

 dia_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching dia_oncdrs_rpdr_feature_gen")
  
  ##############################################################################
  ### load the dia_feature code
  dia_oncdrs_rpdr <- dia_feature_gen(cohort, cohort_key_var_merge, cohort_key_var, 
  	dia_oncdrs_file_mod, leak_dia_day, combine=TRUE, dia_file_mod)

  ### return
  return(dia_oncdrs_rpdr)

}


#----------------------------------------------------------------------------#



