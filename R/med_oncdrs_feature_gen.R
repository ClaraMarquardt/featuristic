#----------------------------------------------------------------------------#

#' @title Generate DFCI medication-related features (med_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

 med_oncdrs_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {

  print("launching med_oncdrs_feature_gen")
  
  ##############################################################################
  ### load the dia_feature code
  med_oncdrs <- med_feature_gen(cohort, cohort_key_var_merge, cohort_key_var, 
    med_oncdrs_file_mod, leak_oncdrs_med_day)

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


#----------------------------------------------------------------------------#

