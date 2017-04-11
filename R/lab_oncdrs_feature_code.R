#----------------------------------------------------------------------------#

#' @title Generate DFCI lab-related features (lab_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @return
#' @examples


lab_oncdrs_feature_gen <- function() {


  ##############################################################################
  ### load the dia_feature code
  source(lab_feature_code)
  lab_oncdrs <- lab_feature_gen(lab_oncdrs_file_mod, leak_oncdrs_lab_day)

  ##############################################################################
  ### rename the variables
  var_rename <- setdiff(names(lab_oncdrs), cohort_key_var_merge)
  setnames(lab_oncdrs, var_rename,
    gsub("^lab", "lab.dfci", var_rename))
  var_rename <- setdiff(names(lab_oncdrs), cohort_key_var_merge)
  setnames(lab_oncdrs,  var_rename , paste0(gsub("_", "_dfci.", gsub("(.*)(\\.\\.)(.*)", "\\1", 
    var_rename)), "..", gsub("(.*)(\\.\\.)(.*)", "\\3", 
    var_rename)))
  setnames(lab_oncdrs, gsub("(.*)(lab.dfci_time)", "lab_oncdrs_time", names(lab_oncdrs)))

  ### return
  return(lab_oncdrs)


}

#----------------------------------------------------------------------------#

