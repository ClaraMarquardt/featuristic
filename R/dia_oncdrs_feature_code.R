#----------------------------------------------------------------------------#

#' @title Generate DFCI diagnosis-related features (dia_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param 
#' @return
#' @examples


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


#----------------------------------------------------------------------------#
