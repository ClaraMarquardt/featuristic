#----------------------------------------------------------------------------#

#' @title Generate DFCI chemotherapy-related features (chemo_oncdrs data).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param 
#' @return
#' @examples


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


#----------------------------------------------------------------------------#
