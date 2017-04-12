#----------------------------------------------------------------------------#

#' @title Generate individual feature sets.
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param feature_set_name
#' @return
#' @examples

indiv_feature_gen <- function(feature_set_name, ...) {

	function_list <- list(
		dem=dem_feature_gen, 
		enc=enc_feature_gen, 
		dia=dia_feature_gen, 
		prc=prc_feature_gen, 
		lvs=lvs_feature_gen, 
		lab=lab_feature_gen, 
		med=med_feature_gen, 
		mic=mic_feature_gen,
		ed=ed_feature_gen, 
		dia_oncdrs=dia_oncdrs_feature_gen, 
		med_oncdrs=med_oncdrs_feature_gen, 
		chemo_oncdrs=chemo_oncdrs_feature_gen, 
		enc_oncdrs=enc_oncdrs_feature_gen, 
		lab_oncdrs=lab_oncdrs_feature_gen, 
		dia_oncdrs_rpdr=dia_oncdrs_rpdr_feature_gen, 
		med_chemo_oncdrs_rpdr=med_chemo_oncdrs_rpdr_feature_gen, 
		lab_oncdrs_rpdr=lab_oncdrs_rpdr_feature_gen
	)

	temp_function <- function_list[[feature_set_name]] 
	temp_output   <- temp_function(...)

}

#----------------------------------------------------------------------------#