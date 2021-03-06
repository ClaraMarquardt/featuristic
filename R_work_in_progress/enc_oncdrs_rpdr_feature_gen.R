#----------------------------------------------------------------------------#

#' @title Generate DFCI & RPDR (combined) enc-related features (enc_oncdrs & enc data).
#'
#' @description \
#'
#' @export
#' @param cohort
#' @param cohort_key_var_merge
#' @return
#' @examples

 enc_oncdrs_rpdr_feature_gen <- function(cohort, cohort_key_var_merge, cohort_key_var) {
  
  print("launching enc_oncdrs_rpdr_feature_gen")
  
  # generate both files
  enc        <- enc_feature_gen(cohort, cohort_key_var_merge, cohort_key_var)

  # source(enc_oncdrs_feature_code)
  enc_oncdrs <- enc_oncdrs_feature_gen(cohort, cohort_key_var_merge, cohort_key_var)
  
  ## merge
  enc_oncdrs_rpdr <- enc

  # (a) rename department variable
  setnames(enc_oncdrs, gsub("enc.dfci_dfci.enc.enc_dfci.enc.count_dfci.department.name.count", 
    "enc_enc.enc_enc.count_clinic.count_clinic.name", names(enc_oncdrs)))
  setnames(enc_oncdrs, gsub("enc.dfci_dfci.enc.enc_dfci.enc.count_dfci.enc.type.count", 
    "enc_enc.enc_enc.count_dfci.enc.type", names(enc_oncdrs)))

  # (b) generate sum of encounters
  inv_lapply(c(name_ext_extended), function(x) if(length(setdiff(grep(paste0(x, "$"), names(enc_oncdrs), value=T), 
      grep("_time_", names(enc_oncdrs), value=T))>0)) {enc_oncdrs[, paste0("enc_count_aggregated", x):=
    rowSums(.SD), .SDcols=setdiff(grep(paste0(x, "$"), names(enc_oncdrs), value=T), 
      grep("_time_", names(enc_oncdrs), value=T))]})

  # merge
  enc_oncdrs_rpdr <- enc_oncdrs_rpdr[enc_oncdrs, on=cohort_key_var]
  enc_oncdrs_rpdr[, enc_time_min:=min(enc_time_min, get("enc_oncdrs_time_min"))]
  enc_oncdrs_rpdr[, enc_time_max:=max(enc_time_max, get("enc_oncdrs_time_max"))]
  enc_oncdrs_rpdr[, grep("enc_oncdrs_time", names(enc_oncdrs_rpdr), value=T):=NULL]
    
  inv_lapply(name_ext_extended, function(x) if(paste0("enc_count_aggregated", x) %in% names(enc_oncdrs_rpdr)) {
    enc_oncdrs_rpdr[, grep(paste0("enc_enc.enc_enc.count..clinic", x), names(enc_oncdrs_rpdr), value=T):=
    get(grep(paste0("enc_enc.enc_enc.count..clinic", x), names(enc_oncdrs_rpdr), value=T)) + 
    get(paste0("enc_count_aggregated", x))]})

  inv_lapply(name_ext_extended, function(x) if(paste0("enc_count_aggregated", x) %in% names(enc_oncdrs_rpdr)) {
    enc_oncdrs_rpdr[, grep(paste0("enc_enc.enc_enc.count_clinic.count_clinic.cat..onc", x), 
    names(enc_oncdrs_rpdr), value=T):=get(grep(paste0("enc_enc.enc_enc.count_clinic.count_clinic.cat..onc", x), 
    names(enc_oncdrs_rpdr), value=T)) + get(paste0("enc_count_aggregated", x))]})

  enc_oncdrs_rpdr[, grep("enc_count_aggregated", names(enc_oncdrs_rpdr), value=T):=NULL]

  # categorize variables to ensure proper treatment in models -- integer 
  enc_oncdrs_rpdr_integer <- enc_oncdrs_rpdr[, mget(setdiff(names(enc_oncdrs_rpdr), c("outcome_id", "t0_date", "empi", "enc_time_min", "enc_time_max")))]
  enc_oncdrs_rpdr_integer[, names(enc_oncdrs_rpdr_integer):=lapply(.SD, function(x) as.integer(x))]

  enc_oncdrs_rpdr <- cbind(enc_oncdrs_rpdr[, mget(c("outcome_id", "t0_date", "empi", "enc_time_max", "enc_time_min"))], enc_oncdrs_rpdr_integer)


  return (enc_oncdrs_rpdr)

}

#----------------------------------------------------------------------------#




