#----------------------------------------------------------------------------#

#' @title Verify the correct compilation of a feature set. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param feature_dt_name 
#' @param cohort_dt 
#' @param cohort_key_var_list 
#' @param extra_var_list 
#' @param save_name 
#' @return
#' @examples

feature_check <- function(feature_dt, feature_dt_name, cohort_dt,cohort_key_var_list, 
  extra_var_list, save_name=TRUE) {

  # number of obs check
  print(sprintf("number of cohort observations: %d", nrow(cohort_dt)))
  print(sprintf("number of observations in %s file: %d", feature_dt_name,
    nrow(feature_dt)))
  ifelse(nrow(cohort_dt)==nrow(feature_dt), print("correct number of rows"), 
    print("incorrect number of rows"))

  # complete check
  feature_missing <- sum(sapply(feature_dt, function(y) sum(is.na(y))))
  feature_missing_perc <- (feature_missing/(nrow(feature_dt[, 
    setdiff(names(feature_dt), c(cohort_key_var_list, extra_var_list)), with=F])*
    ncol(feature_dt[,setdiff(names(feature_dt), c(cohort_key_var_list, 
    extra_var_list)), with=F])))*100
  complete <- nrow(feature_dt[complete.cases(feature_dt)])

  print(sprintf("%s file contains %f missing values %f perc missing values -- %f complete observations", 
    feature_dt_name,feature_missing, feature_missing_perc, complete))

  if (save_name==TRUE) {

    ps("feature names saved: %s", paste0(temp_folder, "var_name_raw_", 
      feature_dt_name, "_", cohort_name, ".csv"))

    write.csv(data.table(var_name=names(feature_dt), 
       var_type=sapply(feature_dt, function(x) class(x)[1])), 
      paste0(temp_folder, "var_name_raw_", feature_dt_name, "_", cohort_name, ".csv"), 
      row.names=F)

    var_dt <- data.table(var_type=sapply(feature_dt[, 
      setdiff(names(feature_dt), 
      c(cohort_key_var_list, extra_var_list)), with=F], function(x) class(x)[1]))
    print(table_mod(var_dt$var_type))

  }
}

#----------------------------------------------------------------------------#


