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

feature_check <- function(feature_dt_name, 
  cohort_dt=cohort,cohort_key_var_list=cohort_key_var, 
  extra_var_list=names(cohort_extra_col), save_name=TRUE) {

  # number of obs check
  print(sprintf("number of cohort observations: %d", nrow(cohort_dt)))
  print(sprintf("number of %s observations: %d", feature_dt_name, 
    nrow(get(feature_dt_name))))
  ifelse(nrow(cohort_dt)==nrow(get(feature_dt_name)), print("correct number of rows"), 
    print("incorrect number of rows"))

  # complete check
  feature_missing <- sum(sapply(get(feature_dt_name), function(y) sum(is.na(y))))
  feature_missing_perc <- (feature_missing/(nrow(get(feature_dt_name)[, 
    setdiff(names(get(feature_dt_name)), c(cohort_key_var_list, extra_var_list)), with=F])*
    ncol(get(feature_dt_name)[,setdiff(names(get(feature_dt_name)), c(cohort_key_var_list, 
    extra_var_list)), with=F])))*100
  complete <- nrow(get(feature_dt_name)[complete.cases(get(feature_dt_name))])

  print(sprintf("%s file contains %f missing values %f perc missing values -- %f complete observations", 
    feature_dt_name,feature_missing, feature_missing_perc, complete))

  if (save_name==TRUE) {

    ps("feature names saved: %s", paste0(temp_folder, "var_name_raw_", 
      feature_dt_name, "_", cohort_name, ".csv"))

    write.csv(data.table(var_name=names(get(feature_dt_name)), 
       var_type=sapply(get(feature_dt_name), function(x) class(x)[1])), 
      paste0(temp_folder, "var_name_raw_", feature_dt_name, "_", cohort_name, ".csv"), 
      row.names=F)

    var_dt <- data.table(var_type=sapply(get(feature_dt_name)[, setdiff(names(get(feature_dt_name)), 
      c(cohort_key_var_list, extra_var_list)), with=F], function(x) class(x)[1]))
    print(table_mod(var_dt$var_type))

  }
}

#----------------------------------------------------------------------------#


