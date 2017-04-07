# # ##########################################
# # DESCRIPTION: This file specifies the libraries for the feature generation code
# # ##########################################
# # Creator: Clara Marquardt
# # Date: 5th January 
# # ##########################################
# # Language: R
# # ##########################################
# # TO-DO-LIST 

# ################################################################################
# ########################  (B) LIBRARY SPECIFICATION ############################
# ################################################################################

# package_list <- list("data.table", "shape", "diagram", "dplyr", "futile.logger", "ggplot2", 
#   "icd", "lubridate", "magrittr", "NCmisc", "parallel", "pbapply", "plyr", 
#   "reader", "reshape", "reshape2", "stringr", "taxize", "tidyr", 
#   "VennDiagram", "zoo", "riverplot", "RColorBrewer",
#   "munsell", "knitr", "rmarkdown", "imputeMissings")


# if ("function_selection" %in% ls() & "function_selection"=="basic") {
#   package_list <- list( "plyr", "dplyr", "data.table","stringr","lubridate", "icd", "taxize", 
#     "tidyr", "reshape", "reshape2", "ggplot2", "NCmisc", "reader")
# }

# ## other potentially needed packages
# # package_list <- list( "scales", "xtable", "gridExtra", "grid", "Rtsne","reports", 
# # "validate", "RecordLinkage")

# ################################################################################
# ## helper function

# # text_print <- function(file_path) {
  
# #   # purpose: read in txt file and print formatted output to the terminal 
  
# #   text <- suppressWarnings(readLines(file_path))
# #   text <- paste0(text, collapse="\n")

# #   cat(text)

# # }

# ################################################################################
# ##############  (B) LOAD FUNCTION SPECIFICATION & EXECUTION ####################
# ################################################################################

# load_or_install <- function(package_names, lib_path="R_libs") {  


#   ## dev tools library
#   library(devtools)
#   # set to dev_mode -> ggplot2 dev version
#    dev_mode(TRUE)

#   ## libraries not installable from specified mirror 
#   library(Matrix)


#   if(lib_path=="R_libs"){
#     if (!("R_libs" %in% list.files(normalizePath("~")))) {
#     dir.create("~/R_libs")
#     }
#     lib_path <- paste0(normalizePath("~"), "/R_libs")
#   }

#   lapply(package_names, function(x) if(!x %in% installed.packages(lib.loc=lib_path)) {
    
#     # package specific set-up instructions
#     if(x %in% c("xlsxjars", "xlsx")) {
#       #text_print(paste0('/data/zolab/methods_new/base_code/generic_lib/package_setup', "xlsx_R_setup.txt"))
#     }

#     suppressMessages(install.packages(x,repos="http://cran.cnr.berkeley.edu/", 
#       dependencies=TRUE, lib=lib_path))
#     })

#   # update.packages(lib.loc=lib_path", repos="http://cran.cnr.berkeley.edu/")
#   # lapply(package_names, function(x) library(x,character.only=TRUE, quietly=TRUE,
#   #   verbose=FALSE, lib.loc=lib_path"))
  
#  packages_loaded <- lapply(package_names, function(x) suppressMessages(library(x,
#     character.only=TRUE, quietly=TRUE,verbose=FALSE, lib.loc=lib_path)))

#  cat("\n\n*****************\n\nThe following packages were succesfully loaded:\n\n")
#  print(rev(packages_loaded[[length(package_names)]]))
#  cat("\n*****************\n\n")
# } 

# if(exists("alt_Rlib_path")) {
#   load_or_install(package_list[package_list != "RODBC"], lib_path=alt_Rlib_path)
# } else load_or_install(package_list)



# ################################################################################
# ##################################  END  #######################################
# ################################################################################

