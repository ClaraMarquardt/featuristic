#----------------------------------------------------------------------------#

# Purpose:     Load scripts locally
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# NOTE: manually define "package_path" (path to directory containing 
# the featuristic repository (e.g. '/Users/../Desktop/'))

print(sprintf("package_path: %s",  package_path))

# paths
setwd(package_path)

#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#

# load scripts locally
#--------------------------------------# 
for (x in list.files(paste0(package_path, "featuristic", "/R"))) {

  print(sprintf("source: %s", x))

  source(paste0(package_path, "featuristic", "/R/", x))

}


# load datasets locally
#--------------------------------------# 
for (x in list.files(paste0(package_path, "featuristic", "/data"))) {

  print(sprintf("load: %s", x))

  load(paste0(package_path, "featuristic", "/data/", x))

}

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#

