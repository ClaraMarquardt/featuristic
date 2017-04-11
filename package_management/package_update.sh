#----------------------------------------------------------------------------#

# Purpose:     Shell script to update package 
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#


# update data
#----------------------------------------------------------------------------#

# variable_list_default.Rds
#-------------------------------------
cd ${package_path}

R CMD BATCH --no-save "--args ${package_path} ${package_name}" \
	package_management/variable_list_default_update.R \
	package_management/variable_list_default_update.Rout


# update package
#----------------------------------------------------------------------------#

cd ${package_path}

R CMD BATCH --no-save "--args ${package_path} ${package_name}" \
	package_management/package_update.R \
	package_management/package_update.Rout

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#
