# Shell script to update the featuristic package 
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#

## Notes:
# - Assume that in directory containing the featuristic repository/folder
# - Does not update the data sets (use devtools::use_data([object],overwrite=TRUE))

R CMD BATCH --no-save featuristic/package_management/package_update.R \
	featuristic/package_management/package_update.Rout

# [ -e "featuristic/package_management/package_update.Rout" ] & rm featuristic/package_management/package_update.Rout

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#
