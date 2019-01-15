# HSK_531E_SM_Moselle_Mask
#
# These codes were written by me during the course season to contribute to team project.
#
# The purpose was to obtain soil moisture data and use it in a lumped hydrological model. 
# So I used target basin extends and I converted multivariate global data to 1D data 
# (only depends on time) via calculating the mean of value according to basin area.
#
# You need to have NetCDF format file to use and Shapefile to mask.
# 
# Steps ; Identify target folder, Find daily data respectively, Read netcdf, NetCDF to Raster,
#         Recognize shapefile or plygon, Crop and Mask, Average of basin, Write 1D ascii file
#
