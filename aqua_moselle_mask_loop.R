
install.packages("ncdf4")
install.packages("rgdal")
install.packages("raster")


library("ncdf4", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#############  shp and map

moselle_shp_project <- readOGR("/Users/emirtoker/Desktop/Doktora_Ders/Hyd.Mod.and.Rem.Sen./SMAP_Moselle_Shapefile_Mask/Moselle_shapeFILES/moselle_project.shp")

#############  ############# #############  FILE_LIST #############  #############  #############  

path_aqua <- "/Users/emirtoker/Desktop/AQUA/"
file_list <- list.files(path = path_aqua, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#############  ############# #############  FULL DATE LIST #############  #############  #############  


full_date_list <- seq(as.Date("2002/06/19"), as.Date("2011/10/03"), by = "day")


#############  ############# #############  READ NC FILES #############  #############  #############  

############# #############  #############  aqua nc

i = 1973
j = 2001
#n <- length(full_date_list)
n <- 500

#aqua_date <- seq(1,n,1)
aqua_year <- seq(1,n,1)
aqua_month <- seq(1,n,1)
aqua_day <- seq(1,n,1)
aqua_asc_avg <- seq(1,n,1)
aqua_des_avg <- seq(1,n,1)
aqua_all_avg <- seq(1,n,1)

aqua_results <- data.frame(aqua_year,aqua_month,aqua_day,aqua_asc_avg,aqua_des_avg,aqua_all_avg)

for( a in 1:n ) {
  
  aqua_path_name_nc <- paste(path_aqua,file_list[i],sep="")
  aqua_nc <- nc_open(aqua_path_name_nc)
  
  year <- substr(file_list[i], 25, 28)
  month <- substr(file_list[i], 29, 30)
  day  <- substr(file_list[i], 31, 32)
  date_file <- paste(year,"-",month,"-",day,sep="")
  
  
  if ( full_date_list[j] == date_file )  {
    
    print(a)
    
    
    aqua_lat <- ncvar_get(aqua_nc,aqua_nc$dim$Latitude)
    aqua_lon <- ncvar_get(aqua_nc,aqua_nc$dim$Longitude)
    
    aqua_ll <- extent( min(aqua_lat), max(aqua_lat) ,  min(aqua_lon), max(aqua_lon) )
    
    
    
    ############# #############  #############  soil moisture and quality control flag
    
    aqua_asc <-  aqua_nc$var$A_Soil_Moisture
    aqua_asc_data <- ncvar_get( aqua_nc, aqua_asc)
    aqua_asc_data[aqua_asc_data==-9999] <- NA
    
    aqua_asc_qual <-  aqua_nc$var$A_Inversion_QC_Flag
    aqua_asc_qual_data <- ncvar_get( aqua_nc, aqua_asc_qual)
    aqua_asc_qual_data[aqua_asc_qual_data==-9999] <- NA
    
    
    
    aqua_des <- aqua_nc$var$D_Soil_Moisture
    aqua_des_data <- ncvar_get( aqua_nc, aqua_des)
    aqua_des_data[aqua_des_data==-9999] <- NA
    
    aqua_des_qual <-  aqua_nc$var$D_Inversion_QC_Flag
    aqua_des_qual_data <- ncvar_get( aqua_nc, aqua_des_qual)
    aqua_des_qual_data[aqua_des_qual_data==-9999] <- NA        
    
    
    
    ############# #############  #############  raster
    
    aqua_asc_ras = raster(aqua_asc_data)
    extent(aqua_asc_ras) <- aqua_ll
    aqua_asc_r <- setExtent(aqua_asc_ras, aqua_ll, keepres=TRUE)  
    
    aqua_asc_qual_ras = raster(aqua_asc_qual_data)
    extent(aqua_asc_qual_ras) <- aqua_ll
    aqua_asc_qual_r <- setExtent(aqua_asc_qual_ras, aqua_ll, keepres=TRUE)  
    
    
    
    aqua_des_ras = raster(aqua_des_data)
    extent(aqua_des_ras) <- aqua_ll
    aqua_des_r <- setExtent(aqua_des_ras, aqua_ll, keepres=TRUE)
    
    aqua_des_qual_ras = raster(aqua_des_qual_data)
    extent(aqua_des_qual_ras) <- aqua_ll
    aqua_des_qual_r <- setExtent(aqua_des_qual_ras, aqua_ll, keepres=TRUE)
    
    
    
    ############# #############  #############  crop mask
    
    aqua_asc_r_t = t(aqua_asc_r)
    aqua_des_r_t = t(aqua_des_r)
    aqua_asc_qual_r_t = t(aqua_asc_qual_r)
    aqua_des_qual_r_t = t(aqua_des_qual_r)
    
    aqua_asc_r_t_c <- crop(aqua_asc_r_t,moselle_shp_project) 
    aqua_des_r_t_c <- crop(aqua_des_r_t,moselle_shp_project) 
    aqua_asc_qual_r_c <- crop(aqua_asc_qual_r_t,moselle_shp_project) 
    aqua_des_qual_r_c <- crop(aqua_des_qual_r_t,moselle_shp_project) 
    
    aqua_asc_r_t_c_mask <- mask(aqua_asc_r_t_c,moselle_shp_project)
    aqua_des_r_t_c_mask <- mask(aqua_des_r_t_c,moselle_shp_project)
    aqua_asc_qual_r_c_mask <- mask(aqua_asc_qual_r_c,moselle_shp_project)
    aqua_des_qual_r_c_mask <- mask(aqua_des_qual_r_c,moselle_shp_project)
    
    
    ############# #############  #############  NA conditions
    
    if ( length(which(!is.na(aqua_asc_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      aqua_asc_mean <- NA
      print("asc==0")
    }
    
    if ( length(which(!is.na(aqua_des_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      aqua_des_mean <- NA
      print("des==0")
    }
    
    if ( length(which(!is.na(aqua_asc_r_t_c_mask)[,]==TRUE)) == 0  & length(which(!is.na(aqua_des_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      aqua_all_mean <- NA
      print("all==0")
    }
    
    if ( 0 < length(which(!is.na(aqua_asc_r_t_c_mask)[,]==TRUE)) & length(which(!is.na(aqua_asc_r_t_c_mask)[,]==TRUE)) <= 4 ) {
      
      aqua_asc_mean <- round(mean(aqua_asc_r_t_c_mask@data@values,na.rm=TRUE),3)
      
      print("asc<=4")
    }
    
    if ( 0 < length(which(!is.na(aqua_des_r_t_c_mask)[,]==TRUE)) & length(which(!is.na(aqua_des_r_t_c_mask)[,]==TRUE)) <= 4 ) {
      
      aqua_des_mean <- round(mean(aqua_des_r_t_c_mask@data@values,na.rm=TRUE),3)
      print("des<=4")
    }
    
    
    ############# #############  #############  quality conditions
    
    if ( length(which(!is.na(aqua_asc_r_t_c_mask)[,]==TRUE)) >= 5 ) { 
      
      list_asc <- aqua_asc_r_t_c_mask[which(!is.na(aqua_asc_r_t_c_mask[])==TRUE)]
      list_asc_qual <- aqua_asc_qual_r_c_mask[which(!is.na(aqua_asc_r_t_c_mask[])==TRUE)]
      
      asc_sort <- list_asc[order(-list_asc_qual[])]
      asc_first_value <- round(length(asc_sort)*4/5,0)
      
      aqua_asc_mean <- round(mean(asc_sort[1:asc_first_value],na.rm=TRUE),3)
      
      print("asc>=5")
    } 
    
    if ( length(which(!is.na(aqua_des_r_t_c_mask)[,]==TRUE)) >= 5 ) { 
      
      list_des <- aqua_des_r_t_c_mask[which(!is.na(aqua_des_r_t_c_mask[])==TRUE)]
      list_des_qual <- aqua_des_qual_r_c_mask[which(!is.na(aqua_des_r_t_c_mask[])==TRUE)]      
      
      des_sort <- list_des[order(-list_des_qual[])]
      des_first_value <- round(length(des_sort)*4/5,0)
      
      aqua_des_mean <- round(mean(des_sort[1:des_first_value],na.rm=TRUE),3)
      
      print("des>=5")
    }  
    
    
    aqua_all_mean <- round((aqua_asc_mean+aqua_des_mean)/2,3)
    
    if ( is.na(aqua_des_mean) == TRUE || is.na(aqua_asc_mean) == TRUE  ) {
      
        if ( is.na(aqua_des_mean) == TRUE  ) {
          
          aqua_all_mean <- aqua_asc_mean
        }
        else {
          aqua_all_mean <- aqua_des_mean
        }
    }
    
    
    aqua_results$aqua_year[a] <-substr(full_date_list[j],1,4)
    aqua_results$aqua_month[a] <-substr(full_date_list[j],6,7)
    aqua_results$aqua_day[a] <- substr(full_date_list[j],9,10)
    aqua_results$aqua_asc_avg[a] <- aqua_asc_mean
    aqua_results$aqua_des_avg[a] <- aqua_des_mean
    aqua_results$aqua_all_avg[a] <- aqua_all_mean 
    
    j = j+1
    i = i+1
    
  }  else {  letter <- paste(a,"No date/NA",full_date_list[j] )
  
  print(letter)
  
  aqua_asc_mean <- NA
  aqua_des_mean <- NA
  aqua_all_mean <- NA
  
  aqua_results$aqua_year[a] <-substr(full_date_list[j],1,4)
  aqua_results$aqua_month[a] <-substr(full_date_list[j],6,7)
  aqua_results$aqua_day[a] <- substr(full_date_list[j],9,10)
  aqua_results$aqua_asc_avg[a] <- aqua_asc_mean
  aqua_results$aqua_des_avg[a] <- aqua_des_mean
  aqua_results$aqua_all_avg[a] <- aqua_all_mean 
  
  j = j+1               
  
  }
  
} 
head(aqua_results)
tail(aqua_results)

write.csv(aqua_results, file = "aqua_results_qual_4.csv", quote = FALSE)

