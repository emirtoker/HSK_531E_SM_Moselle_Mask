
install.packages("ncdf4")
install.packages("rgdal")
install.packages("raster")


library("ncdf4", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#############  shp and map

moselle_shp_project <- readOGR("/Users/emirtoker/Desktop/Doktora_Ders/Hyd.Mod.and.Rem.Sen./SMAP_Moselle_Shapefile_Mask/Moselle_shapeFILES/moselle_project.shp")

#############  ############# #############  FILE_LIST #############  #############  #############  

path_smap <- "/Users/emirtoker/Desktop/SMAP/"
file_list <- list.files(path = path_smap, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#############  ############# #############  FULL DATE LIST #############  #############  #############  


full_date_list <- seq(as.Date("2015/03/31"), as.Date("2018/12/07"), by = "day")


#############  ############# #############  READ NC FILES #############  #############  #############  

############# #############  #############  smap nc

i = 498
j = 501
#n <- length(full_date_list)
n <- 500

#aqua_date <- seq(1,n,1)
smap_year <- seq(1,n,1)
smap_month <- seq(1,n,1)
smap_day <- seq(1,n,1)
smap_asc_avg <- seq(1,n,1)
smap_des_avg <- seq(1,n,1)
smap_all_avg <- seq(1,n,1)

smap_results <- data.frame(smap_year,smap_month,smap_day,smap_asc_avg,smap_des_avg,smap_all_avg)

for( a in 1:n ) {
  
  smap_path_name_nc <- paste(path_smap,file_list[i],sep="")
  smap_nc <- nc_open(smap_path_name_nc)
  
  year <- substr(file_list[i], 14, 17)
  month <- substr(file_list[i], 18, 19)
  day  <- substr(file_list[i], 20, 21)
  date_file <- paste(year,"-",month,"-",day,sep="")
  
  
  if ( full_date_list[j] == date_file )  {
    
    print(a)

    
    smap_lat <- ncvar_get(smap_nc,smap_nc$dim$`Soil_Moisture_Retrieval_Data_AM/lat`)
    smap_lon <- ncvar_get(smap_nc,smap_nc$dim$`Soil_Moisture_Retrieval_Data_AM/lon`)
    
    smap_ll <- extent( min(smap_lat), max(smap_lat) ,  min(smap_lon), max(smap_lon) )
    
    
    
    ############# #############  #############  soil moisture and quality control flag
    
    smap_asc <-  smap_nc$var$`Soil_Moisture_Retrieval_Data_PM/soil_moisture_pm`
    smap_asc_data <- ncvar_get( smap_nc, smap_asc)
    smap_asc_data[smap_asc_data==-9999] <- NA
    
    smap_asc_qual <-  smap_nc$var$`Soil_Moisture_Retrieval_Data_PM/retrieval_qual_flag_pm`
    smap_asc_qual_data <- ncvar_get( smap_nc, smap_asc_qual)
    smap_asc_qual_data[smap_asc_qual_data==-9999] <- NA
    
    
    
    smap_des <- smap_nc$var$`Soil_Moisture_Retrieval_Data_AM/soil_moisture`
    smap_des_data <- ncvar_get( smap_nc, smap_des)
    smap_des_data[smap_des_data==-9999] <- NA

    smap_des_qual <-  smap_nc$var$`Soil_Moisture_Retrieval_Data_AM/retrieval_qual_flag`
    smap_des_qual_data <- ncvar_get( smap_nc, smap_des_qual)
    smap_des_qual_data[smap_des_qual_data==-9999] <- NA        
    
    
   
    ############# #############  #############  raster
    
    smap_asc_ras = raster(smap_asc_data)
    extent(smap_asc_ras) <- smap_ll
    smap_asc_r <- setExtent(smap_asc_ras, smap_ll, keepres=TRUE)  
    
    smap_asc_qual_ras = raster(smap_asc_qual_data)
    extent(smap_asc_qual_ras) <- smap_ll
    smap_asc_qual_r <- setExtent(smap_asc_qual_ras, smap_ll, keepres=TRUE)  
    
    
    
    smap_des_ras = raster(smap_des_data)
    extent(smap_des_ras) <- smap_ll
    smap_des_r <- setExtent(smap_des_ras, smap_ll, keepres=TRUE)
    
    smap_des_qual_ras = raster(smap_des_qual_data)
    extent(smap_des_qual_ras) <- smap_ll
    smap_des_qual_r <- setExtent(smap_des_qual_ras, smap_ll, keepres=TRUE)
    
    
    
    ############# #############  #############  crop mask
    
    smap_asc_r_t = t(smap_asc_r)
    smap_des_r_t = t(smap_des_r)
    smap_asc_qual_r_t = t(smap_asc_qual_r)
    smap_des_qual_r_t = t(smap_des_qual_r)
    
    smap_asc_r_t_c <- crop(smap_asc_r_t,moselle_shp_project) 
    smap_des_r_t_c <- crop(smap_des_r_t,moselle_shp_project) 
    smap_asc_qual_r_c <- crop(smap_asc_qual_r_t,moselle_shp_project) 
    smap_des_qual_r_c <- crop(smap_des_qual_r_t,moselle_shp_project) 
    
    smap_asc_r_t_c_mask <- mask(smap_asc_r_t_c,moselle_shp_project)
    smap_des_r_t_c_mask <- mask(smap_des_r_t_c,moselle_shp_project)
    smap_asc_qual_r_c_mask <- mask(smap_asc_qual_r_c,moselle_shp_project)
    smap_des_qual_r_c_mask <- mask(smap_des_qual_r_c,moselle_shp_project)
    
    
    ############# #############  #############  NA conditions
    
    if ( length(which(!is.na(smap_asc_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      smap_asc_mean <- NA
      print("asc==0")
    }
    
    if ( length(which(!is.na(smap_des_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      smap_des_mean <- NA
      print("des==0")
    }
    
    if ( length(which(!is.na(smap_asc_r_t_c_mask)[,]==TRUE)) == 0  & length(which(!is.na(smap_des_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      smap_all_mean <- NA
      print("all==0")
    }
    
    if ( 0 < length(which(!is.na(smap_asc_r_t_c_mask)[,]==TRUE)) & length(which(!is.na(smap_asc_r_t_c_mask)[,]==TRUE)) <= 4 ) {
      
      smap_asc_mean <- round(mean(smap_asc_r_t_c_mask@data@values,na.rm=TRUE),3)
      
      print("asc<=4")
    }
    
    if ( 0 < length(which(!is.na(smap_des_r_t_c_mask)[,]==TRUE)) & length(which(!is.na(smap_des_r_t_c_mask)[,]==TRUE)) <= 4 ) {
      
      smap_des_mean <- round(mean(smap_des_r_t_c_mask@data@values,na.rm=TRUE),3)
      print("des<=4")
    }
    
    
    ############# #############  #############  quality conditions
    
    if ( length(which(!is.na(smap_asc_r_t_c_mask)[,]==TRUE)) >= 5 ) { 
      
      list_asc <- smap_asc_r_t_c_mask[which(!is.na(smap_asc_r_t_c_mask[])==TRUE)]
      list_asc_qual <- smap_asc_qual_r_c_mask[which(!is.na(smap_asc_r_t_c_mask[])==TRUE)]
      
      asc_sort <- list_asc[order(-list_asc_qual[])]
      asc_first_value <- round(length(asc_sort)*4/5,0)
      
      smap_asc_mean <- round(mean(asc_sort[1:asc_first_value],na.rm=TRUE),3)
      
      print("asc>=5")
    } 
    
    if ( length(which(!is.na(smap_des_r_t_c_mask)[,]==TRUE)) >= 5 ) { 
      
      list_des <- smap_des_r_t_c_mask[which(!is.na(smap_des_r_t_c_mask[])==TRUE)]
      list_des_qual <- smap_des_qual_r_c_mask[which(!is.na(smap_des_r_t_c_mask[])==TRUE)]      

      des_sort <- list_des[order(-list_des_qual[])]
      des_first_value <- round(length(des_sort)*4/5,0)
      
      smap_des_mean <- round(mean(des_sort[1:des_first_value],na.rm=TRUE),3)
      
      print("des>=5")
    }  
    
    
    smap_all_mean <- round((smap_asc_mean+smap_des_mean)/2,3)
  
    
    
    smap_results$smap_year[a] <-substr(full_date_list[j],1,4)
    smap_results$smap_month[a] <-substr(full_date_list[j],6,7)
    smap_results$smap_day[a] <- substr(full_date_list[j],9,10)
    smap_results$smap_asc_avg[a] <- smap_asc_mean
    smap_results$smap_des_avg[a] <- smap_des_mean
    smap_results$smap_all_avg[a] <- smap_all_mean 
    
    j = j+1
    i = i+1
    
  }  else {  letter <- paste(a,"No date/NA",full_date_list[j] )
    
    print(letter)
    
    smap_asc_mean <- NA
    smap_des_mean <- NA
    smap_all_mean <- NA
    
    smap_results$smap_year[a] <-substr(full_date_list[j],1,4)
    smap_results$smap_month[a] <-substr(full_date_list[j],6,7)
    smap_results$smap_day[a] <- substr(full_date_list[j],9,10)
    smap_results$smap_asc_avg[a] <- smap_asc_mean
    smap_results$smap_des_avg[a] <- smap_des_mean
    smap_results$smap_all_avg[a] <- smap_all_mean 
    
    j = j+1               
    
  }
  
} 
head(smap_results)
tail(smap_results)

write.csv(smap_results, file = "smap_results_qual_2.csv", quote = FALSE)

