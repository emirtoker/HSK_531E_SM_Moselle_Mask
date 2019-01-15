
install.packages("ncdf4")
install.packages("rgdal")
install.packages("raster")


library("ncdf4", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#############  shp and map

moselle_shp_project <- readOGR("/Users/emirtoker/Desktop/Doktora_Ders/Hyd.Mod.and.Rem.Sen./SMAP_Moselle_Shapefile_Mask/Moselle_shapeFILES/moselle_project.shp")

#############  ############# #############  FILE_LIST #############  #############  #############  

path_esa <- "/Users/emirtoker/Desktop/ESA_CCI_SM_v04.4_COMBINED/"
file_list <- list.files(path = path_esa, pattern = NULL, all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
head(file_list)

#############  ############# #############  FULL DATE LIST #############  #############  #############  


full_date_list <- seq(as.Date("1978/11/01"), as.Date("2018/06/30"), by = "day")
head(full_date_list)

#############  ############# #############  READ NC FILES #############  #############  #############  

############# #############  #############  esa nc

i = 14244
j = 14244
#n <- length(full_date_list)
n <- 500

#esa_date <- seq(1,n,1)
esa_year <- seq(1,n,1)
esa_month <- seq(1,n,1)
esa_day <- seq(1,n,1)
esa_comb_avg <- seq(1,n,1)


esa_results <- data.frame(esa_year,esa_month,esa_day,esa_comb_avg)

for( a in 1:n ) {
  
  esa_path_name_nc <- paste(path_esa,file_list[i],sep="")
  esa_nc <- nc_open(esa_path_name_nc)
  
  year <- substr(file_list[i], 39, 42)
  month <- substr(file_list[i], 43, 44)
  day  <- substr(file_list[i], 45, 46)
  date_file <- paste(year,"-",month,"-",day,sep="")
  
  
  if ( full_date_list[j] == date_file )  {
    
    print(a)
    
    
    esa_lat <- ncvar_get(esa_nc,esa_nc$dim$lat)
    esa_lon <- ncvar_get(esa_nc,esa_nc$dim$lon)
    
    esa_ll <- extent( min(esa_lat), max(esa_lat) ,  min(esa_lon), max(esa_lon) )
    
    
    
    ############# #############  #############  soil moisture and quality control flag
    
    esa_comb <-  esa_nc$var$sm
    esa_comb_data <- ncvar_get( esa_nc, esa_comb)
    esa_comb_data[esa_comb_data==-9999] <- NA
    
    esa_comb_qual <-  esa_nc$var$flag
    esa_comb_qual_data <- ncvar_get( esa_nc, esa_comb_qual)
    esa_comb_qual_data[esa_comb_qual_data==-9999] <- NA
    
  
    
    ############# #############  #############  raster
    
    esa_comb_ras = raster(esa_comb_data)
    extent(esa_comb_ras) <- esa_ll
    esa_comb_r <- setExtent(esa_comb_ras, esa_ll, keepres=TRUE)  
    
    esa_comb_qual_ras = raster(esa_comb_qual_data)
    extent(esa_comb_qual_ras) <- esa_ll
    esa_comb_qual_r <- setExtent(esa_comb_qual_ras, esa_ll, keepres=TRUE)  
    
  
    
    ############# #############  #############  crop mask
    
    esa_comb_r_t = t(esa_comb_r)
    esa_comb_qual_r_t = t(esa_comb_qual_r)

    esa_comb_r_t_c <- crop(esa_comb_r_t,moselle_shp_project) 
    esa_comb_qual_r_c <- crop(esa_comb_qual_r_t,moselle_shp_project) 

    esa_comb_r_t_c_mask <- mask(esa_comb_r_t_c,moselle_shp_project)
    esa_comb_qual_r_c_mask <- mask(esa_comb_qual_r_c,moselle_shp_project)

    
    ############# #############  #############  NA conditions
    
    if ( length(which(!is.na(esa_comb_r_t_c_mask)[,]==TRUE)) == 0 ) {
      
      esa_comb_mean <- NA
      print("comb==0")
      
    }
    
    
    if ( 0 < length(which(!is.na(esa_comb_r_t_c_mask)[,]==TRUE)) & length(which(!is.na(esa_comb_r_t_c_mask)[,]==TRUE)) <= 4 ) {
      
      esa_comb_mean <- round(mean(esa_comb_r_t_c_mask@data@values,na.rm=TRUE),3)
      
      print("comb<=4")
      
    }
    
    ############# #############  #############  quality conditions
    
    if ( length(which(!is.na(esa_comb_r_t_c_mask)[,]==TRUE)) >= 5 ) { 
      
      list_comb <- esa_comb_r_t_c_mask[which(!is.na(esa_comb_r_t_c_mask[])==TRUE)]
      list_comb_qual <- esa_comb_qual_r_c_mask[which(!is.na(esa_comb_r_t_c_mask[])==TRUE)]
      
      comb_sort <- list_comb[order(-list_comb_qual[])]
      comb_first_value <- round(length(comb_sort)*4/5,0)
      
      esa_comb_mean <- round(mean(comb_sort[1:comb_first_value],na.rm=TRUE),3)
      
      print("asc>=5")
      
    } 
    
    
    
    esa_results$esa_year[a] <-substr(full_date_list[j],1,4)
    esa_results$esa_month[a] <-substr(full_date_list[j],6,7)
    esa_results$esa_day[a] <- substr(full_date_list[j],9,10)
    esa_results$esa_comb_avg[a] <- esa_comb_mean

    j = j+1
    i = i+1
    
  }  else {  letter <- paste(a,"No date/NA",full_date_list[j] )
  
  print(letter)
  
  esa_comb_mean <- NA

  esa_results$esa_year[a] <-substr(full_date_list[j],1,4)
  esa_results$esa_month[a] <-substr(full_date_list[j],6,7)
  esa_results$esa_day[a] <- substr(full_date_list[j],9,10)
  esa_results$esa_comb_avg[a] <- esa_comb_mean

  j = j+1               
  
  }
  
  write.csv(esa_results, file = "esa_results_qual_12.csv", quote = FALSE)
  
} 
head(esa_results)
tail(esa_results)

write.csv(esa_results, file = "esa_results_qual_12.csv", quote = FALSE)

