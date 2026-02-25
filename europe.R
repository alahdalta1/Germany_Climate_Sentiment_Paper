setwd('G:/Heidelberg_hiwi/')

library(terra)     # Raster data package faster than raster
library(tidyverse)  # Data process and viz
library(sf)
library(dtplyr)
library(exactextractr)


shape1 <- read_sf(dsn = "G:/Heidelberg_hiwi/nuts3/NUTS_RG_20M_2006_4326/NUTS_RG_20M_2006_4326.shp") # NUTS shapefile
shape1<-shape1[shape1$LEVL_CODE==3,] # ONLY LEVEL 3

# shp1 <- st_geometry(shape1) # just for plot


# Create list with files

# list <- Sys.glob("G:/Heidelberg_hiwi/Tareq/wild_fire/*.grib")
list <- Sys.glob("G:/Heidelberg_hiwi/Tareq/*.nc") # your netcdf folder here

# list <- list[26:34]

year <- seq(2015,2022) # for naming
# year <- seq(2015,2023) # precipitation

# year<-2023

# ii=1

for  (ii in 1:length(list)){
  
  raster <- rast(list[ii])
  
  # Reproject
  
  prj <- crs(shape1, proj=TRUE)
  crs(raster) <- prj
  
  # Cropping the data
  
  #crop the weather raster based on the states shapefile
  

  raster <- terra::crop(raster,shape1,mask=T,touches=T)
 
  data<-shape1$NAME_LATN # USE LATIN NAMES FROM SHAPEFILE
  
  #aggregate by shapefile
  
  
  v1<- exact_extract(raster[[1]],shape1,fun='mean',force_df=T) # aggregation by mean
  # v1<- exact_extract(raster[[1]],shape1,fun='sum',force_df=T) # aggregation by sum
  

  data<-cbind(data,v1)

  names(data)[1] <- "District" # CHANGE COLUMN NAME

  # Date as column names
  
  time<-format(as.Date((terra::time(raster[[1]])),format="%Y-%m-%d"))
  # time<-format(as.Date((terra::time(raster[[1]]))-1,format="%Y-%m-%d")) # precipitation -> shift date -1 day
  
  names(data)[2] <- time

  
  rm(v1,time)

  
  
  for (k in 2:dim(raster)[3]){
    
    
    #aggregate by shapefile    
    
    v1<- exact_extract(raster[[k]],shape1,fun='mean',force_df=T)
    # v1<- exact_extract(raster[[k]],shape1,fun='sum',force_df=T) # aggregation by sum
    

    data<-cbind(data,v1)

    
    time<-format(as.Date((terra::time(raster[[k]])),format="%Y-%m-%d"))
    # time<-format(as.Date((terra::time(raster[[k]]))-1,format="%Y-%m-%d")) # precipitation shift date -1 day
    
    names(data)[k+1] <- time

    
    rm(v1,time)

    
    
  }
  
  myfile <- file.path(paste0("G:/Heidelberg_hiwi/Tareq/wild_fire/wildfire_rp_", year[ii], ".csv")) # CHANGE ADDRESS AND NAME HERE

  
  write.table(data,myfile, append=FALSE, sep= ",", row.names = F, col.names=T)

 
   rm(data,raster)  

}

rm(ii,k,list,myfile,prj,year)


