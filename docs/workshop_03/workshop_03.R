#Workshop focusing on raster-raster operations

library(pacman) #if you have never used this package, you need to install it: install.packages("pacman)
pacman::p_load(tidyverse,sf,raster,terra,tmap)

#setwd("your_path/workshop_03")

# Connect to 
aichi_dem <- rast("inputs/jp_dem/aichi_ken.tif")

any(is.na(values(aichi_dem)))
na_count <- sum(is.na(values(aichi_dem)))
na_count/ncell(aichi_dem)

plot(aichi_dem,#colNA = "red",
     col = map.pal("magma"))

# Read in land cover raster data
aichi_lc <- terra::rast("inputs/JAXA_HRLULC_Japan_v23.12_250m.tif") %>%
  terra::crop(aichi_dem) #crop to dem extent

print(aichi_lc)

plot(aichi_lc)

# Resample elevation data to 250m to match land cover resolution
aichi_dem_resampled <- resample(x = aichi_dem, #to be resampled
                                y = aichi_lc, #resampled to
                                method = "bilinear") # Bilinear for continuous data

#Print the metadata
print(aichi_dem_resampled)

#Formally compare the layers
compareGeom(aichi_dem_resampled, aichi_lc, stopOnError = FALSE)

# Calculate elevation statistics by land cover class
aichi_stats <- zonal(x = aichi_dem_resampled, #the layer to ve calculated
                     z = aichi_lc, #the layer defining the zones
                     fun = "mean", #calculate the mean
                     na.rm=TRUE, #ignore NAs
                     as.raster = FALSE) #output into a dataframe instead of raster - this is the default

aichi_stats <- aichi_stats %>%
  rename(land_class = JAXA_HRLULC_Japan_v23.12_250m,
         elevation = aichi_ken)

print(aichi_stats)

#Calculate terrain
aichi_dem_ter <- terra::terrain(x = aichi_dem,
                                v = c("slope"),
                                neighbors = 8)

print(aichi_dem_ter)

plot(aichi_dem_ter)

#Calculate terrain
aichi_dem_ter <- terra::terrain(aichi_dem,v=c("slope","aspect","TRI"),neighbors=8)

print(aichi_dem_ter)

#reample to 250 meter
aichi_dem_ter_resampled <- resample(aichi_dem_ter, aichi_lc, method = "bilinear")

#Calculate slope on land classes
aichi_ter_stats <- zonal(aichi_dem_ter_resampled, aichi_lc, fun = "mean",na.rm=TRUE)

print(aichi_ter_stats)

