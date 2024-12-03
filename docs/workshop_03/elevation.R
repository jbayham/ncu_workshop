

# solar_point <- st_read("~/Downloads/4644036/Point_solar_500kw_JPKR_2020Dec30_v1.1.shp")
# solar_poly <- st_read("~/Downloads/4644036/Solar_500kw_JPKR_2020Dec30_v1.1.shp")
# mapview::mapview(solar_poly[1:10,])


library(pacman)
p_load(tidyverse,sf,elevatr,terra,mapview,janitor)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_03/")
# Read Japan prefecture boundaries
pref <- st_read("inputs/japan_prefecture.gpkg") %>%
  st_transform(4326)

mapview(pref)
i=1
if(!dir.exists("inputs/jp_dem")) dir.create("inputs/jp_dem",recursive = TRUE)
walk(5:nrow(pref),
     function(i){
       tryCatch({
         # Attempt to download the elevation raster
         elev_rast <- get_elev_raster(locations = pref[i, ],
                                      src = "gl3",
                                      verbose = TRUE)
         
         # Define a unique filename for each tile
         filename <- paste0("inputs/jp_dem/",make_clean_names(pref$nam[i]),".tif")
         
         # Save the raster to a file
         writeRaster(elev_rast, filename, overwrite = TRUE)
         
         # Message for successful download
         message(paste("Successfully downloaded and saved tile:", i))
         
       }, error = function(e) {
         # Handle errors and print a message
         message(paste("Failed to download tile:", i, "- Error:", e$message))
       })
     }
   )


mapview(elev_rast)

elev_rast2 <- get_elev_raster(locations = pref[9,],
                              src = "gl3",
                             verbose=TRUE
)

terra::writeRaster(elev_rast,"dem_test.tif")

elev_col <- sprc(rast(elev_rast),rast(elev_rast2))
merged_col <- merge(elev_col)

mapview::mapview(merged_col)


elev_crop <- crop(elev_rast,ncu_buffered)



# Resample elevation data to 250m to match land cover resolution
elevation_resampled <- resample(rast(elev_rast), jp_lc, method = "bilinear") # Bilinear for continuous data
mapview(elevation_resampled)

