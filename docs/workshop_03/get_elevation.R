#This script uses the elevatr package to access the 90 meter global DEM from opentopography (https://portal.opentopography.org/raster?opentopoID=OTSRTM.042013.4326.1)

library(pacman)
p_load(tidyverse,sf,elevatr,terra,mapview,janitor)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_03/")
####################################################################
# Read Japan prefecture boundaries
pref <- st_read("../workshop_02/inputs/japan_prefecture.gpkg") %>%
  st_transform(4326)
#mapview(pref)

#The 90 meter DEM for all of Japan is too large to access as a single raster from the API. But, we can access in parts over each prefecture
#Register for an API key and store it
#elevatr::set_opentopo_key("your_key")

#Create dir to store each prefecture section
if(!dir.exists("inputs/jp_dem")) dir.create("inputs/jp_dem",recursive = TRUE) 

#Loop over each prefecture to download the DEM tiles
#i=1
walk(1:nrow(pref),
     function(i){
       tryCatch({ #using tryCatch in case a download fails
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


#Read in all layers and merge 
dem <- list.files("inputs/jp_dem",full.names = TRUE) %>% #generate list of files in directory
  lapply(rast) %>% #apply rast() to each element of the vector (i.e., establish connections to each prefecture file)
  sprc() #combine them in a collection

#Merge and write output - about 150MBs
dem_merge <- merge(dem,filename = "inputs/jp_dem.tif") 

#Connect to check if layer is correct
dem_merge_read <- rast("inputs/jp_dem.tif")
plot(dem_merge_read)

