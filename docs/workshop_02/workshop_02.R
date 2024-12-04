#This script will process landcover data in Japan prefectures

library(pacman)
p_load(tidyverse,sf,raster,terra,exactextractr,tmap,janitor,fixest,measurements)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_02/")
#######################

# Read in land cover raster data
jp_lc <- terra::rast("inputs/JAXA_HRLULC_Japan_v23.12_250m.tif")

# Define a point with latitude and longitude
ncu_point <- data.frame(lat = 35.138496, lon = 136.925901) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) # Convert to spatial object

# Create a 50 km buffer around the point
ncu_buffered <- ncu_point %>%
  st_buffer(dist = 50000) # Distance is in meters for CRS 4326


# Crop raster layer to the buffered area
jp_lc_cropped <- terra::crop(jp_lc, ncu_buffered)

# Create a map visualization
ncu_map_rast <- tm_basemap("CartoDB.Positron") +  # Add a basemap
  tm_basemap("Esri.WorldImagery") +              # Add satellite imagery
  tm_shape(ncu_point) +                          # Add the point
  tm_symbols(col = "red", size = 0.001) +        # Style the point
  tm_shape(ncu_buffered) +                       # Add the buffer
  tm_borders(col = "darkred") +                  # Style the buffer
  tm_shape(jp_lc_cropped) +                      # Add the cropped raster
  tm_raster()                                    # Display the raster

# Render the map
tmap_leaflet(ncu_map_rast)

# Define categories and colors
categories <- c("Water bodies", "Built-up", "Paddy field", "Cropland", "Grassland",
                "Deciduous Broad-leaved Forest (DBF)", "Deciduous Needle-leaved Forest (DNF)",
                "Evergreen Broad-leaved Forest (EBF)", "Evergreen Needle-leaved Forest (ENF)", 
                "Bare", "Bamboo forest", "Solar panel", "Wetland", "Greenhouse")

colors <- c("#76C1FF", "#D3D3D3", "#FFD966", "#FFA07A", "#98FB98",
            "#228B22", "#556B2F", "#006400", "#2E8B57", "#F5F5DC",
            "#8B4513", "#D01A11", "#4682B4", "#ADFF2F")

# Assign categories to the raster
cat_ref <- data.frame(ID = 1:length(categories), label = categories)

levels(jp_lc_cropped) <- cat_ref

# Update map with color-coded raster
ncu_map_rast <- tm_basemap("CartoDB.Positron") +  # Add basemap
  tm_basemap("Esri.WorldImagery") +
  tm_shape(ncu_point) + 
  tm_symbols(col = "red", size = 0.001) +
  tm_shape(ncu_buffered) +
  tm_borders(col = "darkred") +
  tm_shape(jp_lc_cropped) +
  tm_raster(palette = colors, alpha = 0.5, style = "cat")

# Render the updated map
tmap_leaflet(ncu_map_rast)

###############
#Starting day 3

# Calculate area of each land cover type within the buffer
# Step 1: Extract raster values and weights within the buffer
extraction <- exact_extract(jp_lc_cropped, ncu_buffered, include_area = TRUE)

# Step 2: Summarize the results by land cover type
area_summary <- extraction[[1]] %>%
  group_by(value) %>% # Group by land cover type (value)
  summarize(total_area_m2 = sum(coverage_fraction * area, na.rm = TRUE)) # Weighted area in m²

# Step 3: Merge with category labels for clarity
area_summary <- area_summary %>%
  left_join(cat_ref, by = c("value" = "ID")) %>% # Join with category labels
  dplyr::select(label, total_area_m2) %>% # Select relevant columns
  arrange(desc(total_area_m2)) # Arrange by area in descending order

# View the summarized area table
print(area_summary)

# Read Japan prefecture boundaries
pref <- st_read("inputs/japan_prefecture.gpkg") %>%
  st_transform(st_crs(jp_lc))

#We've already loaded the first layer, so let's just rename it for clarity
jp_lc_2022 = jp_lc

#Connect to 2018 land cover data
jp_lc_2018 <- terra::rast("inputs/JAXA_HRLULC_Japan_v21.11_250m.tif")

# Use prefecture level data to extract land cover cell values for each prefecture
ex_jp_lc <- exact_extract(x = jp_lc, 
                          y = pref, 
                          include_area = TRUE,
                          progress = FALSE)

aichi <- ex_jp_lc[[1]] %>%
  group_by(value) %>%
  summarise(area_m2 = sum(coverage_fraction * area, na.rm = TRUE)) %>%
  mutate(adm_code = pref$adm_code[1])

# Process results for each polygon
areas_list <- lapply(seq_along(ex_jp_lc), function(i) { #Define an anonymous function to apply to each element i of a vector of 1 to 47 (the length of ex_jp_lc)
  data <- ex_jp_lc[[i]] %>%
    group_by(value) %>% # Group by land cover type
    summarise(area_m2 = sum(coverage_fraction * area, na.rm = TRUE)) %>% # Calculate area
    mutate(adm_code = pref$adm_code[i]) # Add prefecture name (assumes NAME_1 is the column with prefecture names)
})

pref_areas <- bind_rows(areas_list)

# Lets make it a function
calculate_areas <- function(raster_layer, user_polygons) {
  ex_temp <- exact_extract(raster_layer, 
                           user_polygons, 
                           include_area = TRUE,
                           progress = FALSE)
  
  
  # Process results for each polygon
  areas_list <- lapply(seq_along(ex_temp), function(i) {
    data <- ex_temp[[i]] %>%
      group_by(value) %>% # Group by land cover type
      summarise(area_m2 = sum(coverage_fraction * area, na.rm = TRUE)) %>% # Calculate area
      mutate(adm_code = user_polygons$adm_code[i]) # Add prefecture name (assumes NAME_1 is the column with prefecture names)
  }) %>%
    bind_rows()
  
}

sum_2022 <- calculate_areas(raster_layer = jp_lc_2022,
                            user_polygons = pref) %>%
  mutate(year=2022) 

sum_2018 <- calculate_areas(jp_lc_2018,pref) %>%
  mutate(year=2018) 

pref_lc <- bind_rows(sum_2018,sum_2022) %>%
  inner_join(cat_ref,by = c("value"="ID")) %>%
  inner_join(st_drop_geometry(pref),by = "adm_code")

#Plot the change in solar panels
pref_lc %>%
  filter(value==12) %>%
  ggplot(aes(y=nam,x=area_m2,fill = as_factor(year))) +
  geom_col(position = "dodge")

#Plot the change in solar panels
pref_lc %>%
  filter(value==12) %>%
  ggplot(aes(y=reorder(nam,area_m2),x=conv_unit(area_m2,"m2","hectare"),fill = as_factor(year))) +
  geom_col(position = "dodge") +
  labs(x="Area (hectares)",y=NULL,title = "Solar Coverage in Japan Prefectures") +
  scale_fill_discrete(name = "Year") +
  theme_bw(base_size = 12)


#Prepare data
reg_dat <- pivot_wider(pref_lc,
                       id_cols = c(nam,year),
                       names_from = label,
                       values_from = area_m2) %>%
  janitor::clean_names() %>%
  mutate(df=deciduous_broad_leaved_forest_dbf + deciduous_needle_leaved_forest_dnf,
         ef=evergreen_needle_leaved_forest_enf + evergreen_broad_leaved_forest_ebf)


#Estimate FE OLS regression and store results in m1
m1 <- feols(solar_panel ~ built_up + cropland + paddy_field + bare + df + ef | nam + year,
            data = reg_dat)

#Print table out in console
etable(m1)





