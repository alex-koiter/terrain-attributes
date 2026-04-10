library(tidyverse)
#library(readxl)
library(janitor)
library(terra)
library(sf)

## geochemistry data
geo_data <- read_csv("../Data/Raw data/Geochemistry analysis - Copy 2.csv") %>%
  filter(sample_design == "Grid") %>%
  select(-...1, -sample_design) %>%
  pivot_longer(cols = Ag:Zr, names_to = "element", values_to = "value") %>%
  filter(element %in% c("Ag", "Al", "As","B","Ba","Be","Bi","Ca","Cd","Ce","Co", "Cr", "Cs", "Cu", "Fe", "Ga", "Hf", "Hg", "In", "K", "La", "Li", "Mg", "Mn", "Mo", "Nb", "Ni", "P", "Pb", "Rb", "S", "Sb", "Sc", "Se", "Sn", "Sr", "Te", "Th", "Tl", "U", "V", "Y", "Zn", "Zr")) %>%
  mutate(group = "geochem")

## colour data
colour_data <- read_csv("../Data/Raw data/final results revised.csv") %>%
  filter(sample_design == "Grid") %>%
  select(-...1, -sample_design) %>%
  pivot_longer(cols = X:B, names_to = "element", values_to = "value") %>%
  mutate(group = "colour",
         element = paste(element, "col", sep = "_")) # Differentiate between Boron (B) and Blue (B)

## organic matter data         
org_data <- read_csv("../Data/Raw data/OM_data.csv") %>%
  clean_names() %>%
  filter(sampling_design == "Grid") %>%
  select(sample_number, site, om) %>%
  mutate(element = "organic",
         site = fct_recode(site, "Agriculture" = "Agricultural")) %>%
  rename("value" = "om") %>%
  mutate(group = "organic")

## grain size data
psa_data <- read_csv("../Data/Raw data/PSA_data.csv") %>%
  clean_names() %>%
  filter(sampling_design == "Grid") %>%
  filter(measurement_date_time != "8/3/2021 11:22") %>%
  select(sample_number, site, dx_50, specific_surface_area) %>%
  rename("ssa" = specific_surface_area) %>%
  pivot_longer(cols = c(dx_50, ssa), names_to = "element", values_to = "value") %>%
  mutate(group = "psa")
  
all_data <- geo_data %>%
  bind_rows(colour_data, org_data, psa_data)

# write_csv(x = all_data, file = "./notebooks/soil_data.csv")

## Terrain data
## Uses fingerprints from https://doi.org/10.1007/s11368-024-03805-x
## `_c` are colour properties, the rest our geochem conc.

attribute <- c("plan_curvature", "profile_curvature", "saga_wetness_index", "catchment_area", "relative_slope_position", "channel_network_distance", "elevation")


ag_prints <- c("li", "a_c", "fe", "co", "x_c", "cs", "la", "ni", "nb", "h_c", "b_c", "rb", "ca", "sr", "c_c")

x_data <- foreign::read.dbf("../Data/Raw data/x_agric_1m.dbf") %>%
  rename("id" = "pointid", "x_c" = "grid_code")

ag_data <- read_csv("../Data/Raw data/agric_soil_prop_pnts_terrain_values_new.csv") %>%
  clean_names() %>%
  select(-x_c) %>% # Remove incorrect x colour
  left_join(x_data) %>% # Insert updated x colour
  rename("elevation" = "dem") %>%
  select("x", "y",
         any_of(ag_prints),
         any_of(attribute)) %>%
  drop_na(x, y)

# resample from 1m to 10 m res
r <- resample(rast(ag_data), rast(extent = ext(rast(ag_data)), resolution = 10))

#writeRaster(r, filename = "./notebooks/ag_data.tif", overwrite=TRUE)
ag_data <- as.data.frame(r, xy = TRUE)

# write_csv(x = ag_data, file = "./notebooks/ag_terrain_data.csv")

## Two catchment areas. selecting the first 1
forest_prints <- c("li", "co", "cs", "la", "ni", "nb", "h_c", "ca", "sr")
forest_data <- read_csv("../Data/Raw data/forest_soil_prop_pnts_terrain_values_new.csv") %>%
  clean_names() %>%
  rename("catchment_area" = "catchment_area_29",
         "x_c" = "x_2",
         "h_c" = "h",
         "elevation" = "dem") %>%
  select("x", "y",
         any_of(forest_prints),
         any_of(attribute))

# resample from 1m to 10 m res
r2 <- resample(rast(forest_data), rast(extent = ext(rast(forest_data)), resolution = 10))

#writeRaster(r2, filename = "./notebooks/forest_data.tif", overwrite=TRUE)

forest_data <- as.data.frame(r2, xy = TRUE)

# write_csv(x = forest_data, file = "./notebooks/forest_terrain_data.csv")

## Sample coordinates

coords <- read_csv("../Data/Raw data/Sampling coordinates.csv") %>%
  clean_names() %>%
  filter(sampling_design == "Grid") %>%
  select(site, long, lat)

# write_csv(x = coords, file = "./notebooks/coords.csv")

## orginal 49

org_49 <- read.csv("../Data/Raw data/OM_data.csv") %>%
  filter(Sampling_Design %in% c("Grid", "Likely to erode", "Transect")) %>%
  dplyr::select("Site", "Sampling_Design", "Latitude", "Longitude", "Sample_Number") %>%
  mutate(long = parzer::parse_lon(Longitude),
         lat = parzer::parse_lat(Latitude)) %>%
  mutate(Site = fct_recode(Site, "Agriculture" = "Agricultural")) %>%
  mutate(Sampling_Design = fct_relevel(Sampling_Design, "Grid", "Transect", "Likely to erode")) %>% 
  filter(Sampling_Design == "Grid") %>%
  clean_names() %>%
  right_join(all_data)

# write_csv(x = org_49, file = "../Data/Datasets/orig_49.csv")

org_49_wide <- org_49 %>%
  pivot_wider(id_cols = - group, names_from = element, values_from = value)

# write_csv(x = org_49_wide, file = "../Data/Datasets/orig_49_wide.csv")

## 49 data points with terrain attributes

attribute <- c("plan_curvature", "profile_curvature", "saga_wetness_index", "catchment_area", "relative_slope_position", "channel_network_distance", "elevation")

ag_prints <- c("li", "a_c", "fe", "co", "x_c", "cs", "la", "ni", "nb", "h_c", "b_c", "rb", "ca", "sr", "c_c")

ag_data <- read_csv(here::here("./notebooks/ag_terrain_data.csv"), show_col_types = FALSE) %>%
  select("x", "y", any_of(attribute), any_of(ag_prints)) %>% 
  rename("long" = "x", "lat" = "y") %>%
  st_as_sf(coords = c("long", "lat"),  crs = 32614) %>%
  st_transform(crs = 26914)

coords <- read_csv(here::here("./notebooks/coords.csv"), show_col_types = FALSE) %>% 
  filter(site == "Agriculture") %>%
  st_as_sf(coords = c("long", "lat"),  crs = 4326) %>%
  st_transform(crs = 26914)


ag_data_49 <- st_join(coords, ag_data, st_nearest_feature) %>%
  select(-site) %>%
  st_drop_geometry()

# write_csv(x = ag_data_49, file = "./notebooks/ag_data_49.csv")


forest_prints <- c("li", "co", "cs", "la", "ni", "nb", "h_c", "ca", "sr")

forest_data <- read_csv(here::here("./notebooks/forest_terrain_data.csv"), show_col_types = FALSE) %>%
  select("x", "y", any_of(attribute), any_of(forest_prints)) %>% 
  rename("long" = "x", "lat" = "y") %>%
  st_as_sf(coords = c("long", "lat"),  crs = 32614) %>%
  st_transform(crs = 26914)

coords <- read_csv(here::here("./notebooks/coords.csv"), show_col_types = FALSE) %>% 
  filter(site == "Forest") %>%
  st_as_sf(coords = c("long", "lat"),  crs = 4326) %>%
  st_transform(crs = 26914)


forest_data_49 <- st_join(coords, forest_data, st_nearest_feature) %>%
  select(-site) %>%
  st_drop_geometry()

# write_csv(x = forest_data_49, file = "./notebooks/forest_data_49.csv")
