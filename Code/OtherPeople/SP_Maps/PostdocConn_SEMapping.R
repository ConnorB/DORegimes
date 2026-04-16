#######################################################
#
# Postdoc Connectivity Paper: SE Watershed Mapping Code
# Created: 24 July 2025 SP, Last Update: 24 July 2025 SP
#
######################################################

###################
# Load Packages
###################

library(tidyverse)
library(readr)
library(scales)
library(patchwork)
library(sf)
library(stars)
library(terra)
library(mapview)
library(raster)
library(ggtext)

######################
#Read in Shapefiles
######################

## NOTE -- you'd need to change the paths for these but the titles should all be right
sites <- read_csv("site_locations.csv") %>% 
  filter(!is.na(siteId))

sites_sf <- sites %>% st_as_sf(coords = c("long", "lat"), agr = "siteId",
                               crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(sites, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') 

#watershed polygons
tal_shed <- st_read("watershed_TAL.shp") 
whr_shed <- st_read("watershed_WHR.shp") 
prf_shed <- st_read("watershed_PRF.shp") 

#DEMs
tal_DEM <- raster("croppedDEM_TAL.tif")
whr_DEM <- raster("croppedDEM_WHR.tif")
prf_DEM <- raster("croppedDEM_PRF.tif")

#stream networks (& clip to watersheds)
tal_streams <- st_read("streams_hires_tal.shp")
st_crs(tal_streams) <- st_crs(tal_DEM) 
tal_streams <- tal_streams[tal_shed,]


whr_streams <- st_read("streams_hires_whr.shp")
st_crs(whr_streams) <- st_crs(whr_DEM) 
whr_streams <- whr_streams[whr_shed,]


prf_streams <- st_read("streams_hires_prf.shp")
st_crs(prf_streams) <- st_crs(prf_DEM) 
prf_streams <- prf_streams[prf_shed,]


#also make contours for these
tal_iso <- rasterToContour(tal_DEM) %>% 
  st_as_sf() %>% 
  st_transform(crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs')

prf_iso <- rasterToContour(prf_DEM) %>% 
  st_as_sf() %>% 
  st_transform(crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs')

whr_iso <- rasterToContour(whr_DEM) %>% 
  st_as_sf() %>% 
  st_transform(crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs')

#crop to watershed
tal_iso <- st_intersection(tal_iso, tal_shed)
whr_iso <- st_intersection(whr_iso, whr_shed)
prf_iso <- st_intersection(prf_iso, prf_shed)

# read in data chunks we used ---
tal_pw <- read_csv("Talladega_allsites_dailyPW.csv") %>% 
  dplyr::select(siteId, percentwet) %>% 
  left_join(sites_sf) %>%
  dplyr::select(siteId, percentwet, type, geometry)

whr_pw <- read_csv("Weyerhaeuser_dailyPW.csv") %>% 
  dplyr::select(siteId, percentwet) %>% 
  left_join(sites_sf) %>%
  dplyr::select(siteId, percentwet, geometry)

prf_pw <- read_csv("PaintRock_dailyPW.csv")%>% 
  dplyr::select(siteId, percentwet) %>% 
  left_join(sites_sf) %>%
  dplyr::select(siteId, percentwet, geometry)


#IF YOU NEED TO SEPERATE OUT OUTLETS, LTMs, STICs, and BONUS STICs, USE THIS
sites <- read_csv("site_locations.csv") %>% 
  filter(!is.na(siteId)) %>% 
  mutate(type = NA,
         type = ifelse(str_detect(siteId, 'M01'), "SP", type),
         type = ifelse(siteId == "PRM06"| 
                         siteId == "PRM09"| 
                         siteId == "PRM11"| 
                         siteId == "PRM15"| 
                         siteId == "PRM19"|
                         siteId == "PRB01"| 
                         siteId =="PRA01", "LT", type),
         type = ifelse(siteId == "TLM06"| 
                         siteId == "TLM10"| 
                         siteId == "TLM16"| 
                         siteId == "TLM19"| 
                         siteId == "TLC01"| 
                         siteId == "TLA01", "LT", type),
         type = ifelse(siteId == "WHM03"| 
                         siteId == "WHM07"| 
                         siteId == "WHM10"| 
                         siteId == "WHM12"| 
                         siteId == "WHB01"| 
                         siteId == "WHA02", "LT", type),
         type = ifelse(siteId == "TLA05"| 
                         siteId == "TLA06"| 
                         siteId == "TLA07"| 
                         siteId == "TLA08"| 
                         siteId == "TLAG4"| 
                         siteId == "TLB02"| 
                         siteId == "TLB03"| 
                         siteId == "TLB04"| 
                         siteId == "TLB05"| 
                         siteId == "TLC03"| 
                         siteId == "TLC04"| 
                         siteId == "TLC05"| 
                         siteId == "TLC06"| 
                         siteId == "TLC07"| 
                         siteId == "TLCG1"| 
                         siteId == "TLCG2"| 
                         siteId == "TLM21"| 
                         siteId == "TLM22"| 
                         siteId == "TLMG3"| 
                         siteId == "TLMG5"| 
                         siteId == "TLX01"| 
                         siteId == "TLX02"| 
                         siteId == "TLX03"| 
                         siteId == "TLY01"| 
                         siteId == "TLY02"| 
                         siteId == "TLY03"| 
                         siteId == "TLZ01"| 
                         siteId == "TLZ02"| 
                         siteId == "TLZ03", "BS", type),
         type = ifelse(is.na(type), "ST", type))


#######################################################################################
# Figure 2: TAL Sample Dates (2B), Hydrograph (2C), and % Network Wet (2D) Time Series
#######################################################################################

#Figure 2A: TAL Watershed map with STIC locations colored by time spent wet
ggplot() +
  geom_sf(data = tal_shed, color = "black", fill = "white", lwd = .35) +
  geom_sf(data = tal_iso, color = "grey60", alpha = 0.5, lwd = 0.25) +
  #scale_fill_continuous(type = "gradient", high = "#ffffff", low = "#000000") +
  geom_sf(data = tal_shed, color = "#2f5597", fill = "transparent", lwd = 0.75) +
  geom_sf(data = tal_streams, col = "black", lwd = 0.25, lineend = "round") +
  geom_sf(data = tal_pw, shape = 21, aes(fill = percentwet, geometry = geometry), size = 2, stroke = 0.35) +
  scale_fill_viridis(direction = -1, name = "Water \nPersistence") +
  geom_sf_label(data = tal_iso, aes(label = level), size = 1, nudge_y = 15, nudge_x = 0, label.size = 0, fill = NA) +
  ggspatial::annotation_scale(pad_x = unit(6, "mm"), pad_y = unit(5, "mm"), bar_cols = c("black", "white"), text_family = "Arial", 
                              text_cex = 0.7, height = unit(0.6, "mm")) +
  ggspatial::annotation_north_arrow(
    pad_x = unit(2, "mm"), pad_y = unit(6, "mm"), which_north = "true", height = unit(5, "mm"), width = unit(3, "mm")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.box = "vertical",
        legend.title = element_text(face = "bold", size = 6),
        legend.text = element_text(size = 6),
        legend.key.width = unit(5, 'mm'),
        legend.key.height = unit(1, 'mm')) +
  guides(
    color = guide_legend(title = "Water \nPersistence"),
  )

ggsave("methods_figure4_talpw_v1.png", height = 3.25, width = 2.75, units = "in", , dpi = 600)


#######################################################################################
# Figure 3: PRF Sample Dates (3B), Hydrograph (3C), and % Network Wet (3D) Time Series
#######################################################################################


#Figure 3A: PRF Watershed map with STIC locations colored by time spent wet
ggplot() +
  geom_sf(data = prf_shed, color = "black", fill = "white", lwd = .35) +
  geom_sf(data = prf_iso, color = "grey60", alpha = 0.5, lwd = 0.25) +
  #scale_fill_continuous(type = "gradient", high = "#ffffff", low = "#000000") +
  geom_sf(data = prf_shed, color = "#8b4726", fill = "transparent", lwd = 0.75) +
  geom_sf(data = prf_streams, col = "black", lwd = 0.25, lineend = "round") +
  geom_sf(data = prf_pw, shape = 21, aes(fill = percentwet, geometry = geometry), size = 2, stroke = 0.35) +
  scale_fill_viridis(direction = -1, name = "Water Persistence") +
  geom_sf_label(data = prf_iso, aes(label = level), size = 1, nudge_y = 15, nudge_x = 0, label.size = 0, fill = NA) +
  ggspatial::annotation_scale(pad_x = unit(6, "mm"), pad_y = unit(1, "mm"), bar_cols = c("black", "white"), text_family = "Arial", 
                              text_cex = 0.7, height = unit(0.6, "mm")) +
  ggspatial::annotation_north_arrow(
    pad_x = unit(2, "mm"), pad_y = unit(2, "mm"), which_north = "true", height = unit(5, "mm"), width = unit(3, "mm")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 6),
        legend.text = element_text(size = 6),
        legend.key.width = unit(5, 'mm'),
        legend.key.height = unit(1, 'mm')) 

ggsave("methods_figure3_prfpw_v1.png", height = 2.2, width = 2.75, units = "in", dpi = 600)


#######################################################################################
# Figure 4: WHR Sample Dates (4B), Hydrograph (4C), and % Network Wet (4D) Time Series
#######################################################################################

#Figure 4A: WHR Watershed map with STIC locations colored by time spent wet
ggplot() +
  geom_sf(data = whr_shed, color = "black", fill = "white", lwd = .35) +
  geom_sf(data = whr_iso, color = "grey60", alpha = 0.5, lwd = 0.25) +
  #scale_fill_continuous(type = "gradient", high = "#ffffff", low = "#000000") +
  geom_sf(data = whr_shed, color = "#548235", fill = "transparent", lwd = 1) +
  geom_sf(data = whr_streams, col = "black", lwd = 0.25, lineend = "round") +
  geom_sf(data = whr_pw, shape = 21, aes(fill = percentwet, geometry = geometry), size = 2, stroke = 0.35) +
  scale_fill_viridis(direction = -1, name = "Water Persistence") +
  geom_sf_label(data = whr_iso, aes(label = level), size = 1, nudge_y = 15, nudge_x = 0, label.size = 0, fill = NA) +
  ggspatial::annotation_scale(pad_x = unit(6, "mm"), pad_y = unit(1, "mm"), bar_cols = c("black", "white"), text_family = "Arial", 
                              text_cex = 0.7, height = unit(0.6, "mm")) +
  ggspatial::annotation_north_arrow(
    pad_x = unit(2, "mm"), pad_y = unit(2, "mm"), which_north = "true", height = unit(5, "mm"), width = unit(3, "mm")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 6),
        legend.text = element_text(size = 6),
        legend.key.width = unit(5, 'mm'),
        legend.key.height = unit(1, 'mm')) 

ggsave(filename = "methods_figure2_whrpw_v1.png", height = 2, width = 2.75, units = "in", dpi = 600)
