#########################################################
# Map background
#########################################################
# This produces background maps for plotting sites
# and networks.

library(ggplot2)
library(sf)
library(ggspatial)
library(terrainr)
library(terra)
library(dplyr)

# Load geographic data
load(here::here("analysis", "data", "derived_data", "geodata.rda"))

# 1. Greyscale map with scale bar

maptheme_bw_scale <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), "cm")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

map_bw_scale <- list(
  geom_sf(data = mali_adm0, fill = NA),
  geom_sf(data = rivers, fill = "grey", colour = "grey"),
  ggspatial::annotation_scale(aes(location = "br", style = "ticks")),
  maptheme_bw_scale
)

# 2. Greyscale map with degree ticks

maptheme_bw_grid <- theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), "cm")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1.5))

map_bw_grid <- list(
  geom_sf(data = mali_adm0, fill = NA),
  geom_sf(data = rivers, fill = "grey", colour = "grey"),
  maptheme_bw_grid
)

# 3. create an inset map

inset <- ggplot() +
  geom_sf(data = africa_outline, fill = "white") +
  geom_sf(data = mali_adm0, fill = NA, size = 0.6) +
  geom_sf_text(data = mali_adm0,
               aes(label = admin),
               position = position_nudge(x = 15, y = 1), size = 3) +
  theme_void()

# 4. Composite colour map background

# import geotiff
colour_background <- rast(
  here::here("analysis/data/raw_data/ne_background.tif")
  ) %>%
  mask(vect(mali_adm0))

# export to data frame, rename bands, and return to raster format
colour_background <- as.data.frame(colour_background, xy = TRUE) %>%
  rename(
    r = ne_background_1,
    g = ne_background_2,
    b = ne_background_3
  ) %>%
  rast()

# this can now be plotted using terrainr's geom_spatial_rgb
color_map <- list(
  geom_spatial_rgb(data = colour_background,
                   aes(x = x, y = y, r = red, g = green, b = blue)),
  geom_sf(data = rivers, colour = "blue"),
  coord_sf()
)

# 5. Greyscale map showing uplands and Niger river

# isolate altitudes over 420 as upland areas
mountains <- dem.df %>%
  mutate(mountain = case_when(
    alt > 420 ~ 1,
    TRUE ~ 0
  ))

### Find a way to turn this into polygons and plot as geom_sf

bw.dem.map <- list(
  geom_raster(
    data = mountains,
    aes(lon, lat, fill = factor(mountain)),
    show.legend = FALSE
  ),
  scale_fill_manual(values = c("white", "grey")),
  geom_sf(data = mali_adm0, fill = NA),
  geom_sf(data = rivers, colour = "black"),
  coord_sf()
)
