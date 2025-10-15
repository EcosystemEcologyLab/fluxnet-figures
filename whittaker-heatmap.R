library(plotbiomes)
library(measurements)
library(colorspace)
library(ggtext)
library(sf)
library(hexbin)

source(file = "R/fcn_utility_FLUXNET.R")

### Load Data ###
metadata <- load_fluxnet_metadata()
manifest <- discover_AMF_files(data_dir = here::here("data/FLUXNET/AMF")) %>% 
  distinct(
    site, data_product, dataset, time_integral, start_year, end_year,
    .keep_all = TRUE
  )
annual <- manifest %>%
  filter(time_integral == "YY", dataset == "FULLSET") %>%
  load_fluxnet_data() %>%                                    
  mutate(
    across(where(is.numeric), \(x) na_if(x, -9999)),
    year = as.integer(TIMESTAMP),
    .before = TIMESTAMP
  ) %>%
  left_join(metadata %>% select(-SITEID, -SITE_ID), by = join_by(site)) %>% 
  select(-path) %>%
  relocate(site, .before = 1)

# Convert prcip to cm and filter out large values
plot_df <- annual %>% 
  mutate(precip_cm = conv_unit(P_F, "mm", "cm")) %>% 
  filter(precip_cm < 500) %>% 
  select(precip_cm, temp_c = TA_F, NEE = NEE_VUT_REF)

# Convert adjacent polygons with overlapping borders to polygons with borders
# that don't quite touch for nicer plotting

# Scale coords so buffering works equally on both dimensions
whit_scaled <- Whittaker_biomes %>% mutate(across(c(temp_c, precp_cm), scale))
# create sf object of boundaries
borders <- whit_scaled %>%
  st_as_sf(coords = c("temp_c", "precp_cm")) %>%
  group_by(biome) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_buffer(dist = -0.01) %>% # <- might need to tweak this number
  st_boundary()

# Convert back to data frame and unscale
temp_center <- attr(whit_scaled$temp_c, "scaled:center")
temp_scale <- attr(whit_scaled$temp_c, "scaled:scale")
precip_center <- attr(whit_scaled$precp_cm, "scaled:center")
precip_scale <- attr(whit_scaled$precp_cm, "scaled:scale")

borders_df <- borders %>%
  as_tibble() %>%
  group_by(biome) %>%
  reframe(
    temp_c = st_coordinates(geometry)[, 1],
    precip_cm = st_coordinates(geometry)[, 2]
  ) %>%
  #unscale
  mutate(
    temp_c = (temp_c * temp_scale) + temp_center,
    precip_cm = (precip_cm * precip_scale) + precip_center
  )

ggplot() +
  geom_hex(
    data = plot_df,
    aes(x = temp_c, y = precip_cm, z = NEE),
    stat = "summary_hex",
    bins = 40,
    fun = mean,
    na.rm = TRUE
  ) +
  scale_fill_continuous_diverging(
    name = "NEE (g C m<sup>-2</sup> y<sup>-1</sup>)"
  ) +
  geom_polygon(
    data = borders_df,
    aes(x = temp_c, y = precip_cm, color = biome),
    linewidth = 1.5, # <- might need to tweak this number
    fill = NA
  ) +
  scale_color_manual(
    name = "Whittaker biomes",
    breaks = names(Ricklefs_colors),
    labels = names(Ricklefs_colors),
    values = Ricklefs_colors,
  ) +
  labs(x = "Temperature (C)", y = "Precipitation (cm y<sup>-1</sup>)") +
  theme_minimal() +
  theme(legend.title = element_markdown(), axis.title.y = element_markdown())
