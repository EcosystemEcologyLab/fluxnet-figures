library(dplyr)
library(ggplot2)

# 0) your open‐symbol palette
my_15_shapes <- 0:14

# 1) bin width in degrees
bin_width <- 5

# 2) summarize annual NEE by site (no metadata yet!)
site_summary <- combined_annual %>%
  group_by(site) %>%
  summarize(
    min_NEE  = min(NEE_VUT_REF, na.rm = TRUE),
    max_NEE  = max(NEE_VUT_REF, na.rm = TRUE),
    mean_NEE = mean(NEE_VUT_REF, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  # now join in just the two metadata columns you need
  left_join(
    site_metadata %>% select(SITE_ID, LOCATION_LAT, IGBP),
    by = c("site" = "SITE_ID")
  ) %>%
  rename(
    lat  = LOCATION_LAT
  )

# 3) bin those sites by latitude, then get the envelope per bin
breaks_vec <- seq(-90, 90, by = bin_width)
binned_summary <- site_summary %>%
  mutate(
    lat_bin = cut(
      lat,
      breaks         = breaks_vec,
      include.lowest = TRUE,
      right          = FALSE
    )
  ) %>%
  group_by(lat_bin) %>%
  summarize(
    lat_lower = first(breaks_vec[as.integer(lat_bin)]), # pick one value
    lat_upper = lat_lower + bin_width,
    min_NEE   = min(min_NEE),
    max_NEE   = max(max_NEE),
    .groups   = "drop"
  )

# 4) Plot: horizontal ribbons + overlay site‐means
ggplot() +
  # ribbon via geom_ribbon + coord_flip
  geom_ribbon(
    data = binned_summary,
    aes(
      x    = lat_lower + bin_width/2,  # bin center
      ymin = min_NEE,
      ymax = max_NEE
    ),
    fill  = "steelblue",
    alpha = 0.4
  ) +
  coord_flip() +
  
  # overlay each site’s mean NEE
  geom_point(
    data        = site_summary,
    aes(
      x     = lat,
      y     = mean_NEE,
      shape = IGBP
    ),
    size   = 3,
    color  = "black",
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
  
  scale_shape_manual(values = my_16_shapes) +
  
  labs(
    x     = "Latitude (°)",
    y     = "NEE (μmol m⁻² s⁻¹)",
    title = paste0("Annual NEE range in ", bin_width, "° lat bands")
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title      = element_text(size = 16),
    axis.text       = element_text(size = 10),
    panel.background = element_rect(color = "black"),
    legend.position  = "right"
  )


#analysis by year 

library(dplyr)
library(ggplot2)

# 0) slim metadata to only the columns we need
meta_small <- site_metadata %>% 
  select(SITE_ID, LOCATION_LAT)

# 1) Bin width (degrees)
bin_width <- 5

site_yearly_summary <- combined_annual %>%
  group_by(site, year = TIMESTAMP) %>%
  summarize(
    lat     = first(LOCATION_LAT),           # already in the table
    min_NEE = min(NEE_VUT_REF, na.rm = TRUE),
    max_NEE = max(NEE_VUT_REF, na.rm = TRUE),
    .groups = "drop"
  )


# C) bin by latitude + envelope per bin and year
binned_yearly <- site_yearly_summary %>%
  mutate(
    lat_bin = cut(
      lat,
      breaks         = seq(-90, 90, by = bin_width),
      include.lowest = TRUE,
      right          = FALSE
    )
  ) %>%
  group_by(year, lat_bin) %>%
  summarize(
    lat_mid = mean(range(lat)),
    min_NEE = min(min_NEE),
    max_NEE = max(max_NEE),
    .groups = "drop"
  )

# D) plot horizontal ribbons facetted by year
ggplot(binned_yearly, aes(
  y    = lat_mid, 
  xmin = min_NEE, 
  xmax = max_NEE
)) +
  geom_ribbon(
    aes(xmin = min_NEE, xmax = max_NEE), 
    fill  = "steelblue", 
    alpha = 0.4
  ) +
  facet_wrap(~ year, ncol = 3) +
  labs(
    x     = "NEE (μmol m⁻² s⁻¹)",
    y     = "Latitude (°)",
    title = paste0("Annual NEE range in ", bin_width, "° lat bands")
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title      = element_text(size = 16),
    axis.text       = element_text(size = 10),
    strip.text      = element_text(size = 12),
    panel.background = element_rect(color = "black"),
    legend.position  = "none"
  )





if(!dir.exists("saved_plots")) dir.create("saved_plots")

# loop over each year
for(yr in sort(unique(binned_yearly$year))) {
  p <- ggplot(
    filter(binned_yearly, year == yr),
    aes(y = lat_mid, xmin = min_NEE, xmax = max_NEE)
  ) +
    geom_ribbon(fill = "steelblue", alpha = 0.4) +
    labs(
      x     = "NEE (μmol m⁻² s⁻¹)",
      y     = "Latitude (°)",
      title = paste0("Annual NEE range – ", yr)
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.title      = element_text(size = 16),
      axis.text       = element_text(size = 10),
      panel.background = element_rect(color = "black")
    )
  
  ggsave(
    filename = sprintf("saved_plots/Lat_annual_NEE_%s.png", yr),
    plot     = p,
    width    = 10, height = 8, dpi = 300
  )
}


#Create a GIF

library(gganimate)

binned_yearly <- binned_yearly %>%
  filter(year > 1991)

anim <- ggplot(binned_yearly, aes(
  y    = lat_mid,
  xmin = min_NEE,
  xmax = max_NEE
)) +
  geom_ribbon(fill = "steelblue", alpha = 0.4) +
  labs(
    x     = "NEE (μmol m⁻² s⁻¹)",
    y     = "Latitude (°)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title      = element_text(size = 16),
    axis.text       = element_text(size = 10),
    panel.background = element_rect(color = "black")
  ) +
  transition_states(
    states     = year,
    transition_length = 2,
    state_length      = 1
  ) +
  ease_aes('linear') +
  ggtitle('Annual NEE range – {closest_state}')

# render & save a GIF
# 1) render the animation into a gif object
gif_obj <- animate(
  anim,
  nframes = length(unique(binned_yearly$year)) * 10,  # as before
  fps     = 10,
  renderer = gifski_renderer()                       # default writes to R object
)

# 2) save it to disk
anim_save(
  filename  = "saved_plots/annual_NEE.gif",
  animation = gif_obj
)



library(magick)
img <- image_read("saved_plots/annual_NEE.gif")
print(img)