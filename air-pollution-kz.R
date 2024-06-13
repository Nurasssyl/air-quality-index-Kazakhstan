
libs <- c(
  "tidyverse", "httr",
  "giscoR", "sf", "gstat",
  "classInt"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(
  lapply(
    libs, library, character.only = T
  )
)

url <- "https://api.waqi.info/search/?token=******keyword=Kazakhstan" # PLEASE INSERT YOUR TOKEN INSTEAD OF *
request <- httr::GET(
  url = url
)

make_aqi_dataframe <- function(request) {
  waqi_data <- httr::content(
    request, as = "parsed",
    type = "application/json"
  )$data
  
  waqi_flat <- waqi_data |>
    purrr::map(as.data.frame) |>
    purrr::map(
      purrr::flatten
    )
  
  waqi_df <- data.frame(t(
    do.call(
      cbind, waqi_flat
    )
  )
  ) |>
    dplyr::select(2, 7, 8)
  
  names(waqi_df) <- c("aqi", "lat", "long")
  
  waqi_df_clean <- waqi_df |>
    dplyr::mutate_at(
      c("aqi", "lat", "long"),
      as.numeric
    )
  
  return(waqi_df_clean)
}

waqi_df_clean <- make_aqi_dataframe(
  request = request
) |>
  na.omit()

country_sf <- giscoR::gisco_get_countries(
  country = "KZ",
  resolution = "1"
)

country_transformed <- country_sf |>
  sf::st_transform(3857)

country_grid <- sf::st_make_grid(
  country_transformed,
  cellsize = units::as_units(
    100, "km^2"
  ),
  what = "polygons",
  square = T
) |>
  sf::st_intersection(
    sf::st_buffer(
      country_transformed, 0
    )
  ) |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

sf::st_geometry(country_grid) <- "geometry"

plot(sf::st_geometry(country_grid))


waqi_sf <-  waqi_df_clean |>
  sf::st_as_sf(
    coords = c(
      "long", "lat"
    )
  ) |>
  sf::st_set_crs(4326)

waqi_interp <- gstat::idw(
  aqi ~ 1, waqi_sf, country_grid
) |>
  dplyr::select(
    1:3
  ) |>
  dplyr::rename(
    aqi = var1.pred
  )

head(waqi_interp)


theme_for_the_win <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "top",
      legend.title = element_text(
        size = 9, color = "grey20"
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(
        c(
          t = 0, r = 0,
          b = 0, l = 0
        ), "lines"
      ),
      plot.background = element_rect(
        fill = "white", color = NA
      ),
      legend.background = element_rect(
        fill = "white", color = NA
      )
    )
}

cols <- hcl.colors(
  5, "Viridis",
  rev = T
)

pal <- colorRampPalette(
  cols
)(512)

breaks <- classInt::classIntervals(
  waqi_interp$aqi,
  n = 6,
  style = "equal"
)$brks

crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

map <- ggplot() +
  geom_sf(
    data = waqi_interp,
    aes(
      fill = aqi,
      color = aqi
    ),
    size = 0
  ) +
  scale_fill_gradientn(
    name = "Air quality index",
    colors = pal,
    breaks = round(
      breaks, 0
    ),
    labels = round(
      breaks, 0
    ),
    na.value = "grey80"
  ) +
  scale_color_gradientn(
    name = "Air quality index",
    colors = pal,
    breaks = round(
      breaks, 0
    ),
    labels = round(
      breaks, 0
    ),
    na.value = "grey80"
  ) +
  guides(
    color = "none",
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = 12,
      barheight = .5
    )
  ) +
  coord_sf(crs = crs_lambert) +
  theme_for_the_win()

ggsave(
  "kazakhstan-aqi.png",
  map,
  width = 7,
  height = 7,
  units = "in",
  bg = "white"
)

