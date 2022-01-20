# load packages
packages <- c("readr", "tidyr", "dplyr", "sf", "ggplot2")
load <- lapply(packages, library, character.only = TRUE)

# read RRI Hs output file
hs.out <-
  read_table("C:/Users/Georgios Boumis/Desktop/RRIr/hs_example.out",
             col_names = FALSE)

# read RRI dem file
dem <-
  read_lines(
    "C:/Users/Georgios Boumis/Desktop/RRIr/dem_example.txt",
    skip = 2,
    n_max = 3
  )

# initialize latitude/longitude matrices
lat <- matrix(nrow = dim(hs.out)[1], ncol = dim(hs.out)[2])
lon <- matrix(nrow = dim(hs.out)[1], ncol = dim(hs.out)[2])

# extract latitude/longitude of lower-left corner and cell size
yllcorner <- as.numeric(sub(".* ", "", dem[2]))
xllcorner	<- as.numeric(sub(".* ", "", dem[1]))
cellsize <- as.numeric(sub(".* ", "", dem[3]))

# compute latitudes/longitudes of each cell and fill in matrices
latitudes <-
  seq(yllcorner, yllcorner + (dim(hs.out)[1] - 1) * cellsize, by = cellsize)
latitudes <- rev(latitudes)

longitudes <-
  seq(xllcorner, xllcorner + (dim(hs.out)[2] - 1) * cellsize, by = cellsize)

for (i in 1:ncol(lat)) {
  lat[, i] <- latitudes
}

for (i in 1:nrow(lon)) {
  lon[i, ] <- longitudes
}

# prepare latitude/longitude matrices for plotting
lat <- as.data.frame(lat)
lat <-
  lat %>% pivot_longer(cols = 1:ncol(lat),
                       names_to = "Temporary",
                       values_to = "Latitude") %>% select("Latitude")

lon <- as.data.frame(lon)
lon <-
  lon %>% pivot_longer(cols = 1:ncol(lon),
                       names_to = "Temporary",
                       values_to = "Longitude") %>% select("Longitude")

# prepare Hs for plotting
hs <-
  hs.out %>% pivot_longer(
    cols = 1:ncol(hs.out),
    names_to = "Temporary",
    values_to = "Hs"
  ) %>% select("Hs")

# create sf data frame with Hs, latitude and longitude columns
hs.sf <-
  data.frame(Hs = hs[['Hs']], Y = lat[['Latitude']], X = lon[['Longitude']])
hs.sf <- st_as_sf(hs.sf, coords = c("X", "Y"))
st_crs(hs.sf) <- 4326

hs.sf[['Hs']][which(hs.sf[['Hs']] == -0.1)] <- NA

# plot inundation depth
ggplot() +
  theme_classic() +
  labs(title = expression(underline("Inundation depth"))) +
  geom_sf(data = hs.sf, aes(color = Hs)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_gradientn(
    colours = c("#999999", "#0099FF", "#0066FF", "#0033FF"),
    na.value = "#FFFFFF",
    limits = c(min(hs.sf[['Hs']]), max(hs.sf[['Hs']]))
  ) +
  guides(color = guide_colourbar(
    title = "Hs (m)",
    barwidth = 0.5,
    barheight = 7.5
  ))

# save inundation depth image
ggsave(
  "Hs.png",
  device = "png",
  unit = "in",
  height = 4.5,
  width = 7,
  dpi = 300
)
