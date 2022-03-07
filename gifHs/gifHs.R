# load packages
packages <-
  c(
    "readr",
    "tidyr",
    "dplyr",
    "stringr",
    "lubridate",
    "animation",
    "ggplot2",
    "sf",
    "magick"
  )
load <- lapply(packages, library, character.only = TRUE)

# path of RRI out folder
out.folder <- "C:/Users/Georgios Boumis/Desktop/RRIr/out_example"

# read RRI input file
rri.input <-
  read_lines("C:/Users/Georgios Boumis/Desktop/RRIr/RRI_Input_example.txt")

# read RRI time file and get starting date
time <-
  read_lines("C:/Users/Georgios Boumis/Desktop/RRIr/time_example.dat")
date <- as.POSIXct(time, "UTC")

# list all hs files within out folder
hs.files <-
  list.files(out.folder, pattern = "^hs", full.names = TRUE)

# get duration of simulation period (hours)
lasth.line <- rri.input[which(str_detect(rri.input, "lasth"))]
lasth <- as.numeric(gsub("[^0-9]", "", lasth.line))

# get number of simulation time steps
outnum.line <- rri.input[which(str_detect(rri.input, "outnum"))]
outnum <- as.numeric(gsub("[^0-9]", "", outnum.line))

# compute time step (hours)
time.step <- lasth / outnum

# get all dates during simulation period
dates <- c(date)

for (i in 1:(outnum - 1)) {
  date <- date + hours(time.step)
  dates <- append(dates, date)
}

# read RRI dem file
dem <-
  read_lines(
    "C:/Users/Georgios Boumis/Desktop/RRIr/dem_example.txt",
    skip = 2,
    n_max = 3
  )

hs.all <- lapply(hs.files, read_table, col_names = FALSE)
hs.sample <- sample(hs.all, 1)[[1]]

maxi <- 0
for (i in 1:length(hs.all)) {
  hs <- hs.all[[i]]
  maxi <- max(maxi, max(hs))
}

# initialize latitude/longitude matrices
lat <- matrix(nrow = dim(hs.sample)[1], ncol = dim(hs.sample)[2])
lon <- matrix(nrow = dim(hs.sample)[1], ncol = dim(hs.sample)[2])

# extract latitude/longitude of lower-left corner and cell size
yllcorner <- as.numeric(sub(".* ", "", dem[2]))
xllcorner	<- as.numeric(sub(".* ", "", dem[1]))
cellsize <- as.numeric(sub(".* ", "", dem[3]))

# compute latitudes/longitudes of each cell and fill in matrices
latitudes <-
  seq(yllcorner, yllcorner + (dim(hs.sample)[1] - 1) * cellsize, by = cellsize)
latitudes <- rev(latitudes)

longitudes <-
  seq(xllcorner, xllcorner + (dim(hs.sample)[2] - 1) * cellsize, by = cellsize)

for (i in 1:ncol(lat)) {
  lat[, i] <- latitudes
}

for (i in 1:nrow(lon)) {
  lon[i,] <- longitudes
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

# function to plot each frame
plotHs <- function(hs) {
  hs.plot <- hs %>% pivot_longer(cols = 1:ncol(hs),
                                 names_to = "Temporary",
                                 values_to = "Hs") %>% select("Hs")
  hs.sf <-
    data.frame(Hs = hs.plot[['Hs']], Y = lat[['Latitude']], X = lon[['Longitude']])
  hs.sf <- st_as_sf(hs.sf, coords = c("X", "Y"))
  st_crs(hs.sf) <- 4326
  
  hs.sf[['Hs']][which(hs.sf[['Hs']] == -0.1)] <- NA
  
  p <- ggplot() +
    theme_classic() +
    geom_sf(data = hs.sf, aes(color = Hs)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_gradientn(
      colours = c("#999999", "#0099FF", "#0066FF", "#0033FF"),
      na.value = "#FFFFFF",
      limits = c(0, maxi)
    ) +
    guides(color = guide_colourbar(
      title = "Hs (m)",
      barwidth = 0.5,
      barheight = 7.5
    ))
  return(p)
}

# store each frame
hs.plots <- lapply(hs.all, plotHs)

# prepare each image for gif
# here 15/60 images are used as an example to speed-up running time
img.list <- list()
for (i in 5:20) {
  hs.plots[[i]] + labs(title = dates[i])
  ggsave(
    paste0("tempHs", ".png"),
    device = "png",
    unit = "in",
    height = 4.5,
    width = 7,
    dpi = 350
  )
  img.list <- append(img.list, image_read("tempHs.png"))
}
file.remove(paste0("tempHs", ".png"))

# join the images together
img.joined <- image_join(img.list)

# animate images at 1 frame per second
img.animated <- image_animate(img.joined, fps = 1)

# save gif
image_write(image = img.animated,
            path = "gifHs.gif")
