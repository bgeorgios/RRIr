# load packages
packages <- c("raster")
load <- lapply(packages, library, character.only = TRUE)

# choose River Forecast Center
rfc <- "SERFC"

# choose year
year <- "2018"

# choose month
month <- "09"

# create url
my.url <-
  paste0(
    "https://hydrology.nws.noaa.gov/aorc-historic/AORC_",
    rfc,
    "_4km/",
    rfc,
    "_precip_partition/AORC_APCP_4KM_",
    rfc,
    "_",
    year,
    month,
    ".zip"
  )

# download rain data in .zip format
download.file(url = my.url,
              destfile = "Rain.Data.zip",
              method = 'curl')

# unzip rain data
dir.create("Rain.Data")
unzip("Rain.Data.zip", exdir = "Rain.Data")

# create empty rain.dat file for RRI
file.create("rain.dat")

# read rain data files
rain.files <- list.files("Rain.Data", full.names = TRUE)

# write data to rain.dat
step <- 0
for (f in rain.files) {
  rain <- raster(f)
  mat <-
    as.matrix(rain,
              nrow = dim(rain)[1],
              ncol = dim(rain)[2])
  mat[is.na(mat)] <- 0.0
  write(c(step, dim(mat)[2], dim(mat)[1]),
        "rain.dat",
        ncol = 3,
        append = TRUE)
  for (r in 1:nrow(mat)) {
    write(mat[r,],
          "rain.dat",
          ncol = dim(mat)[2],
          append = TRUE)
  }
  step <- step + 3600
}
