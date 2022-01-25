# load packages
packages <- c("readr", "dplyr", "stringr", "lubridate", "ggplot2")
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

# list all qr files within out folder
qr.files <-
  list.files(out.folder, pattern = "^qr", full.names = TRUE)

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

# select cell for which the hydrograph should be plotted
loc.i <- 103
loc.j <- 49

# get hydrograph at selected cell
qr <- c()

for (i in 1:length(qr.files)) {
  qr.out <-
    read_table(qr.files[i],
               col_names = FALSE)
  qr <- append(qr, qr.out[[loc.i, loc.j]])
}

# create hydrograph data frame and plot
qr.df <- data.frame(Time = dates, Qr = qr)

qr.df %>% ggplot(aes(x = Time, y = Qr)) + geom_line(color = "#0066FF") +
  theme_classic() + scale_x_datetime(labels = scales::date_format("%Y-%m-%d %H:00"),
                                     date_breaks = "60 hours") +
  theme(axis.text.x = element_text(angle = 5)) +
  labs(y = "Discharge (m\u00b3/s)", x = "")

# save hydrograph image
ggsave(
  "Qr.png",
  device = "png",
  unit = "in",
  height = 4.5,
  width = 7,
  dpi = 300
)
