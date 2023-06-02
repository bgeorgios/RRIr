# load packages
packages <- c("hydromad")
load <- lapply(packages, library, character.only = TRUE)

# drop R's scientic notation
options("scipen" = 100)

# read discharge observations
q.obs <- read.csv("Q.Obs.csv")

# set parameters' range
nsriver <- c(0.015, 0.04)
nsslope <- c(0.15, 1.0)
soildepth <- c(0.5, 2.0)
gammaa <- c(0.3, 0.5)
kv <- c(0.00000015, 0.00007)
sf <- c(0.05, 0.30)
cw <- c(0.0, 10)
sw <- c(0.1, 0.9)
cd <- c(0.0, 10)
sd <- c(0.1, 0.9)

# prepare list of parameters
params <- list(
  nsriver = nsriver,
  nsslope = nsslope,
  soildepth = soildepth,
  gammaa = gammaa,
  kv = kv,
  sf = sf,
  cw = cw,
  sw = sw,
  cd = cd,
  sd =  sd
)

# lower/upper bounds of parameters
lower <- sapply(params, min)
upper <- sapply(params, max)

# set initial parameters
initpars <- sapply(params, mean)

# set control options of SCE (see hydromad documentation...)
control <- list(trace = 1,
                ncomplex = 15,
                maxit = 5)
control <- modifyList(list(fnscale = -1), control)

if (isTRUE(hydromad.getOption("trace"))) {
  control$trace <- 1
} else {
  control$trace <- -1
}

# set inital values of optimal model/NSE
optmod <- NULL
optfun <- Inf * control$fnscale

# main function
doSCE <- function(pars) {
  # update parameters
  nsriver.new <- pars[1]
  nsslope.new <- pars[2]
  soildepth.new <- pars[3]
  gammaa.new <- pars[4]
  kv.new <- pars[5]
  sf.new <- pars[6]
  cw.new <- pars[7]
  sw.new <- pars[8]
  cd.new <- pars[9]
  sd.new <- pars[10]
  
  # read RRI_Input.xtx
  rri <- readLines("RRI_Input.txt")
  
  # replace lines with updated parameters
  rri[18] <- paste0(nsriver.new, "d0", " # ns_river")
  rri[21] <- paste0(nsslope.new, "d0", " # ns_slope")
  rri[22] <- paste0(soildepth.new, "d0", " # soildepth")
  rri[23] <- paste0(gammaa.new, "d0", " # gammaa")
  rri[25] <- paste0(kv.new, "d0", " # kv")
  rri[26] <- paste0(sf.new, "d0", " # Sf")
  rri[39] <- paste0(cw.new, "d0", " # width_param_c")
  rri[40] <- paste0(sw.new, "d0", " # width_param_s")
  rri[41] <- paste0(cd.new, "d0", " # depth_param_c")
  rri[42] <- paste0(sd.new, "d0", " # depth_param_s")
  
  # write new RRI_Input.txt
  write(rri, file = "RRI_Input.txt")
  
  # run RRI model
  system("0_rri_1_4_2_5.exe")
  
  # extract hydrograph for selected location
  qr.files <- list.files("out", pattern = "^qr", full.names = TRUE)
  qr <- c()
  loc.i <- 231
  loc.j <- 302
  for (i in 1:length(qr.files)) {
    qr.out <-
      read.table(qr.files[i])
    qr <- append(qr, qr.out[[loc.i, loc.j]])
  }
  
  mod <- data.frame(Qr = qr)
  
  # compute NSE
  fun <- hmadstat("r.squared")(Q = q.obs$Discharge, X = mod$Qr)
  
  # store optimal values of model/NSE
  if (isTRUE(fun * control$fnscale < optfun * control$fnscale)) {
    optmod <<- mod
    optfun <<- fun
    nse <- readLines("optNSE.txt")
    nse[1] <- optfun
    write(nse, "optNSE.txt")
  }
  return(fun)
}

# run main function
runcode <-
  SCEoptim(doSCE,
           initpars,
           lower = lower,
           upper = upper,
           control = control)
