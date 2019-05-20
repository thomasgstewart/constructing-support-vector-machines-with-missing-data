require(dplyr)
require(dtplyr)
require(data.table)
require(e1071)
require(foreach)
require(doParallel)

source("cc-sim.R")

cores_2_use <- min(detectCores(), 8)
cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 1)
registerDoParallel(cl)

signal_settings <- expand.grid(
    D = c(2,20,200)
  , signal = c(.6,.75,.9)
  , delta = NA
)

signal_settings$delta <- foreach(i = 1:nrow(signal_settings), .combine = "c") %dopar% {
  show(i)
  d1 <- try(get_data(
      N = 500
    , D = signal_settings[i,"D"]
    , signal = signal_settings[i, "signal"]
    , beta = 0
    , missing_type = 1
    , missing_pct = .1
    , seed = 1
    , delta = NA
    , alpha = 1
  ), TRUE)
  if(is.list(d1)){d1$settings$delta}else{NA}
}

missing_data_settings <- expand.grid(
    beta = c(-5, -1, 0, 1, 5)
  , missing_type = 1:4
  , missing_pct = c(.1, .4, .7)
  , alpha = NA
)

`%expand.df%` <- function(A,B){
  nA <- nrow(A)
  nB <- nrow(B)
  out <- cbind(
      A[rep(1:nA, each = nB),]
    , B[rep(1:nB, nA), , drop = FALSE]
  )
  rownames(out) <- NULL
  return(out)
}

sim_settings <- signal_settings %expand.df% missing_data_settings

sim_settings$alpha <- foreach(
  i = 1:nrow(sim_settings)
  , .combine = "c"
) %dopar% {
  d1 <- try(get_data(
      N = 500
    , D = sim_settings[i,"D"]
    , signal = sim_settings[i, "signal"]
    , beta = sim_settings[i,"beta"]
    , missing_type = sim_settings[i,"missing_type"]
    , missing_pct = sim_settings[i,"missing_pct"]
    , seed = 1
    , delta = sim_settings[i,"delta"]
    , alpha = NA
  ), TRUE)
  if(is.list(d1)){ d1$settings$alpha }else{NA}
}

sim_settings[,c("seed","peao","pe_c","pe_o")] <- NA
sim_settings <- sim_settings %expand.df% data.frame(rep = 1:100)
sim_settings$seed <- sample(100000:999999,nrow(sim_settings))

saveRDS(sim_settings, file = "sim-settings.RDS")
