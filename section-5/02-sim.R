args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)


.libPaths("~/R/rlib")
require(e1071)
require(data.table)
source("cc-sim.R")
ss <- readRDS("sim-settings.RDS")
ss$N <- 500
ss[,c("obs_missing_pct","obs_bayes_risk")] <- NA

out <- list()
k <- 1
for(i in 1:nrow(ss)){
#if(i > 1) next
  if(i %% 1000 != args) next
  out[[k]] <- with(ss[i,], 
    {
      ss1 <- single_sim(
            N
          , D
          , signal
          , beta
          , missing_type
          , missing_pct
          , seed
          , delta
          , alpha
      )
      if(is.data.frame(ss1)){
        lo <- ss[i,,drop = FALSE]
        lo[1, c("peao","pe_o","pe_c","obs_missing_pct","obs_bayes_risk")] <-
          ss1[1, c("peao","pe_o","pe_c","obs_missing_pct","obs_bayes_risk")]
        lo
      }else{
        NULL
      }
    }   
  )
  k <- k + 1
}

bo <- rbindlist(out)
saveRDS(bo,sprintf("./results/bo-%03.0f.RDS", args))
