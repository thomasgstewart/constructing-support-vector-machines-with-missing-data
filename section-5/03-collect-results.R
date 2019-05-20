.libPaths("~/R/rlib")
require(data.table)
setwd("./results/")


ff <- list.files()
out <- list()
for(i in seq_along(ff)) out[[i]] <- readRDS(ff[i])

bo <- rbindlist(out)

saveRDS(bo, "all-results.RDS")
