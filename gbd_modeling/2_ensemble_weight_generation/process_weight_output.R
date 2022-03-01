

library(dplyr)
library(data.table)

# Reading through output of an M2 run and giving you the best weights from all the initial condition/iterations

##################

me_type = "HAZ"
strategy = "m2"
option = "thresholds_5"
optim_type = "sbplx"
launch.date = "even.weights"





all.ic.fp <- paste0("filepath", me_type, "/", strategy, "/", option, "/1/", optim_type, "/aggregates/", launch.date)
all.ic.files <- list.files(all.ic.fp)

all.ic.weights <- lapply(all.ic.files, function(file){
  single.ic <- readRDS(file.path(all.ic.fp, file))
  return(single.ic)
}) %>% rbindlist(fill = T, use.names = T)

all.ic.weights[ ,c("it.time", "nid_loc_yr_i") := NULL]


best.ic.it <- all.ic.weights[ks == min(ks), ]
best.ic.it <- best.ic.it[1:10]

best.weights <- data.table(Exp = best.ic.it$weights[1],
                           Gamma = best.ic.it$weights[2],
                           Gumbel = best.ic.it$weights[3],
                           InvGamma = best.ic.it$weights[4],
                           LLogis = best.ic.it$weights[5],
                           LNorm = best.ic.it$weights[6],
                           MGamma = best.ic.it$weights[7],
                           MGumbel = best.ic.it$weights[8],
                           Norm = best.ic.it$weights[9],
                           Weibull = best.ic.it$weights[10])

dir.create(paste0("filepath", launch.date))
dir.create(paste0("filepath", launch.date, "/", me_type))
dir.create(paste0("filepath", launch.date, "/", me_type, "/", optim_type))
dir.create(paste0("filepath", launch.date, "/", me_type, "/", optim_type, "/", strategy))

write.csv(best.weights, paste0("filepath", launch.date, "/", me_type, "/", optim_type, "/", strategy, "/", "bestweights_", option, ".csv"), row.names = F)







































