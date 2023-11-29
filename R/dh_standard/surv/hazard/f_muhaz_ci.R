library(muhaz)
library(survival)

muhaz_ci <- function(times, delta, subset, min.time, max.time, bw.grid, bw.pilot,
                     bw.smooth, bw.method = "local", b.cor = "both", n.min.grid = 51,
                     n.est.grid = 101, kern = "epanechnikov", B = 0) {
  if (B == 0) {
    res <- muhaz::muhaz(times = times, delta = delta, subset = subset, min.time = min.time,
                        max.time = max.time, bw.grid = bw.grid, bw.pilot = bw.pilot,
                        bw.smooth = bw.smooth, bw.method = bw.method, b.cor = b.cor,
                        n.min.grid = n.min.grid, n.est.grid = n.est.grid, kern = kern)
  } else {
    times <- times[subset]
    delta <- delta[subset]
    res <- muhaz::muhaz(times = times, delta = delta, min.time = min.time,
                        max.time = max.time, bw.grid = bw.grid, bw.pilot = bw.pilot,
                        bw.smooth = bw.smooth, bw.method = bw.method, b.cor = b.cor,
                        n.min.grid = n.min.grid, n.est.grid = n.est.grid, kern = kern)
    min.time <- res$pin$min.time
    max.time <- res$pin$max.time
    
    boot_haz <- matrix(NA, B, length(res$haz.est))
    for (i in 1:B) {
      boot_sample <- sample(1:length(times), length(times), replace = TRUE)
      boot_res <- muhaz::muhaz(times = times[boot_sample], delta = delta[boot_sample], min.time = min.time,
                               max.time = max.time, bw.grid = bw.grid, bw.pilot = bw.pilot,
                               bw.smooth = bw.smooth, bw.method = bw.method, b.cor = b.cor,
                               n.min.grid = n.min.grid, n.est.grid = n.est.grid, kern = kern)
      boot_haz[i, ]  <- boot_res$haz.est
    }
    res$haz.lcl <- apply(boot_haz, 2, quantile, prob = 0.025)
    res$haz.ucl <- apply(boot_haz, 2, quantile, prob = 0.975)
    
  }
  res
}