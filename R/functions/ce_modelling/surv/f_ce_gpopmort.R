# Functions to help simulate the mortality of the general population


#' @title Estimate the General Population Overall Survival
#' @description This function estimates the overall survival of the general 
#'   population, based on the provided baseline age and the given life tables for 
#'   males and females. An optional assumption about the maximum age of death 
#'   (defaulted to 100) is also incorporated.
#' 
#' @param bl_age Numeric. Baseline age.
#' @param cl_yr Numeric. Cycle length in years.
#' @param th_yr Numeric. Time horizon in years.
#' @param pr_fem Numeric. Proportion female at baseline.
#' @param LT_ma Data frame. Life table for males with columns "age" and "qx".
#' @param LT_fe Data frame. Life table for females with columns "age" and "qx".
#' @param assume_die_at Numeric. Age at which individuals are assumed to die. Default is 100.
#'
#' @return Numeric vector. The age and sex weighted overall survival line.
#'
#' @importFrom stats cumprod
#' @importFrom data.table shift
#' 
#' 
#' @examples
#' 
#' 
#' # Grab the ONS life table for England and Wales from the ONS website, load it
#' # pull out the 2018-2020 life tables, separate into male and female variants
#' # then close the excel file and delete the excel file, plus the folder its in
#' library(openxlsx)
#' library(data.table)
#' tpm_fol <- tempdir()
#' download.file(
#'   url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables/current/nationallifetables3yrew1.xlsx",
#'   destfile = file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx"),
#'   mode = "wb" 
#' )
#' file_loc <- file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx")
#' wb <- openxlsx::loadWorkbook(file_loc)
#' LT <- read.xlsx(wb,sheet = "2018-2020",startRow = 6)
#' LT_ma <- LT[,1:6]
#' LT_fe <- LT[,7:12]
#' rm(LT)
#' rm(wb)
#' unlink(file_loc,recursive = TRUE)
#' unlink(tpm_fol)
#' 
#' # Use the life table to generate a gpop OS line.
#' gpop_os <- f_ce_gpopmort_simple(
#'   bl_age = 50,
#'   pr_fem = 0.5,
#'   cl_yr = 1/52,
#'   th_yr = 50,
#'   LT_ma = LT_ma,
#'   LT_fe = LT_fe,
#'   assume_die_at = 100
#' )
#' 
#' # Plot the resulting OS line
#' plot(gpop_os, type="l")
#' 
#' @export
f_ce_gpopmort_simple <- function(bl_age, cl_yr, th_yr, pr_fem, LT_ma, LT_fe, assume_die_at = 100) {
  
  # Make sure columns age and qx are present in the male and female life tables:
  stopifnot(all(c("age", "qx") %in% colnames(LT_ma)))
  stopifnot(all(c("age", "qx") %in% colnames(LT_fe)))
  
  # Make a vector of age going from baseline to time horizon
  th_cyc <- ceiling(th_yr / cl_yr) 
  age <- floor(bl_age + (0:th_cyc * cl_yr))
  
  # implement the assumption that people die at a given age, default 100 years old
  LT_ma[LT_ma$age >= assume_die_at,"qx"] <- 1
  LT_fe[LT_fe$age >= assume_die_at,"qx"] <- 1
  
  # Convert qx to per cycle from per year:
  r_ma <- -log(1-LT_ma$qx)
  r_fe <- -log(1-LT_fe$qx)
  
  qx_ma <- 1- exp(-r_ma * cl_yr)
  qx_fe <- 1- exp(-r_fe * cl_yr)
  
  # Now, the vector age can be used as the n-1th index in qx_ma and qx_fe, that
  # is, the 1st element is age 0, so the 50th is age 49 and so on.
  # 
  # What we do here then is compute the cumulative product of 1 and then 1 - survival
  # probability for each model cycle, i.e., the OS line for males and females:
  # 
  os_ma <- cumprod(c(1,1-qx_ma[age+1]))
  os_fe <- cumprod(c(1,1-qx_fe[age+1]))
  
  # Compute the % female over time
  pr_fe <- os_fe / (os_ma + os_fe)
  
  
  # Compute the weighted hazard:
  h_w <- ((pr_fe) * (1-(os_fe/shift(os_fe,fill = 1)))) + ((1-pr_fe) * (1-(os_ma/shift(os_ma,fill = 1))))
  h_w[is.na(h_w)] <- 1
  
  
  # compute age and sex adjusted general population overall survival:
  os_w <- cumprod(c(1,1-h_w[2:length(h_w)]))[1:(th_cyc+1)]
  
  # Return the age and sex weighted overall survival line
  return(os_w)
  
}


#' @title Estimate age and sex-weighted general population overall survival taking
#' into consideration the distribution of age at baseline by sex.
#' 
#' @description This function computes an age and sex-weighted overall survival (OS) line based
#' on input life tables and age distributions. 
#'
#' @param age_mean A named numeric vector of length 2 with the mean ages for males (`m`) and females (`f`).
#' @param age_sd A named numeric vector of length 2 with the standard deviations of age for males (`m`) and females (`f`).
#' @param cl_yr Cycle length in years.
#' @param th_yr Time horizon in years.
#' @param pr_fem Proportion of females in the sample. 
#' @param LT_ma Data frame for the male life table. Must include `age` and `qx` columns.
#' @param LT_fe Data frame for the female life table. Must include `age` and `qx` columns.
#' @param assume_die_at Numeric. Age at which individuals are assumed to die. Default is 100.
#' 
#' @return Numeric vector. The age and sex weighted overall survival line.
#'
#' @importFrom stats cumprod
#' @importFrom data.table shift
#'
#' @details The function performs the following steps:
#' 
#' 1. Validates input data and parameters.
#' 2. Computes the distribution of age at baseline for both sexes.
#' 3. Creates a matrix representing age over the model cycles, capped at the maximum age (default 100).
#' 4. Converts annual mortality rates to per cycle values.
#' 5. Calculates the overall survival for each baseline age using the age matrix.
#' 6. Aggregates the age- and sex-specific survival curves to get a weighted average OS line for every cycle.
#' 7. Computes the proportion of females over time and adjusts the hazards accordingly.
#' 8. Computes the age and sex-adjusted general population overall survival.
#'
#' 
#' @examples
#' 
#' 
#' # Grab the ONS life table for England and Wales from the ONS website, load it
#' # pull out the 2018-2020 life tables, separate into male and female variants
#' # then close the excel file and delete the excel file, plus the folder its in
#' library(openxlsx)
#' library(data.table)
#' tpm_fol <- tempdir()
#' download.file(
#'   url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables/current/nationallifetables3yrew1.xlsx",
#'   destfile = file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx"),
#'   mode = "wb" 
#' )
#' file_loc <- file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx")
#' wb <- openxlsx::loadWorkbook(file_loc)
#' LT <- read.xlsx(wb,sheet = "2018-2020",startRow = 6)
#' LT_ma <- LT[,1:6]
#' LT_fe <- LT[,7:12]
#' rm(LT)
#' rm(wb)
#' unlink(file_loc,recursive = TRUE)
#' unlink(tpm_fol)
#' 
#' # Mean and SD of age by sex:
#' age_mean <- structure(c(50,50), .Names=c("m","f"))
#' age_sd <- structure(c(5,5), .Names=c("m","f"))
#' 
#' 
#' # Use the life table to generate a gpop OS line.
#' gpop_os <- f_ce_gpopmort_dist(
#'   age_mean = age_mean,
#'   age_sd = age_sd,
#'   cl_yr = 1/52,
#'   th_yr = 50,
#'   LT_ma = LT_ma,
#'   LT_fe = LT_fe,
#'   assume_die_at = 100
#' )
#' 
#' # Plot the resulting OS line
#' plot(gpop_os, type="l")
#'
#' @export
f_ce_gpopmort_dist <- function(age_mean, age_sd, dist="norm", cl_yr, th_yr, pr_fem, LT_ma, LT_fe, assume_die_at = 100) {
  
  # Make sure columns age and qx are present in the male and female life tables:
  stopifnot(dist %in% c("norm", "lnorm"))
  stopifnot(all(c("age", "qx") %in% colnames(LT_ma)))
  stopifnot(all(c("age", "qx") %in% colnames(LT_fe)))
  stopifnot(all(c("m","f") %in% names(age_mean)))
  stopifnot(all(c("m","f") %in% names(age_sd)))
  
  # Make a vector of age going from baseline to time horizon
  th_cyc <- ceiling(th_yr / cl_yr) 
  
  # Simulate the distribution of age using the mean and sd provided one for each sex:
  abl <- 0:99
  if (dist == "norm") {
    cdf_m <- pnorm(abl,.subset2(age_mean,"m"),.subset2(age_sd,"m"))
  } else {
    cdf_m <- plnorm(abl,.subset2(age_mean,"m"),.subset2(age_sd,"m"))
  }
  dens_m <- cdf_m - shift(cdf_m,fill=0)
  
  if (dist == "norm") {
    cdf_f <- pnorm(abl,.subset2(age_mean,"f"),.subset2(age_sd,"f"))
  } else {
    cdf_f <- plnorm(abl,.subset2(age_mean,"f"),.subset2(age_sd,"f"))
  }
  dens_f <- cdf_f - shift(cdf_f,fill=0)
  
  # Compile a matrix of age per model cycle going from age 0 to age 99. Remember
  # that the numbers in this matrix are the INDEX in dens_m or dens_f NOT the age
  # however, that's just age+1 so its fine! (i.e., 101st element is age 100)
  age_matrix <- matrix(
    data = rep(abl,th_cyc+1),
    ncol = th_cyc+1
  ) + 
    matrix(
      data = rep(floor((0:th_cyc * cl_yr)) + 1,100),
      ncol = th_cyc+1,
      byrow = TRUE
    )
  
  # Cap age at 100
  age_matrix[age_matrix > 101] <- 101
  
  # Now we have the lookup indices we need

  # implement the assumption that people die at a given age, default 100 years old
  LT_ma[LT_ma$age >= assume_die_at,"qx"] <- 1
  LT_fe[LT_fe$age >= assume_die_at,"qx"] <- 1
  
  # Convert qx to per cycle from per year (length 101 to include age 0):
  r_ma <- -log(1-LT_ma$qx)
  r_fe <- -log(1-LT_fe$qx)
  
  qx_ma <- 1- exp(-r_ma * cl_yr)
  qx_fe <- 1- exp(-r_fe * cl_yr)
  
  # Similarly to the simple version, we make an OS line. however we make an
  # OS line for each baseline age using the age matrix, and then multiply those.
  # 
  # Now for something completely different: R internals
  # 
  # R is way faster at indexing by column than row. Like thousands of times faster.
  # Therefore, by simply transposing the age matrix and working column by column
  # instead of row by row we can process individual columns faster than if we
  # did it by row...
  # 
  # 2nd point, multiplying a matrix by a vector will multiply element-column-wise.
  # That is, the vector will multiply the first column, then the 2nd then the 3rd etc.
  # In this case, the density of age at baseline for males multiplies the matrix
  # of OS given baseline age to give the weighted OS line for each baseline age.
  # 
  # 3rd point - colSums is extremely fast in R. Therefore we add up the columns
  # to get the weighted average OS line for every cycle for males. repeat for
  # females and continue as with the simple function after this point :)
  # 
  # 
  # NOTE: the index in the lapply is baseline age +1 year (i.e. the 1st index
  # is age 0 at baseline, 2nd is age 1 etc)
  # 
  # 
  # Components:
  #  - The cumulative product of the conditional survival probability is the OS line:
  #  - The proportion at each age at basline multiplies the rows
  # 
  age_matrix <- t(age_matrix)
  os_ma <- colSums(matrix(
    data = unlist(lapply(1:ncol(age_matrix), function(bl_age_plus_one) {
      cumprod(c(1,1-qx_ma[age_matrix[,bl_age_plus_one]]))
    }),use.names = F),
    nrow = ncol(age_matrix),
    byrow = TRUE
  ) * dens_m)
  
  os_fe <- colSums(matrix(
    data = unlist(lapply(1:ncol(age_matrix), function(bl_age_plus_one) {
      cumprod(c(1,1-qx_fe[age_matrix[,bl_age_plus_one]]))
    }),use.names = F),
    nrow = ncol(age_matrix),
    byrow = TRUE
  ) * dens_f)
  
  # Compute the % female over time
  pr_fe <- os_fe / (os_ma + os_fe)
  
  
  # Compute the weighted hazard:
  h_w <- ((pr_fe) * (1-(os_fe/shift(os_fe,fill = 1)))) + ((1-pr_fe) * (1-(os_ma/shift(os_ma,fill = 1))))
  h_w[is.na(h_w)] <- 1
  
  
  # compute age and sex adjusted general population overall survival:
  os_w <- cumprod(c(1,1-h_w[2:length(h_w)]))[1:(th_cyc+1)]
  
  # Return the age and sex weighted overall survival line
  return(os_w)
  
}




# Examples ----------------------------------------------------------------



if (FALSE) {
  
  # Get male and female ONS life tables for 2018-2020 straight from the internet
  library(openxlsx)
  library(data.table)
  library(ggplot2)
  tpm_fol <- tempdir()
  download.file(
    url      = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables/current/nationallifetables3yrew1.xlsx",
    destfile = file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx"),
    mode     = "wb"
  )
  file_loc <- file.path(tpm_fol,"ONS_LIFE_TABLE.xlsx")
  wb       <- openxlsx::loadWorkbook(file_loc)
  LT       <- read.xlsx(wb,sheet = "2018-2020",startRow = 6)
  LT_ma    <- LT[,1:6]
  LT_fe    <- LT[,7:12]
  rm(LT)
  rm(wb)
  unlink(file_loc,recursive = TRUE)
  unlink(tpm_fol)
  
  # Compute simple and complex lines:
  gpop_os_simple <- f_ce_gpopmort_simple(
    bl_age        = 50,
    pr_fem        = 0.5,
    cl_yr         = 1/52,
    th_yr         = 50,
    LT_ma         = LT_ma,
    LT_fe         = LT_fe,
    assume_die_at = 100
  )
  
  # Mean and SD of age by sex:
  age_mean <- structure(c(50,50), .Names=c("m","f"))
  age_sd   <- structure(c(5,5), .Names=c("m","f"))


  # Use the life table to generate a gpop OS line.
  gpop_os_complex <- f_ce_gpopmort_dist(
    age_mean      = age_mean,
    age_sd        = age_sd,
    cl_yr         = 1/52,
    th_yr         = 50,
    LT_ma         = LT_ma,
    LT_fe         = LT_fe,
    dist          = "norm",
    assume_die_at = 100
  )
  
  plot_dat <- data.table(
    os     = c(gpop_os_simple,gpop_os_complex),
    t      = rep(0:ceiling(th_yr / cl_yr)*cl_yr,2),
    method = c(rep("No age distribution",ceiling(th_yr / cl_yr)+1),rep("Including age distribution",ceiling(th_yr / cl_yr)+1))
  )
  
  ggplot(plot_dat, aes(x  = t, y = os, colour = method)) + 
    geom_line() + 
    theme_classic() + 
    theme(legend.position = "bottom")
  
  
  # Difference in OS between the two in years (simple gives half a year overestimate!):
  (sum(gpop_os_simple) - sum(gpop_os_complex))/52
  
  # Difference in 10 year OS between the two (simple gives 0.025 years overestimate)
  (sum(gpop_os_simple[1:520]) - sum(gpop_os_complex[1:520]))/52
  
  # This matters to cost effectiveness estimates which rely on gpop OS.
  # 
  # 
  # The difference gets bigger the larger the variance is on age. At no variance
  # the two are identical, and at large variacne, the no distribution simple
  # method generates a large overestimate of OS. 
  # 
  # 
  # To prove this:
  # 
  age_sd <- structure(c(15,15), .Names=c("m","f"))
  
  
  # Use the life table to generate a gpop OS line.
  gpop_os_complex <- f_ce_gpopmort_dist(
    age_mean      = age_mean,
    age_sd        = age_sd,
    cl_yr         = 1/52,
    th_yr         = 50,
    LT_ma         = LT_ma,
    LT_fe         = LT_fe,
    dist          = "norm",
    assume_die_at = 100
  )
  
  plot_dat <- data.table(
    os     = c(gpop_os_simple,gpop_os_complex),
    t      = rep(0:ceiling(th_yr / cl_yr)*cl_yr,2),
    method = c(rep("No age distribution",ceiling(th_yr / cl_yr)+1),rep("Including age distribution",ceiling(th_yr / cl_yr)+1))
  )
  
  ggplot(plot_dat, aes(x = t, y = os, colour = method)) + 
    geom_line() + 
    theme_classic() + 
    theme(legend.position = "bottom")
  
  
  # Difference in OS between the two in years (simple gives 1.33 years overestimate!):
  (sum(gpop_os_simple) - sum(gpop_os_complex))/52
  
  # Difference in 10 year OS between the two (simple gives 0.26 years overestimate!)
  (sum(gpop_os_simple[1:520]) - sum(gpop_os_complex[1:520]))/52
  
  # Therefore, the larger the variance in age, the larger the error of assuming
  # no variance in age, which is intuitive. 
  # 
  # This is particularly relevant when simulating something like a CAR-T therapy
  # which often go to indications with young patients but with large variance.
  # 
  # In such cases, a normal distribution is not appropriate, and something like
  # a log-normal distribution should be used instead. For this reason, the
  # distribution argument has been added so you can add more distributions
  # as you see fit
  # 
  
  
}
