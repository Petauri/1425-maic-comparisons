# Functions for handling time-to-event & Kaplan-Meier data.

#' Function to make the equation object that is required to run survival analysis
#' 
#' @param time_var text of the time variable, e.g. "time" or "futime" e.t.c.
#' @param event_var text of the EVENT (NOT censor) variable, as character
#' @param strat_fac optional. stratification factors. Don't use this if you want to use the plot for extrapolations
#'
f_surv_eq_make <- function(time_var,
                           event_var,
                           covariates = NULL,
                           strat_fac = NULL) {
  
  if (all(
    is.null(strat_fac),
    is.null(covariates)
  )) {
    # Covariate unadjusted non-stratified model (time is only factor affecting survival):
    eq <- as.formula(paste0("Surv(",time_var,", ",event_var,") ~ 1"))
  } else if(all(
    !is.null(strat_fac),
    is.null(covariates)
  )) {
    # Stratified model that is not covariate adjusted
    eq <- as.formula(paste0("Surv(",time_var,", ",event_var,") ~ as.factor(", paste(strat_fac, collapse = ") + as.factor("),")"))
  } else if (all(
    is.null(strat_fac),
    !is.null(covariates)
  )) {
    # covariate adjusted model that is not stratified
    eq <- as.formula(paste0("Surv(",time_var,", ",event_var,") ~ ", paste(strat_fac, collapse = " + ")))
  } else if (all(
    !is.null(strat_fac),
    !is.null(covariates)
  )) {
    # Covariate-adjusted stratified model
    eq <-
      as.formula(paste0("Surv(",time_var,", ",event_var,") ~ as.factor(",
                        paste(strat_fac, collapse = ") + as.factor("),") + ", paste(covariates, collapse = " + ")
      )
      )
  } else {
    stop("Impossible. strat_fac and covariates must be or not be NULL!")
  }
  return(eq)
}



#' Use survminer to produce a KM plot either stratified or not.
#' 
#' @param data the data
#' @param time_var text of the time variable, e.g. "time" or "futime" e.t.c.
#' @param event_var text of the EVENT (NOT censor) variable, as character
#' @param strat_fac optional. stratification factors. Don't use this if you want to use the plot for extrapolations
#' @param save_to optional. If you put a location, the function will save the plot there. very handy!
#' @param ... additional arguments to the `ggsurvplot` call, like palette (useful for overlaying extraps)
#' 
#' @details This function takes a `data.frame` of survival data with an EVENT column
#'          which is 1 for event and 0 for censor. If you have a censor column,
#'          make an event column as `1-censor`, or if your event column is values
#'          1 and 2 instead of 0 and 1 (like the example dataset `ovarian` from
#'          the `survival` package), then `event-1` to make it 0s and 1s.
#'          
#'          Put in all the `_var` and `_fac` as strings, so that if you want to
#'          stratify by 2 different columns, then do e.g. `c("col1","col2")`
#'          to stratify by both of those columns. 
#' 
#' 
f_surv_km_plotKM <- function(data, time_var, event_var, strat_fac = NULL, save_to = NULL, ...) {
  
  requireNamespace("survival")
  requireNamespace("data.table")
  requireNamespace("survminer")
  
  # If the model is not stratified, just ~ 1 in the equation:
  eq <- f_surv_eq_make(time_var,event_var,strat_fac = strat_fac)
  
  # fit the model.
  surv_fit <- survfit(eq,data = data)
  
  # Special trick to make it work with survminer, which needs this object in the fit!
  surv_fit$call$formula <- eq
  
  # Make the plot
  p <- survminer::ggsurvplot(
    fit = surv_fit,
    data = data,
    risk.table = TRUE,
    ...
  )
  
  if (is.null(save_to)) {
    return(p)
  } else {
    ggsave(save_to, survminer:::.build_ggsurvplot(p))
    return(p)
  }
}


# Takes a data.frame of survival data with EVENT column and fits flexsurv with
# covariates and/or factors.
f_surv_fitTSD14 <-
  function(data,
           time_var,
           event_var,
           covariates = NULL,
           strat_fac = NULL,
           save_to = NULL) {
  
    requireNamespace("survival")
    requireNamespace("data.table")  
    requireNamespace("flexsurv")  
    
    # Make the equation using the function we defined at the top of this script.
    eq <- f_surv_eq_make(
      time_var = time_var,
      event_var = event_var,
      covariates = covariates,
      strat_fac = strat_fac
    )
    
    dists <- structure(
      c("gengamma","exp","weibull","lnorm","gamma","gompertz","llogis"),
      .Names=c("gengamma","exp","weibull","lnorm","gamma","gompertz","llogis")
    )
    
    # Run all the regressions, storing them in a list:
    fs_fits <- lapply(dists, function(d) {
      fit <- flexsurv::flexsurvreg(
        formula = eq,
        data = data,
        dist = d
      )
      
      # make an empty list for the output:
      out <- list()
      
      # Get the name of this distribution:
      out$di <- gsub("\\..*$","",fit$dlist$name)
      
      # get the coefficient estimates:
      out$coef <- fit$res.t[,"est"]
      
      # in case of stratifications, we need to figure out where they are (what
      # columns) and change them from as.factor(x) to x
      if (out$di == "gengamma") {
        expected_length <- 3
      } else if (out$di == "exp") {
        expected_length <- 1
      } else {
        expected_length <- 2
      }
      out$vcov <- vcov(fit)
      
      # if the length of the coef names isn't the same then there are either 
      # covariates or factors in this regression analysis
      if(length(names(out$coef)) != expected_length) {
        if (!is.null(strat_fac)) {
          # we have stratification factors. these always come before the
          # covariates in the formula and after the expected_length variables:
          
          sf_lab <- structure(strat_fac,.Names=strat_fac)
          levs_to_rename <- lapply(sf_lab, function(sf) {
            l <- levels(as.factor(data[,sf]))
            return(l[2:length(l)])
          })
          nams_to_assign <- unlist(lapply(sf_lab, function(sf_l) {
            paste0(sf_l,levs_to_rename[[sf_l]])
          }),use.names = F)
          
          # Stick the new names in in the right places. we'll do this to vcov too!
          names(out$coef)[(expected_length+1):(expected_length+length(nams_to_assign))] <- nams_to_assign
          
          dimnames(out$vcov)[[1]][(expected_length+1):(expected_length+length(nams_to_assign))] <- nams_to_assign
          dimnames(out$vcov)[[2]][(expected_length+1):(expected_length+length(nams_to_assign))] <- nams_to_assign
        }
      }      
      
      # goodness of fit
      
      out$gof <- data.table(dist = out$di, AIC = AIC(fit), BIC = BIC(fit))
      
      # return the list containing coefficients and variance-covariance
      return(out)
    })
    
    # Goodness of fit for all distributions:
    gof <- rbindlist(lapply(fs_fits, function(di) {di$gof}))
    
    return(list(
      fits = lapply(fs_fits, function(x) x[c("di", "coef", "vcov")]),
      gof  = gof
    ))
  
}


# basic maths for un-stratified, covariate unadjusted curve extrapolations
f_surv_extrap_exp <- function(v_cycles, rate) {
  output<- exp(-1*exp(rate)*v_cycles)
  output
}
f_surv_extrap_weibull <- function(v_cycles, coefs) {
  output<- 1 - pweibull(v_cycles, exp(coefs[1]), exp(coefs[2]))
  output
}
f_surv_extrap_lnorm <- function(v_cycles, coefs) {
  output<- 1 - plnorm(v_cycles, coefs[1], 1/exp(-1*coefs[2]))
  output
}
f_surv_extrap_llogis <- function(v_cycles, coefs) {
  output<- 1/(1+(v_cycles*exp(-1*coefs[2]))^(1/exp(-1*coefs[1])))
  output
}
f_surv_extrap_gengamma <- function(v_cycles, coefs) {
  pgamma <- pgamma(((-1*coefs[3])^-2)*exp(-1*coefs[3]*-((log(v_cycles)-(coefs[1]))/exp(coefs[2]))), ((-1*coefs[3])^-2), scale = 1)
  output<- if (coefs[3]<0) {pgamma} else {1-pgamma}
  output
}
f_surv_extrap_gompertz <- function(v_cycles, coefs) {
  output<- exp((-1/coefs[1])*exp(coefs[2])*(exp(coefs[1]*v_cycles)-1))
  output
}
f_surv_extrap_gamma <- function(v_cycles, coefs) {
  output<- 1-((exp(logOfGamma::gammaln(exp(coefs[1])))*pgamma(exp(coefs[2])*v_cycles,exp(coefs[1]),1))/gamma(exp(coefs[1])))
  output
}

#' Function to extrapolate all TSD14 very quickly using the above functions, only for
#' covariate unadjusted unstratified models:
f_surv_extrapTSD14_simple <- function(TSD14_fits, time_vector) {
  requireNamespace("logOfGamma")
  t  <- time_vector
  matrix(
    unlist(lapply(TSD14_fits, function(this_dist) {
      di <- this_dist$di
      co <- this_dist$coef
      
      # check the dist is one of those in TSD14 (plus gamma)
      # stopifnot(di %in% c("gengamma", "exp", "weibull", "lnorm", "gamma", "gompertz", "llogis"))
      
      switch (
        di,
        "gengamma" = f_surv_extrap_gengamma(t, co),
        "exp"      = f_surv_extrap_exp(t, co),
        "weibull"  = f_surv_extrap_weibull(t, co),
        "lnorm"    = f_surv_extrap_lnorm(t, co),
        "gamma"    = f_surv_extrap_gamma(t, co),
        "gompertz" = f_surv_extrap_gompertz(t, co),
        "llogis"   = f_surv_extrap_llogis(t, co)
      )
    }), use.names = F),
    ncol = length(TSD14_fits),
    nrow = length(t),
    dimnames = list(NULL,names(TSD14_fits))
  )
}

# A plot without stratification can have extrapolations stuck onto it with
# this function

#' Function to add a set of extrapolations to an unstratified KM plot
#' 
#' @param km_plot the result of function `f_surv_km_plotKM`
#' 
#' 
f_surv_extrap_addToKM <- function(km_plot, extraps, extrap_t, max_t = NULL,...) {
  
  requireNamespace("data.table")
  requireNamespace("ggplot2")
  requireNamespace("survminer")
  
  # manipulate the extrapolations into a dataset which can go into a ggplot
  plot_dat <- rbindlist(lapply(structure(colnames(extraps), .Names=colnames(extraps)), function(dis) {
    data.table(
      dis = dis,
      t   = extrap_t,
      s_t = as.numeric(extraps[,dis])
    )
  }))
  
  # If the user has given a smaller x max then cut off there, otherwise cut off
  # at the end of the time horizon:
  tmax <- ifelse(is.null(max_t), max(extrap_t), max_t)
  
  # update the plot component:
  km_plot$plot <- km_plot$plot + 
    geom_line(data = plot_dat, aes(x = t, y = s_t, colour = dis)) + 
    geom_vline(xintercept = 0,  alpha = 0.1) +
    geom_hline(yintercept = 1,  alpha = 0.1) +
    theme(legend.title = element_blank()) +
    coord_cartesian(xlim=c(0, tmax), clip = "off") + 
    scale_x_continuous(limits = c(0,tmax), expand = expansion(mult = c(0.05,0.05)))
  
  km_plot$table <- km_plot$table + 
    coord_cartesian(xlim=c(0, tmax), clip = "off") + 
    scale_x_continuous(limits = c(0,tmax), expand = expansion(mult = c(0.05,0.05)))
  
  # return the completed plot
  return(km_plot)
  
}




# Worked example ----------------------------------------------------------

if (FALSE) {
  
  # This function "just works" for producing the Kaplan-Meier plot with stratification.

  # note the ... argument lets me pass arbitrary stuff into the plot specification :)
  f_surv_km_plotKM(
    data = ovarian,
    time_var = "futime",
    event_var = "fustat",
    strat_fac = NULL,
    palette= 'black'
  )
  f_surv_km_plotKM(
    data = ovarian,
    time_var = "futime",
    event_var = "fustat",
    strat_fac = NULL
  )
  f_surv_km_plotKM(
    data = ovarian,
    time_var = "futime",
    event_var = "fustat",
    strat_fac = "resid.ds",
    save_to = "./output/test_plot.png"
  )
  f_surv_km_plotKM(
    data = lung %>% mutate(status = status-1, time = time / 365.25),
    time_var = "time",
    event_var = "status",
    strat_fac = NULL,
    save_to = NULL
  )
  f_surv_km_plotKM(
    data = lung %>% mutate(status = status-1),
    time_var = "time",
    event_var = "status",
    strat_fac = "ph.ecog",
    save_to = NULL
  )
  f_surv_km_plotKM(
    data = lung %>% mutate(status = status-1),
    time_var = "time",
    event_var = "status",
    strat_fac = c("sex","ph.ecog"),
    save_to = NULL
  )
  
  
  
  ex_data <- lung %>% mutate(status = status-1, time = time / 365.25)
  
  km_plot <- f_surv_km_plotKM(
    data = ex_data,
    time_var = "time",
    event_var = "status",
    strat_fac = NULL,
    save_to = NULL
  )
  
  fs_fits <- f_surv_fitTSD14(ex_data, "time", "status")
  
  extrap_t <- 0:5000 / 365.25
  
  fs_extraps <- f_surv_extrapTSD14_simple(
    TSD14_fits = fs_fits$fits,
    time_vector = extrap_t
  )
  
  p <- f_surv_extrap_addToKM(
    km_plot = km_plot,
    extraps = fs_extraps,
    extrap_t = extrap_t,
    max_t = 7
  )
  
  # You can then edit the risk table or plot individually
  p$plot <- p$plot  + 
    scale_x_continuous(breaks = 0:7)
  
  p$table <- p$table  + 
    scale_x_continuous(limits = c(0,7),breaks = 0:7)
  
  p
  
  # Stand-alone example to make a KM:
  library(survival)
  library(survminer)
  eq <- Surv(futime, fustat) ~ rx
  surv_fit <- survfit(eq,data = ovarian)
  surv_fit$call$formula <- eq
  
  survminer::ggsurvplot(
    fit = surv_fit,
    data = ovarian,
    risk.table = TRUE
  )
  
  
  # Note that ggflexsurvplot is vulnerable to the same issue, but there's an
  # open issue in survminer that ggflexsurvplot doesn't let you stratify:
  eq <- Surv(futime, fustat) ~ as.factor(rx)
  fs_fit <- flexsurv::flexsurvreg(eq,data = ovarian,dist = "exp")
  fs_fit$call$formula <- eq
  survminer::ggflexsurvplot(
    fit = fs_fit,
    data = ovarian,
    summary.flexsurv = summary(fs_fit),
    risk.table = TRUE
  )
  
  # Works fine when not stratified
  eq <- Surv(futime, fustat) ~ 1
  fs_fit <- flexsurv::flexsurvreg(eq,data = ovarian,dist = "gengamma")
  fs_fit$call$formula <- eq
  survminer::ggflexsurvplot(
    fit = fs_fit,
    data = ovarian,
    summary.flexsurv = summary(fs_fit),
    risk.table = TRUE
  )
  
  
  
}