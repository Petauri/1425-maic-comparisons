# Function to extract parameters from models 

library(tidyverse)

f_model_extract <- function(survival_formula, df, template, model_type, arms) {
  
  #' Extract model parameters (AIC, BIC, Coefs, V-covs) from survival models ready for CE modelling
  #'
  #' @description This function fills in a template of model parameters for standard models or spline models.
  #' 
  #' survival_formula is self explanatory. e.g., survival_formula <- Surv(os_months, os_event) ~ 1
  #' 
  #' df is the dataframe in question 
  #' 
  #' template is a df that is going to be filled in. Speak to Rachael or Kurt if unsure.
  #' 
  #' model_type - either "simple" or "spline"
  #' 
  #' arms - "single" only option for now - look to update this in future for multi-arm studies. 
  #' 
  #' @details Author: Kurt Taylor, Delta Hat
  
  # Error catching for models 
  
  f_tcatch_dnc <- function(code) {
    tryCatch({
      eval(code)
    }, 
    warning = function(warning) {"Did not converge"}, error = function(error) {"Did not converge"})
  }
  
  # Model type 
  
  if(model_type == "simple" & arms == "one") {
    
    message("Running simple parametric models:
            1 = exponential, 
            2 = generalised gamma, 
            3 = gompertz, 
            4 = log logistic, 
            5 = log normal, 
            6 = weibull, 
            7 = gamma")
    
    # Build the model list 
    
    model_list <- list()
    
    model_list <- list(
      parametric_exp = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "exp")),
      parametric_gengamma = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gengamma")),
      parametric_gompertz = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gompertz")),
      parametric_llogis = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "llogis")),
      parametric_lnorm = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "lnorm")),
      parametric_weibull = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "weibull")),
      parametric_gamma = f_tcatch_dnc(flexsurvreg(survival_formula, data = df, dist = "gamma"))
    )
    
    # Create a dataframe with columns "Model", "AIC", and "BIC" filled with NA values ready for extraction 
    
    models <- c("Exponential", "Generalized gamma", "Gompertz", "Log-logistic", "Log-normal", "Weibull", "Gamma")
    num_models <- length(models)
    
    aic_bic <- data.frame(Model = rep(models, each = 1),
                          AIC = rep(NA, num_models * 1),
                          BIC = rep(NA, num_models * 1))
    
    # Lists that we will populate with coefs and vcov
    
    aicbicparams <- list()
    vcov_matrix <- list()
    
    # Loop to extract AIC, BIC, coefs and vcovs from the  models 
    
    for (p in 1:length(model_list)) {
      
      extract <- model_list[[p]]
      
      if(extract[[1]] == "Did not converge") {
        
        aic_bic[p, 2:3] <- c(999999, 999999)
        aicbicparams[[p]] <- data.frame("shape" = 999999, "scale" = 999999, "mu" = 999999, "sigma" = 999999, "Q" = 999999, "rate" = 999999, "meanlog" = 999999, "sdlog" = 999999)
        row_names <- col_names <- c("gamma0", "gamma1", "gamma2", "gamma3", "gamma4")
        vcov_matrix[[p]] <- matrix(999999, nrow = length(row_names), ncol = length(col_names), 
                                   dimnames = list(row_names, col_names))
        
      } else {
        
        aic_bic[p, 2:3] <- c(AIC(extract), BIC(extract))
        aicbicparams[[p]] <- extract$coef
        vcov_matrix[[p]] <- vcov(extract)
        
      }
      
    }
    
    # First add AIC and BIC to the template by joining
    
    template_filled <- template %>%
      dplyr::select(-c(AIC, BIC)) %>%
      left_join(aic_bic)
    
    # Then coefs using ifelse statements which are explicit (rather than using cell numbers which are prone to errors if things change)
    
    template_filled <- template_filled %>%
      mutate(Coef = ifelse(Model == "Exponential" & Parameter == "rate", aicbicparams[[1]],
                           ifelse(Model == "Generalized gamma" & Parameter == "mu", aicbicparams[[2]]["mu"],
                                  ifelse(Model == "Generalized gamma" & Parameter == "sigma", aicbicparams[[2]]["sigma"],
                                         ifelse(Model == "Generalized gamma" & Parameter == "Q", aicbicparams[[2]]["Q"],
                                                ifelse(Model == "Gompertz" & Parameter == "shape", aicbicparams[[3]]["shape"],
                                                       ifelse(Model == "Gompertz" & Parameter == "rate", aicbicparams[[3]]["rate"],
                                                              ifelse(Model == "Log-logistic" & Parameter == "shape", aicbicparams[[4]]["shape"],
                                                                     ifelse(Model == "Log-logistic" & Parameter == "scale", aicbicparams[[4]]["scale"],
                                                                            ifelse(Model == "Log-normal" & Parameter == "meanlog", aicbicparams[[5]]["meanlog"],
                                                                                   ifelse(Model == "Log-normal" & Parameter == "sdlog", aicbicparams[[5]]["sdlog"],
                                                                                          ifelse(Model == "Weibull" & Parameter == "shape", aicbicparams[[6]]["shape"],
                                                                                                 ifelse(Model == "Weibull" & Parameter == "scale", aicbicparams[[6]]["scale"],
                                                                                                        ifelse(Model == "Gamma" & Parameter == "shape", aicbicparams[[7]]["shape"],
                                                                                                               ifelse(Model == "Gamma" & Parameter == "rate", aicbicparams[[7]]["rate"],
                                                                                                                      NA)))))))))))))))
    
    
    # V-Cov - 1
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-1` = ifelse(Model == "Exponential" & Parameter == "rate", vcov_matrix[[1]],
                                    ifelse(Model == "Generalized gamma" & Parameter == "mu", vcov_matrix[[2]][1],
                                           ifelse(Model == "Generalized gamma" & Parameter == "sigma", vcov_matrix[[2]][2],
                                                  ifelse(Model == "Generalized gamma" & Parameter == "Q", vcov_matrix[[2]][3],
                                                         ifelse(Model == "Gompertz" & Parameter == "shape", vcov_matrix[[3]][1],
                                                                ifelse(Model == "Gompertz" & Parameter == "rate", vcov_matrix[[3]][2],
                                                                       ifelse(Model == "Log-logistic" & Parameter == "shape", vcov_matrix[[4]][1],
                                                                              ifelse(Model == "Log-logistic" & Parameter == "scale", vcov_matrix[[4]][2],
                                                                                     ifelse(Model == "Log-normal" & Parameter == "meanlog", vcov_matrix[[5]][1],
                                                                                            ifelse(Model == "Log-normal" & Parameter == "sdlog", vcov_matrix[[5]][2],
                                                                                                   ifelse(Model == "Weibull" & Parameter == "shape", vcov_matrix[[6]][1],
                                                                                                          ifelse(Model == "Weibull" & Parameter == "scale", vcov_matrix[[6]][2],
                                                                                                                 ifelse(Model == "Gamma" & Parameter == "shape", vcov_matrix[[7]][1],
                                                                                                                        ifelse(Model == "Gamma" & Parameter == "rate", vcov_matrix[[7]][2],
                                                                                                                               NA)))))))))))))))
    
    # V-Cov - 2
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-2` = ifelse(Model == "Generalized gamma" & Parameter == "mu", vcov_matrix[[2]][4],
                                    ifelse(Model == "Generalized gamma" & Parameter == "sigma", vcov_matrix[[2]][5],
                                           ifelse(Model == "Generalized gamma" & Parameter == "Q", vcov_matrix[[2]][6],
                                                  ifelse(Model == "Gompertz" & Parameter == "shape", vcov_matrix[[3]][3],
                                                         ifelse(Model == "Gompertz" & Parameter == "rate", vcov_matrix[[3]][4],
                                                                ifelse(Model == "Log-logistic" & Parameter == "shape", vcov_matrix[[4]][3],
                                                                       ifelse(Model == "Log-logistic" & Parameter == "scale", vcov_matrix[[4]][4],
                                                                              ifelse(Model == "Log-normal" & Parameter == "meanlog", vcov_matrix[[5]][3],
                                                                                     ifelse(Model == "Log-normal" & Parameter == "sdlog", vcov_matrix[[5]][4],
                                                                                            ifelse(Model == "Weibull" & Parameter == "shape", vcov_matrix[[6]][3],
                                                                                                   ifelse(Model == "Weibull" & Parameter == "scale", vcov_matrix[[6]][4],
                                                                                                          ifelse(Model == "Gamma" & Parameter == "shape", vcov_matrix[[7]][3],
                                                                                                                 ifelse(Model == "Gamma" & Parameter == "rate", vcov_matrix[[7]][4],
                                                                                                                        NA))))))))))))))
    
    # V-Cov - 3
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-3` = ifelse(Model == "Generalized gamma" & Parameter == "mu", vcov_matrix[[2]][7],
                                    ifelse(Model == "Generalized gamma" & Parameter == "sigma", vcov_matrix[[2]][8],
                                           ifelse(Model == "Generalized gamma" & Parameter == "Q", vcov_matrix[[2]][9],
                                                  NA))))
    
  } else if(model_type == "spline" & arms == "one") {
    
    message("Running spline models:
            1 = hazard 1 knot, 
            2 = hazard 2 knot, 
            3 = hazard 3 knot, 
            4 = odds 1 knot, 
            5 = odds 2 knot, 
            6 = odds 3 knot,            
            7 = normal 1 knot, 
            8 = normal 2 knot, 
            9 = normal 3 knot")
    
    # Build the model list 
    
    model_list <- list()
    
    model_list <- list(
      onekhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "hazard")),
      twokhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "hazard")),
      threekhaz = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "hazard")),
      onekodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "odds")),
      twokodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "odds")),
      threekodd = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "odds")),
      oneknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 1, scale = "normal")),
      twoknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 2, scale = "normal")),
      threeknorm = f_tcatch_dnc(flexsurvspline(survival_formula, data = df, k = 3, scale = "normal"))
    )
    
    # Create a dataframe with columns "Model", "AIC", and "BIC" filled with NA values ready for extraction 
    
    models <- c("hazard 1 knot", "hazard 2 knot", "hazard 3 knot",
                "odds 1 knot", "odds 2 knot", "odds 3 knot",
                "normal 1 knot", "normal 2 knot", "normal 3 knot")
    
    num_models <- length(models)
    
    aic_bic <- data.frame(Model = rep(models, each = 1),
                          AIC = rep(NA, num_models * 1),
                          BIC = rep(NA, num_models * 1))
    
    knots <- list()
    aicbicparams <- list()
    vcov_matrix <- list()
    
    for (p in 1:length(model_list)) {
      
      extract <- model_list[[p]]
      
      if(extract[[1]] == "Did not converge") {
        
        aic_bic[p, 2:3] <- c(999999, 999999)
        knots[[p]] <- data.frame(" " = 999999, " " = 999999, " " = 999999, " " = 999999, " " = 999999)
        coefs <- data.frame("Parameter" = 999999, "Coef" = 999999)
        aicbicparams[[p]] <- coefs[rep(seq_len(nrow(coefs)), each = 5), ]
        row_names <- col_names <- c("gamma0", "gamma1", "gamma2", "gamma3", "gamma4")
        vcov_matrix[[p]] <- matrix(999999, nrow = length(row_names), ncol = length(col_names), 
                                   dimnames = list(row_names, col_names))
        
      } else {
        
        aic_bic[p, 2:3] <- c(AIC(extract), BIC(extract))
        knots[[p]] <- extract$knots
        aicbicparams[[p]] <- data.frame("Parameter" = c(rownames(data.frame(coef(model_list[[p]])))), "Coef" = (coef(model_list[[p]])))
        vcov_matrix[[p]] <- extract$cov
        
      }
      
    }
    
    # First add AIC and BIC to the template by joining
    
    template_filled <- template %>%
      dplyr::select(-c(AIC, BIC)) %>%
      left_join(aic_bic)
    
    # COEFS - HAZ
    
    template_filled <- template_filled %>%
      mutate(Coef = ifelse(Model == "hazard 1 knot" & Parameter == 1, aicbicparams[[1]]$Coef[1],
                           ifelse(Model == "hazard 1 knot" & Parameter == 2, aicbicparams[[1]]$Coef[2],
                                  ifelse(Model == "hazard 1 knot" & Parameter == 3, aicbicparams[[1]]$Coef[3],
                                         ifelse(Model == "hazard 2 knot" & Parameter == 1, aicbicparams[[2]]$Coef[1],
                                                ifelse(Model == "hazard 2 knot" & Parameter == 2, aicbicparams[[2]]$Coef[2],
                                                       ifelse(Model == "hazard 2 knot" & Parameter == 3, aicbicparams[[2]]$Coef[3],
                                                              ifelse(Model == "hazard 2 knot" & Parameter == 4, aicbicparams[[2]]$Coef[4],
                                                                     ifelse(Model == "hazard 3 knot" & Parameter == 1, aicbicparams[[3]]$Coef[1],
                                                                            ifelse(Model == "hazard 3 knot" & Parameter == 2, aicbicparams[[3]]$Coef[2],
                                                                                   ifelse(Model == "hazard 3 knot" & Parameter == 3, aicbicparams[[3]]$Coef[3],
                                                                                          ifelse(Model == "hazard 3 knot" & Parameter == 4, aicbicparams[[3]]$Coef[4],
                                                                                                 ifelse(Model == "hazard 3 knot" & Parameter == 5, aicbicparams[[3]]$Coef[5],
                                                                                                        NA)))))))))))))
    
    # COEFS - ODDS 
    
    template_filled <- template_filled %>%
      mutate(Coef = ifelse(Model == "odds 1 knot" & Parameter == 1, aicbicparams[[4]]$Coef[1],
                           ifelse(Model == "odds 1 knot" & Parameter == 2, aicbicparams[[4]]$Coef[2],
                                  ifelse(Model == "odds 1 knot" & Parameter == 3, aicbicparams[[4]]$Coef[3],
                                         ifelse(Model == "odds 2 knot" & Parameter == 1, aicbicparams[[5]]$Coef[1],
                                                ifelse(Model == "odds 2 knot" & Parameter == 2, aicbicparams[[5]]$Coef[2],
                                                       ifelse(Model == "odds 2 knot" & Parameter == 3, aicbicparams[[5]]$Coef[3],
                                                              ifelse(Model == "odds 2 knot" & Parameter == 4, aicbicparams[[5]]$Coef[4],
                                                                     ifelse(Model == "odds 3 knot" & Parameter == 1, aicbicparams[[6]]$Coef[1],
                                                                            ifelse(Model == "odds 3 knot" & Parameter == 2, aicbicparams[[6]]$Coef[2],
                                                                                   ifelse(Model == "odds 3 knot" & Parameter == 3, aicbicparams[[6]]$Coef[3],
                                                                                          ifelse(Model == "odds 3 knot" & Parameter == 4, aicbicparams[[6]]$Coef[4],
                                                                                                 ifelse(Model == "odds 3 knot" & Parameter == 5, aicbicparams[[6]]$Coef[5],
                                                                                                        Coef)))))))))))))
    
    # COEFS - NORMAL 
    
    template_filled <- template_filled %>%
      mutate(Coef = ifelse(Model == "normal 1 knot" & Parameter == 1, aicbicparams[[7]]$Coef[1],
                           ifelse(Model == "normal 1 knot" & Parameter == 2, aicbicparams[[7]]$Coef[2],
                                  ifelse(Model == "normal 1 knot" & Parameter == 3, aicbicparams[[7]]$Coef[3],
                                         ifelse(Model == "normal 2 knot" & Parameter == 1, aicbicparams[[8]]$Coef[1],
                                                ifelse(Model == "normal 2 knot" & Parameter == 2, aicbicparams[[8]]$Coef[2],
                                                       ifelse(Model == "normal 2 knot" & Parameter == 3, aicbicparams[[8]]$Coef[3],
                                                              ifelse(Model == "normal 2 knot" & Parameter == 4, aicbicparams[[8]]$Coef[4],
                                                                     ifelse(Model == "normal 3 knot" & Parameter == 1, aicbicparams[[9]]$Coef[1],
                                                                            ifelse(Model == "normal 3 knot" & Parameter == 2, aicbicparams[[9]]$Coef[2],
                                                                                   ifelse(Model == "normal 3 knot" & Parameter == 3, aicbicparams[[9]]$Coef[3],
                                                                                          ifelse(Model == "normal 3 knot" & Parameter == 4, aicbicparams[[9]]$Coef[4],
                                                                                                 ifelse(Model == "normal 3 knot" & Parameter == 5, aicbicparams[[9]]$Coef[5],
                                                                                                        Coef)))))))))))))
    
    # KNOTS - HAZ
    
    template_filled <- template_filled %>%
      mutate(Knots = ifelse(Model == "hazard 1 knot" & Parameter == 1, knots[[1]][1],
                            ifelse(Model == "hazard 1 knot" & Parameter == 2, knots[[1]][2],
                                   ifelse(Model == "hazard 1 knot" & Parameter == 3, knots[[1]][3],
                                          ifelse(Model == "hazard 2 knot" & Parameter == 1, knots[[2]][1],
                                                 ifelse(Model == "hazard 2 knot" & Parameter == 2, knots[[2]][2],
                                                        ifelse(Model == "hazard 2 knot" & Parameter == 3, knots[[2]][3],
                                                               ifelse(Model == "hazard 2 knot" & Parameter == 4, knots[[2]][4],
                                                                      ifelse(Model == "hazard 3 knot" & Parameter == 1, knots[[3]][1],
                                                                             ifelse(Model == "hazard 3 knot" & Parameter == 2, knots[[3]][2],
                                                                                    ifelse(Model == "hazard 3 knot" & Parameter == 3, knots[[3]][3],
                                                                                           ifelse(Model == "hazard 3 knot" & Parameter == 4, knots[[3]][4],
                                                                                                  ifelse(Model == "hazard 3 knot" & Parameter == 5, knots[[3]][5],
                                                                                                         NA)))))))))))))
    
    # KNOTS - ODDS 
    
    template_filled <- template_filled %>%
      mutate(Knots = ifelse(Model == "odds 1 knot" & Parameter == 1, knots[[4]][1],
                            ifelse(Model == "odds 1 knot" & Parameter == 2, knots[[4]][2],
                                   ifelse(Model == "odds 1 knot" & Parameter == 3, knots[[4]][3],
                                          ifelse(Model == "odds 2 knot" & Parameter == 1, knots[[5]][1],
                                                 ifelse(Model == "odds 2 knot" & Parameter == 2, knots[[5]][2],
                                                        ifelse(Model == "odds 2 knot" & Parameter == 3, knots[[5]][3],
                                                               ifelse(Model == "odds 2 knot" & Parameter == 4, knots[[5]][4],
                                                                      ifelse(Model == "odds 3 knot" & Parameter == 1, knots[[6]][1],
                                                                             ifelse(Model == "odds 3 knot" & Parameter == 2, knots[[6]][2],
                                                                                    ifelse(Model == "odds 3 knot" & Parameter == 3, knots[[6]][3],
                                                                                           ifelse(Model == "odds 3 knot" & Parameter == 4, knots[[6]][4],
                                                                                                  ifelse(Model == "odds 3 knot" & Parameter == 5, knots[[6]][5],
                                                                                                         Knots)))))))))))))
    
    # KNOTS - NORMAL
    
    template_filled <- template_filled %>%
      mutate(Knots = ifelse(Model == "normal 1 knot" & Parameter == 1, knots[[7]][1],
                            ifelse(Model == "normal 1 knot" & Parameter == 2, knots[[7]][2],
                                   ifelse(Model == "normal 1 knot" & Parameter == 3, knots[[7]][3],
                                          ifelse(Model == "normal 2 knot" & Parameter == 1, knots[[8]][1],
                                                 ifelse(Model == "normal 2 knot" & Parameter == 2, knots[[8]][2],
                                                        ifelse(Model == "normal 2 knot" & Parameter == 3, knots[[8]][3],
                                                               ifelse(Model == "normal 2 knot" & Parameter == 4, knots[[8]][4],
                                                                      ifelse(Model == "normal 3 knot" & Parameter == 1, knots[[9]][1],
                                                                             ifelse(Model == "normal 3 knot" & Parameter == 2, knots[[9]][2],
                                                                                    ifelse(Model == "normal 3 knot" & Parameter == 3, knots[[9]][3],
                                                                                           ifelse(Model == "normal 3 knot" & Parameter == 4, knots[[9]][4],
                                                                                                  ifelse(Model == "normal 3 knot" & Parameter == 5, knots[[9]][5],
                                                                                                         Knots)))))))))))))
    
    # HAZARD - V-Cov - 1
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-1` = ifelse(Model == "hazard 1 knot" & Parameter == 1, vcov_matrix[[1]]["gamma0", "gamma0"],
                                    ifelse(Model == "hazard 1 knot" & Parameter == 2, vcov_matrix[[1]]["gamma1", "gamma0"],
                                           ifelse(Model == "hazard 1 knot" & Parameter == 3, vcov_matrix[[1]]["gamma2", "gamma0"],
                                                  ifelse(Model == "hazard 2 knot" & Parameter == 1, vcov_matrix[[2]]["gamma0", "gamma0"],
                                                         ifelse(Model == "hazard 2 knot" & Parameter == 2, vcov_matrix[[2]]["gamma1", "gamma0"],
                                                                ifelse(Model == "hazard 2 knot" & Parameter == 3, vcov_matrix[[2]]["gamma2", "gamma0"],
                                                                       ifelse(Model == "hazard 2 knot" & Parameter == 4, vcov_matrix[[2]]["gamma3", "gamma0"],
                                                                              ifelse(Model == "hazard 3 knot" & Parameter == 1, vcov_matrix[[3]]["gamma0", "gamma0"],
                                                                                     ifelse(Model == "hazard 3 knot" & Parameter == 2, vcov_matrix[[3]]["gamma1", "gamma0"],
                                                                                            ifelse(Model == "hazard 3 knot" & Parameter == 3, vcov_matrix[[3]]["gamma2", "gamma0"],
                                                                                                   ifelse(Model == "hazard 3 knot" & Parameter == 4, vcov_matrix[[3]]["gamma3", "gamma0"],
                                                                                                          ifelse(Model == "hazard 3 knot" & Parameter == 5, vcov_matrix[[3]]["gamma4", "gamma0"],
                                                                                                                 NA)))))))))))))
    
    # ODDS - V-Cov - 1
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-1` = ifelse(Model == "odds 1 knot" & Parameter == 1, vcov_matrix[[4]]["gamma0", "gamma0"],
                                    ifelse(Model == "odds 1 knot" & Parameter == 2, vcov_matrix[[4]]["gamma1", "gamma0"],
                                           ifelse(Model == "odds 1 knot" & Parameter == 3, vcov_matrix[[4]]["gamma2", "gamma0"],
                                                  ifelse(Model == "odds 2 knot" & Parameter == 1, vcov_matrix[[5]]["gamma0", "gamma0"],
                                                         ifelse(Model == "odds 2 knot" & Parameter == 2, vcov_matrix[[5]]["gamma1", "gamma0"],
                                                                ifelse(Model == "odds 2 knot" & Parameter == 3, vcov_matrix[[5]]["gamma2", "gamma0"],
                                                                       ifelse(Model == "odds 2 knot" & Parameter == 4, vcov_matrix[[5]]["gamma3", "gamma0"],
                                                                              ifelse(Model == "odds 3 knot" & Parameter == 1, vcov_matrix[[6]]["gamma0", "gamma0"],
                                                                                     ifelse(Model == "odds 3 knot" & Parameter == 2, vcov_matrix[[6]]["gamma1", "gamma0"],
                                                                                            ifelse(Model == "odds 3 knot" & Parameter == 3, vcov_matrix[[6]]["gamma2", "gamma0"],
                                                                                                   ifelse(Model == "odds 3 knot" & Parameter == 4, vcov_matrix[[6]]["gamma3", "gamma0"],
                                                                                                          ifelse(Model == "odds 3 knot" & Parameter == 5, vcov_matrix[[6]]["gamma4", "gamma0"],
                                                                                                                 `Var-covar-1`)))))))))))))
    
    # NORMAL - V-Cov - 1
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-1` = ifelse(Model == "normal 1 knot" & Parameter == 1, vcov_matrix[[7]]["gamma0", "gamma0"],
                                    ifelse(Model == "normal 1 knot" & Parameter == 2, vcov_matrix[[7]]["gamma1", "gamma0"],
                                           ifelse(Model == "normal 1 knot" & Parameter == 3, vcov_matrix[[7]]["gamma2", "gamma0"],
                                                  ifelse(Model == "normal 2 knot" & Parameter == 1, vcov_matrix[[8]]["gamma0", "gamma0"],
                                                         ifelse(Model == "normal 2 knot" & Parameter == 2, vcov_matrix[[8]]["gamma1", "gamma0"],
                                                                ifelse(Model == "normal 2 knot" & Parameter == 3, vcov_matrix[[8]]["gamma2", "gamma0"],
                                                                       ifelse(Model == "normal 2 knot" & Parameter == 4, vcov_matrix[[8]]["gamma3", "gamma0"],
                                                                              ifelse(Model == "normal 3 knot" & Parameter == 1, vcov_matrix[[9]]["gamma0", "gamma0"],
                                                                                     ifelse(Model == "normal 3 knot" & Parameter == 2, vcov_matrix[[9]]["gamma1", "gamma0"],
                                                                                            ifelse(Model == "normal 3 knot" & Parameter == 3, vcov_matrix[[9]]["gamma2", "gamma0"],
                                                                                                   ifelse(Model == "normal 3 knot" & Parameter == 4, vcov_matrix[[9]]["gamma3", "gamma0"],
                                                                                                          ifelse(Model == "normal 3 knot" & Parameter == 5, vcov_matrix[[9]]["gamma4", "gamma0"],
                                                                                                                 `Var-covar-1`)))))))))))))
    
    # HAZARD - V-Cov - 2
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-2` = ifelse(Model == "hazard 1 knot" & Parameter == 1, vcov_matrix[[1]]["gamma0", "gamma1"],
                                    ifelse(Model == "hazard 1 knot" & Parameter == 2, vcov_matrix[[1]]["gamma1", "gamma1"],
                                           ifelse(Model == "hazard 1 knot" & Parameter == 3, vcov_matrix[[1]]["gamma2", "gamma1"],
                                                  ifelse(Model == "hazard 2 knot" & Parameter == 1, vcov_matrix[[2]]["gamma0", "gamma1"],
                                                         ifelse(Model == "hazard 2 knot" & Parameter == 2, vcov_matrix[[2]]["gamma1", "gamma1"],
                                                                ifelse(Model == "hazard 2 knot" & Parameter == 3, vcov_matrix[[2]]["gamma2", "gamma1"],
                                                                       ifelse(Model == "hazard 2 knot" & Parameter == 4, vcov_matrix[[2]]["gamma3", "gamma1"],
                                                                              ifelse(Model == "hazard 3 knot" & Parameter == 1, vcov_matrix[[3]]["gamma0", "gamma1"],
                                                                                     ifelse(Model == "hazard 3 knot" & Parameter == 2, vcov_matrix[[3]]["gamma1", "gamma1"],
                                                                                            ifelse(Model == "hazard 3 knot" & Parameter == 3, vcov_matrix[[3]]["gamma2", "gamma1"],
                                                                                                   ifelse(Model == "hazard 3 knot" & Parameter == 4, vcov_matrix[[3]]["gamma3", "gamma1"],
                                                                                                          ifelse(Model == "hazard 3 knot" & Parameter == 5, vcov_matrix[[3]]["gamma4", "gamma1"],
                                                                                                                 NA)))))))))))))
    
    # ODDS - V-Cov - 2
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-2` = ifelse(Model == "odds 1 knot" & Parameter == 1, vcov_matrix[[4]]["gamma0", "gamma1"],
                                    ifelse(Model == "odds 1 knot" & Parameter == 2, vcov_matrix[[4]]["gamma1", "gamma1"],
                                           ifelse(Model == "odds 1 knot" & Parameter == 3, vcov_matrix[[4]]["gamma2", "gamma1"],
                                                  ifelse(Model == "odds 2 knot" & Parameter == 1, vcov_matrix[[5]]["gamma0", "gamma1"],
                                                         ifelse(Model == "odds 2 knot" & Parameter == 2, vcov_matrix[[5]]["gamma1", "gamma1"],
                                                                ifelse(Model == "odds 2 knot" & Parameter == 3, vcov_matrix[[5]]["gamma2", "gamma1"],
                                                                       ifelse(Model == "odds 2 knot" & Parameter == 4, vcov_matrix[[5]]["gamma3", "gamma1"],
                                                                              ifelse(Model == "odds 3 knot" & Parameter == 1, vcov_matrix[[6]]["gamma0", "gamma1"],
                                                                                     ifelse(Model == "odds 3 knot" & Parameter == 2, vcov_matrix[[6]]["gamma1", "gamma1"],
                                                                                            ifelse(Model == "odds 3 knot" & Parameter == 3, vcov_matrix[[6]]["gamma2", "gamma1"],
                                                                                                   ifelse(Model == "odds 3 knot" & Parameter == 4, vcov_matrix[[6]]["gamma3", "gamma1"],
                                                                                                          ifelse(Model == "odds 3 knot" & Parameter == 5, vcov_matrix[[6]]["gamma4", "gamma1"],
                                                                                                                 `Var-covar-2`)))))))))))))
    
    # NORMAL - V-Cov - 2
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-2` = ifelse(Model == "normal 1 knot" & Parameter == 1, vcov_matrix[[7]]["gamma0", "gamma1"],
                                    ifelse(Model == "normal 1 knot" & Parameter == 2, vcov_matrix[[7]]["gamma1", "gamma1"],
                                           ifelse(Model == "normal 1 knot" & Parameter == 3, vcov_matrix[[7]]["gamma2", "gamma1"],
                                                  ifelse(Model == "normal 2 knot" & Parameter == 1, vcov_matrix[[8]]["gamma0", "gamma1"],
                                                         ifelse(Model == "normal 2 knot" & Parameter == 2, vcov_matrix[[8]]["gamma1", "gamma1"],
                                                                ifelse(Model == "normal 2 knot" & Parameter == 3, vcov_matrix[[8]]["gamma2", "gamma1"],
                                                                       ifelse(Model == "normal 2 knot" & Parameter == 4, vcov_matrix[[8]]["gamma3", "gamma1"],
                                                                              ifelse(Model == "normal 3 knot" & Parameter == 1, vcov_matrix[[9]]["gamma0", "gamma1"],
                                                                                     ifelse(Model == "normal 3 knot" & Parameter == 2, vcov_matrix[[9]]["gamma1", "gamma1"],
                                                                                            ifelse(Model == "normal 3 knot" & Parameter == 3, vcov_matrix[[9]]["gamma2", "gamma1"],
                                                                                                   ifelse(Model == "normal 3 knot" & Parameter == 4, vcov_matrix[[9]]["gamma3", "gamma1"],
                                                                                                          ifelse(Model == "normal 3 knot" & Parameter == 5, vcov_matrix[[9]]["gamma4", "gamma1"],
                                                                                                                 `Var-covar-2`)))))))))))))
    
    # HAZARD - V-Cov - 3
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-3` = ifelse(Model == "hazard 1 knot" & Parameter == 1, vcov_matrix[[1]]["gamma0", "gamma2"],
                                    ifelse(Model == "hazard 1 knot" & Parameter == 2, vcov_matrix[[1]]["gamma1", "gamma2"],
                                           ifelse(Model == "hazard 1 knot" & Parameter == 3, vcov_matrix[[1]]["gamma2", "gamma2"],
                                                  ifelse(Model == "hazard 2 knot" & Parameter == 1, vcov_matrix[[2]]["gamma0", "gamma2"],
                                                         ifelse(Model == "hazard 2 knot" & Parameter == 2, vcov_matrix[[2]]["gamma1", "gamma2"],
                                                                ifelse(Model == "hazard 2 knot" & Parameter == 3, vcov_matrix[[2]]["gamma2", "gamma2"],
                                                                       ifelse(Model == "hazard 2 knot" & Parameter == 4, vcov_matrix[[2]]["gamma3", "gamma2"],
                                                                              ifelse(Model == "hazard 3 knot" & Parameter == 1, vcov_matrix[[3]]["gamma0", "gamma2"],
                                                                                     ifelse(Model == "hazard 3 knot" & Parameter == 2, vcov_matrix[[3]]["gamma1", "gamma2"],
                                                                                            ifelse(Model == "hazard 3 knot" & Parameter == 3, vcov_matrix[[3]]["gamma2", "gamma2"],
                                                                                                   ifelse(Model == "hazard 3 knot" & Parameter == 4, vcov_matrix[[3]]["gamma3", "gamma2"],
                                                                                                          ifelse(Model == "hazard 3 knot" & Parameter == 5, vcov_matrix[[3]]["gamma4", "gamma2"],
                                                                                                                 NA)))))))))))))
    
    # ODDS - V-Cov - 3
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-3` = ifelse(Model == "odds 1 knot" & Parameter == 1, vcov_matrix[[4]]["gamma0", "gamma2"],
                                    ifelse(Model == "odds 1 knot" & Parameter == 2, vcov_matrix[[4]]["gamma1", "gamma2"],
                                           ifelse(Model == "odds 1 knot" & Parameter == 3, vcov_matrix[[4]]["gamma2", "gamma2"],
                                                  ifelse(Model == "odds 2 knot" & Parameter == 1, vcov_matrix[[5]]["gamma0", "gamma2"],
                                                         ifelse(Model == "odds 2 knot" & Parameter == 2, vcov_matrix[[5]]["gamma1", "gamma2"],
                                                                ifelse(Model == "odds 2 knot" & Parameter == 3, vcov_matrix[[5]]["gamma2", "gamma2"],
                                                                       ifelse(Model == "odds 2 knot" & Parameter == 4, vcov_matrix[[5]]["gamma3", "gamma2"],
                                                                              ifelse(Model == "odds 3 knot" & Parameter == 1, vcov_matrix[[6]]["gamma0", "gamma2"],
                                                                                     ifelse(Model == "odds 3 knot" & Parameter == 2, vcov_matrix[[6]]["gamma1", "gamma2"],
                                                                                            ifelse(Model == "odds 3 knot" & Parameter == 3, vcov_matrix[[6]]["gamma2", "gamma2"],
                                                                                                   ifelse(Model == "odds 3 knot" & Parameter == 4, vcov_matrix[[6]]["gamma3", "gamma2"],
                                                                                                          ifelse(Model == "odds 3 knot" & Parameter == 5, vcov_matrix[[6]]["gamma4", "gamma2"],
                                                                                                                 `Var-covar-3`)))))))))))))
    
    # NORMAL - V-Cov - 3
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-3` = ifelse(Model == "normal 1 knot" & Parameter == 1, vcov_matrix[[7]]["gamma0", "gamma2"],
                                    ifelse(Model == "normal 1 knot" & Parameter == 2, vcov_matrix[[7]]["gamma1", "gamma2"],
                                           ifelse(Model == "normal 1 knot" & Parameter == 3, vcov_matrix[[7]]["gamma2", "gamma2"],
                                                  ifelse(Model == "normal 2 knot" & Parameter == 1, vcov_matrix[[8]]["gamma0", "gamma2"],
                                                         ifelse(Model == "normal 2 knot" & Parameter == 2, vcov_matrix[[8]]["gamma1", "gamma2"],
                                                                ifelse(Model == "normal 2 knot" & Parameter == 3, vcov_matrix[[8]]["gamma2", "gamma2"],
                                                                       ifelse(Model == "normal 2 knot" & Parameter == 4, vcov_matrix[[8]]["gamma3", "gamma2"],
                                                                              ifelse(Model == "normal 3 knot" & Parameter == 1, vcov_matrix[[9]]["gamma0", "gamma2"],
                                                                                     ifelse(Model == "normal 3 knot" & Parameter == 2, vcov_matrix[[9]]["gamma1", "gamma2"],
                                                                                            ifelse(Model == "normal 3 knot" & Parameter == 3, vcov_matrix[[9]]["gamma2", "gamma2"],
                                                                                                   ifelse(Model == "normal 3 knot" & Parameter == 4, vcov_matrix[[9]]["gamma3", "gamma2"],
                                                                                                          ifelse(Model == "normal 3 knot" & Parameter == 5, vcov_matrix[[9]]["gamma4", "gamma2"],
                                                                                                                 `Var-covar-3`)))))))))))))
    
    # HAZARD - V-Cov - 4
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-4` = ifelse(Model == "hazard 2 knot" & Parameter == 1, vcov_matrix[[2]]["gamma0", "gamma3"],
                                    ifelse(Model == "hazard 2 knot" & Parameter == 2, vcov_matrix[[2]]["gamma1", "gamma3"],
                                           ifelse(Model == "hazard 2 knot" & Parameter == 3, vcov_matrix[[2]]["gamma2", "gamma3"],
                                                  ifelse(Model == "hazard 2 knot" & Parameter == 4, vcov_matrix[[2]]["gamma3", "gamma3"],
                                                         ifelse(Model == "hazard 3 knot" & Parameter == 1, vcov_matrix[[3]]["gamma0", "gamma3"],
                                                                ifelse(Model == "hazard 3 knot" & Parameter == 2, vcov_matrix[[3]]["gamma1", "gamma3"],
                                                                       ifelse(Model == "hazard 3 knot" & Parameter == 3, vcov_matrix[[3]]["gamma2", "gamma3"],
                                                                              ifelse(Model == "hazard 3 knot" & Parameter == 4, vcov_matrix[[3]]["gamma3", "gamma3"],
                                                                                     ifelse(Model == "hazard 3 knot" & Parameter == 5, vcov_matrix[[3]]["gamma4", "gamma3"],
                                                                                            NA))))))))))
    
    # ODDS - V-Cov - 4
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-4` = ifelse(Model == "odds 2 knot" & Parameter == 1, vcov_matrix[[5]]["gamma0", "gamma3"],
                                    ifelse(Model == "odds 2 knot" & Parameter == 2, vcov_matrix[[5]]["gamma1", "gamma3"],
                                           ifelse(Model == "odds 2 knot" & Parameter == 3, vcov_matrix[[5]]["gamma2", "gamma3"],
                                                  ifelse(Model == "odds 2 knot" & Parameter == 4, vcov_matrix[[5]]["gamma3", "gamma3"],
                                                         ifelse(Model == "odds 3 knot" & Parameter == 1, vcov_matrix[[6]]["gamma0", "gamma3"],
                                                                ifelse(Model == "odds 3 knot" & Parameter == 2, vcov_matrix[[6]]["gamma1", "gamma3"],
                                                                       ifelse(Model == "odds 3 knot" & Parameter == 3, vcov_matrix[[6]]["gamma2", "gamma3"],
                                                                              ifelse(Model == "odds 3 knot" & Parameter == 4, vcov_matrix[[6]]["gamma3", "gamma3"],
                                                                                     ifelse(Model == "odds 3 knot" & Parameter == 5, vcov_matrix[[6]]["gamma4", "gamma3"],
                                                                                            `Var-covar-4`))))))))))
    
    # NORMAL - V-Cov - 4
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-4` = ifelse(Model == "normal 2 knot" & Parameter == 1, vcov_matrix[[8]]["gamma0", "gamma3"],
                                    ifelse(Model == "normal 2 knot" & Parameter == 2, vcov_matrix[[8]]["gamma1", "gamma3"],
                                           ifelse(Model == "normal 2 knot" & Parameter == 3, vcov_matrix[[8]]["gamma2", "gamma3"],
                                                  ifelse(Model == "normal 2 knot" & Parameter == 4, vcov_matrix[[8]]["gamma3", "gamma3"],
                                                         ifelse(Model == "normal 3 knot" & Parameter == 1, vcov_matrix[[9]]["gamma0", "gamma3"],
                                                                ifelse(Model == "normal 3 knot" & Parameter == 2, vcov_matrix[[9]]["gamma1", "gamma3"],
                                                                       ifelse(Model == "normal 3 knot" & Parameter == 3, vcov_matrix[[9]]["gamma2", "gamma3"],
                                                                              ifelse(Model == "normal 3 knot" & Parameter == 4, vcov_matrix[[9]]["gamma3", "gamma3"],
                                                                                     ifelse(Model == "normal 3 knot" & Parameter == 5, vcov_matrix[[9]]["gamma4", "gamma3"],
                                                                                            `Var-covar-4`))))))))))
    
    # HAZARD - V-Cov - 5
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-5` = ifelse(Model == "hazard 3 knot" & Parameter == 1, vcov_matrix[[3]]["gamma0", "gamma4"],
                                    ifelse(Model == "hazard 3 knot" & Parameter == 2, vcov_matrix[[3]]["gamma1", "gamma4"],
                                           ifelse(Model == "hazard 3 knot" & Parameter == 3, vcov_matrix[[3]]["gamma2", "gamma4"],
                                                  ifelse(Model == "hazard 3 knot" & Parameter == 4, vcov_matrix[[3]]["gamma3", "gamma4"],
                                                         ifelse(Model == "hazard 3 knot" & Parameter == 5, vcov_matrix[[3]]["gamma4", "gamma4"],
                                                                NA))))))
    
    # ODDS - V-Cov - 5
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-5` = ifelse(Model == "odds 3 knot" & Parameter == 1, vcov_matrix[[6]]["gamma0", "gamma4"],
                                    ifelse(Model == "odds 3 knot" & Parameter == 2, vcov_matrix[[6]]["gamma1", "gamma4"],
                                           ifelse(Model == "odds 3 knot" & Parameter == 3, vcov_matrix[[6]]["gamma2", "gamma4"],
                                                  ifelse(Model == "odds 3 knot" & Parameter == 4, vcov_matrix[[6]]["gamma3", "gamma4"],
                                                         ifelse(Model == "odds 3 knot" & Parameter == 5, vcov_matrix[[6]]["gamma4", "gamma4"],
                                                                `Var-covar-5`))))))
    
    # NORMAL - V-Cov - 5
    
    template_filled <- template_filled %>%
      mutate(`Var-covar-5` = ifelse(Model == "normal 3 knot" & Parameter == 1, vcov_matrix[[9]]["gamma0", "gamma4"],
                                    ifelse(Model == "normal 3 knot" & Parameter == 2, vcov_matrix[[9]]["gamma1", "gamma4"],
                                           ifelse(Model == "normal 3 knot" & Parameter == 3, vcov_matrix[[9]]["gamma2", "gamma4"],
                                                  ifelse(Model == "normal 3 knot" & Parameter == 4, vcov_matrix[[9]]["gamma3", "gamma4"],
                                                         ifelse(Model == "normal 3 knot" & Parameter == 5, vcov_matrix[[9]]["gamma4", "gamma4"],
                                                                `Var-covar-5`))))))
    
    
  } else if(model_type == "simple" & arms == "two") {
    
  } else if(model_type == "spline" & arms == "two") {
    
  }
  
  return(template_filled)
  # END
}
