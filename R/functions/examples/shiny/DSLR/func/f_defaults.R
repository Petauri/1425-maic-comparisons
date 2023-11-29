# Functions to generate default values for the list D for the DLSLR method

#' Generate default input lists for basic inputs:
f_D_basic_inputs <- function(cl_y = 28/365.25, th_cyc = 1000, discC = 0.035, discQ = 0.035) {
  list(
    # Cycle length
    cl_d = cl_y * 365.25,
    cl_w = (cl_y * 365.25)/7,
    cl_m = (cl_y * 365.25) / (365.25/12),
    cl_y = cl_y,
    # time horizon
    th_cyc = th_cyc,
    th_d = ceiling((cl_y * 365.25) * th_cyc),
    th_w = ((cl_y * 365.25)/7)* th_cyc,
    th_m = ((cl_y * 365.25) / (365.25/12)) * th_cyc,
    th_y = cl_y*th_cyc,
    # time
    t_d = ceiling((cl_y * 365.25) * 0:th_cyc),
    t_w = ((cl_y * 365.25)/7)* 0:th_cyc,
    t_m = ((cl_y * 365.25) / (365.25/12)) * 0:th_cyc,
    t_y = cl_y*0:th_cyc,
    # discounting
    discC = discC, 
    discFacC = 1/((1+discC)^(cl_y*0:th_cyc)), 
    discQ = discQ,
    discFacQ = 1/((1+discQ)^(cl_y*0:th_cyc))
  )
}


#' Generate default input lists for a drug for use in a shiny application with
#' ex ante unknown number of regimen or compounds
#' 
#' @param n the number of drugs to generate default values for
#' @param th time horizon in cycles
#' 
#' @examples
#' str(f_D_dcost_inputs(3,1000))
#' 
#' @export
#' 
f_D_dcost_inputs <- function(n = 3, th) {
  
  # First, make a metadata list which tells us the maximum regimen generated ever.
  # This lets us know later on if we're adding a new one or not (i.e. compared
  # to max_reg_so_far, what will length be when we do this new one? will it be
  # bigger than 3? if so we're adding something new. If not, then we're replacing
  # information for a drug which already exists in the model, so we need to 
  # do a load of extra stuff)
  out <- list(
    meta = list(
      max_reg_so_far = n,
      nreg_now       = n,
      th_at_gen      = th,
      which_reg      = 1,
      which_comb     = 1
    )
  )
  
  out$dat <- lapply(1:n, function(reg) {
    
    reg_info_list <- list(
      info = list(
        reg_name        = paste0("regimen ", reg),
        components      = 3,
        component_names = paste0("regimen ", reg, " component ",1:3)
      )
    )
    
    reg_info_list$schedule <- lapply(1:reg_info_list$info$components, function(reg_comp_n) {
      list(
        tx_per_tx  = 1,
        cyc_on     = 1,
        cyc_off    = 0,
        tx_max_dur = th,
        dos_per_tx = 100,
        rdi        = 1
      )
    })
    
    reg_info_list$cost <- lapply(1:reg_info_list$info$components, function(reg_comp_n) {
      list(
        n_size       = 5,
        units        = seq(100,500,100),
        c_per_size   = seq(10,20,by=((20-10)/5))
      )
    })
    
    return(reg_info_list)
    
  })
  
  return(out)
}
