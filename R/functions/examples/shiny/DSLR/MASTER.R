# This app uses the DSLR system to handle ex ante unknowns through the 
# systematic introduction of "memory" and "extensibility"


# Source all libraries
source("./R/dh_standard/examples/shiny/DSLR/libraries.R")

# Source all functions
source("./R/dh_standard/examples/shiny/DSLR/func_master.R")

# Notes:
# 
# - D is generated using a series of functions which populate a reasonable number
#   of input sets to start off. These can expand infinitely as long as those
#   elements that're added aren't ever deleted (they can be overwritten but
#   not deleted, otherwise the model with "forget" they every existed!)
# 


D <- list()
D$basic <- f_D_basic_inputs(th_cyc = 2000)
D$reg_cost <- f_D_dcost_inputs(n = 5, th = D$basic$th_cyc)


# For application of L and R look at the server script. Note that the UI script
# only has uiOutput commands for the elements. See the readme for a runthrough
# of how to add new elements.
shiny::shinyAppDir(
  appDir = "./R/dh_standard/examples/shiny/DSLR/app"
)
