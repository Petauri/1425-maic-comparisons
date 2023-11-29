# The following goes through the example detailed in https://adv-r.hadley.nz/r6.html
# Please read that page (it's not long)
# 
# 
# Why use OoO?
# 
# - It edits things in place instead of copy pasting (way faster for large objects)
# - lets you replicate stuff easily via thing$new() and all that
# 
# 

library(R6)
library(tidyverse)
library(data.table)

# Rules:
# 
# - classname should be UpperCamelCase
# - methods and fields should be lower_snake_case
# 
Accumulator <- R6Class(
  classname = "Accumulator", 
  public    = list(
    sum = 0,
    add = function(x = 1) {
      self$sum <- self$sum + x 
      invisible(self)
    }
  )
)

# Taking a look:
Accumulator


# Make a new accumulator and call it x:
x <- Accumulator$new()

# Add 4 to sum. Note that in public we made sum a field
x$add(4)
x$sum





# Method chaining ---------------------------------------------------------


# Because the method "add" returns self invisibly, it can be repeatedly called:
x <- Accumulator$new()

# You can separate by line in a similar way to a dplyr chain:
x$
  add(10)$
  add(10)$
  sum


# Method chaining is done a lot in python and javascript. In fact it's pretty much
# how a pipe works in dplyr



# methods which should always be included ---------------------------------

# initialize and print should be included, as it's always really important to
# have them.


# Here's an example with an initialize
Person <- R6Class(
  classname = "Person", 
  public = list(
    name = NULL,
    age  = NA,
    initialize = function(name, age = NA) {
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.numeric(age), length(age)     == 1)
      
      self$name <- name
      self$age  <- age
    },
    print = function(...) {
      cat("Person: \n")
      cat("  Name: ", self$name, "\n", sep = "")
      cat("  Age:  ", self$age, "\n", sep = "")
      cat("  Yo dude I'm printing over here", "\n", sep = "")
      invisible(self)
    }
  )
)

# Note that this will error becauswe I've added the validation that age has
# to be a numeric
# darren <- Person$new(name = "Darren", age = "thirty-five")

# Correct version:
darren <- Person$new(name = "Darren", age = 35)


# For really complicated validation, add a method for it.


# Now, note that because print has been defined as a method, when you simply
# return the object you get the print output!
darren




# Breaking up the definition into blocks ----------------------------------

# You don't have to define the entire class in one call. you can define the
# basic and then add stuff to it separately 

Accumulator <- R6Class("Accumulator")
Accumulator$set("public", "sum", 0)
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x 
  invisible(self)
})
Accumulator$set("public", "mult", function(x = 1) {
  self$sum <- self$sum * x 
  invisible(self)
})
Accumulator$set("public", "div", function(x = 1) {
  self$sum <- self$sum / x 
  invisible(self)
})


# This is the preferred way of doing it because it lets you explicitly 
# say all the stuff that's in the class definition with separate headers and
# maybe even separate files. 
# 
# Imagine that the above isn't just adding up etc, but e.g. costing up different
# administration methods for a drug. something like this for a drug dosing schedule:

DosingSchedule <- R6Class(
  classname = "DosingSchedule",
  public = list(
    name       = NULL,
    basis      = NULL,
    weeks_on   = NULL,
    weeks_off  = NULL,
    tx_on      = NULL,
    unit_sizes = NULL,
    starting_from = NULL,
    tx_schedule = NULL,
    initialize = function(name, basis, weeks_on = 1, weeks_off = 0, tx_on = 1, unit_sizes = 1, starting_from = 0) {
      stopifnot(is.character(name)    , length(name)       == 1)
      stopifnot(is.numeric(weeks_on)  , length(weeks_on)   == 1)
      stopifnot(is.numeric(weeks_off) , length(weeks_off)  == 1)
      stopifnot(is.numeric(tx_on)     , length(tx_on)      == 1)
      stopifnot(is.numeric(unit_sizes), length(unit_sizes) == 1)
      stopifnot(
        is.character(basis), 
        length(basis) == 1,
        basis %in% c("fixed", "continuous", "banded")
      )
      
      self$name          <- name
      self$basis         <- basis
      self$weeks_on      <- weeks_on
      self$weeks_off     <- weeks_off
      self$tx_on         <- tx_on
      self$unit_sizes    <- unit_sizes
      self$starting_from <- starting_from
    },
    print = function(...) {
      cat("Drug: \n")
      cat("  Name: ", self$name, "\n", sep = "\t")
      cat("  type: ", self$basis, "\n", sep = "\t")
      cat("  weeks_on: ", self$weeks_on, "\n", sep = "\t")
      cat("  weeks_off: ", self$weeks_off, "\n", sep = "\t")
      cat("  tx_on: ", self$tx_on, "\n", sep = "\t")
      cat("  unit_sizes: ", self$unit_sizes, "\n", sep = "\t")
      cat("  starting: ", self$starting_from, "\n", sep = "\t")
      invisible(self)
    }
  )
)


# Make a function to work out a dosing schedule (treatments received per cycle)
# for a time horizon in cycles (starting from cycle 0)

#' Works out the "schedule" for treatments given the input set
DosingSchedule$set("public", "schedule", function(th_cyc, max_tx_dur = NULL) {
  
  # Pull out the other data we need for this
  won   <- self$weeks_on
  woff  <- self$weeks_off
  ton   <- self$tx_on
  start <- self$starting_from
  
  # Validation:
  stopifnot(is.numeric(won)  , length(won)   == 1, won  > 0)
  stopifnot(is.numeric(woff) , length(woff)  == 1, woff >= 0)
  stopifnot(is.numeric(ton)  , length(ton)   == 1, ton  > 0)
  stopifnot(is.numeric(start), length(start) == 1, ton  >= 0)
  
  # If a max treatment duration isn't supplied, then set it to the time horizon
  if (is.null(max_tx_dur)) {
    max_tx_dur <- th_cyc
  }
  
  
  # Now generate a vector which depicts the cycles in which a patient gets
  # this drug, and how many times in that cycle.
  
  # Generate the schedule during the treated period
  tx_schedule <- rep_len(length.out = max_tx_dur - start, c(rep(ton,won),rep(0,woff)))
  
  # If the treated period isn't from the start then adjust
  if (start > 0) {
    tx_schedule <- c(rep(0,start),tx_schedule)
  }
  
  # If the vector after adjusted for delay from start is still shorter than the time
  # horizon, then 
  if (length(tx_schedule) < th_cyc) {
    tx_schedule <- c(tx_schedule, rep(0,th_cyc - max_tx_dur))
  }
  
  self$tx_schedule <- tx_schedule
  
  invisible(self)
})


# Now we've added that new method, we can create a drug with a specific scheduling
# input set, and compute the schedule from that:

x <- DosingSchedule$new(
  name          = "nivo",
  basis         = "fixed",
  weeks_on      = 3,
  weeks_off     = 1,
  tx_on         = 2,
  unit_sizes    = 3,
  starting_from = 5
)
x$schedule(th_cyc = 200, max_tx_dur = 150)




# A class object for drug costing ---------------------------------------------------------

# This is the way that one would generate effectively a list of "stuff" which can
# grow infinitely, or be modified (e.g. deleting an item out of it, or adding a new one).
# 
# This is really important for shiny apps of CE models because when you run a 
# shiny CE model, in the first instance you DO NOT KNOW:
# 
#   - The decision problem
#   - How many regimen there are in total
#   - The model cycle length and time horizon
#   - How many combinates there are within each regimen
#   - How many unit sizes there are for each combinate
#     - A label for each unit size
#     - The amount of drug in each unit size
#     - The amount of units in a pack of each unit size
#     - The cost of each unit size
#   - Whether there are some combinates which are used in multiple regimen
#   - The basis of each combinate (flat-dosed, weight based, BSA based, banded based on weight, banded based on BSA, age-based, others, combinations)
#   - The scheduling inputs of each combinate
#     - weeks on followed by weeks off (e.g. 3 on 1 off is: 1, 1, 1, 0, 1, 1, 1, 0, ...,)
#     - tx per treated period (e.g. 2 in above is 2, 2, 2, 0, 2, 2, 2, 0,...,)
#     - delay before start after init (for tunnel-based models only - e.g. 5 is 0, 0, 0, 0, 0, 2, 2, 2, 0, 2, 2, 2, 0,...,)
#     - max tx dur (e.g. 10 is 0, 0, 0, 0, 0, 2, 2, 2, 0, 2, 0, 0, 0,...,)
#   - RDI, whether that RDI changes over time, usually doesn't
#   - Potentially some other factors (complex PAS, others)
# 
# 
# Yes - That's a lot of information, and if you have a keen eye you'll have noticed
# that several of these inputs will interact with each other (e.g. max tx dur can't
# be bigger than the time horizon)
# 
# Invariably, drug costs are the facet of a CE model most dense with input information.
# By comparison, other aspects of a CE model are relatively simple in terms of
# programming. For instance, efficacy is simply some estimates which produce some
# parameters without any interdependence. However, for drug costs a great deal
# of contingency is required, altering the method by which drug costs are ultimately
# estimated over time.
# 
# This mass of interdependence creates a huge stumbling block inside of R-based
# cost-effectiveness models with a shiny front end. Most notably, the ex ante
# unknowable inputs which then determine the very existence of entire sets of
# lower-level inptus, which themselves determine the existence of lower level
# inputs...
# 
# R6 helps with this by allowing you to have your class definition house a LIST
# of items (called regimen in this case). 
# 
# 
# 



# ~ intial definition -----------------------------------------------------

DrugInputs <- R6::R6Class("DrugInputs", list(
  regimen      = list(),
  schedules    = list(),
  dose_table   = list(),
  cpdr         = list(),
  cost_vectors = list(),
  n_regimen    = NULL,
  initialize   = function() {
    self$n_regimen <- 0
  },
  remove_regimen = function(name) {
    
    # Error out if the name given isn't in the names of the items in regimen:
    stopifnot(name %in% names(self$regimen))
    
    # Figure out the number for the one being removed from the list, then remove it
    index        <- which(names(self$regimen) %in% name)
    
    # If there's only one regimen left, then change the rgimen list to just an empty list
    if (length(self$regimen) == 1) {
      self$regimen <- list()
    } else {
      # otherwise remove the specific item we want to delete
      self$regimen <- self$regimen[[-index]]
    }
    
    # Reduce the number of regimen by 1 & return
    self$n_regimen <- self$n_regimen - 1
    invisible(self)
  },
  remove_combinate = function(regimen_name, combinate_name) {
    # Error out if the name given isn't in the names of the items in regimen:
    stopifnot(regimen_name %in% names(self$regimen))
    
    # Do the same for combinates within this regimen
    stopifnot(combinate_name %in% self$regimen[[regimen_name]]$nam_combinates)
    
    # Figure out the number for the one being removed from the list, then remove it
    index                                       <- which(self$regimen[[regimen_name]]$nam_combinates %in% combinate_name)
    
    if (length(self$regimen[[regimen_name]]$inputs) == 1) {
      self$regimen[[regimen_name]]$inputs         <- NULL
      self$regimen[[regimen_name]]$nam_combinates <- NULL
    } else {
      self$regimen[[regimen_name]]$inputs         <- self$regimen[[regimen_name]]$inputs[[-index]]
      self$regimen[[regimen_name]]$nam_combinates <- self$regimen[[regimen_name]]$nam_combinates[-index]
    }
    invisible(self)
  },
  get_n_regimen = function() {
    self$n_regimen
  },
  get_n_combinates = function(regimen_name) {
    stopifnot(regimen_name %in% names(self$regimen))
    length(self$regimen[[regimen_name]])
  },
  print = function() {
    cat("Drug inputs: \n")
    cat("\tNumber of regimen:\t", self$n_regimen, "\n", sep = "")
  }
))


# ~ Adding regimen --------------------------------------------------------

#' Method to add a new regimen (1st level list element) to the regimen object
#' within the `DrugInputs` R6 class object.
#' 
#' @details
#' Simply add a name. 
#' 
#' 
#' 
DrugInputs$set("public", "add_regimen", function(name) {
  
  # Validation step:
  stopifnot(is.character(name), length(name) == 1)
  
  # If there aren't any regimen added yet, then you don't need to check
  # for name clashes. otherwise, you do!
  if (!is.null(names(self$regimen))) {
    stopifnot(!name %in% names(self$regimen))
  }
  
  # Right, now we can make our new regimen in self$regimen by just making a new
  # list and assigning
  
  self$regimen[[name]] <- list(
    regimen_name   = name,
    n_combinates   = 0,
    nam_combinates = character(0),
    inputs         = list()
  )
  
  self$n_regimen <- length(self$regimen)
  
  invisible(self)
})


# ~ adding a combinate to a regimen  --------------------------------------

#' Method to add a combinate to an existing regimen within a DrugInputs R6 object
#' 
#' @param regimen_name Name of the regimen to add a combinate to - case-sensitive!
#' @param combinate_name Name of the combinate to add to the regimen. Case-sensitive and can't already exist in the list!
#' @param dose_basis Basis of dosing, e.g. flat-dosed, IV based on weight or BSA and so on
#' @param cyc_on How many model cycles (taking into account the model cycle length) do patients get treated before a break (default 1)
#' @param cyc_off How long is the break when it comes (default 0 for no breaks)
#' @param tx_on How many times in a treated model cycle does the patient get treated (default 1)
#' @param unit_sizes How many different unit sizes are there for this drug? (default 1)
#' @param starting_from Is there a delay before this combinate starts within the regimen (default 0)?
#' 
#' @details
#' This method adds an input set for a combinate WITHIN an already existing regimen.
#' That is, in the DrugInputs R6 object an element must already exist within regimen
#' matching the `regimen_name` given here, and a `combinate_name` must be provided
#' which isn't already in the list of combinates for this regimen. 
#' 
#' This allows the method to find where to put the new data you want to enter.
#' 
#' Following this, the rest of the inputs are validated and then organised into 
#' a dataset. 
#' 
#' Separate methods called XXX and so on then compute the dosing schedule, the unit cost
#' optimisation (if required depending on dose_basis), and finally the schedule
#' of costs. These are within a different R6 class called `DrugOutputs`.
#' 
#' In the `unit_table`, there is a column called `max`. This describes the maximum
#' amount of that size of unit which can be used in one dose 
#' 
DrugInputs$set("public", "add_combinate", function(
    regimen_name,
    combinate_name,
    dose_basis,
    rdi           = NULL,
    flat_dose     = NULL,
    dose_stat     = NULL,
    dose_per_stat = NULL,
    stat_dist     = NULL,
    stat_mean     = NULL,
    stat_sd       = NULL,
    cyc_on        = 1,
    cyc_off       = 0,
    tx_on         = 1,
    starting_from = 0,
    treated_until = 100,
    unit_table = data.frame(
      unit = 1,
      size = 1,
      pack = 1,
      dose = 1,
      max  = 3,
      cost = 0
    )
) {
  
  # Step 1: regimen level
  
  # Validation
  stopifnot(
    is.character(regimen_name),
    length(regimen_name)   == 1,
    regimen_name %in% names(self$regimen)
  )
  
  
  # Step 2: combinate level
  
  # Validation of inputs:
  stopifnot(
    is.character(combinate_name), length(combinate_name) == 1,
    !combinate_name %in% names(self$regimen[[regimen_name]]$inputs),
    is.numeric(cyc_on)          , length(cyc_on)         == 1,
    is.numeric(cyc_off)         , length(cyc_off)        == 1,
    is.numeric(tx_on)           , length(tx_on)          == 1,
    is.numeric(rdi)             , length(rdi)            == 1,
    is.numeric(starting_from)   , length(starting_from)  == 1,
    is.numeric(treated_until)   , length(treated_until)  == 1
  )
  stopifnot(
    class(unit_table) == "data.frame",
    nrow(unit_table) > 0,
    all(c("unit", "size", "pack", "dose", "cost") %in% colnames(unit_table))
  )
  stopifnot(
    is.character(dose_basis), 
    length(dose_basis) == 1,
    dose_basis %in% c("fixed", "continuous", "banded")
  )
  # Now, if the basis of dosing is continuous our banded, then the stat it's based
  # on must be one of mg per kg (weight), or body surface area. Of course in those
  # cases one must supply information about the stat to be used for dose costing!
  # 
  # Furthermore, if it's a continuous spectrum of dosing, we need distributional 
  # shape, as well as mean and sd
  if(dose_basis %in% c("continuous", "banded")) {
    stopifnot(
      !is.null(dose_stat),
      !is.null(stat_dist),
      stat_dist %in% c("normal", "lognorm"),
      !is.null(dose_per_stat),
      !is.null(stat_mean),
      stat_mean > 0,
      !is.null(stat_sd),
      stat_sd >= 0,
      dose_stat %in% c("mg/kg", "bsa")
    )
  } else {
    stopifnot(!is.null(flat_dose))
  }
  
  
  # Now we can collate the input data before then entering it into the class object:
  
  if (dose_basis %in% c("continuous", "banded")) {
    input_list <- list(
      combinate_name = combinate_name,
      dose_basis     = dose_basis,
      rdi            = rdi,
      dose_stat      = dose_stat,
      dose_per_stat  = dose_per_stat,
      stat_dist      = stat_dist,
      stat_mean      = stat_mean,
      stat_sd        = stat_sd,
      cyc_on         = cyc_on,
      cyc_off        = cyc_off,
      tx_on          = tx_on,
      unit_table     = unit_table,
      starting_from  = starting_from,
      treated_until  = treated_until
    )
  } else {
    input_list <- list(
      combinate_name = combinate_name,
      dose_basis     = dose_basis,
      rdi            = rdi,
      flat_dose      = flat_dose,
      cyc_on         = cyc_on,
      cyc_off        = cyc_off,
      tx_on          = tx_on,
      unit_table     = unit_table,
      starting_from  = starting_from,
      treated_until  = treated_until
    )
  }
  
  # slot the new inputs into the right place.
  self$regimen[[regimen_name]]$inputs[[combinate_name]] <- input_list
  self$regimen[[regimen_name]]$n_combinates             <- length(self$regimen[[regimen_name]]$inputs)
  self$regimen[[regimen_name]]$nam_combinates           <- names(self$regimen[[regimen_name]]$inputs)
  invisible(self)
  
})



# ~ clone a combinate -----------------------------------------------------


#' Method to "clone" a combinate from one regimen to another 
#' 
#' @details
#' Simple method to copy the input set from one regimen to another under the same
#' name. Simply gets a combinate from one regimen and puts it at the end of the
#' combinate list of another.
#' 
#' This is very handy if the same combinate is used in multiple regimen, reducing
#' repitition and scope for human error. Combined with the ability to remove
#' a combinate using the `remove_combinate` method, this provides the ability
#' to shuffle about the dataset for most needs.
#' 
#' 
DrugInputs$set(
  which = "public",
  name  = "clone_combinate",
  value = function(orig, combi, dest) {
    
    # Get the inputs:
    inputs <- self$regimen
    
    # some validation (origin and destination must exist, combinate must exist, 
    # combinate must not exist in destination already):
    stopifnot(
      orig %in% names(self$regimen),
      combi %in% self$regimen[[orig]]$nam_combinates,
      dest %in% names(self$regimen),
      !combi %in% self$regimen[[dest]]$nam_combinates
    )
    
    # Assuming that the combinate name remains the same, put the combinate
    # into the inputs for the target regimen
    self$regimen[[dest]]$inputs[[combi]] <- self$regimen[[orig]]$inputs[[combi]]
    self$regimen[[dest]]$nam_combinates  <- c(self$regimen[[dest]]$nam_combinates,combi)
    self$regimen[[dest]]$n_combinates    <- self$regimen[[dest]]$n_combinates + 1
    invisible(self)
    
  }
)


# ~ Compute the dosing schedule for all combinates in all regimen ---------

#' Method within the `DrugInputs` class object to compute the pattern with which
#' patients are dosed.
#' 
#' @details
#' For instance, if the model has a weekly time horizon and the drug is dosed
#' 3 weeks on, one week off for 2 years, we need to have a set of inputs to
#' facilitate that. Furthermore, if in a "treated cycle" a patient gets 3 doses
#' of drug, then the pattern would need to be `3, 3, 3, 0, 3, 3, 3, 0`. To make matters
#' even more complicated, if the cycle length is 4 weeks, then the pattern should
#' be `9, 9, 9, 9, ...` as that is the total amount of doses in a 4 week period
#' would be 9. Similarly, if the drug is given once every 3 weeks (`1, 0, 0, 1, 0, 0`),
#' then this would become more complicated, with `2, 1, 1, 2, 1, 1, 2, 1, 1, 2...`
#' being that pattern! To combat this, longer time horizons often "smooth" dosing
#' by calculating the expected number of doses in a 4 week period. However, in 
#' the case above, this would overestimate drug costs by `50%` depending on the
#' calculation method (if done just on the first 4 weeks, 50% overestimate compared
#' to mean(c(2,1,1)) = 1.3333 = 4/3. because of this, 2 / (4/3) = 1.5x)
#' 
#' Therefore, the method must be able to cope with generating the number of
#' doses for any cycle length (based on cycle length in days), any pattern of dosing
#' (cycles on followed by cycles off).
#' 
#' The method has no arguments as it is internal, using `self$regimen`
#' 
#' 
DrugInputs$set(
  which = "public",
  name  = "get_dosing_schedule",
  value = function() {
    
    # Grab the inputs:
    inputs <- self$regimen
    
    # Cycle through them all producing schedules with the same list structure:
    self$schedules <- lapply(inputs, function(regimen) {
      lapply(regimen$inputs, function(combinate) {
        
        one_rotation <- c(rep(combinate$tx_on,combinate$cyc_on),rep(0,combinate$cyc_off))
        
        lead_in      <- rep(0,combinate$starting_from)
        max_dur      <- combinate$treated_until
        
        stopifnot(max_dur > length(lead_in))
        
        return(c(lead_in,rep(one_rotation,length = max_dur - length(lead_in))))
      })
    })
    invisible(self)
  }
)

# ~ computing the cheapest way to provide each dose -----------------------

#' Calculate the cheapest way to provide each possible dose given unit sizes, pack sizes, costs, and maximum allowed units.
#'
#' This function takes unit sizes, pack sizes, costs, and the maximum allowed units for each unit size and calculates the
#' cheapest way to provide each possible dose. It returns a data.table with columns for each unit size, the total dose, and
#' the minimum cost for each possible dose.
#'
#' @param sizes A numeric vector of unit sizes.
#' @param pack_sizes A numeric vector of pack sizes (e.g., packs of tablets or pills).
#' @param costs A numeric vector of costs per pack for each unit size.
#' @param max_allowed A numeric vector of the maximum number of units allowed for each unit size.
#' @param return_matrix A logical value. If TRUE, the result is returned as a matrix. If FALSE, the result is returned as a data.table.
#'
#' @return A data.table with the cheapest way to provide each possible dose.
#'
#' @author Darren Burns
#'
#' @import gtools data.table
#'
#' @examples
#' sizes       <- c(50, 100, 200)
#' pack_sizes  <- c(30, 20, 10)
#' costs       <- c(10, 15, 20)
#' max_allowed <- c(3, 2, 1)
#' result      <- f_dcost_cheapestComb(sizes, pack_sizes, costs, max_allowed)
#'
#' @export
f_dcost_cheapestComb <- function(sizes, pack_sizes, costs, max_allowed, return_matrix = TRUE) {

  requireNamespace("gtools")
  requireNamespace("data.table")
  
  stopifnot(
    all(lengths(
      x = list(
        sizes,
        pack_sizes,
        costs,
        max_allowed
      )
    ) == length(sizes))
  )
  
  # some useful parameters that avoid repetition later
  
  cwz           <- c(0,sizes)
  mg_per_pack   <- sizes * pack_sizes
  cost_per_unit <- costs / pack_sizes
  cost_per_mg   <- costs / mg_per_pack
  
  lensize <- length(sizes)
  
  # Columns are unit sizes, numbers are numbers of units. cycle through
  # columns filtering using max_allowed
  permu <- Reduce(
    x          = seq_along(max_allowed),
    init       = gtools::permutations(
      n               = max(max_allowed)+1,
      r               = lensize,
      v               = 0:max(max_allowed),
      repeats.allowed = TRUE
    ),
    accumulate = FALSE,
    f          = function(prev, maxforthis) {
      return(prev[which(prev[,maxforthis] <= max_allowed[maxforthis]),])
    }
  )
  
  # Dose per unit size per combination:
  dos           <- permu %*% diag(sizes)
  dos           <- data.table::data.table(dos)
  colnames(dos) <- paste0("u_",sizes)
  dos$tot       <- rowSums(dos)
  dos$cost      <- rowSums(permu %*% diag(cost_per_unit))
  
  # Group by dose and take the smallest cost to give the cheapest way to 
  # provide each possible dose:
  
  dos <- data.table::merge.data.table(dos,dos[, .(min_cost = min(cost)), by = tot],by="tot")
  dos <- dos[cost == min_cost,]
  dos$min_cost <- NULL
  
  if (return_matrix) {
    return(as.matrix(dos[order(tot),]))
  } else {
    return(dos[order(tot),])
  }
}



# ~ Get table of cheapest combinations ------------------------------------


#' method to compute the cheapest way to provide each dose, assuming 0 cost
#' of going to get a new pack of drug
#' 
#' 
#' 
DrugInputs$set(
  which = "public",
  name  = "get_dose_table",
  value = function() {
    
    self$dose_table <- lapply(self$regimen, function(reg) {
      lapply(reg$inputs, function(combi) {
        
        ut <- combi$unit_table
        
        # if there is only 1 row in unit_table then it's just the cost of that unit
        # going up:
        if (nrow(ut) == 1) {
          data.frame(
            unit = rep(ut$unit, ut$max),
            dose = ut$dose * 1:ut$max,
            cost = (ut$cost/ut$pack) * 1:ut$max
          )
        } else {
          # If there are multiple unit sizes, then we must generate all possible
          # doses through all possible combinations of units up to the max allowed
          # of each unit per dose received
          
          # maximum dose possible for each unit size
          ut$maxdose <- ut$dose * ut$max
          
          # Maximum total possible dose at max of each units
          max_dose <- sum(ut$maxdose)
          
          # Each possible dose up to max_dose:
          return(f_dcost_cheapestComb(
            sizes         = ut$dose,
            pack_sizes    = ut$pack,
            costs         = ut$cost,
            max_allowed   = ut$max,
            return_matrix = FALSE
          )[tot <= max_dose,])
        }
      })
    })
    invisible(self)
  }
)



# ~ Get cost per dose recieved (cpdr) -------------------------------------


DrugInputs$set(
  which = "public",
  name  = "get_cpdr",
  value = function() {
    
    # Don't do this unless the dose table has already been computed:
    stopifnot(length(self$dose_table) > 0)
    
    # Cycle through the inputs and corresponding dose tables. For each one, 
    # if it is flat dosed, select the cheapest way to give that dose (rounding
    # down to the nearest possible dose), otherwise use the characteristic
    # data to estimate the proportion of patients t
    reg_nams <- structure(names(self$regimen), .Names=names(self$regimen))
    
    self$cpdr <- lapply(reg_nams, function(reg_nam) {
      
      # Get the names of the combinates for this regimen and make it an index
      # to loop through, so we can pull from 2 different objects (inputs and
      # the dose_table)
      combi_nams <- structure(self$regimen[[reg_nam]]$nam_combinates, .Names=self$regimen[[reg_nam]]$nam_combinates)
      
      lapply(combi_nams, function(combi_nam) {
        
        # Get the input set and the dosing table for this combinate within this regimen:
        inp <- self$regimen[[reg_nam]]$inputs[[combi_nam]]
        dos <- self$dose_table[[reg_nam]][[combi_nam]]
        
        # Now we have everything we need to compute the distribution of dosing
        # given the characteristic values, if there are any!
        
        # Expected dose per characteristic value. For flat dosing, this is just
        # the dose (i.e., the characteristic technically being 1 human).
        # 
        # Take relative dose intensity (RDI) into account (for flat dosing this
        # is usually taking dose-breaks into account)
        dpc <- inp$flat_dose * inp$rdi
        
        if (inp$dose_basis == "fixed") {
          
          if (!floor(dpc) %in% floor(dos$tot)) {
            stop("This flat dose is not included in the dosing table!")
          }
          
          # Filter the table to the combinations which satisfy the floor of the
          # rdi-adjusted dose:
          dt <- dos[tot == floor(dpc),]
          
          # Choose the cheapest one:
          dt <- dt[cost == min(cost),]
          
          # If it's still multiple rows, take the top row
          if (nrow(dt) > 1) {
            dt <- dt[1,]
          }
          
          # Get the final cost per dose received (just one number):
          return(dt$cost)
          
        } else if (inp$dose_basis == "continuous") {
          
          # Work out the upper bound of the characteristic value associated with
          # a given dose for all possible doses and add it to the table:
          dos$char <- dos$tot / inp$dose_per_stat
          
          # Use this to compute the proportion of patients which will fall into
          # each bound. First use the cdf of the function and the characteristics
          # provided to plot out the CDF in terms of possible doses:
          if (inp$stat_dist == "normal") {
            cdf <- pnorm(dos$char, inp$stat_mean, inp$stat_sd)
          } else {
            cdf <- plnorm(dos$char, inp$stat_mean, inp$stat_sd)
          }
          
          # Add the weighting as a column:
          dos$prop <- cdf - data.table::shift(cdf,fill = 0)
          
          self$dose_table[[reg_nam]][[combi_nam]] <- dos
          
          # return the sum of the product of cost and weighting for cost per dose
          # received:
          return(sum(dos$cost * dos$prop))
          
          
        } else if (inp$dose_basis == "banded") {
          stop("dose banding is not implemented yet, sorry!")
        } else {
          stop("dose basis must be flat, continuous or banded")
        }
      })
    })
  }
)


# ~ Plotting dose distribution --------------------------------------------


DrugInputs$set(
  which = "public",
  name = "plot_dose_dist",
  value = function(regimen_name, combinate_name) {
    print(x$dose_table[[regimen_name]][[combinate_name]] %>%
            dplyr::filter(prop > 0) %>%
            ggplot2::ggplot(aes(x = tot, y = prop)) + 
            ggplot2::geom_col() + 
            ggplot2::geom_line() + 
            ggplot2::theme_classic())
  }
)




# ~ Computing cost per cycle vector ---------------------------------------

# After we've gone through the process of computing distribution of dosing
# and schedule of dosing, we simply need to multiply one by the other
# to get a vector of expected cost per model cycle from t=0 to the time
# horizon.


DrugInputs$set(
  which = "public",
  name = "get_cost_vectors",
  value = function() {
    
    # TODO: First, we have to check whether the component parts have been populated
    # yet or not
    
    cpdr <- self$cpdr
    sch  <- self$schedules
    cpdr <- x$cpdr
    sch  <- x$schedules
    
    nams <- structure(names(cpdr), .Names=names(cpdr))
    
    # Replace the cpdr elements with vectors:
    self$cost_vectors <-  lapply(nams, function(regimen) {
      comb_nams <- structure(names(cpdr[[regimen]]), .Names=names(cpdr[[regimen]]))
      lapply(comb_nams, function(combinate) {
        return(
          cpdr[[regimen]][[combinate]] * sch[[regimen]][[combinate]]
        )
      })
    })
    invisible(self)
  }
)


# Perform a full update in order: -----------------------------------------


DrugInputs$set(
  which = "public",
  name = "update",
  value = function() {
    
    # TODO: First, we have to check whether the component parts have been populated
    # yet or not
    
    self$get_dosing_schedule()
    self$get_dose_table()
    self$get_cpdr()
    self$get_cost_vectors()
    invisible(self)
  }
)





# Testing out the methods -----------------------------------------------





# Of course this is all just nonsense data, but you can see the potential
# to add any number of drugs with any number of combinates with any type
# of scheduling, whilst the code does not grow!


x <- DrugInputs$new()
x$add_regimen("nivo+ipi")
x$add_regimen("len+pem")
x$add_regimen("ipi+len") # for a laugh, and testing the cloning
x$add_regimen("TMZ") # TMZ is known for having lots of different combinations possible

x$add_combinate(
  regimen_name   = "nivo+ipi",
  combinate_name = "nivo (dose-loading)",
  dose_basis     = "fixed",
  rdi            = 0.86,
  flat_dose      = 3,
  cyc_on         = 3,
  cyc_off        = 1,
  tx_on          = 1,
  starting_from  = 0,
  treated_until  = 16,
  unit_table     = data.frame(
    unit = c(1,2),
    size = c(1,2),
    pack = c(5,1),
    dose = c(1,2),
    max  = c(3,1),
    cost = c(5,2)
  )
)

x$add_combinate(
  regimen_name   = "nivo+ipi",
  combinate_name = "nivo (full weight-based)",
  dose_basis     = "continuous",
  rdi            = 0.73,
  dose_stat      = "mg/kg",
  stat_dist      = "lognorm",
  dose_per_stat  = 5,
  stat_mean      = 4.325,
  stat_sd        = 0.025,
  cyc_on         = 3,
  cyc_off        = 1,
  tx_on          = 2,
  starting_from  = 16,
  treated_until  = 100,
  unit_table     = data.frame(
    unit = c(1,2),
    size = c(1,2),
    pack = c(5,1),
    dose = c(1,2),
    max  = c(3,1),
    cost = c(5,2)
  )
)

x$add_combinate(
  regimen_name   = "nivo+ipi",
  combinate_name = "ipi",
  dose_basis     = "continuous",
  rdi            = 0.92,
  dose_stat      = "bsa",
  stat_dist      = "normal",
  dose_per_stat  = 40,
  stat_mean      = 2,
  stat_sd        = 0.2,
  cyc_on         = 1,
  cyc_off        = 0,
  tx_on          = 1,
  starting_from  = 0,
  treated_until  = 100,
  unit_table     = data.frame(
    unit = c(1,2),
    size = c(1,2),
    pack = c(5,1),
    dose = c(1,2),
    max  = c(3,1),
    cost = c(5,2)
  )
)




x$add_combinate(
  regimen_name   = "len+pem",
  combinate_name = "len",
  dose_basis     = "continuous",
  rdi            = 0.81,
  dose_stat      = "mg/kg",
  stat_dist      = "lognorm",
  dose_per_stat  = 5,
  stat_mean      = 4.325,
  stat_sd        = 0.025,
  cyc_on         = 3,
  cyc_off        = 1,
  tx_on          = 1,
  starting_from  = 0,
  treated_until  = 100,
  unit_table     = data.frame(
    unit = c(1,2),
    size = c(1,2),
    pack = c(5,1),
    dose = c(1,2),
    max  = c(3,1),
    cost = c(5,2)
  )
)

x$add_combinate(
  regimen_name   = "len+pem",
  combinate_name = "pem",
  dose_basis     = "continuous",
  rdi            = 0.70,
  dose_stat      = "bsa",
  stat_dist      = "normal",
  dose_per_stat  = 40,
  stat_mean      = 2,
  stat_sd        = 0.2,
  cyc_on         = 1,
  cyc_off        = 0,
  tx_on          = 1,
  starting_from  = 0,
  treated_until  = 100,
  unit_table     = data.frame(
    unit = c(1,2),
    size = c(1,2),
    pack = c(5,1),
    dose = c(1,2),
    max  = c(3,1),
    cost = c(5,2)
  )
)



x$add_combinate(
  regimen_name   = "TMZ",
  combinate_name = "temozolomide",
  dose_basis     = "continuous",
  dose_stat      = "bsa",
  stat_dist      = "lognorm",
  rdi            = 0.86,
  dose_per_stat  = 75,
  stat_mean      = 0.6867764,
  stat_sd        = 0.1018223,
  cyc_on         = 1,
  cyc_off        = 0,
  tx_on          = 7,
  starting_from  = 0,
  treated_until  = 100,
  unit_table     = data.frame(
    unit = 1:6,
    size = c(5,20,100,140,180,250),
    pack = rep(5,6),
    dose = c(5,20,100,140,180,250),
    max  = rep(3,6),
    cost = c(3.54,10.67,38.53,46.30,80.47,81.01),
    source = "eMIT 2023"
  )
)

# x$remove_combinate("TMZ","temozolomide")


# Now for our cloning test - let's take ipi and len from the two different
# combinates and put them into our ipi+len regimen to test whether it works

x$clone_combinate(
  orig  = "nivo+ipi",
  combi = "ipi",
  dest  = "ipi+len"
)
x$clone_combinate(
  orig  = "len+pem",
  combi = "len",
  dest  = "ipi+len"
)

x$update()

# You can now see that the nivo+ipi inputs are there for computing the following:
# 
#   1. The cheapest way to provide each possible dose (using unit, size, dose, cost, max, pack)
#     - Note that this goes up to the maximum number of each available unit per dose, which is reaslistic
#       e.g. 3 on two different unit sizes would mean 3 of A and 3 of B for 6 units, so for 
#       something like TMZ (5+ unit sizes available), this would be an extremely high dose
#   2. Flat dosed or based on a patient characteristic
#   3. If based on patient characteristic, parametric information about it
#   4. dosing schedule inputs (cycles on, break duration after cycles on, treatments per on cycle, max tx dur)
# 
str(x$regimen$`nivo+ipi`)
str(x$regimen$`len+pem`)


# To compute the cost of treating patients per cycle, simply run update:
x$update()



