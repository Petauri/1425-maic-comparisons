

#' basic function to extrapolate a transition probability matrix - more of an
#' example than a universal function
#' 
#' @details
#' Note that the time horizon in cycles should NOT include cycle 0. This is factored
#' in during the function, i.e. that `p_0` is the time 0 state residency.
#' 
#' There's a bit to unpack here. So, we have 3 components:
#' 
#'  - The (time-invariant) transition probability matrix (TPM)
#'  - The starting distribution of states (p_0)
#'  - The time horizon in cycles (max cycle number, th_cyc)
#' 
#' The code first checks that the TPM has rows that sum to 1 (to 15 dp). This 
#' ensures that in a probabilistic sensitivity analysis (PSA) when dirichlet
#' draws are used to generate probabilsitic TPMs there are no errors.
#' 
#' The more technical bit comes next. This does a series of things. 
#' 
#' So, first we use the `Reduce` function, which lets you repeat something like
#' a for-loop but in an `*apply` framework. In this case, starting from `p_0`,
#' multiply the result of the previous run `p_tminus1` by the TPM `tpm` to get
#' the next cycle's state residency. 
#' 
#' A simple `Reduce` with accumulation would be code 1 in the examples below
#' which adds the previous result and increments up, so 1 + 2 = 3, 3 + 3 = 6, 6+4=10 and so on.
#' 
#' An example with the same state-transition model structure as ours is 
#' in code 2
#' 
#' This takes the numbers 1 to 10 and adds 1, then 2 then 3 etc until 10, giving 
#' a list of vetors. If accumulate=FALSE, it will only give you the last one.
#' 
#' If we then turn that into a matrix we get code 3
#' 
#' This function does code 3, except we are taking the vector of state residency
#' for `t-1` and matrix-multiplying that by the TPM  to get the next state
#' residency.
#' 
#' 
#' @examples
#' 
#' # code 1
#' Reduce(`+`,1:10,accumulate = TRUE)
#' 
#' # code 2
#' Reduce(
#'  x = 1:10,
#'  init = 1:10,
#'  accumulate = TRUE,
#'  f = function(prev_res, step) {
#'    prev_res + step
#'  }
#' )
#' 
#' # code 3
#' matrix(
#'  unlist(Reduce(
#'    x = 1:10,
#'    init = 1:10,
#'    accumulate = TRUE,
#'    f = function(prev_res, step) {
#'      prev_res + step
#'    }
#'  ),use.names = F),
#'  nrow = 10
#' )
#' 
#' 
#' # Code 4
#' tpm <-
#'   matrix(
#'     c(
#'       0.754873137849046 , 0.16453331708905 , 0.0805935450619043,
#'       0.0696505144438114, 0.882485728945667, 0.0478637566105213,
#'       0                 , 0                , 1
#'     ),
#'     nrow = 3,
#'     byrow = TRUE
#'   )
#' 
#' p_0 <- c(1,0,0)
#' th_cyc <- 100
#' 
#' # The result is a Markov trace:
#' tr <- f_ce_pflow_mk_simple(tpm,th_cyc,p_0)
#' colnames(tr) <- c("sick", "sicker", "dead")
#' 
#' 
#' # Make a trace plot:
#' library(data.table)
#' plot_dat <- rbindlist(lapply(1:ncol(tr), function(col) {
#'   data.table(
#'     t  = 0:th_cyc,
#'     pr = tr[,col],
#'     state = colnames(tr)[col]
#'   )
#' }))
#' 
#' ggplot(plot_dat, aes(x = t, y = pr, colour = state)) + geom_line()
#' 
#' 
#' @export
#' 
f_ce_pflow_mk_simple <- function(tpm, th_cyc, p_0) {
  
  # QC checks
  stopifnot(round(rowSums(tpm),12)==1)
  stopifnot(length(p_0) == dim(tpm)[[1]])
  
  # extrapolate and stick it together as a matrix (100x as efficient as do.call(rbind,x))
  matrix(
    unlist(Reduce(
      x = 1:(th_cyc),
      init = p_0,
      accumulate = TRUE,
      f = function(p_tminus1, cycle) p_tminus1 %*% tpm
    ),use.names = FALSE),
    nrow = th_cyc + 1,
    byrow = TRUE
  )
}




#' Similar to `f_ce_pflow_mk_simple` but expecting a list of TPMs, one for each
#' model cycle (except cycle 0)
#' 
#' @examples
#' # example code
#' 
#' set.seed(698735)
#' random_tpm_list <- lapply(1:th_cyc, function(cyc) {
#'   do.call(rbind, list(gtools::rdirichlet(1,c(93,20,8)),
#'     gtools::rdirichlet(1,c(5,85,3)),
#'     c(0,0,1)
#'   ))
#' })
#' p_0 <- c(1,0,0)
#' th_cyc <- 100
#' 
#' tr <- f_ce_pflow_mk_tvtpm(random_tpm_list,th_cyc,p_0)
#' 
#' colnames(tr) <- c("sick", "sicker", "dead")
#' 
#' # Make a trace plot:
#' library(data.table)
#' plot_dat <- rbindlist(lapply(1:ncol(tr), function(col) {
#'   data.table(
#'     t  = 0:th_cyc,
#'     pr = tr[,col],
#'     state = colnames(tr)[col]
#'   )
#' }))
#' 
#' ggplot(plot_dat, aes(x = t, y = pr, colour = state)) + geom_line()
#' 
#' 
f_ce_pflow_mk_tvtpm <- function(tpm_list, th_cyc, p_0) {
  
  # QC checks
  stopifnot(round(rowSums(tpm_list[[1]]),12)==1)
  stopifnot(length(tpm_list) == th_cyc)
  stopifnot(length(p_0) == dim(tpm_list[[1]])[[1]])
  
  # extrapolate and stick it together as a matrix (100x as efficient as do.call(rbind,x))
  matrix(
    unlist(Reduce(
      x = 1:(th_cyc),
      init = p_0,
      accumulate = TRUE,
      f = function(p_tminus1, cycle) p_tminus1 %*% tpm_list[[cycle]]
    ),use.names = FALSE),
    nrow = th_cyc + 1,
    byrow = TRUE
  )
}



# A much more advanced Markov modelling issue - the need for tunnel states
# 
# Tunnels are required in sequencing models to properly track both time since
# baseline and time in state at the same time.
# 
# In order to do this correctly time needs to be "expanded" into what's called
# a block-diagonal or block-triangular sparse matrix. Simply put, this is blocks of empty space with
# diagonals of numbers.
# 
# Each diagonal is time in state ignoring absolute time since baseline. 
# Each upper triangular is time in state given absolute time since baseline.
# 
# In the PATT pathways project only time in state ignoring time since baseline
# was used, thereby assuming that a patient entering a state (i.e. 2L+ in a pathway)
# was entering at the baseline of the corresponding KM or relative efficacy applied
# to models of that.
# 
# In most treatment sequencing model contexts, this will be the case.
# 
# In a Markov model, Simple tunnel states will also follow this rule, as 
# there will be a probability of death, a probability of exiting tunnel and
# the rest is the probability of staying in the tunnel for another cycle. 99%
# of the time there is only death or remain in tunnel, which simplifies things
# further.
# 
# Consequently, there are several different transition probability matrix "compilers"
# which suit different situations. These are:
# 
#  - No tunnel states: just use f_ce_pflow_mk_tvtpm
#  - "Simple" tunnel state (only death or continue whilst in tunnel)
#  - Sequencing model. Given time in state:
#   - on tx: disc, next, die, stay
#   - off tx: next, die, stay
#   - last line (e.g. BSC, hospice care etc.):  die, stay
#   
# In the simple tunnel case, one needs to know which health states are tunnels, 
# and how long each tunnel is (e.g. state 3 is a tunnel for 10 cycles). This
# leads to a matrix expansion which is typically quite small. This may not require
# sparse matrices, and it may be simpler to use `tpm_list` from the above examples
# and expand each matrix for the tunnel.
# 
# As an example, if the 2nd state is a tunnel lasting 3 cycles, the matrix would
# be broken up into 4 pieces:
# 
#  1. the part of the matrix before the tunnel (just 1,1 in this case)
#  2. the ROWS of the matrix after the tunnel
#  3. The COLS of the matrix after the tunnel for the row(s) before the tunnel
# 
# Those pieces stay the same. Then tunnel-state probabilities for stay and die
# (two numeric vectors of length equalling the length of the tunnel) are put in,
# death ones in the last column, and stay ones as a diagonal matrix starting from
# TPM[2,2] then TPM[3,3] etc.
# 
# Done. Now there is a tunnel state in the markov model, and f_ce_pflow_mk_tvtpm can
# be used as normal to compute the trace.
# 
# 
# For a sequencing model it is much more complicated
# 
# 
#  
# 
# 




#' Function to add a tunnel state to a transition probability matrix using
#' block-diagonals:
#' 
#' 
#' @examples
#' 
#' tpm <-do.call(rbind, list(
#'   gtools::rdirichlet(1,c(93,20,15,8)),
#'   gtools::rdirichlet(1,c(0 ,45,10,8)),
#'   gtools::rdirichlet(1,c(0 ,0 ,85,3)),
#'   c(0,0,0,1)
#' ))
#' tun_probs <- matrix(
#'   c(
#'     0.9, 0.85, 0.95,
#'     0.1, 0.15, 0.05
#'   ),
#'   ncol = 2
#' )
#' tun_pos <- 2
#' 
#' tpm_tun <- f_ce_pflow_mk_AddOneSimpleTunnel(
#'   tpm = tpm,
#'   tun_probs = tun_probs,
#'   tun_pos = 2
#' )
#' 
#'   
#' # time-invariant:
#' p_0 <- c(1,0,0,0,0,0)
#' th_cyc <- 100
#' 
#' 
#' tr <- f_ce_pflow_mk_simple(tpm_tun,th_cyc,p_0)
#' colnames(tr) <- c("sick", "t1_1", "t1_2", "t1_3", "sicker", "dead")
#' 
#' round(rowSums(tr),12)==1
#' 
#' library(data.table)
#' plot_dat <- rbindlist(lapply(1:ncol(tr), function(col) {
#'   data.table(
#'     t  = 0:th_cyc,
#'     pr = tr[,col],
#'     state = colnames(tr)[col]
#'   )
#' }))
#' 
#' ggplot(plot_dat, aes(x = t, y = pr, colour = state)) + geom_line()
#' 
f_ce_pflow_mk_AddOneSimpleTunnel <- function(tpm, tun_probs, tun_pos) {
  
  tdim <- dim(tpm)[[1]]
  tun_len <- nrow(tun_probs)
  N <- (tdim-1) + tun_len
  
  p_before     <- 1:(tun_pos-1)
  p_after_orig <- (tun_pos+1):tdim
  
  # Slice up the matrix so we have before and after tunnel stored
  
  # Cells to the left of the tunnel, plus their co-ordinates in the FINAL
  # matrix:
  tup <- matrix(
    tpm[1:p_before,],
    nrow = length(1:p_before),
    byrow = TRUE
  )
  coord_up <- matrix(
    c(
      rep(1:p_before,each = tdim),
      rep(1:tdim + ((1:tdim > tun_pos) * (tun_len-1)),length(1:p_before)),
      c(t(tup))
    ),
    ncol = 3
  )
  
  # Cells to the right of the tunnel, plus their coords in the final matrix:
  tdown <- matrix(
    tpm[p_after_orig,],
    nrow = length(p_after_orig)
  )
  coord_down <- matrix(
    c(
      rep(p_after_orig+tun_pos,each = tdim),
      rep(1:tdim + ((1:tdim > tun_pos) * (tun_len-1)),length(p_after_orig)),
      c(t(tpm[p_after_orig,]))
    ),
    ncol = 3
  )
  
  # Tunnel co-ordinates and values
  tun_rows <- tun_pos:(tun_pos+tun_len-1)
  
  coord_tun <- matrix(
    c(
      tun_rows,
      tun_rows+1,
      tun_probs[,1]
    ),
    ncol = 3
  )

  coord_tun_death <- matrix(
    c(
      tun_rows,
      rep(N,tun_len),
      tun_probs[,2]
    ),
    ncol = 3
  )
  
    
  # Compile into a co-ordinate matrix:
  coord <- do.call(
    rbind,
    list(coord_up,coord_tun,coord_tun_death,coord_down)
  )
  
  # Make an empty matrix with the expanded size:
  mat <- matrix(
    0,
    nrow = N,
    ncol = N
  )
  
  # enter the data
  mat[coord[,1:2]] <- coord[,3]
  
  # return the matrix with the tunnel state added to it:
  return(mat)
  
}





# Examples ----------------------------------------------------------------


if (FALSE) {
  
  # Adding tunnel to time-varying hazard:
  
  th_cyc <- 100
  
  
  # Generate some random transition probs (just for this example)
  # 
  # In a real setting, you'd work out the TPs using e.g. survival extrapolations
  # 
  # 
  set.seed(698735)
  random_tpm_list <- lapply(1:th_cyc, function(cyc) {do.call(rbind, list(
      gtools::rdirichlet(1,c(93,20,15,8)),
      gtools::rdirichlet(1,c(0 ,45,10,8)),
      gtools::rdirichlet(1,c(0 ,0 ,85,3)),
      c(0,0,0,1)
    ))})
  p_0 <- c(1,0,0,0,0,0)
  th_cyc <- 100

  tun_probs <- matrix(
    c(
      0.9, 0.85, 0.95,
      0.1, 0.15, 0.05
    ),
    ncol = 2
  )
  tun_pos <- 2
  
  
  rand_tpm_list_with_tunnels <- lapply(random_tpm_list, function(tpm_t) {
    f_ce_pflow_mk_AddOneSimpleTunnel(
      tpm = tpm_t,
      tun_probs = tun_probs,
      tun_pos = tun_pos
    )
  })
  
  # all rows sum to 1 in all TPMs:
  lapply(random_tpm_list,rowSums)
  lapply(rand_tpm_list_with_tunnels,rowSums)
  
  
  tr <- f_ce_pflow_mk_tvtpm(
    tpm_list = rand_tpm_list_with_tunnels,
    th_cyc = th_cyc,
    p_0 = p_0)

  
  colnames(tr) <- c("sick", "t1_1", "t1_2", "t1_3", "sicker", "dead")

  round(rowSums(tr),12)==1

  library(data.table)
  plot_dat <- rbindlist(lapply(1:ncol(tr), function(col) {
    data.table(
      t  = 0:th_cyc,
      pr = tr[,col],
      state = colnames(tr)[col]
    )
  }))
  ggplot(plot_dat, aes(x = t, y = pr, colour = state)) + geom_line()
  
  
  
  # so that's time-varying with a fixed tunnel sorted. Now we might want
  # time-varying with a tunnel which varies from cycle to cycle!
  
  tun_prob_list <- lapply(1:th_cyc, function(cyc) {
    do.call(
      rbind,
      list(
        gtools::rdirichlet(1,c(90,10)),
        gtools::rdirichlet(1,c(85,15)),
        gtools::rdirichlet(1,c(95, 5))
      )
    )
  })
  
  rand_tpm_list_with_tv_tunnels <- lapply(1:th_cyc, function(cyc) {
    f_ce_pflow_mk_AddOneSimpleTunnel(
      tpm = random_tpm_list[[cyc]],
      tun_probs = tun_prob_list[[cyc]],
      tun_pos = tun_pos
    )
  })
  
  # Still working, even if the tunnel probabilities are absolute time-dependent!!!
  lapply(rand_tpm_list_with_tv_tunnels,rowSums)
  
  
  tr <- f_ce_pflow_mk_tvtpm(
    tpm_list = rand_tpm_list_with_tv_tunnels,
    th_cyc = th_cyc,
    p_0 = p_0)
  
  
  colnames(tr) <- c("sick", "t1_1", "t1_2", "t1_3", "sicker", "dead")
  
  round(rowSums(tr),12)==1
  
  # Add a consolidated within-tunnel population
  in_tun <- rowSums(tr[,grep("t[0-9]_",colnames(tr))])
  tr <- do.call(
    cbind,
    list(
      sick = tr[,"sick"],
      in_tun = in_tun,
      tr[,c("sicker","dead")]
    )
  )
  
  library(data.table)
  plot_dat <- rbindlist(lapply(1:ncol(tr), function(col) {
    data.table(
      t  = 0:th_cyc,
      pr = tr[,col],
      state = colnames(tr)[col]
    )
  }))
  ggplot(plot_dat, aes(x = t, y = pr, colour = state)) + geom_line()
  
  
  
  
}

