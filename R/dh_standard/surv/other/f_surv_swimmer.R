# swimmer plot example:
# 


if(FALSE) {
  
  # There are several tricks in this example:
  # 
  #   - Using group and fill at the same time
  #   - setting the position of the labels to match the grouping, not the fill
  #   - Setting up custom ordering of categories, such that the factor level of 
  #     the ordering is different from the factor level of the variable 
  #   - setting up custom ordering of the higher level categories using
  #     the reorder() function, which allows you to use e.g. the sum of a grouped
  #     variable to set the factor order. This lets us have the plot ordered by
  #     the size of each stack!
  #   - transposing the plot (coord_flip())
  # 
  
  library(ggplot2)
  
  n <- 30
  
  # number of patients
  N <- n * 3
  
  # Generate some competing risks for response and progression
  t_start  <- pmax(0,rnorm(N,1.2,0.8))
  t_end  <- pmax(0,rnorm(N,1.5,0.4))
  
  # time to death
  duration <- t_start + t_end
  
  # Use the above to make a test dataset, which has random states and uses
  # the random numbers above for the duration within each state.
  test_dat <- data.frame(
    id = rep(1:n, each=3),
    t_start,
    t_end,
    duration,
    state = unlist(lapply(1:n, function(x) sample(c("A", "B", "C"),replace = FALSE)))
  )
  
  # So we have a factor for state (the fill of the blocks in the plot) and
  # a factor for ordering (the ordering of the blocks in each row)
  # This means that fill can be state, and group can be order. The group takes
  # a higher priortiy than the fill, so it becomes the y axis (flipped to x) ordering
  # of the individual blocks
  
  test_dat$state <- factor(test_dat$state,levels = c("C", "A", "B"),ordered = FALSE)
  test_dat$order <- factor(unlist(lapply(1:n, function(x) sample(c("1st", "2nd", "3rd"),replace = FALSE))),levels = c("1st", "2nd", "3rd"),ordered = FALSE)
  
  # N.B. The reorder function lets you group a variable and create a corresponding
  # factor level for it. In this case the sum of duration grouping by id is used
  # to set the factor level for the x-axis (which is then swapped to the y via
  # coord_flip() later). This results in the height of the stacked bar charts
  # being the sort criteria and not the factor level of the id variable.
  ggplot(test_dat,
         aes(x = reorder(id, duration,sum),
             y = duration,
             group = order,
             fill = state,
             label = paste0(state, "=",round(duration,2)))) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5, reverse = TRUE)) + 
    coord_flip()
  
  
}
