#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
    
  
    # Make an empty reactivevalues list for L and R:
    L <- reactiveValues()
    R <- reactiveValues()
    
    # Now pass the defaults D iunto L and R to create a stable starting state where
    # D == L == R is true. The for loop means that it doesn't matter how many
    # categories (top level lists) you add to D, it will fully populate L and
    # R without any extra code.
    # 
    # The app process is like this:
    # 
    #  - User changes an input in the UI
    #  - Input -> R happens immediately, such that when someone changes it, it changes in R but not L
    #  - User makes some other changes affecting R
    #  - User confirms by pressing a confirmation button or clicking off a tab etc
    #  - confirm leads to the following process 
    #   - intermediate calculations WITHIN R
    #   - Results and updated input values go from R to L (i.e. L <- R)
    #  - As L has now updated, the UI elements will refresh.
    # 
    # 
    for (category in structure(names(D),.Names = names(D))) {
      L[[category]] <- D[[category]]
      R[[category]] <- D[[category]]
    }

    
    # First thing is a nice debug printer for R and L so we can see what we're doing:
    output$debugi <- renderPrint({print(reactiveValuesToList(input))})
    output$debugL <- renderPrint({print(reactiveValuesToList(L))})
    output$debugR <- renderPrint({print(reactiveValuesToList(R))})
    
    # The below should house all of the UI elements containing inputs.
    # 
    # this means that they only ever refresh when the confirm button is pressed
    # 
    # The priority of this should be VERY LOW. this means that everything else
    # happens before the UI stuff contining inputs updates, which would then
    # trigger further changes to R which are VERY HIGH priority triggers. 
    # 
    # this avoids e.g. change numeric, press button to confirm, number in numeric 
    # goes back to L value from before you changed it.
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#     number of treatment regimen    ~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    
    
    observeEvent(input$confirm_i_reg_n_reg, {
      
      # make a static version of the bit of L we need (Thankfully it's quite quick, 
      # but it doesn't work when trying to take a bit of L out and then static it)
      Lstat <- reactiveValuesToList(L)
      
      
      output$ui_i_reg_n_reg <- renderUI({
        # Generate UI element for picking the number of regimen "now". Note
        # that this is coming from L (the "live" values)
        # 
        # Note that the value argument is NOT hard coded and depends on L not D.
        # 
        # - If it is hard-coded it will reset to that number every time it's rendered
        # - If it's linked to D it'll go back to the default value every time it's rendered
        # - If it's linked to L, and changes to input$i_reg_n_reg then update R, and 
        #     THEN button press updates L, it will only change with L (confirmed changes)
        # 
        # If you don't do DSLR, bookmarking doesn't work if things don't exist
        # at the time of update, therefore if you have nested render UI elements
        # (ALWAYS required in CEM shiny apps), bookmarking a previous state wont
        # work as it will apply the value to things that don't exist, those things
        # will then render and will have either empty values or the default values
        # instead of what they were in the bookmark .rds file. This is a standing
        # problem with Shiny. Hence this method to introduce memory to the system.
        # 
        numericInput(
          inputId = "i_reg_n_reg",
          label   = "Set the number of regimen",
          value   = Lstat$reg_cost$meta$nreg_now
        )
      }) 
    }, priority = -1, ignoreNULL = FALSE)
    
    
    # Once we pass D to L and R we are in a steady state, where they're all the same.
    # 
    # The app process is:
    # 
    # 1. User does something in the UI (changes value, presses button)
    # 2. This changes something in R (or triggers observeEvent to do that if button-controlled)
    # 3. R code runs updating bits of R including any downstream calcs
    # 4. The affected bits of R go into the corresponding bits in L
    # 5. The update of L triggers re-rendering of the UI elements that correspond (output tables etc)
    # 
    # This is the process that everything works under. 
    # 
    # For saving a snapshot of the model, literally do this:
    # 
    # saveRDS(reactiveValuesToList(L),file = "path/to/filename")
    # 
    # yes, that's it. 
    # 
    # To change the state of the entire model, one would simply
    # 
    # L <- reactivevalues(readRDS("path/to/file"))
    # R <- reactivevalues(readRDS("path/to/file"))
    # 
    # Thus bringing the entire model in line with the saved RDS file (S) instantly,
    # and EVEN IF SOME INPUTS DIDNT EXIST AT LOADING TIME.
    # 
    # 
    
    
    # Remember, all UI elements depend ONLY on L.
    # 
    # Also, you can now work outside of shiny to edit and debug becuase you can
    # generate D and then just assign R and L to D!
    # 
    # R <- D
    # L <- D
    # 
    # 
    # Final note, linking a confirm button in the line inside a render UI will
    # limit updating that UI to only when that input changes (quirk of shiny)

    
    # Part 2 for this input - updating R:
    observeEvent(input$i_reg_n_reg, {
      R$reg_cost$meta$nreg_now <- input$i_reg_n_reg
    }, priority = 1000)
    
    # Part 3 for this input - when the confirm button is pressed, pass R to L.
    # Note I'm doing this for all of the metadata for regimen costs so one button
    # confirms a bunch of stuff.
    # 
    # The reason I'm doing this so that there can be a default number of regimen
    # (5) to start with, BUT the user can increase it beyond that. When they do
    # it creates NEW ELEMENTS inside R and L, in R$reg_cost$dat and L$reg_cost$dat,
    # as well as updating the metadata in R$reg_cost$meta and L$reg_cost$meta.
    # 
    # To simplify:
    # 
    #  - Model starts with 5 regimen as a default with some default input values in each
    #  - The user is free to "expand" this to any number they want just by changing a number and pressing a button
    #  - The model responds to regimen above 5 by adding more to the L and R lists
    #  - This then creates full input sets to be populated for the additional regimen
    #  - The model knows from the updated metadata how many sets of UIs to render for the individual regimen
    #  - There is a place for all of the data to go when it's changed and confirmed
    #  
    # The process:
    #  
    #  - User changes regimen from 5 to 6
    #  - This only updates R$reg_cost$meta$nreg_now, but it does that immediately
    #  - User presses the confirm button "confirm_i_reg_n_reg"
    #  - This triggers the below event
    #  - The metadata for R (with the updated number) and for L (with the old number) is extracted
    #  - Now we know what the value was before and what it is now, and also what the previous max was (via R$reg_cost$meta$max_reg_so_far)
    #  - If the previous max is lower than the current value then we need to add more data to R and L
    #   - Generate some default values for the new entries using the default values generator function f_D_dcost_inputs and picking the indices for the new ones out of it
    #   - slap those new sets onto the end of the current list of inputs in R$reg_cost$dat
    #   - update the max_reg_so_far entry to be the new maximum
    #  - update L$reg_cost with R$reg_cost, now that it's all finished.
    # 
    # 
    observeEvent(input$confirm_i_reg_n_reg,{
      
      # First, check if the values inside meta have changed:
      # R$reg_cost$meta$nreg_now <- 7
      meta_R <- reactiveValuesToList(R)$reg_cost$meta
      meta_L <- reactiveValuesToList(L)$reg_cost$meta
      
      # If something has changed inside meta, we need to have a think about
      # whether we need to generate some more space for adding inputs.
      # For example R$reg_cost$meta$nreg_now <- 7 i.e. the user has put 6 
      # regimen needing inputs.
      if (!identical(meta_R, meta_L)) {
        
        # Check if we need totally new input slots for new regimen that haven't
        # been generated before:
        if (meta_R$nreg_now > meta_L$max_reg_so_far) {
          
          new_sets   <- meta_R$nreg_now - meta_L$max_reg_so_far
          
          # Generate default input sets for the new sets that need generating:
          new_inputs <- get_elem(f_D_dcost_inputs(n = meta_R$nreg_now, th = L$basic$th_cyc),"dat")
          
          # Filter down to just those that need generating (using nth logic)
          new_inputs <- new_inputs[(meta_L$max_reg_so_far+1):meta_R$nreg_now]
          
          # Put the new ones in R and acknowledge that we've now increased the
          # historical maximum of regimen included:
          R$reg_cost$dat                 <- c(R$reg_cost$dat,new_inputs)
          R$reg_cost$meta$max_reg_so_far <- meta_R$nreg_now
          
        } 
        
      }
      
      # Outside of stuff affecting the metatada, we can also affect the existing
      # input sets, for example changing the number oc omponents for one of them,
      # changing costs, dosing, scheduling and so on.
      # 
      # THESE UPDATES WILL GO HERE. Note that each individual set of inputs 
      # will still need unique naming, but we can do this with some numbers. The
      # nested dynamic renderUI elements will drop in and out of existence, but
      # L and R will remain constant, so data won't be lost. 
      
      
      
      
      # Now that we've done all changes, we can update L. This will then trigger
      # changes in the UI. For instance, if I have said that drug 3 has 4 components
      # not 3, the UI will now update (after pressing the button) to giving me
      # the input set for 4 different components rather than just 3.
      L$reg_cost <- R$reg_cost
    })
    
    # Now that we've controlled the flow of information for how many regimen,
    # we can think about rendering some dynamic UIs and tracking their changes:
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#     inputs for selecting regimen ~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    # This responsive dynamic UI will provide a set of buttons allowing one to 
    # choose which regimen and which component you want displayed in the box.
    # 
    # This UI required its own confirm button because the options themselves
    # are a result of the inputs...
    # 
    # The names of the regimen (i.e. the names of the radio buttons) are determined
    # inside of this input, as well as the names of the combinates between them.
    # 
    # this means we need:
    # 
    #  - a way to change the names of the regimen
    #  - a way to select which regimen we want to display in the input box
    #  - a way to change the names of the combinates within the regimen
    #  - a way to select one combinate from the list of options for the selected regimen
    # 
    # i.e., this is very fiddly! However, we can simplify things a bit by using
    # the confirm button and DSLR. We do the following to avoid circular referencing:
    # 
    # - First box selects regimen by providing user with radio buttons to pick from
    #   - note the number of tabs is determined by L$$reg_cost$meta$nreg_now,
    #     so when you change that, it also changes the buttons you can pick from.
    #   - These buttons can only be updated by clicking confirm_i_reg_n_reg at the top
    # - The UI given is two-fold. One lets you rename and the other lets you choose.
    #   This will work with doing both at the same time as it's linked to confirm_i_reg_n_reg
    #   which will then take in the updated inputs from R and pass them along all
    #   in one go.
    # - Second box presents the combinates to select WITHIN the selected regimen. 
    #   - the name of the combinate is changed inside of its input box, so
    #     the options to pick from need updating one at a time. Usually there's
    #     only one or two combinates to consider, so it's not a big task to 
    #     rename, click update, go to next one, rename it etc. you only need
    #     to do this once.
    #  
    #  To get committed regimen names as a vector:
    #   unlist(get_elem(L$reg_cost$dat,"reg_name"))
    #  
    #  To get all committed component names as a list:
    #   get_elem(L$reg_cost$dat,"component_names")
    #  
    #  to get meta data (for which reg and which comb)
    #   L$reg_cost$meta
    #  
    #  that's all the information we actually need for this!
    #  
    
    f_ui_dcost_reg_comb_select <- function(reg_names, comb_nam_list, meta, n_components) {
      
      # Pull out some inputs to start off with:
      nreg       <- meta$nreg_now
      regnams    <- reg_names[1:nreg]
      which_reg  <- meta$which_reg
      which_comb <- meta$which_comb
      
      # First, some quick validation. if which_reg is now impossible because the 
      # user has reduced the number of regimen and now an impossible selection is
      # indicated, then we have to make sure the selected one updates. There are
      # 2 options, make it 1 which looks a bit like a bug, or make it the highest
      # available one. I choose the latter here.
      if (which_reg > length(regnams)) {
        selected_regimen <- regnams[length(regnams)]
      } else {
        selected_regimen <- regnams[which_reg]
      }
      
      # Get the selected regimen as a number rather than string:
      reg_n <- match(selected_regimen,regnams)
      
      # Now, we have to do the same for the combinate within the regimen. if we've
      # reduced the number below meta$which_comb then we need to tweak it
      combs <- comb_nam_list[[reg_n]][1:n_components]
      
      if (which_comb > length(combs)) {
        selected_comb <- combs[length(combs)]
      } else {
        selected_comb <- combs[which_comb]
      }
      
      # Get the selected combinate as a number rather than string:
      comb_n <- match(selected_comb, combs)
      
      # Good, now we're at the starting line. We've determined whether the metadata
      # was giving us impossible selections or not, and have made it so that we
      # know which button should be selected, how many of the possible buttons
      # to actually display, and what to call them all...phew!
      # 
      # For the regimen it's a bit simpler - it's just regnams for the buttons
      # and regnams[reg_n] for the selected one :)
      
      
      i_regimen_select <- radioButtons(
        inputId      = "ui_dcost_regimen_select",
        label        = paste0("Select a regimen from the available ", nreg, ":"),
        choiceValues = 1:nreg,
        choiceNames  = regnams,
        selected     = reg_n
      )
      i_regimen_rename <- textInput(
        inputId      = "ui_dcost_regimen_rename",
        label        = paste0("Rename the selected regimen (", selected_regimen, "):"),
        value        = selected_regimen
      )
      
      
      i_combinate_n <- numericInput(
        inputId      = "ui_dcost_combinate_n",
        label        = paste0("Select the number of combinates for ", selected_regimen, ":"),
        value        = n_components,
        min          = 1
      )
      i_combinate_select <- radioButtons(
        inputId      = "ui_dcost_combinate_select",
        label        = paste0("Select a combinate from the available ", n_components, ":"),
        choiceValues = 1:n_components,
        choiceNames  = combs,
        selected     = comb_n
      )
      
      # Put the ui bits we've made into a container to be passed into the UI
      fluidRow(
        width = 12,
        column(
          12,
          h3("Select regimen:"),
          i_regimen_select,
          hr(),
          h3(paste0("Rename regimen (",selected_regimen,"):")),
          i_regimen_rename,
          hr(),
          h3("Select number of combinates:"),
          i_combinate_n,
          hr(),
          h3("Select combinate:"),
          i_combinate_select
        )
      )
    }
    
    
    # Now that we've made a function which generates the UI to select regimen and
    # combinate, we use it to generate a dynamic UI element which can respond to
    # the number and names of regimen, the number and names of combinates within
    # regimen and allow one to update both without circular referencing.
    
    
    observeEvent(input$confirm_i_reg_n_reg, {
      
      # Collect all of the inputs we need to generate the UI, severing the 
      # live link and instant refresh of the UI element every time something
      # changes (since L is reactive)
      live          <- reactiveValuesToList(L)
      
      reg_names     <- unlist(get_elem(live$reg_cost$dat,"reg_name"))
      comb_nam_list <- get_elem(live$reg_cost$dat,"component_names")
      meta          <- live$reg_cost$meta
      n_components  <- live$reg_cost$dat[[meta$which_reg]]$info$components
      
      rm(live)
      
      output$ui_drug_cost_select <- renderUI({
        f_ui_dcost_reg_comb_select(
          reg_names     = reg_names,
          comb_nam_list = comb_nam_list,
          meta          = meta,
          n_components  = n_components
        )
      })
      
    },
    ignoreNULL = FALSE)
    
    # Here, as we're selecting something to display, it doesn't need the kind
    # of protection that you need with actual model inputs. They can go
    # straight into L and R immediately. Note that the buttons only return
    # strings, so you have to convert to number:
    # 
    # Notice I've put the priority one higher than the other stuff to make sure
    # this happens first.
    observeEvent(input$confirm_i_reg_n_reg, {
      
      which_reg <- isolate(L$reg_cost$meta$which_reg)
      
      # Firstly, update the number of components
      if (!is.null(input$ui_dcost_combinate_n)) {
        n_components <- isolate(input$ui_dcost_combinate_n)
        R$reg_cost$dat[[which_reg]]$info$components <- n_components
        L$reg_cost$dat[[which_reg]]$info$components <- n_components
      }
      
      # There's an issue with the order of events here. If we want to rename
      # this regimen and then change to another one (e.g. select the 3rd one
      # after typing a new name for the first), we need to first rename the 1st one
      # and then update R and L with the selected regimen:
      reg_nam <- as.character(isolate(input$ui_dcost_regimen_rename))
      R$reg_cost$dat[[which_reg]]$info$reg_name <- reg_nam
      L$reg_cost$dat[[which_reg]]$info$reg_name <- reg_nam
      
      # Now update which regimen and combinate have been selected:
      dcost_regimen_select       <- as.numeric(isolate(input$ui_dcost_regimen_select))
      R$reg_cost$meta$which_reg  <- dcost_regimen_select
      L$reg_cost$meta$which_reg  <- dcost_regimen_select
      dcost_combinate_select     <- as.numeric(isolate(input$ui_dcost_combinate_select))
      R$reg_cost$meta$which_comb <- dcost_combinate_select
      L$reg_cost$meta$which_comb <- dcost_combinate_select
      
      
    }, priority = 1001)
    
    
    
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#     inputs for treatment regimen ~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    
    # Now that we've made a system for expanding and contracting the active 
    # set of inputs around treatment regimen from 1 to any number, we need to be
    # able to dynamically generate an ex ante unknowable number of sets of inputs.
    # 
    # We do not know the following:
    # 
    #  - how many sets of inputs we need (as we don't know regimen number beforehand)
    #  - What inputs are needed within each set of inputs (e.g. IV drug, flat dosed etc)
    #    - In exactly the same way as regimen, this is ex ante unknown
    #      - different numbers of units available
    #      - different basis for dosing
    #      - Different sets of inputs regarding the scheduling of dosing
    #       - e.g. every 3 weeks for first year then every 4
    #       - e.g. weekly
    #       - e.g. weekly for 4, bi-weekly for 8, then every 3 weeks, stopping at 2 years
    #      - different initiation periods, or multiple titration steps etc
    #      - Complex PAS factors
    #      - Response-based treatment discontinuation rules (i.e. a proportion at a time point)
    #      - grace periods between treatments
    #      - dose breaks instead of RDIs
    #      - potentially many other factors
    #  - In this example, the costing is slightly simplified and emphasises the ability
    #    to dynamically change the number of components (default is 3). this uses
    #    exactly the same pattern of input --> R --> Button --> calcs update R --> L <- R
    #    to protect the integrity of the model and avoid unecessarily refreshing
    #    the whole UI
    #    
    #    the easiest way to manage this is to make a function to generate a UI
    #    for just ONE combinate. This can take the list of inputs for one combinate
    #    (e.g. D$reg_cost$dat[[1]]$cost[[1]] is the 1st regimen's first combinate
    #    cost inputs, D$reg_cost$dat[[1]]$schedule[[1]] is the scheduling inputs
    #    and so on. to collect all cost inputs get_elem(D$reg_cost$dat,"cost")
    #    helps to organise it all nicely for example)
    #    
    #    Let's give it a try!
    # 
    
    # Function for one input set:
    
    # For an example which works with Default values D:
    # which_reg <- 1
    # which_comb <- 1
    # dcost_cost_list     <- get_elem(D$reg_cost$dat,"cost")
    # dcost_schedule_list <- get_elem(D$reg_cost$dat,"schedule")
    # id_list             <- get_elem(D$reg_cost$dat,"info")
    
    #' Makes a drug cost input ui for all combinates within one treatment regimen
    #' 
    #' @details
    #' This allows you to essentially cycle through different combinations, putting
    #' this function inside of another function which pull out the drug cost related
    #' inputs associated with one combinate within one regimen. 
    #' 
    #' this this allows infinite expansion of comparators and components within
    #' each comparator, generating a situation of extreme flexibility. For instance,
    #' one can define different parts of the same drug within one regimen as different
    #' combinates (e.g. induction period and full dose period) so that breakdown
    #' tables within the CE model disaggregate in that fashion
    #' 
    #' in terms of the user interface of a shiny application, essentially this lets
    #' the UI have the illusion of a nested tabbed box with dynamically determined
    #' nubmers of tabs and sub-tabs, whereas really it's just selecting `which_reg`
    #' and `which_comb`, which then determines where to get the input values from,
    #' which then determine the UI that's rendered
    #' 
    #' 
    #' 
    #' 
    f_ui_dcost_cost_inputs <- function(which_reg,
                                       which_comb,
                                       dcost_cost_list,
                                       dcost_schedule_list,
                                       id_list) {
      # pull out the cost schedule and info
      cost     <- dcost_cost_list[[which_reg]][[which_comb]]
      schedule <- dcost_schedule_list[[which_reg]][[which_comb]]
      info     <- id_list[[which_reg]]
      
      # pull out the regimen and combinate names to help with labelling:
      reg_nam    <- info$reg_name
      comb_nam   <- info$component_names[which_comb]
      
      # Now generate a user interface to allow the user to put in values 
      
      # input for the naming of this component:
      i_comb_nam <- textInput(
        inputId = paste0("i_dcost_reg_",which_reg,"_comb_",which_comb,"_combnam"),
        label   = paste0("Name for the combinate: ", comb_nam, " within ", reg_nam),
        value   = comb_nam
      )
      
      
      # Generate the numeric input for number of unit sizes:
      i_n_units <- numericInput(
        inputId = paste0("i_dcost_reg_",which_reg,"_comb_",which_comb,"_n_units"),
        label   = paste0("Number of different unit sizes for ", comb_nam, " in the ", reg_nam, " regimen"),
        min     = 1,
        max     = 20,
        value   = cost$n_size,
        step    = 1,
        width   = "100%"
      )
      
      # Generate a numeric input for each unit size:
      i_unit_sizes <- tagList(lapply(1:cost$n_size, function(s_unit) {
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb, "_usize_", s_unit),
          label   = NULL,
          value   = cost$units[s_unit],
          min     = 0
        )
      }))
      
      # Do the same for unit costs:
      i_unit_costs <- tagList(lapply(1:cost$n_size, function(s_unit) {
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb, "_ucost_", s_unit),
          label   = NULL,
          value   = cost$c_per_size[s_unit],
          min     = 0
        )
      }))
      
      # The scheduling inputs are a bit "simpler" - they are all just numeric inputs 
      i_scheduling <- tagList(
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_tx_per_tx"),
          label   = paste0("Treatments given in a treated period:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$tx_per_tx,
          width   = "100%",
          min     = 0
        ),
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_cyc_on"),
          label   = paste0("Successive cycles on treatment before break:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$cyc_on,
          width   = "100%",
          min     = 1
        ),
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_cyc_off"),
          label   = paste0("Break length:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$cyc_off,
          width   = "100%",
          min     = 0
        ),
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_tx_max_dur"),
          label   = paste0("Maximum treatment duration in model cycles:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$tx_max_dur,
          width   = "100%",
          min     = 1
        ),
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_dos_per_tx"),
          label   = paste0("Dose given per treatment:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$dos_per_tx,
          width   = "100%",
          min     = 0
        ),
        numericInput(
          inputId = paste0("i_dcost_reg_", which_reg, "_comb_", which_comb,"_rdi"),
          label   = paste0("Relative dose intensity:", comb_nam, " in the ", reg_nam, " regimen"),
          value   = schedule$rdi,
          width   = "100%",
          min     = 0
        )
      )
      
      
      # Bring it all together into an input set
      box(
        title = paste0("Drug cost inputs: ", comb_nam, " in ", reg_nam),
        footer = paste0("In the ",
                        reg_nam,
                        " regimen, there are ",
                        info$components,
                        " components. These are: ",
                        paste(info$component_names[1:(info$components-1)], collapse = ", "),
                        ", and ",
                        info$component_names[info$components],
                        ". You are currently looking at component ", 
                        which_comb,
                        ", which is called ",
                        comb_nam
                      ),
        width = 12,
        solidHeader = TRUE,
        collapsible = FALSE,
        status = "success",
        fluidRow(
          width = 12,
          column(
            12,
            h2(paste0("Rename the combinate ", comb_nam, ":")),
            i_comb_nam,
            hr(),
            h2(paste0("Determine the number of unit sizes for ", comb_nam, ":")),
            i_n_units,
            hr(),
            fluidRow(
              width = 12,
              column(
                6,
                h3(paste0("Unit sizes for ", comb_nam, ":")),
                i_unit_sizes
              ),
              column(
                6,
                h3(paste0("Cost per unit for ", comb_nam, ":")),
                i_unit_costs
              )
            ),
            h2(paste0("Scheduling inputs for ", comb_nam, " here:")),
            i_scheduling
          )
        )
      )
      
    }
    
    
    # Now that we have a function to generate 1 input set for a combination of
    # regimen and combinate, we can bring it all together.
    # 
    # We need:
    #   - a numeric input for the number of regimen (which we have already!)
    #   - a radiobutton input for regimen, which needs to respond to the number of
    #     regimen
    #   - a numeric input for the number of combinates for this regimen
    #   - a system like the regimen system to allow "expansion" beyond the current
    #     parameters for each regimen (i.e. if there's more than 3 combinates)
    #   - a radobutton input for combinate within regimen, which needs to respond
    #     to the number and names of combinates for that regimen
    # 
    # 
    
    
    # For now let's keep it simple and do regimen 1 combinate 1
    
    observeEvent(input$confirm_i_reg_n_reg, {
      
      live       <- reactiveValuesToList(L)
      
      which_reg  <- live$reg_cost$meta$which_reg
      which_comb <- live$reg_cost$meta$which_comb
      
      dcost_cost_list     <- get_elem(live$reg_cost$dat,"cost")
      dcost_schedule_list <- get_elem(live$reg_cost$dat,"schedule")
      id_list             <- get_elem(live$reg_cost$dat,"info")
      
      rm(live)
      
      output$ui_drug_cost_inputs <- renderUI({
        f_ui_dcost_cost_inputs(
          which_reg           = which_reg,
          which_comb          = which_comb,
          dcost_cost_list     = dcost_cost_list,
          dcost_schedule_list = dcost_schedule_list,
          id_list             = id_list
        )
      })
      
    })
    
    # Now - we have a bunch of inputs to observe and link into reactive R
    observe({
      for (r in 1:20) {
        for (c in 1:10) {
          
          ref <- paste0("i_dcost_reg_",r,"_comb_",c)
          
          # all of the input names to be monitoried using observeevent:
          inam_commbnam   <- paste0(ref,"_combnam")
          inam_n_units    <- paste0(ref,"_n_units")
          inam_n_usize    <- paste0(ref,"_usize_",1:20)
          inam_n_ucost    <- paste0(ref,"_ucost_",1:20)
          inam_tx_per_tx  <- paste0(ref,"_tx_per_tx")
          inam_cyc_on     <- paste0(ref,"_cyc_on")
          inam_cyc_off    <- paste0(ref,"_cyc_off")
          inam_tx_max_dur <- paste0(ref,"_tx_max_dur")
          inam_dos_per_tx <- paste0(ref,"_dos_per_tx")
          inam_rdi        <- paste0(ref,"_rdi")
          
          
          # For each one, ask if the input exists. if it does exist then pass input into R:
          if (!is.null(input[[inam_commbnam]])) {
            R$reg_cost$dat[[r]]$info$component_names[c]   <- isolate(input[[inam_commbnam]])
          }
          if (!is.null(input[[inam_n_units]])) {
            R$reg_cost$dat[[r]]$cost[[c]]$n_size          <- isolate(input[[inam_n_units]])
          }
          for (u in 1:20) {
            if (!is.null(input[[inam_n_usize[u]]])) {
              R$reg_cost$dat[[r]]$cost[[c]]$units[u]      <- isolate(input[[inam_n_usize[u]]])
            }
            if (!is.null(input[[inam_n_ucost[u]]])) {
              R$reg_cost$dat[[r]]$cost[[c]]$c_per_size[u] <- isolate(input[[inam_n_ucost[u]]])
            }
          }
          if (!is.null(input[[inam_tx_per_tx]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$tx_per_tx   <- isolate(input[[inam_tx_per_tx]])
          }
          if (!is.null(input[[inam_cyc_on]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$cyc_on      <- isolate(input[[inam_cyc_on]])
          }
          if (!is.null(input[[inam_cyc_off]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$cyc_off     <- isolate(input[[inam_cyc_off]])
          }
          if (!is.null(input[[inam_tx_max_dur]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$tx_max_dur  <- isolate(input[[inam_tx_max_dur]])
          }
          if (!is.null(input[[inam_dos_per_tx]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$dos_per_tx  <- isolate(input[[inam_dos_per_tx]])
          }
          if (!is.null(input[[inam_rdi]])) {
            R$reg_cost$dat[[r]]$schedule[[c]]$rdi         <- isolate(input[[inam_rdi]])
          }
          
          # PHEW! that's a lot of things being watched! However, it's now looking after all
          # possible inputs up to 10 regimen with 10 combinates each with 20 unit
          # sizes in each combinate for each regimen. No model should ever really
          # need more than that!
          
        }
      }
    })
    
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    output$save_debug_i <- downloadHandler(
      filename = function() {
        paste0("input.rds")
      },
      content = function(file) {
        saveRDS(reactiveValuesToList(input), file)
      }
    )
    output$save_debug_L <- downloadHandler(
      filename = function() {
        paste0("L.rds")
      },
      content = function(file) {
        saveRDS(reactiveValuesToList(L), file)
      }
    )
    output$save_S <- downloadHandler(
      filename = function() {
        paste0("S.rds")
      },
      content = function(file) {
        saveRDS(reactiveValuesToList(L), file)
      }
    )
    output$save_debug_R <- downloadHandler(
      filename = function() {
        paste0("R.rds")
      },
      content = function(file) {
        saveRDS(reactiveValuesToList(R), file)
      }
    )
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~# loading in a previous model #~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    
    # When the user loads in a snapshot file, it works in a very similar way to
    # the bookmarking behaviour which shiny has built-in. However the shiny behaviour
    # ONLY extends to the object input. In our case, things are falling in and
    # out of existence within input.
    # 
    # Because of this, if one is to load in a previous model, it will try to
    # update the "value" or "selected" argument of a load of UI elements which
    # do not exist at the time the snapshot is loaded, leading to those UI
    # elements not being updated and the model becoming out of sync with the
    # file that's loaded in.
    # 
    # To get around this, DSLR simply replaces this behaviour by slotting
    # the saved file, S (which is just a saved copy of L), into both R and L, thus
    # updating all inputs to all POSSIBLE UI elements, regardless of whether
    # they exist at the time or not.
    # 
    # The only difference really is that all of the data has somewhere to go
    # with this system, and it doesn't have anywhere to go normally, so it
    # gets "forgotten" by the shiny app.
    
    observeEvent(input$load_S, {
      if(file.exists(input$load_S$datapath)) {
        
        S     <- readRDS(input$load_S$datapath)
        nam_s <- names(S)
        
        # Cycle through all the categories in S putting the data into R and L.
        # This will update the entire app's back end.
        for (cate in nam_s) {
          R[[cate]] <- S[[cate]]
          L[[cate]] <- S[[cate]]
        }
        
        click("confirm_i_reg_n_reg")
        click("confirm_i_reg_n_reg")
        
      }
    }, priority = 9999)
    
    
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
    
    
    
}
