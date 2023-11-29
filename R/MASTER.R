# This is the master script for the project XXX

# Define standard DH code -------------------------------------------------

# The script is initially empty except for calling the master script
# to define all of the Delta Hat "standard code", which includes 
# many convenience functions which ensure correctness and avoid
# repetition of work across different projects:
# 
# 
# If you want to see the files that are being run, set verbose to TRUE. Otherwise
# leave it as FALSE
# Once you've set up your own repo from the template, you can delete any functions that you won't use and keep the ones you will use 

verbose <- FALSE
source("./R/dh_standard/dh_master.R")
rm(verbose)

# ILD SharePoint folder location ------------------------------------------

# This popup window should link to the ILD sharepoint, which contains all of the
# inputs and outputs containing any ILD from the code. NO OUTPUTS CONTAINING ILD SHOULD GO INTO THE PROJECT DIRECTORY!!! 

if (!exists("ild_sp_folder")) {ild_sp_folder <- rstudioapi::selectDirectory()}

# Delta Hat Project Folder Location ---------------------------------------

# This popup window should link to the project folder on the OneDrive (e.g., "1111 - XXXX")
# Any results or outputs that wished to be saved and shared with the wider team should be 
# outputted and stored here (e.g., plots and aggregated results)

if (!exists("project_folder")) {project_folder <- rstudioapi::selectDirectory()}

# After this point, all loading and saving of ILD should refer to ild_sp_folder.
# This can easily be done like this (the function file.path() makes it easy!):
# 
#   - my_data <- read.csv(file.path(ild_sp_folder,"data","raw","thing_i_need.csv"))
#   - write.csv(my_data_to_save,file.path(ild_sp_folder,"outputs","folder1","subfolder1","saved_filename.csv"))
# 
# PLEASE DO THIS FOR ALL LOADING AND SAVING TO MAKE YOUR LIFE EASY
# 
# Also, yes this does work after sharing with a client. Simply send them the 
# ILD folder from the ILD sharepoint, or a zip with the same folder structure.
# Then it is very easy for them as they just select that folder.
# 
# On the other hand, you may want to output some stuff to the project folder instead. 
# MAKE SURE THIS IS ONLY EVER AGGREGATED NON-SENSITIVE STUFF. Here's how to do it
# super easily:
# 
# pw_folder <- file.path(project_folder,"3. Project work")
# ggsave(my_plot,file.path(pw_folder,"999. example stuff","my_plot.png"))
# 
# Very easy to do, avoids hard-coding any paths, allows code to be identical
# on multiple computers at the same time and still work on all of them.
# 


# End of standardised DH code. Everything from this point onwards is code relating
# to this project.


# Project code ------------------------------------------------------------






