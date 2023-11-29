# Master file for DH standard code. calls a set of small scripts containing
# functions within a particular context.
# 
# The script is organised into sections. please click the outline button to
# see a breakdown (upper right of this window, to the right of the "Source" button)


# this bit of code automatically goes through all the scripts in the folder,
# no matter how many sub folders and how it's organised :)
# 
# It won't run the examples, and it won't run itself (i.e. this script)
# 
# DO NOT CHANGE THE NAME OF THIS SCRIPT WITHOUT ALSO UPDATING
# "./R/dh_standard/dh_master.R" BELOW!!!
#
#

scripts <- list.files("./R/dh_standard/", pattern = "^f_", recursive = TRUE, full.names = TRUE)

for (src in scripts){
  if (src != "./R/dh_standard/dh_master.R") {
    if (length(grep("examples",src)) == 0) {
      if(verbose) {
        cat(paste0(
          "Defining all of the functions in: ",
          basename(src),
          " which is within the ",
          dirname(src),
          " folder of this project.\n"
        ))
      }
      source(src)
    }
  }
}
rm(scripts)



