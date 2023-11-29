## Delta Hat R - template repo

* Any new repositories should be made from this template
* The idea of this repository is for it to be first place people go to when starting a new project that involves analyses in R. Over time this can grow and help avoid duplication of effort, as well as help improve consistency in our work

## Rules for making changes to this repo 

* Please NEVER push to the main branch of this repository. 
* If you want to make a change or add something new, make a new branch, open a pull request and ask someone to review it. This helps to avoid the introduction of errors by having a second set of eyes as QC
* To make changes to an existing function, ideally ask the original author to review your pull request, explaining the improvements you have made

## Info 

* The code for this project is contained in the folder `R`, under the name `MASTER.R`.
* The script can be renamed, and scripts and subfolders created, but the "MASTER.R" script should always be used in the first instance to template from
* Inputs & outputs with ILD should go on the ILD Sharepoint. This is handled in the master script by making the user select that folder on first run. Please read the section in `MASTER.R` which tells you how to easily do this with little to no effort
* For other outputs (e.g. plots and results), use the "project_folder" related to the current project on OneDrive should be used
* If you have a large number of outputs produced (by frequent runs, or large amounts of outputs), consider saving off onedrive, then zipping the files, and uploading this to OneDrive
* Functions are stored in the "dh_standard" folder - any functions that are not required for a project can be removed. Please add any other generalisable functions - again such that it reduces effort for subsequent projects