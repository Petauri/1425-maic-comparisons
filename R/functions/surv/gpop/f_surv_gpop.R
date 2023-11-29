# Functions for gpop adjustment of hazard for oncology models.
# 
# Three methods included:
# 
#   - Assuming everyone is the same age at baseline
#   - Parametrically modelling age distribution for males and females then computing weighted hazard based on baselnie density
#   - IPD-based - drawing an OS line for each baseline individual in the dataset and computing mean hazard over time if each individual is gpop
# 


 
