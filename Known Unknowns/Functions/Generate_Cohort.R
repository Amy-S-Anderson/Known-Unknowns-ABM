
### Function: Generate a Cohort to run through an ABM in which individuals develop skeletal lesions and die.
# Amy Anderson
# 13 July, 2023



#### Build A Starting Cohort ####

Generate.Cohort <- function(n){
 cohort <- data.frame(agent_id = 1:n, # each person gets a unique ID
                      Age = 0, # newborns
                      Lesion = "No", # no one is born with lesions
                      Dead = "No")
}








CohortGen <- function(nPop1, frailty_mean, frailty_sd){
  #Create a population of susceptibles
  Cohort1.1 <- data.frame(CohortNo = 1:nPop1, # each person gets a unique ID
                          Lesion = "No", # no one has skeletal lesions at the start
                          Frailty = rgamma(nPop1, shape = (frailty_mean/frailty_sd)^2, 
                                           rate = frailty_mean/(frailty_sd)^2), # Frailty at Birth, a gamma distribution with mean and SD specified in the function call
                          Age = 0, # Newborns
                          Dead = "No") # ...Not yet.
  return(Cohort1.1)
}