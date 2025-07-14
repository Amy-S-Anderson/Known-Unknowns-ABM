


#### Functions to Form Skeletal Lesions ####

# These functions need to 
## 1. Generate a random uniform distribution of values indicating a stress exposure capable of causing a lesion to form.
## 2. Identify which agents are A) within the age range for forming lesions and B) have not yet formed lesions.
## 3. Alter the lesion state of individuals who meet the age criteria and drew a value for stress exposure that is more extreme than the threshold set for lesion formation (i.e., smaller than the annual probability of forming a lesion) -- you can think of this as 'significant' stress exposure, in a statistical sense. 


# Function 1: Constant probability of formation across all ages of the developmental window
# Function 2: Linearly decreasing probability of formation with increasing age, hitting zero at the high age cutoff for the developmental window. 

#########################################################################################################################



#### Formation Function 1: Constant Probability of Forming Lesions from Birth to a Specified Age cutoff ####


# agent_census = a data frame of agent traits
# formation_rate = a proportion between 0 and 1, the annual probability of being exposed to a lesion-forming stress
# end_of_window = a numeric value, the oldest age at which an agent can form a new lesions
Form.Lesions.Constant <- function(agent_census, formation_rate, end_of_window){
  agent_census$stress <- runif(nrow(agent_census),0,1)
  agents <- which(agent_census$stress < formation_rate & agent_census$Age < end_of_window)
  
  if(length(agents) > 0) agent_census[agents,]$Lesion <- "Yes"
  
  return(agent_census)
}



