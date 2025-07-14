


#### Function to Calculate Agent Deaths ####

# This function needs to:
# 1. Calculate the age-specific probability of dying, as a function of parameter values from a user-chosen Siler model of population mortality schedules. 
# 2. Have agents interact with that probability and either survive it, or not.
# 3. Return a data frame that indicates which agents will survive to the next year of model time, and which will not. Surviving agents will have one year added to their age status, and non-survivors will not be evaluated in subsequent rounds of model time.

######################################################################################################################### Begin



library(tidyverse)


#### Function: Calculate age-dependent risk of mortality (baseline hazard) faced by agents at time (age) t. ####

# age = an integer, k, indicating the time/age value at this stage in the model's run of the cohort of agents.
# mortality_regime = a data frame of siler model parameter values
calculate_age_based_risk <- function(age, mortality_regime){mortality_regime$a1 * exp(-mortality_regime$b1 * age) + # infant mortality
    mortality_regime$a2 + # age-independent mortality 
    mortality_regime$a3 * exp(mortality_regime$b3 * age) # senescent mortality
}


#### Function: Calculate agent deaths at time t. ####

# pop = a data frame of agent traits including agent age, unique ID, and skeletal lesion presence/absence
# baseline_hazard = a data frame of siler model parameter values
# lesion_risk = a number, the proportional hazard modifier describing mortality risk of individuals with lesions relative to baseline hazard
reap <- function(pop, baseline_hazard, lesion_risk){ 

  # create a temporary data frame of living agents
  reaper_math <- data.frame(agent_id = pop$agent_id)
  # calculate their mortality risk based on age under the specified mortality regime
  reaper_math$mortality_risk <- calculate_age_based_risk(age = k, mortality_regime = baseline_hazard)
  # identify the index values for agents with lesions
    with_lesions <- which(pop$Lesion == "Yes")
  # modify the mortality risk for these agents by the proportional constant specified for lesion-associated mortality risk
  if(length(with_lesions) > 0) reaper_math[with_lesions,]$mortality_risk <- reaper_math[with_lesions,]$mortality_risk * lesion_risk
  
    
  # Now, draw a random number between 0 and 1. (Pick a card, any card...)
  reaper_math$death_dice <- sample(runif(n = nrow(pop), min = 0, max = 1), size = nrow(pop), replace = TRUE)
  # If an agent draws a card with a value lower than their calculated mortality risk (based on age and lesion status), then they die.
  if(length(which(reaper_math$death_dice < reaper_math$mortality_risk)) > 0) pop[which(reaper_math$death_dice < reaper_math$mortality_risk),]$Dead <- "Yes"
  
  return(pop)
}





