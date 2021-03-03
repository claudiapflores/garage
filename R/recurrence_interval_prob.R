# Function finds the probability in any given year that a flood of that magnitude will be equaled or exceeded 

# Y = years in records
# T = recurrence interval (note: not an independent input, but a dependent input)
# R = magnitude ranking

recurrence_interval_prob <- function(Y,R){
  # Body 
  T <- Y + 1
  Prob <- (1)/(T/R)
  # Error checking
  R = ifelse((R < 0), 
             return("Ranking must be a positive integer"), 
             R)
  # Output is the probability 
  risk <- case_when( # Print the following "" if certain conditions (e.g. <,>,=...) for Prob results are met
    Prob > 0.5 ~ "High",
    Prob == 0.5 ~ "Medium",
    Prob < 0.5 ~ "Low"
  )
  return(risk)
}