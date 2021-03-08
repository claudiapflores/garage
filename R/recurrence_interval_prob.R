#' recurrence_interval
#'
#' This function finds the probability in any given year that a flood of that magnitude will be equaled or exceeded
#' @param Y years in record
#' @param R magnitude ranking
#' @return the risk in in any given year that a flood of that magnitude will be equaled or exceeded
#' @author Claudia Flores

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
