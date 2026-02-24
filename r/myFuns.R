
library(dplyr)

groupingComorbiditiesRatios <- function(ratiosData, descriptionData) {
  
  comorList <- descriptionData |> 
    purrr::pmap(function(Variable, Description, group, state, age_range, sex) {
      
      keys <- c()
      if (group == 1) keys <- c(keys,"group")
      if (state == 1) keys <- c(keys,"state")
      if (age_range == 1) keys <- c(keys,"age_range")
      if (sex == 1) keys <- c(keys,"sex")
      
      col_sym <- ensym(Variable)
      
      ratiosData <- ratiosData |> 
        select(group, state, age_range, sex, {{col_sym}}) |> 
        summarize(
          ratio=mean({{col_sym}}),
          .by=dplyr::all_of(keys)
        )
      
      if (stringr::str_detect(Variable,"pregnancy")) {
        
        ratiosData |> 
          filter(sex=="Female") |> 
          select(!sex)
      }
      
      ratiosData
      
    })
  
  names(comorList)=descriptionData$Variable
  comorList
  
}

