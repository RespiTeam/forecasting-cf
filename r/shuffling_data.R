
library(lubridate)
library(purrr)
library(dplyr)

init_data <- read.csv(file = "data/defaultData_orig.csv")

new_birth_dates <- map(init_data |> pull(birthDate), \(x) as.Date(x) %m+% months(round(runif(1,-2,2),0)))
            
new_birth_dates = unlist(new_birth_dates)

init_data$birthDate = as.Date(new_birth_dates)

# head(init_data)

init_data <- init_data |> 
  mutate(
    genotype = if_else(genotype=='F508del', 'cftr','non_cftr')
) 

write.csv(init_data, file="data/defaultData.csv", row.names = FALSE)

test_data=read.csv(file = "data/defaultData.csv")
