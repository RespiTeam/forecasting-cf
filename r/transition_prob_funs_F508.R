
# Define transition probabilities
invlogit <- function(x) exp(x)/(1 + exp(x))

# R01: Mild to moderate
mild_to_moderate_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- -3.417143
  b <- 0.038112
  rate <- invlogit(a + (b*age))*eR_vector[1]
  return(rate)
}

# R02: Mild to severe
mild_to_severe_F508 <- function(age, calTime, duration){
  # Coefficients from EDA notebook
  a <- -10.828
  b <- 0.089
  rate <- invlogit(a + (b*age))*eR_vector[2]
  return(rate)
}

# R03: Mild to dead
mild_to_dead_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- ifelse(age > 16, -8.88787, -100)
  b <- ifelse(age > 16, 0.07179 , -100)
  rate <- invlogit(a + (b*age))*eR_vector[3]
  return(rate)
}

# R04: Moderate to mild
moderate_to_mild_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- -0.30295
  b <- -0.05760
  rate <- invlogit(a + (b*age))*eR_vector[4]
  return(rate)
}

# R05: Moderate to severe
moderate_to_severe_F508 <- function(age, calTime, duration){	
  #Coefficients from EDA notebook
  a <- -2.602126
  b <- 0.011626
  rate <- invlogit(a + (b*age))*eR_vector[5]
  return(rate)
}

# R06: Moderate to dead
moderate_to_dead_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- ifelse(age > 16, -5.60136, -100) 
  b <- ifelse(age > 16, 0.02164, -100)
  rate <- invlogit(a + (b*age))*eR_vector[6]
  return(rate)
}

# R07: Severe to mild
severe_to_mild_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- -2.3653
  b <- -0.1099
  rate <- invlogit(a + (b*age))*eR_vector[7]
  return(rate)
}

# R08: Severe to moderate
severe_to_moderate_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- -0.60430
  b <- -0.04273
  rate <- invlogit(a + (b*age))*eR_vector[8]
  return(rate)
}

# R09: Severe to dead
severe_to_dead_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- ifelse(age > 16, -3.087618, -100)				
  b <- ifelse(age > 16,  0.008507 , -100)
  rate <- invlogit(a + (b*age))*eR_vector[9]
  return(rate)					
}

# R10: Severe to transplant
severe_to_transplant_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook  
  a <-  ifelse(age > 16, -0.81900, -100)
  b <- ifelse(age > 16, -0.03228, -100)
  rate <- invlogit(a + (b*age))*eR_vector[10]
  return(rate)
}

# R11: Transplant to dead
transplant_to_dead_F508 <- function(age, calTime, duration){
  #Coefficients from EDA notebook
  a <- ifelse(age > 16, -6.08533, -100)	
  b <- ifelse(age > 16, 0.06924, -100)
  rate <- invlogit(a + (b*age))
  return(rate)
}

# R13: From mild to loss to follow-up
emigrRates_F508 <- function(age, calTime, duration){
  # Rate from EDA notebook
  rate <- ifelse(20 >= age & age < 60, 0.0129, 0)
  return(rate)
}





