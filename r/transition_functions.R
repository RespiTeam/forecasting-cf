setTransitionFunctions <- function(eR_vector) {
  
  transFuns=list()
  invlogit <- function(x) exp(x)/(1 + exp(x))
  
  # GROUP _0cftr G---------
  
  # R01: Mild to moderate
  transFuns$mild_to_moderate_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- -3.404598 #-3.417143
    b <- 0.0322104 #0.038112
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R02: Mild to severe
  transFuns$mild_to_severe_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- -5.771441 #-10.828
    b <- 0 #0.089
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R03: Mild to dead
  transFuns$mild_to_dead_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -6.26, -1000) #ifelse(age > 16, -8.88787, -100)
    b <- 0 #ifelse(age > 16, 0.07179 , -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R04: Moderate to mild
  transFuns$moderate_to_mild_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- -0.7943486 #-0.30295
    b <- -0.0419111 #-0.05760
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R05: Moderate to severe
  transFuns$moderate_to_severe_0cftr <- function(age, calTime, duration, cftrTime=NULL){	
    #Coefficients from EDA notebook
    a <- -2.358466 #-2.602126
    b <- 0 #0.011626
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R06: Moderate to dead
  transFuns$moderate_to_dead_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -4.477337, -1000)  #ifelse(age > 16, -5.60136, -100) 
    b <- 0 #ifelse(age > 16, 0.02164, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R07: Severe to mild
  transFuns$severe_to_mild_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- -6.51323  #-2.3653
    b <- 0 #-0.1099
    rate <- invlogit(a + (b*age))	
    return(rate)
  }
  
  # R08: Severe to moderate
  transFuns$severe_to_moderate_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- -2.509121 #-0.60430
    b <- 0 #-0.04273
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R09: Severe to dead
  transFuns$severe_to_dead_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -2.766158, -1000) #ifelse(age > 16, -3.087618, -100)				
    b <- 0 #ifelse(age > 16,  0.008507 , -100)
    rate <- invlogit(a + (b*age))					
    return(rate)					
  }
  
  # R10: Severe to transplant
  transFuns$severe_to_transplant_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook  
    a <- ifelse(age > 16, -2, -1000) #ifelse(age > 16, -0.81900, -100)
    b <- 0 #ifelse(age > 16, -0.03228, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R11: Transplant to dead
  transFuns$transplant_to_dead_0cftr <- function(age, calTime, duration, cftrTime=NULL){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -3.218876, -1000)	 #ifelse(age > 16, -6.08533, -100)	
    b <- 0 #ifelse(age > 16, 0.06924, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # GROUP _cftr -----------
  
  # R01: Mild to moderate
  transFuns$mild_to_moderate_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -3.675
    b1 <- 0.028
    b2 <- -0.839
    b3 <- 0.184
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R02: Mild to severe
  transFuns$mild_to_severe_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -6.881
    b1 <- 0.006
    b2 <- -17.825
    b3 <- -17.851
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R03: Mild to dead
  transFuns$mild_to_dead_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    return(0)
  }
  
  # R04: Moderate to mild
  transFuns$moderate_to_mild_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -0.161
    b1 <- -0.041
    b2 <- 1.324
    b3 <- -0.890
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R05: Moderate to severe
  transFuns$moderate_to_severe_cftr <- function(age, calTime, duration, cftrTime=NULL){	
    # Coefficients from Transitions Report
    b0 <- -1.837
    b1 <- -0.007
    b2 <- -3.025
    b3 <- -0.847
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R06: Moderate to dead
  transFuns$moderate_to_dead_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    return(0)
  }
  
  # R07: Severe to mild
  transFuns$severe_to_mild_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -15.315
    b1 <- -0.161
    b2 <- 18.062
    b3 <- 15.371
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R08: Severe to moderate
  transFuns$severe_to_moderate_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -1.230
    b1 <- 0.003
    b2 <- 1.039
    b3 <- -0.380
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R09: Severe to dead
  transFuns$severe_to_dead_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    return(0)			
  }
  
  # R10: Severe to transplant
  transFuns$severe_to_transplant_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    b0 <- -3.061
    b1 <- 0.008
    b2 <- -19.820
    b3 <- -19.809
    
    tShort = 1
    tLong = 0
    if (cftrTime>2) {
      tShort = 0
      tLong = 1
    }
    
    rate <- invlogit(b0 + (b1*age) +(b2*tShort) + (b3*tLong))*eR_vector[1]
    return(rate)
  }
  
  # R11: Transplant to dead
  transFuns$transplant_to_dead_cftr <- function(age, calTime, duration, cftrTime=NULL){
    # Coefficients from Transitions Report
    return(0)
  }
  
  return(transFuns)
  
}