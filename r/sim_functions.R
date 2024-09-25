library(chron)
library(snowfall)
library(snow)
library(rlecuyer)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Create immigrate population function.
# new_subjects: The total number of new diagnosed subjects (immigrants) per year
build_immigr_pop<- function(start_date=as.Date("2020-01-01"), end_date=as.Date("2021-12-31"), new_subjects=100, lastID, inmigr_probs) {

  #parsing the dates as Date Objects, just in case
  start_date=as.Date(start_date)
  end_date=as.Date(end_date)
  
  n_years=year(end_date)-year(start_date)+1
  
  # Probabilities by age range. These constants were estimated in EDA-Simulation.qmd file. 
  # We assume that in our simulations the largest age of diagnosis is going to be 40 years old.
  # inmigr_probs=tibble(age_range=c('0-1','1-2','2-18','18-40'), prob=c(0.672,0.103,0.155,0.069))
  
  # Getting the number in each age range
  inmigr_probs=inmigr_probs %>% mutate(
    n=as.integer(prob*new_subjects)
  )
  
  # ages 0-1
  immigrPop0 <- immigrAgeGroupGeneration(Id=1, inmigr_probs, 0,1, n_years) 
    
  # ages 1-2
  immigrPop1 <- immigrAgeGroupGeneration(Id=2, inmigr_probs, 1,2, n_years) 
  
  # ages 2-18
  immigrPop2 <- immigrAgeGroupGeneration(Id=3, inmigr_probs, 2,18, n_years) 
  
  # ages 18-40
  immigrPop3 <- immigrAgeGroupGeneration(Id=4, inmigr_probs, 18,40, n_years)
    
  # joining the migration groups in one dataframe
  #immigrPop <- rbind(immigrPop0, immigrPop1, immigrPop2, immigrPop3)
  immigrPop <- bind_rows(immigrPop0, immigrPop1, immigrPop2, immigrPop3)
  
  if (dim(immigrPop)[1]>0) {
    
    # changing the dash ("-") of the dates
    immigrPop <- immigrPop %>%
      transmute(ID = ID,
                immigrDate = gsub(pattern = "-", replacement = "", immigrDate),
                birthDate = gsub(pattern = "-", replacement = "", birthDate), 
                immigrInitState = immigrInitState)
    
    # Adding the ID to the new subjects, continuining the sequence of the original population
    immigrPop$ID <- c(1:nrow(immigrPop)) + lastID
    
  }
  
  return(immigrPop)
  
}


## Simulation function
cf_simulation <- function(start_date=as.Date("2020-01-01"), end_date=as.Date("2021-12-31"), initPop, immigrPop, group, transitionFuns) {
 
  # Define parameters of the simulation
  simHorizon <- c(startDate = start_date, endDate = end_date)
  simHorizon <- as.numeric(gsub(pattern = "-", replacement = "", as.character(simHorizon)))
  maxAge <- 100
  
  # loss is abrev of loss-to-follow-up
  absStates <- c("dead", "loss")
  lungfunction <- c("mild", "moderate", "severe", "transplant")
  
  #Creating a dataframe. Not the best using expan.grid function
  stateSpace <- expand.grid(lungfunction = lungfunction)
  
  # Setting the functions for the scenarios and the group. group should be 'cftr' or 'non_cftr'
  
  R=c()
  R[1]='mild_to_moderate'
  R[2]='mild_to_severe'
  R[3]='mild_to_dead'
  R[4]='moderate_to_mild'
  R[5]='moderate_to_severe'
  R[6]='moderate_to_dead'
  R[7]='severe_to_mild'
  R[8]='severe_to_moderate'
  R[9]='severe_to_dead'
  R[10]='severe_to_transplant'
  R[11]='transplant_to_dead'
  R[12]='' 
  R[13]='mild_to_loss'
  
  for (i in 1:length(R)) 
    R[i]=paste(R[i],group,sep="_")
  
  allTransitions <- cbind(c("moderate->severe", "severe->moderate", "mild->moderate", "moderate->mild", "mild->severe", 
                            "severe->mild", "severe->transplant"), 
                          c(R[5],R[8], R[1], R[4], R[2], R[7], R[10]))
  
  #absStates: Absorving States
  absTransitions <- rbind(c("mild/dead", R[3]), 
                          c("moderate/dead",R[6]), 
                          c("severe/dead",R[9]), 
                          c("transplant/dead", R[11]), 
                          c("mild/loss", R[13]))
  
  transitionMatrix <- buildTransitionMatrix(allTransitions = allTransitions, 
                                            absTransitions = absTransitions, 
                                            stateSpace = stateSpace)
  
  # Simulations
  micsimCFpop <- micSim(initPop = initPop, transitionMatrix = transitionMatrix, absStates = absStates, 
                        maxAge = maxAge, simHorizon = simHorizon, immigrPop = immigrPop, transitionFuns=transitionFuns)
  
  return(micsimCFpop)
  
}

# Function to convert to longitudinal data #
Join_into_Longi = function(initPop,micsimCFpop,immigrPop,start_date) {
  
  # In exceptional cases the patient changes states more than once IN THE SAME DAY, these cases are problematic because generate duplicate records.
  # Therefore, in those cases we will keep only the first change.
  micsimCFpop = micsimCFpop %>% 
    group_by(ID,transitionTime) %>% 
    mutate(reps=n()) %>% 
    ungroup() %>% 
    filter(reps==1 | (reps>1 & initState==From)) %>% 
    select(!reps)
  
  # Selecting only the transitions
  transitions = micsimCFpop %>% 
    filter(!is.na(transitionTime)) %>% 
    mutate(
      birthDate = ymd(birthDate),
      current_date = ymd(as.character(transitionTime)),
      current_year = year(current_date)
    ) %>%
    select(ID,birthDate,To,current_date,current_year) %>%
    rename(initState=To)
  
  # Formatting immigrant population
  immigrPop <- immigrPop %>%
    transmute(ID, birthDate = ymd(birthDate), initState=immigrInitState, current_date = ymd(immigrDate)) %>%
    mutate(
      current_year = year(current_date)
    )
  
  # Formatting initial population
  initPop <- initPop %>%
    transmute(ID, birthDate = ymd(as.character(birthDate)), initState) %>%
    mutate(
      current_date = start_date,
      current_year=year(start_date)
    )
    
  return(
    
    bind_rows(initPop,transitions,immigrPop,.id = "source")
          
  )
  
}


# Function to merge inital Data with longitudinal data #
# It keeps only the last record per patient in the longitudinal data #
mergingSimData<-function(initData,pop_long) {
  
  # Selecting the last appearance of each subject
  last_pop <- pop_long %>% group_by(ID) %>% top_n(1, current_date)
  
  # When a transition happens the same day than its initial state could result in a duplicate, so in that cases we take the last source, ie. immigration initial status (initPop=1,transitions=2,immigrPop=3)
  last_pop <- last_pop %>% group_by(ID) %>% top_n(1, source)
  
  # Adding the result of the simulation to initial dataset
  initData = initData |>select(!birthDate) %>% 
    full_join(
      last_pop %>% 
        select(ID,birthDate,initState,current_date) %>%
        rename(
          new_state=initState
        ),
      by=join_by(ID)
    )
  
  return(initData)
  
}

mergingSimData2<-function(initData,pop_long, period_length, start_date, end_date=NA) {
  
  #Getting the list of milestones
  start_year=year(start_date)-1
  if (is.na(end_date)) end_date=max(pop_long$current_date)
  
  miles=seq.int(start_year, year(end_date), period_length)
  if (!year(end_date) %in% miles) miles=c(miles,year(end_date))
  # removing first year because it is equal to initial state
  miles=miles[-1]
  
  last_pop=tibble()
  for (y in miles) {

    # Selecting the last appearance of each subject
    last_pop_y <- pop_long |> filter(current_year<=y) %>% group_by(ID) %>% top_n(1, current_date)
    
    if (dim(last_pop_y)[1]>0) {
    
      # When a transition happens the same day than its initial state could result in a duplicate, so in that cases we take the last source
      last_pop_y <- last_pop_y %>% group_by(ID) %>% top_n(1, source)
      
      # if (y == year(start_date)) last_pop_y['milestone']="initState"
      # else 
      last_pop_y['milestone']=paste("state_at_",y,sep="")
      
      
      # binding final states at each milestone
      if (dim(last_pop)[1]==0) last_pop=last_pop_y
      else
        last_pop = bind_rows(last_pop,
                             last_pop_y)
      
    }
  }
  
  last_pop_wide <- last_pop %>% select(ID, birthDate, initState, milestone) |>
    pivot_wider(names_from = milestone, values_from = initState)
  
  # Adding the result of the simulation to initial dataset
  initData = initData |> select(!birthDate) %>% 
    full_join(
      last_pop_wide,
      by=join_by(ID)
    )
  
  return(initData)
  
}

iteratingSimulations2 <- function(data, start_date, end_date, nIter, period_length, new_cftr, new_0cftr, eR_vector, inmigr_probs) {
  
  simResults=tibble()
  simResults2=tibble()
  
  for (i in 1:nIter) {
    
    initData_cftr = data |> filter(genotype=="cftr") |> select(!genotype)
    initData_0cftr = data |> filter(genotype=="non_cftr") |> select(!genotype)
    
    print(paste('starting simulation',i,sep=' '))
    
    # Constructing a new initial population for cftr group
    initPop_cftr=initData_cftr %>% 
      mutate(
        birthDate = as.double(gsub(pattern = "-", replacement = "", birthDate)), 
      ) %>% filter(initState!='dead' & initState!='loss')
    
    # Converting from tibble to dataframe
    # if it stays as tibble the following error occurs:
    # Error in getInDays(initPop[, "birthDate"]) : 
    #  'list' object cannot be coerced to type 'double'
    initPop_cftr=as.data.frame(initPop_cftr)
    
    # Constructing a new initial population for non_cftr group
    initPop_0cftr=initData_0cftr %>% 
      mutate(
        birthDate = gsub(pattern = "-", replacement = "", birthDate), 
      ) %>% filter(initState!='dead' & initState!='loss')
    
    # Converting from tibble to dataframe
    # if it stays as tibble the following error occurs:
    # Error in getInDays(initPop[, "birthDate"]) : 
    #  'list' object cannot be coerced to type 'double'
    initPop_0cftr=as.data.frame(initPop_0cftr)
    
    ## Generating immigration groups
    # for patients without cftr mutation
    lastID=max(initPop_0cftr$ID)
    inmigrPop_0cftr=build_immigr_pop(start_date, end_date, new_0cftr,lastID, inmigr_probs)
    
    # for patients with cftr mutation
    lastID=max(initPop_cftr$ID)
    inmigrPop_cftr=build_immigr_pop(start_date, end_date, new_cftr,lastID, inmigr_probs)
    
    # DEFINING TRANSITION PROBABILITIES #
    transFuns <- setTransitionFunctions()
    
    # Running simulation for patients without cftr mutation
    transitions_0cftr=cf_simulation(start_date, end_date, initPop=initPop_0cftr, immigrPop=inmigrPop_0cftr,'0cftr',transFuns)
    
    ## Running simulation for patients with cftr mutation
    transitions_cftr=cf_simulation(start_date, end_date, initPop=initPop_cftr, immigrPop=inmigrPop_cftr,'cftr',transFuns)
    
    # Joining the datasets
    popcftr_long=Join_into_Longi(initPop_cftr,transitions_cftr,inmigrPop_cftr,start_date)
    pop0cftr_long=Join_into_Longi(initPop_0cftr,transitions_0cftr,inmigrPop_0cftr,start_date)
    
    # Formatting and merging with initial datasets for number of patients
    endData = formattingForSummary1(initData_cftr,popcftr_long,initData_0cftr,pop0cftr_long,period_length,start_date,end_date,i)
    
    ## joining the result of this iteration with the previous
    simResults = bind_rows(simResults,
                           endData)
    
    # Formatting and merging with initial datasets for survival analysis
    endData = formattingForSummary2(initData_cftr,popcftr_long,initData_0cftr,pop0cftr_long,i)
    
    ## joining both groups in the same dataset
    simResults2 = bind_rows(simResults2,
                           endData)
    
  }
  
  # Building summarize results
  summarizeResults=buildingSummarizeData(simResults, start_date)
  
  # Building dataset for plotting K-M curve
  summarizeKMResults=buildingSummarizeKMData(simResults2, end_date)
  
  return(
    list("qty"=summarizeResults,"km"=summarizeKMResults)
  )
  
}

buildingSummarizeData<- function(simResults, start_date) {
  
  # Getting the milestone years
  years <- simResults %>%
    select(contains("state_at_"))
  
  years = names(years)
  years_int= sub("^state_at_", "", years)
  
  #summarizing for initState
  new_state=sym("initState")
  last_day = as.Date(paste(year(start_date)-1,"12-31",sep="-"))
  #reducing the dataset to keep only the summarize groups of interest
  simResults_temp = simResults |>
    filter(!is.na(!!new_state)) |> 
    mutate(
      current_age=as.numeric((last_day - birthDate)/365),
      age_range=case_when(current_age <=18~'Pediatrics',
                          current_age >18~'Adults',
                          .default=NA)
    ) |>
    select(iteration,group,age_range,!!new_state) |> 
    group_by(iteration,group, age_range, !!new_state) |>
    summarize(
      value=n(), 
      .groups="drop"
    ) |>
    group_by(group,age_range,!!new_state) |>
    summarise(
      med = median(value, na.rm=TRUE),
      .groups="drop"
    )
  
  simResults_temp = simResults_temp |> mutate(
    milestone=as.numeric(year(start_date)-1)
  ) |> rename(
    state=!!new_state
  )
  
  summarizeResults <- simResults_temp
  
  for (y in years_int) {
    
    new_state=sym(paste("state_at_",y,sep=""))
    last_day = as.Date(paste(y,"12-31",sep="-"))
    #reducing the dataset to keep only the summarize groups of interest
    simResults_temp = simResults |>
      filter(!is.na(!!new_state)) |> 
      mutate(
        current_age=as.numeric((last_day - birthDate)/365),
        age_range=case_when(current_age <=18~'Pediatrics',
                            current_age >18~'Adults',
                            .default=NA)
      ) |>
      select(iteration,group,age_range,!!new_state) |> 
      group_by(iteration,group, age_range, !!new_state) |>
      summarize(
        value=n(), 
        .groups="drop"
      ) |>
      group_by(group,age_range,!!new_state) |>
      summarise(
        med = median(value, na.rm=TRUE),
        .groups="drop"
      )
    
    simResults_temp = simResults_temp |> mutate(
      milestone=as.numeric(y)
    ) |> rename(
      state=!!new_state
    )
    
    #Adding the reduced dataset to summarizeResults
    summarizeResults = bind_rows(summarizeResults,
                                 simResults_temp)
    
    
  }
  
  return(summarizeResults)
  
}


buildingSummarizeKMData <- function(simResults2, end_date) {
  
  kmResults=tibble()
  
  kmData=simResults2 |> 
    mutate(
      current_date = if_else(new_state==0, end_date, current_date),
      time=round(as.numeric(difftime(current_date,birthDate, units="days")/365),2),
      group=as.factor(group)
    ) |> rename(
      status=new_state
    )
  
  for (i in unique(simResults2$iteration)) {
    
    sp <- summary(survfit2(Surv(time, status) ~ group, data = kmData |> filter(iteration==i)), times = c(seq(0, 90, by = 2)))
    kmResults=bind_rows(kmResults,
                        tibble(time=sp$time, atRisk=sp$n.risk, nEvents=sp$n.event, survival=sp$surv, group=sp$strata, iteration=i)
    )
    
  }
  
  # summarizing
  kmResults = kmResults |> mutate(
    group=str_remove(group, "group=")
    ) |> group_by(time, group) |> 
    summarise(
      survival=mean(survival),
      .groups="drop"
    )
  
  return(kmResults)
  
}


initial_data_validation <- function(df) {
  
  valid=TRUE
  msg=""
  #columns names 
  if (sum(names(df) %in% c("ID","birthDate","genotype","initState"))!=4) {
    valid=FALSE
    msg="Error in columns names"
    return(
      list(result=valid, msg=msg)
    )
  }
  
  #genotypes
  analyzed_values=as.data.frame(table(df |> select(genotype) |> pull())) |> select(Var1) |> pull()
  if (sum(analyzed_values == c("cftr","non_cftr"))!=2) {
    valid=FALSE
    msg="Error in genotype values"
    return(
      list(result=valid, msg=msg)
    )
  }
  
  #states
  analyzed_values=as.data.frame(table(df |> select(initState) |> pull())) |> select(Var1) |> pull()
  if (sum(analyzed_values == c("mild","moderate","severe","transplant"))!=4) {
    valid=FALSE
    msg="Error in state values"
    return(
      list(result=valid, msg=msg)
    )
  }
  
  # validating IDs
  analyzed_group = df |> filter(genotype=="cftr") |> select(ID) |> pull()
  if (length(unique(analyzed_group))!=length((analyzed_group))) {
    valid=FALSE
    msg="There are duplicate IDs in CFTR modulator group"
    return(
      list(result=valid, msg=msg)
    )
  }
  
  # validating IDs
  analyzed_group = df |> filter(genotype=="non_cftr") |> select(ID) |> pull()
  if (length(unique(analyzed_group))!=length((analyzed_group))) {
    valid=FALSE
    msg="There are duplicate IDs in non-CFTR modulator group"
    return(
      list(result=valid, msg=msg)
    )
  }
  
  return(
    list(result=valid, msg=msg)
  )
  
}

setTransitionFunctions <- function() {
  
  transFuns=list()
  invlogit <- function(x) exp(x)/(1 + exp(x))
  
  # _0cftr GROUP
  
  # R01: Mild to moderate
  transFuns$mild_to_moderate_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -3.404598 #-3.417143
    b <- 0.0322104 #0.038112
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R02: Mild to severe
  transFuns$mild_to_severe_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -5.771441 #-10.828
    b <- 0 #0.089
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R03: Mild to dead
  transFuns$mild_to_dead_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -6.26, -1000) #ifelse(age > 16, -8.88787, -100)
    b <- 0 #ifelse(age > 16, 0.07179 , -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R04: Moderate to mild
  transFuns$moderate_to_mild_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -0.7943486 #-0.30295
    b <- -0.0419111 #-0.05760
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R05: Moderate to severe
  transFuns$moderate_to_severe_0cftr <- function(age, calTime, duration){	
    #Coefficients from EDA notebook
    a <- -2.358466 #-2.602126
    b <- 0 #0.011626
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R06: Moderate to dead
  transFuns$moderate_to_dead_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -4.477337, -1000)  #ifelse(age > 16, -5.60136, -100) 
    b <- 0 #ifelse(age > 16, 0.02164, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R07: Severe to mild
  transFuns$severe_to_mild_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -6.51323  #-2.3653
    b <- 0 #-0.1099
    rate <- invlogit(a + (b*age))	
    return(rate)
  }
  
  # R08: Severe to moderate
  transFuns$severe_to_moderate_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -2.509121 #-0.60430
    b <- 0 #-0.04273
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R09: Severe to dead
  transFuns$severe_to_dead_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -2.766158, -1000) #ifelse(age > 16, -3.087618, -100)				
    b <- 0 #ifelse(age > 16,  0.008507 , -100)
    rate <- invlogit(a + (b*age))					
    return(rate)					
  }
  
  # R10: Severe to transplant
  transFuns$severe_to_transplant_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook  
    a <- ifelse(age > 16, -2, -1000) #ifelse(age > 16, -0.81900, -100)
    b <- 0 #ifelse(age > 16, -0.03228, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # R11: Transplant to dead
  transFuns$transplant_to_dead_0cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -3.218876, -1000)	 #ifelse(age > 16, -6.08533, -100)	
    b <- 0 #ifelse(age > 16, 0.06924, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  # _cftr GROUP
  
  # R01: Mild to moderate
  transFuns$mild_to_moderate_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -3.870427  #-3.417143
    b <- 0.0130938 #0.038112
    rate <- invlogit(a + (b*age))*eR_vector[1]
    return(rate)
  }
  
  # R02: Mild to severe
  transFuns$mild_to_severe_cftr <- function(age, calTime, duration){
    # Coefficients from EDA notebook
    # a <- -10.828
    # b <- 0.089
    # rate <- invlogit(a + (b*age))*eR_vector[2]
    # return(rate)
    return(0)
  }
  
  # R03: Mild to dead
  transFuns$mild_to_dead_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -6.26, -1000)  #ifelse(age > 16, -8.88787, -100)
    b <- 0 #ifelse(age > 16, 0.07179 , -100)
    rate <- invlogit(a + (b*age))*if_else(age>16,eR_vector[3],1)
    return(rate)
  }
  
  # R04: Moderate to mild
  transFuns$moderate_to_mild_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -1.323925 #-0.30295
    b <- -0.0409952 #-0.05760
    rate <- invlogit(a + (b*age))*eR_vector[4]
    return(rate)
  }
  
  # R05: Moderate to severe
  transFuns$moderate_to_severe_cftr <- function(age, calTime, duration){	
    #Coefficients from EDA notebook
    a <- -2.259841 #-2.602126
    b <- -0.0345258 # 0.011626
    rate <- invlogit(a + (b*age))*eR_vector[5]
    return(rate)
  }
  
  # R06: Moderate to dead
  transFuns$moderate_to_dead_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -4.477337, -1000) # ifelse(age > 16, -5.60136, -100) 
    b <- 0 #ifelse(age > 16, 0.02164, -100)
    rate <- invlogit(a + (b*age))*if_else(age>16,eR_vector[6],1)
    return(rate)
  }
  
  # R07: Severe to mild
  transFuns$severe_to_mild_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    # a <- -2.3653
    # b <- -0.1099
    # rate <- invlogit(a + (b*age))*eR_vector[7]
    # return(rate)
    return(0)
  }
  
  # R08: Severe to moderate
  transFuns$severe_to_moderate_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- -2.599317 #-0.60430
    b <- 0  #-0.04273
    rate <- invlogit(a + (b*age))*eR_vector[8]
    return(rate)
  }
  
  # R09: Severe to dead
  transFuns$severe_to_dead_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -2.766158, -1000) #ifelse(age > 16, -3.087618, -100)				
    b <- 0 #ifelse(age > 16,  0.008507 , -100)
    rate <- invlogit(a + (b*age))*if_else(age>16,eR_vector[9],1)
    return(rate)					
  }
  
  # R10: Severe to transplant
  transFuns$severe_to_transplant_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook  
    a <-  ifelse(age > 16, -2, -1000) #ifelse(age > 16, -0.81900, -100)
    b <- 0 #ifelse(age > 16, -0.03228, -100)
    rate <- invlogit(a + (b*age))*if_else(age>16,eR_vector[10],1)
    return(rate)
  }
  
  # R11: Transplant to dead
  transFuns$transplant_to_dead_cftr <- function(age, calTime, duration){
    #Coefficients from EDA notebook
    a <- ifelse(age > 16, -3.218876, -1000)	 #ifelse(age > 16, -6.08533, -100)	
    b <- 0 #ifelse(age > 16, 0.06924, -100)
    rate <- invlogit(a + (b*age))
    return(rate)
  }
  
  return(transFuns)
  
}

formattingForSummary1 <- function(initData_cftr,popcftr_long,initData_0cftr,pop0cftr_long,period_length,start_date,end_date,i) {
  
  ## Group cftr
  Data_cftr = mergingSimData2(initData_cftr,popcftr_long, period_length, start_date, end_date)
  ## Group 0cftr
  Data_0cftr = mergingSimData2(initData_0cftr,pop0cftr_long, period_length, start_date, end_date)
  
  ## joining both groups in the same dataset
  endData = bind_rows(Data_cftr,
                      Data_0cftr,
                      .id = 'group') %>%
    mutate(
      group = if_else(group == 1, 'cftr', 'non_cftr')
    )
  
  endData['iteration']=i
  
  return(endData)
  
}

formattingForSummary2 <- function(initData_cftr,popcftr_long,initData_0cftr,pop0cftr_long,i) {
  
  ## Following mergings only keep the last change for each patient
  ## Group cftr 
  Data_cftr = mergingSimData(initData_cftr,popcftr_long)
  ## Group 0cftr
  Data_0cftr = mergingSimData(initData_0cftr,pop0cftr_long)
  
  endData = bind_rows(Data_cftr,
                      Data_0cftr,
                      .id = 'group') %>%
    mutate(
      group = if_else(group == 1, 'cftr', 'non_cftr'),
      new_state= if_else(new_state=='dead',1,0)
    ) 
  
  endData['iteration']=i
  
  return(endData)
  
}

immigrAgeGroupGeneration <- function(Id, inmigr_probs, minAge, maxAge, n_years) {
  
  ageRange=paste(minAge,"-",maxAge, sep="")
  
  n=as.integer(inmigr_probs %>% filter(age_range==ageRange) %>% select(n))
  M=n*n_years
  
  if (n>0) {
    
    immigrDatesRange <- as.numeric(c(start_date, end_date))
    
    immigrDates <- as.character(chron(immigrDatesRange[1] + runif(M, min=0 , max = diff(immigrDatesRange)),
                                      out.format = c(dates = "d/m/year", times = "h:m:s")))
    immigrDates <- as.Date(immigrDates, format("(%d/%b/%Y"))
    
    immigrAges <- round(runif(M, min = minAge, max = maxAge) * 365, 0)
    immigrBirthDates <- immigrDates - lubridate::days(immigrAges)
    return(
      data.frame(ID = Id, immigrDate = immigrDates, birthDate=immigrBirthDates, 
                             immigrInitState = "mild")
    )
    
  }
    
  return(
    data.frame()
  )
  
}
