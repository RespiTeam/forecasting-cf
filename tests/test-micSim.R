

source('r/auxFctMicSim.r')
source('r/micSim.r')
source('r/sim_functions.R')
source('r/transition_functions.R')
source('r/micSim_respi.R')

#getNextStep

initPop <- read.csv(file = "data/defaultData.csv")

initPop <- initPop %>% 
  mutate(
    birthDate = as.double(gsub(pattern = "-", replacement = "", birthDate)), 
  ) 

initPop <- initPop[1:2,]

start_date=as.Date("2020-01-01")
end_date=as.Date("2021-12-31")
simHorizon <- c(startDate = start_date, endDate = end_date)
simHorizon <- as.numeric(gsub(pattern = "-", replacement = "", as.character(simHorizon)))

#Creating a dataframe. Not the best using expan.grid function
maxAge <- 100
absStates <- c("dead", "loss")
lungfunction <- c("mild", "moderate", "severe", "transplant")

stateSpace <- expand.grid(lungfunction = lungfunction)

group='cftr'

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


inmigr_probs <- tibble(
  age_range=c('0-1','1-2','2-18','18-40'), 
  prob=c(0.672,0.103,0.155,0.07)
)

immigrPop <- build_immigr_pop(start_date=as.Date("2020-01-01"), end_date=as.Date("2021-12-31"), new_subjects=3, 100, inmigr_probs)
transitionFuns <- setTransitionFunctions(rep(1,10))

# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# B. DEFINITION OF GLOBAL PARAMETERS
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------   

transitionMatrix <- buildTransitionMatrix(allTransitions = allTransitions, 
                                          absTransitions = absTransitions, 
                                          stateSpace = stateSpace)

# Simulation horizon
simStartInDays <- getInDays(simHorizon[1])
simStopInDays  <- getInDays(simHorizon[2])

# Event queue
queue <- matrix(NA,ncol=6,nrow=0)
colnames(queue) <- c('ID','currTime','currState','currAge','nextState','timeToNextState')
# Global time
t.clock <- simStartInDays  # counts in days since 01-01-1970
# Recording transitions performed
transitions <- matrix(NA,ncol=5,nrow=0)
colnames(transitions) <- c('ID', 'From', 'To', 'transitionTime', 'transitionAge')


# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# E. INITIALIZATION
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

IN <- data.frame(ID=initPop[,'ID'],currState=initPop[,'initState'],age= simStartInDays-getInDays(initPop[,'birthDate']),
                 calTime=rep(simStartInDays,dim(initPop)[1]),stringsAsFactors=FALSE) 


IM <- data.frame(ID=immigrPop[,'ID'], currState=immigrPop[,'immigrInitState'], age=getAgeInDays(immigrPop[,'immigrDate'],immigrPop[,'birthDate']),
                 calTime=getInDays(immigrPop[,'immigrDate']),stringsAsFactors=FALSE) 


result <- NULL

while(is.null(result)) {
  output <- getNextStep_new(IN[1,], t.clock, transitionMatrix, simStartInDays, simStopInDays, transitions, initPop=initPop, immigrPop=immigrPop, transitionFuns, queue)
  if (output$ne[1,2]!=Inf) result <- output$ne[1,2]
}

result
output

inp = IN[1,]
ranAccuracyInDays <- (0:(trunc(2*365.25)+0.99))/365.25
cftrTime <- 1
