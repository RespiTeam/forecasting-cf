
getNextStep_new <- function(inp, t.clock, transitionMatrix, simStartInDays, simStopInDays, transitions, initPop, immigrPop, transitionFuns, queue, isIMInitEvent=F, schoolEnrol=F) {     
  # Transform input
  id <- as.numeric(unlist(inp[1], use.names = FALSE)) 
  currState <- as.character(unlist(inp[2], use.names = FALSE)) 
  currAge <- as.numeric(unlist(inp[3], use.names = FALSE)) # age in days 
  # calendar time in days since 01-01-1970, 
  # At the beginning of the simulation its value is equal to simStartInDays for all subjects in initial population and it is equal to the immigration date for immigrants
  # During the simulation it is equal to the current time
  calTime <- as.numeric(unlist(inp[4], use.names = FALSE)) 
  

  if (id %in% as.numeric(immigrPop[,'ID'])) {
    # it assumes that immigrant patients starts taking cftr as soon as they enter the simulation
    cftrStart <- getInDays(immigrPop[,'immigrDate'])
  } else {
    cftrStart <- 0
    if (!is.null(initPop[id,'cftrStart'])) cftrStart <- getInDays(initPop[id,'cftrStart'])
  }
  cftrTime <- (calTime - cftrStart)/365.25 # in years
  
  
  lagToWaitingTime <- 0 # in years
  if(isIMInitEvent) {
    # first event of an immigrant: he/she enters the population later than sim. starting time
    lagToWaitingTime <- (calTime - simStartInDays)/365.25 # in years
  }
  
  #cat('\n-----\nID: ',id,'\n')
  #print(inp)
  ageInYears <- currAge/365.25 
  #cat('Age: ',ageInYears,' - CalTime: ',getYear(calTime),'-',getMonth(calTime),'-',getDay(calTime),'\n')
  # Possible destination states
  possTr <- transitionMatrix[match(currState, rownames(transitionMatrix)),]    
  possTr <- possTr[which(possTr !=0)]
  nextEventMatrix <- matrix(0, ncol=2, nrow=length(possTr))   
  
  # How many years (along age scale) remain until `maxAge'? 
  ranMaxAge <- (maxAge-0.01)-ageInYears     
  # How many years (along cal. time scale) remain until simulation end?
  ranMaxYear <-  (simStopInDays - calTime)/365.25  
  
  # Identify the time range in years that should be considered. 
  ran <- min(ranMaxYear,ranMaxAge)   
  #cat('Ran: ',ran,' - ranMaxAge: ',ranMaxAge,' - ranMaxYear: ',ranMaxYear,'\n')
  #ranAge <- c(ageInYears,ageInYears+ranMaxAge) # age range in years
  #ranYear <- c(getYear(calTime), getYear(calTime)+ran) # year range in years
  #cat('RanAge: ',ranAge,' - ranYear: ',ranYear,'\n')
  
  # Extract transition history of individual until current cal. time.
  historiesInd <- transitions[as.numeric(transitions[,'ID']) %in% id & transitions[,'transitionTime'] <= calTime,,drop=F]
  iPIDout <- as.numeric(initPop[,'ID']) %in% id 
  initPopInd <- initPop[iPIDout,]
  birthTime <-  initPopInd['birthDate']
  initState <-  as.character(unlist(initPopInd['initState']))
  # Extract for each state the duration until transition (in days). 
  # Here, we have to differ between states of which we do not know when they are entered (i.e., `initial states' of members
  # of the starting population and the states of migrants when they entered the country), and 
  # the states we know the `entering date' as well as the `leaving date' (if the state has been left). 
  if(getInDays(birthTime) < simStartInDays | id %in% as.numeric(immigrPop[,'ID'])) {  
    dur <- rbind(c(initState,NA),historiesInd[,c('To','transitionTime')]) 
    dur <- cbind(dur,c(diff(as.numeric(dur[,2])),0))
    colnames(dur) <- c('TransitionTo','AtTime','durUntil')
    dur[which(is.na(dur[,'AtTime'])),'durUntil'] <- NA
  } else {  # Individual is born during simulation.
    birthTime <- getInDays(initPop[as.numeric(initPop[,'ID']) %in% id, 'birthDate']) # in days since 01-01-1970 
    dur <- rbind(c(initState,birthTime),historiesInd[,c('To','transitionTime')]) 
    dur <- cbind(dur,c(diff(as.numeric(dur[,2])),0))
    colnames(dur) <- c('TransitionTo','AtTime','durUntil')
  }
  # Compute for each possible destination state a waiting time.  
  for(i in 1:length(possTr)){
    tr <- possTr[i]
    
    destState <-  names(tr)
    cS <- strsplit(currState,'/')[[1]] # TODO: to speed up, this operation has to be improved/changed
    dS <- strsplit(destState, '/')[[1]] # TODO: to speed up, this operation has to be improved/changed
    # To determine the duration (time elapsed since last transition) that applies for the considered destination state,
    # we have to determine the duration since the last change in the covariate affected. 
    # For example, to specify the time being married, we have to determine the duration since (last) marriage. 
    covToCh <- which((cS==dS)==F)
    durSinceLastCovCh <- Inf  # For the transition to `dead' so far the time elapsed since the last transition does not play any role.  
    if(length(covToCh)==1){
      covHist <- do.call(rbind,sapply(dur[,'TransitionTo'],strsplit,split='/'))[,covToCh]
      idd <- which(covHist==cS[covToCh])
      if(length(idd)>1){
        if(F %in% (diff(idd)==1)){
          y <- rev(idd)[c(-1,diff(rev(idd)))==-1]
          idd <- rev(y)[c(diff(rev(y)),1)==1]
        }
      }
      durSinceLastCovCh <- sum(as.numeric(dur[idd,'durUntil'])) # If I do not know how long an individual already is in a state: This gives NA.
      if(is.na(durSinceLastCovCh))
        durSinceLastCovCh <- 365.25 # Then assume the individual is already for one year (=365.26 days) in that state.
    }  
    if(length(covToCh)>1 & (!destState %in% absStates)){
      cat('Recognized a possible transition implying a change of two or more covariates.',
          'Concerning the derivation of the time being elapsed since the last transition this feature is not yet implemented.', 
          'Current State: ',currState,' -> Possible transition to ',destState,'\n') 
    }
    tageInYears <- trunc(ageInYears)            
    tCalTime <- trunc(1970.001+calTime/365.25)  
    tdurSinceLastCovCh <- trunc(durSinceLastCovCh/365.25)
    
    indRateFctDET <- function(x, cftrTime){               
      # res <- eval(do.call(tr,                   
      #                     args=list(age=tageInYears+x,calTime=tCalTime+x,duration=tdurSinceLastCovCh+x)))
      
      res<-eval(transitionFuns[[tr]](age=tageInYears+x,calTime=tCalTime+x,duration=tdurSinceLastCovCh+x, cftrTime=cftrTime))
      return(res)                               
    }   
    # `ran` is the min between time left to maxAge and time left to reach the end of simulation (in years)
    # I'm not sure why this validation happens
    ranAccuracyInDays <- (0:(trunc(ran*365.25)+0.99))/365.25
    detE <- indRateFctDET(ranAccuracyInDays, cftrTime)
    daysToTrInYears <- (which(detE == Inf)[1] - 1)/365.25
    
    if (Inf %in% detE) {
      timeToNext <- daysToTrInYears
    } else {   
      u <- -log(1-runif(1)) 
      #cat('It: ',i,'--u: ',u,'\n')
      # Extract individual transition rate (depending on age, calendar time, and time elapsed)  
      indRateFct <- function(x,cftrTime){
        ageIn <- ageInYears+x
        calIn <- 1970.001+calTime/365.25+x 
        durIn <- durSinceLastCovCh/365.25+x
        # res <- eval(do.call(tr, args=list(age=ageIn,calTime= calIn,duration=durIn)))   
        res<-eval(transitionFuns[[tr]](age=ageIn,calTime= calIn,duration=durIn, cftrTime=cftrTime))
        
        if(TRUE %in% (res<0))
          stop('I have found negative rate value/s for transition: ',tr,'\n
                 This is implausible. Please check this. Simulation has been stopped.\n')
        #cat('x: ',x,' -- res', res,'\n')
        #cat('\n---\n')
        return(res)
      }
      if(sum(indRateFct(0:ran,cftrTime))==0){ # Rate function contains only zeros.
        intHaz <- 0
      } else {        
        # Integrated hazard at max. value
        intHaz <- try(integrate(indRateFct, lower=0, upper=ran, cftrTime=cftrTime)$value, silent=TRUE)
        if(inherits(intHaz, 'try-error')){          
          intHaz <- integrate(indRateFct, lower=0, upper=ran, stop.on.error = FALSE, rel.tol = 0.01, cftrTime=cftrTime)$value
        }
      }
      # If transformed random variate exceeds max. value of integr. hazard, we will not find a finite random waiting time.      
      if(u<=intHaz){
        invHazFct <- function(x){
          #cat('x: ',x,'\n')
          try.res <- try(integrate(indRateFct, lower=0, upper=x, cftrTime=cftrTime)$value-u, silent=TRUE)
          #print(try.res)
          if(inherits(try.res, 'try-error')){  
            #cat('Seemingly, divergent intergral for ID ',id,
            # ' in state ',currState,' at age ',currAge,' at time ',calTime, ' to state ',destState,
            #  ' for random number: ',u,'\n')  
            try.res <- integrate(indRateFct, lower=0, upper=x, stop.on.error = FALSE, rel.tol = 0.01, cftrTime=cftrTime)$value-u
          } 
          #cat('res: ',try.res,'\n-----\n')
          return(try.res)
        }  
        # Find random waiting time. 
        timeToNext <- uniroot(invHazFct,interval=c(0,ran))$root      
      } else {
        timeToNext <- Inf
      }
    }
    nextEventMatrix[i,1] <- destState    
    nextEventMatrix[i,2] <- (timeToNext+lagToWaitingTime)*365.25    # time to next event in days
  }
  
  # Select the state with the minimun time
  nE <- nextEventMatrix[which(nextEventMatrix[,2]==min(as.numeric(nextEventMatrix[,2]))),,drop=F]
  if(dim(nE)[1]>1)
    nE <- nE[1,,drop=F]
  if(nE[1,2]!=Inf){
    # Cal. time of next event of individual. (If there is one.)
    tt <- calTime + as.numeric(nE[1,2]) 
    #print(tt)
    #cat(nE[1,1],'---',tt,'\n')  
    # Check whether next event implies school enrollment. If yes, adjust transition time to ensure that the individual
    # enters school at Sept. 1 in the year he/she turns seven.
    if(schoolEnrol){ # Is school enrollment considered in this simulation model? Yes, then continue; otherwise skip this part
      if(isSchoolEnrolment(currState,nE[1,1])){
        enYear <- getYear(tt)
        if(getMonth(tt) <= monthSchoolEnrol) {
          enDate <- getInDays_my(enYear, monthSchoolEnrol) 
        } else {
          enYear <- enYear+1
          enDate <- getInDays_my(enYear, monthSchoolEnrol) 
        }      
        diffToEn <- as.numeric(enDate-tt)
        nE[1,2] <- as.numeric(nE[1,2]) + diffToEn 
      }
    }
    # Enqueue new event (if there is one).
    queue <- rbind(queue, c(id, t.clock, currState, currAge - lagToWaitingTime*365.25, nE[1,1], nE[1,2]))
  }  
  #cat('\n----------\n')      
  return(list(ne=nE,queue=queue))
}
