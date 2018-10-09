
createSolution <- function(works, demand, maxHours,minHours, hours, nNurses, maxConsec){ 
    auxDemand <- rep(0,hours)
    nHour = 0
    consecutive = 0
    for(h in 1 : hours) {auxDemand [h] = demand[h] + round(runif(1,0,demand[h]))} #Random values to the solution
    for (nurse in 1 : nNurses){
      nHour = 0
      consecutive = 0
      for(hour in 1 : hours){ 
        if(auxDemand[hour] >= 1 & nHour < maxHours){  #If a nurse have 8 hours, can't assign more than that. #Demand satisfier
          works[nurse,hour]= ifelse(runif(1,0,1)>=0.4,1,0)#Assign a random value between 1 and 0. 
          #Forcign resting 1h at max
          if(consecutive == maxConsec) 
          {
            works[nurse,hour]= 0
          }
          if(hour > 2) 
          {
            if(works[nurse,hour-1]== 0 & works[nurse,hour-2]== 1) {
              works[nurse,hour] = 1
              consecutive = consecutive + 1
            }
          }
          if(works[nurse,hour] == 1)
          {
            auxDemand[hour] = auxDemand[hour] - 1  #Substract to auxdemand
            nHour = nHour + 1
            consecutive = consecutive + 1
          }
        }
        else{
          if(nHour < maxHours & hour > 2)
            {
              if(works[nurse,hour-1]== 0 & works[nurse,hour-2]== 1)
                {
                works[nurse,hour] = 1
                auxDemand[hour] = auxDemand[hour] - 1
                nHour = nHour + 1
                consecutive = consecutive + 1
                }
            }
        }
        if(works[nurse,hour] == 0) consecutive = 0
      }
      #Si entra aqui añadira m´ás horas de las necesarias, por tanto, la soluci´ón ser´á m´ás cara.
      if(sum(works[nurse,]) < minHours & sum(works[nurse,])>0)#Si tenemos menos horas que el minimo, pero tenemos alguna, añadimos más
        {
        for(hour in 1 : hours){
          if(sum(works[nurse,])< minHours)
          {
            if(works[nurse,hour] != 1)
            {
              works[nurse,hour]= ifelse(runif(1,0,1)>=0.5,1,0)#Assign a random value between 1 and 0
            }
          }
        }
      }
    }
    return(works)
  }
  


feasibleFunction <- function(hours, maxHours, minHours, nNurses, maxConsec,maxPresence,demand,works){
  for (nurse in 1 : nNurses){
    if(sum(works[nurse,])>0){
      if(sum(works[nurse,]) > maxHours || sum(works[nurse,]) < minHours) 
        {
        return(FALSE)#Sum hours nurse between min and max hours
        }
      for (hour in 1 : hours){ #not sure
        if(hour == 1)
        {
          FirstHour = 0 #firstHour that the nurse work
          LastHour = 0 #lasthour that the nurse work
          consecutive = 0
          worksBefore= 0
          rest = 0
        }
        #Maxpresence
        if(works[nurse,hour]==1 && FirstHour == 0)
        {
          FirstHour = hour
          LastHour = hour
        }
        if(works[nurse,hour]==1) LastHour = hour
        if(LastHour - FirstHour > maxPresence) {
          return(FALSE)  }
        #maxconsec
        if (works[nurse,hour]== 1) {
          consecutive = consecutive + works[nurse,hour]
        }else {
          consecutive = 0
        }
        if(consecutive > maxConsec) {
          return (FALSE) }
        #MaxRest --> Constrained at creation
      }    
    }
  }
  for (hour in 1 : hours){
    #Demand is fulfilled.
    if(sum(works[,hour]) < demand[hour]) 
      {
      return (FALSE)
      }
  }
  return(TRUE)
} 


getCost <- function(hours, maxHours, nNurses, maxConsec,maxPresence,demand,works){ #Something is missing
  costHoraNurse=1
  costNurse= 1000
  cost = nNurses
  for (nurse in 1 : nNurses){
    cost = cost + costNurse
    for(hour in 1 : hours){
    cost = cost + costHoraNurse * works[nurse,hour] #Works nurse will be 1 or 0.
    }
  }
  for(hour in 1 : hours)cost = cost + sum(works[,hour]) - demand[hour] #Adding cost if there is extra hours worked
  cost = cost * nNurses #More nurses = more cost
  return(cost)
}


localsearch <- function(solution, demand,maxHours,minHours,maxPresence,maxConsec,hours) 
{
  auxSolution = solution
  
  for(nurse in 1: nrow(auxSolution)) #delete empty nurses
  {
    if(sum(auxSolution[nurse,]) == 0) {
      solution = solution[-c(nurse:nurse), ] 
    }
  }
  for(nurse in 1: nrow(auxSolution)) #Delete unecessary nurses
  {
    suma_hores<-vector() 
    for(hour in 1 : hours) 
      suma_hores[hour] <- sum(solution[,hour])
    extrahours = demand - suma_hores
    delete = TRUE
    deleteNurses = max(extrahours)
    if (deleteNurses != 0 ) solution = solution[-c(1:abs(deleteNurses)), ] 
  }
  
  return(solution)
}
grasp <- function(selectPar)
{
  number_solutions= 5
  #Parameters
  
  if(selectPar == 1)
  {
    nNurses=30
    nHours=24
    minHours=2
    maxHours=12
    maxConsec=8
    maxPresence=18
    demand=c(4, 2, 3, 1, 2, 2, 6, 2, 2, 1, 4, 1, 2, 2, 2, 5, 2, 1, 1, 1, 1, 3, 1, 2)
  }
  if(selectPar == 2)
  {
    nNurses=42
    nHours=24
    minHours=2
    maxHours=18
    maxConsec=8
    maxPresence=24
    demand=c(11,7,5,1,4,6,9,10,3,7,11,2,8,6,5,6,6,2,1,3,2,6,5,1)
  }
  if(selectPar == 3)
  {
    nNurses=1100
    nHours=24
    minHours=6
    maxHours=18
    maxConsec=7
    maxPresence=24
    demand=c(130, 400, 400, 764, 300, 387, 624, 430, 611, 468, 403, 561, 700, 597, 430, 855, 300, 230, 300, 356, 232, 670, 230, 349)
  }
  if(selectPar == 4)
  {
    nNurses=1800
    hours=24
    minHours=6
    maxHours=18
    maxConsec=7
    maxPresence=24
    demand=c(964, 650, 966, 1021, 824, 387, 828, 952, 611, 468, 403, 561, 862, 597, 1098, 855, 918, 1016, 897, 356, 615, 670, 826, 349)
  }
  if(selectPar == 5)
  {
    nNurses=73
    hours=24
    minHours=2
    maxHours=18
    maxConsec=8
    maxPresence=24
    demand=c(13,17,7,12,4,14,12,10,4,6,7,4,15,12,9,13,12,11,1,3,2,2,2,3)
  }
  #grasp
  auxCostSol = 0 
  bestSol = 0
  costbestsol = 0 
  numberNursesBestSol = nNurses
  start_time <- Sys.time()
  while(number_solutions != 0)
  {
    solution <- matrix(ncol = hours , nrow = nNurses ,data = 0) #Nurses x hour
    solution = createSolution(works = solution,demand = demand,maxHours = maxHours,minHours = minHours, hours = hours,nNurses=nNurses,maxConsec = maxConsec)
    if(feasibleFunction(works = solution,hours = hours,maxHours = maxHours, minHours = minHours,nNurses = nNurses,maxConsec = maxConsec,maxPresence = maxPresence,demand = demand))
    {
      solution = localsearch(solution = solution,demand = demand,maxHours = maxHours,minHours = minHours,maxPresence = maxPresence,maxConsec = maxConsec, hours = hours)
      auxCostSol = getCost(works =solution,hours = hours,maxHours = maxHours,nNurses = nrow(solution),maxConsec = maxConsec,maxPresence = maxPresence,demand = demand)
      if(costbestsol == 0)
      {
        costbestsol = auxCostSol
        bestSol = solution
        numberNursesBestSol = nrow(solution)
      }
      if(auxCostSol < costbestsol & nrow(solution) <= numberNursesBestSol)
      {
        costbestsol = auxCostSol
        bestSol = solution
        numberNursesBestSol = nrow(solution)
      }
      number_solutions = number_solutions -1
    }
  }
  print(Sys.time() - start_time)
  print("Nurses used:")
  print(nrow(solution))
  print("Cost after localsearch")
  return(costbestsol)
}

