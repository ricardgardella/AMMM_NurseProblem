from __future__ import division
import numpy as np
import sys

from sched import scheduler


def getChromosomeLength(data):
    return int(data["hours"])+int(data["numNurses"])

def decode(population, data):
    for ind in population:   
        solution, fitness=decoder_order(data, ind['chr'])
        ind['solution']=solution
        ind['fitness']=fitness    
    return(population)
    

def decoder_order(data, chromosome):   
    
    def my_function(nurse_order_used, nurse_remain):
        aux = nurse_order_used
        

        for n in range(len(nurse_order_used)): 
            schedule = np.zeros(int(data["numNurses"])*int(data["hours"]),dtype=np.int).reshape((int(data["numNurses"]),int(data["hours"])))        
            H = [0]*int(len(schedule[0][:]))       # vector d'hores        
            demand = map(float, data["demand"][:])
            auxdemand = demand

            chr_hour = chromosome[len(N_used):len(chromosome)]
            hour_order = sorted(range(len(H)), key = lambda k: chr_hour[k])
            
            for i in aux:
                nurse_worked = np.sum(schedule[i,:])
                nursedemand = auxdemand                      
                while (int(data["minHours"]) > nurse_worked):# or (used[i] == 1):
                    for j in hour_order:
                        if auxdemand[j] >= 0: #mentre hi ha demanda a la hora j +2??
                            if available_nurse(schedule.copy(),i,j,data):                  
                                schedule[i][j] = 1 #assignem nurse i a la hora j
                                used[i] = 1
                                    
                                nursedemand[j] -= 1 # restem demanda de la hora j
                                auxdemand_ok = nursedemand
                    nurse_worked = np.sum(schedule[i,:])
    
                    if int(data["minHours"]) > nurse_worked:
                        schedule[i,:] = 0 # reassignem a la nurse i a l'hora j que acabem d'assignar el 0 --> al final no pot treballar perque no cobreix el minim d'hores.
                        used[i] = 0
                        nursedemand = auxdemand                      
                        break
                    else: auxdemand = auxdemand_ok
                    if int(data["maxHours"]) == nurse_worked:
                        auxdemand = auxdemand_ok
                        break
            
            fitness = (int(sum(used))/int(data["numNurses"]))*100
                        # proporcio de nurses utilitzades
            
            
            positive_list = []
            for x in auxdemand:
                if x > 0:
                    positive_list += [x]
            val = False
            for z in range(len(schedule[0,:])):
                demandz  = map(float, data["demand"][:])
                if(int(sum(schedule[:,z])) < int(demandz[z])):
                    val = True
        
        ############# OK!
            if val == True:
                if int(len(nurse_order_used)) < int(len(N)) :
                    value, aux_nurse_order = get_new_nurse(nurse_order_used, nurse_remain)                
                    nurse_order_used += [value]
                    my_function(nurse_order_used, aux_nurse_order)
                else:
                    final_schedule = None
                    final_fitness = sys.maxint
                    return final_schedule, final_fitness
                    
            else:
                
                for k in range(len(schedule[0,:])):
                    demandk  = map(float, data["demand"][:])
                    if(int(sum(schedule[:,k])) < int(demandk[k])):
                        return None, sys.maxint
                final_schedule = schedule
                final_fitness = fitness
                return final_schedule, final_fitness


    N = [0]*(int(data["numNurses"]))
    N_notused = [0]*int(data["numNurses"]*0.4)
    N_used = [0]*(int(data["numNurses"]-int(len(N_notused))))   # vector de nurses
    
    used = N # Mirem si utilitzem la nurse o no 1 used 0 no

    chr_nurse = chromosome[0:len(N)]
    nurse_order = sorted(range(len(N)), key = lambda k: chr_nurse[k])
    nurse_order_used = N_used
    nurse_not_used = N_notused
    nurse_order_used, nurse_remain = order_nurses(nurse_order)

    schedule_aux, fitness_aux = my_function(nurse_order_used, nurse_remain)

    # print '-----------------------------------------------'  
    # print "schedule: "
    # print schedule_aux
    # print " FINAL fitness: ", fitness_aux
    # print '-----------------------------------------------'  
      
    return schedule_aux, fitness_aux

def order_nurses(nurse_order):            
    ## Triem quines nurses utilitzar tenint en compte l'ordre del cromosoma
    aux_nurse_order = nurse_order
    aux = []
    for nurse in aux_nurse_order:                
        if nurse == min(aux_nurse_order):
            aux += [aux_nurse_order.index(nurse)]
            aux_nurse_order[aux_nurse_order.index(nurse)] = sys.maxint
    if len(aux)==1:
        aux += aux
    return aux, aux_nurse_order

def get_new_nurse(nurse_order, aux_nurse_order): 
    for nurse in aux_nurse_order:
        if nurse == min(aux_nurse_order):
            
            nurse_number = aux_nurse_order.index(nurse)            
            aux_nurse_order[nurse_number] = sys.maxint
                        
            return nurse_number, aux_nurse_order

def available_nurse(sch,nurse,hour,data):
    dummySchedule = sch
    dummySchedule[nurse][hour] = 1
    
    # Parameters of data
    minHours = int(data["minHours"])
    maxHours = int(data["maxHours"])
    maxConsec = int(data["maxConsec"])
    maxPresence = int(data["maxPresence"])
    
    # Si es tot 0 fica-la alla ok
    if np.sum(dummySchedule[nurse][:]) == 1:
        #print "init"
        return True
    
    # MaxHours --> Si supera les hores maximes no pot
    if sum(dummySchedule[nurse][:]) > maxHours: 
        #print "maxhours fails"
        return False
    
    # PRESENCE
    # Presence --> Si supera la MaxPresence no pot
    first_hour_worked = np.nonzero(dummySchedule[nurse][:])[0][0]
    last_hour_worked = max(np.nonzero(dummySchedule[nurse][:])[0])
    first_hour = min(first_hour_worked,hour)
    last_hour = max(last_hour_worked,hour)
    if (last_hour - first_hour + 1) > maxPresence:
        #print "presence fails"
        return False
    
    
    # CONSECUTIVE
    # Consecutive --> Si supera la MaxConsec no pot
    worked = 0
    for j in range(int(data['hours'])):
        if(dummySchedule[nurse, j] == 1):
            worked += 1
            if worked > maxConsec:
                #print "consec fails"
                return False
        else:
            worked = 0 
    
    # REST
    # Rest hours --> Si descansa no pot descansar mes d'una hora
    first_hour_worked = np.nonzero(dummySchedule[nurse][:])[0][0]
    last_hour_worked = max(np.nonzero(dummySchedule[nurse][:])[0])
    lastHour = 0;
    for j in range(first_hour_worked, last_hour_worked+1):
        
        currentHour = int(dummySchedule[nurse][j])
        if(currentHour == 0 and lastHour == 0):
            #print "rest fails " , "first ", first_hour_worked, " last ", last_hour_worked," lasthour ", lastHour ," current ", currentHour, "j ", j ," range ",range(first_hour_worked, last_hour_worked+1) 
            return False
        lastHour = currentHour
    
    
    # COMPLEIX REQUISITS --> ASSIGNAR
    #print "all ok"
    return True

    