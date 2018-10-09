# imports
from __future__ import division
import math
import matplotlib.pyplot as plt
from datetime import datetime


import BRKGA as brkga # BRKGA framework (problem independent)
import Decoder as decoder # Decoder algorithm (problem-dependent)
#import provaprovadef as decoder # Decoder algorithm (problem-dependent)
from DATA_1 import data # Input data (problem-dependent and instance-dependent)
from CONFIGURATION import config # Configuration parameters (problem-dependent and execution-dependent)

now = datetime.now()
# initializations
numIndividuals=int(config['numIndividuals'])
numElite=int(math.ceil(numIndividuals*config['eliteProp']))
numMutants=int(math.ceil(numIndividuals*config['mutantProp']))
numCrossover=max(numIndividuals-numElite-numMutants,0)
maxNumGen=int(config['maxNumGen'])
ro=float(config['inheritanceProb'])
evol=[]



# Main body
chrLength=decoder.getChromosomeLength(data)

population=brkga.initializePopulation(numIndividuals,chrLength)

i=0
while (i<maxNumGen):
    population = decoder.decode(population,data)
    evol.append(brkga.getBestFitness(population)['fitness'])
    if numElite>0:
        elite, nonelite = brkga.classifyIndividuals(population,numElite)
    else: 
        elite = []
        nonelite = population
    if numMutants>0: mutants = brkga.generateMutantIndividuals(numMutants,chrLength)
    else: mutants = []
    if numCrossover>0: crossover = brkga.doCrossover(elite,nonelite,ro,numCrossover)
    else: crossover=[]
    population=elite + crossover + mutants
    i+=1
    
population = decoder.decode(population, data)
bestIndividual = brkga.getBestFitness(population)

print ('solution:')
print (bestIndividual['solution'])
print ('fitness:', bestIndividual['fitness'])

print 'time', datetime.now() - now