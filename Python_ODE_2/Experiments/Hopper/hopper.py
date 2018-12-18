"""
    Evolve a quadruped robot that contains an ANN Controller.  This is meant to be a complement to the
    ANN/Mus Model work so we can compare ANN statistics.
"""

import argparse
import sys, os, random, time
import itertools
import math
import multiprocessing as mpc
import numpy
import signal
import time

import hopper_simulation
import hopper_utils

from deap import algorithms
from deap import base
from deap import creator
from deap import tools

##################################################################################################
# Handle ODE Explosions (Gracefully)

# class TimeoutFunctionException(Exception): 
#     """Exception to raise on a timeout""" 
#     pass 

# class TimeoutFunction: 

#     def __init__(self, function, timeout): 
#         self.timeout = timeout 
#         self.function = function 

#     def handle_timeout(self, signum, frame): 
#         raise TimeoutFunctionException()

#     def __call__(self, *args): 
#         old = signal.signal(signal.SIGALRM, self.handle_timeout) 
#         signal.alarm(self.timeout) 
#         try: 
#             result = self.function(*args)
#         finally: 
#             signal.signal(signal.SIGALRM, old)
#         signal.alarm(0)
#         return result 

##################################################################################################
# Logging Methods

def writeHeaders(filename):
    """ Write out the headers for a logging file. """
    with open(filename,"w") as f:
        f.write("Gen,Ind,Ind_ID,Fit_1,Fit_2,Fit_3,Fit_4,"+hopper_classes[args.exp_num].headers()+"\n")

def writeGeneration(filename,generation,individuals):
    """ Write out the fitness information for a generation. """
    with open(filename,"a") as f:
        for i,ind in enumerate(individuals):
            f.write(str(generation)+","+str(i)+","+str(ind.history_index)+",")
            f.write(",".join(str(f) for f in ind.fitness.values))
            f.write(","+str(ind))
            f.write("\n")

def writeGeneaology(filename, genealogy_tree):
    """ Write the geneaology for each individual to the file. """
    with open(filename,"w") as f:
        for key,val in genealogy_tree.iteritems():
            if len(val) > 0:
                f.write(str(key)+":")
                if len(val) == 2:
                    f.write(str(val[0])+","+str(val[1]))
                else:
                    f.write(str(val[0]))
                f.write("\n")

def writeHOF(filename, individuals):
    """ Write the hall of fame file out to a file. """
    with open(filename,"w") as f:
        # Write the headers.
        f.write("Fit_1,Fit_2,Fit_3,Fit_4,"+hopper_classes[args.exp_num].headers()+"\n")

        # Write the individuals.
        for i,ind in enumerate(individuals):
            f.write(str(i)+",")
            f.write(",".join(str(f) for f in ind.fitness.values))
            f.write(","+str(ind))
            f.write("\n")

def writeFronts(filename,fronts):
    """ Write out the pareto fronts captured during evolution. """
    with open(filename,"w") as f:
        f.write("Generation,Ind,Fit_1,Fit_2,Fit_3,Fit_4\n")
        for gen,front in enumerate(fronts):
            for i,ind in enumerate(front):
                f.write(str(gen)+","+ind+"\n")

def writeTimeInformationHeaders(filename):
    """ Write Headers for timing information. """
    with open(filename, "w") as f:
        f.write("gen, select_time, id_time, cross_time, mut_time, eval_time\n")

def writeTimeInformation(filename, gen, select_time, id_time, cross_time, mut_time, eval_time):
    """ Write out time information for a generation. """
    with open(filename,"a") as f:
        f.write(str(gen)+","+str(select_time)+","+str(id_time)+","+str(cross_time)+","+str(mut_time))
        f.write(","+str(eval_time)+"\n")

##################################################################################################

def getValIndGenomeStr(fit_file,gen,ind):
    """ Get the validator individual specified by the arguments.

    Args:
        fit_file: file to parse for the genome
        gen: generation to check against
        ind: individual from the generation

    Returns:
        string containing the genome of an individual.
    """
    with open(fit_file,"r") as f:
        for line in f:
            spl_line = line.split(",")
            if spl_line[0] == str(gen) and spl_line[1] == str(ind):
                return ','.join(i for i in spl_line[7:])
    print("Individual not found for Generation: "+str(gen)+" Individual: "+str(ind))
    exit()

def getValIndGenomeStrs(fit_file):
    """ Get the validator individuals specified by the arguments.

    Args:
        fit_file: file to parse for the genome
        
    Returns:
        list of lists containing the generation and genome of an individual.
    """
    genomes = []
    with open(fit_file,"r") as f:
        f.next() # Skip header
        for line in f:
            spl_line = line.split(",")
            genomes.append([spl_line[0],spl_line[2],','.join(i for i in spl_line[7:])])
    return genomes

def lexicase_selection(population,k,tournsize):
    """ Implements the lexicase selection algorithm proposed by Spector.

    Args:
        population: population of individuals to select from
        k: how many individuals to select
        tournsize: tournament size for each selection
    Returns:
        An individual selected using the algorithm
    """
    selected_individuals = []

    for i in range(k):
        # Sample the tournsize individuals from the population for the comparison
        sel_inds = random.sample(population,tournsize)

        # Get the length of fitnesses used in the experiment and shuffle the indices for comparing fitnesses
        fit_indicies = [i for i in range(len(sel_inds[0].fitness.weights))]
        random.shuffle(fit_indicies)

        # Now that we have the indicies, perform the actual lexicase selection.
        # Using a threshold of .9 (tied if within .9)
        for fi in fit_indicies:
            # Figure out if this is a minimization or maximization problem.
            min_max = (-1*sel_inds[0].fitness.weights[fi])

            # Rank the individuals based on fitness performance for this metric.
            # Format: fit_value,index in sel_ind,rank
            fit_ranks = [[ind.fitness.values[fi],i,-1] for i,ind in enumerate(sel_inds)]
            fit_ranks = [[i[0],i[1],j] for j,i in enumerate(sorted(fit_ranks,key=lambda x: (min_max*x[0])))]

            # Check to see if we're within the threshold value.
            for i in range(1,len(fit_ranks)):
                if math.fabs(fit_ranks[i][0]-fit_ranks[0][0])/(fit_ranks[0][0]+0.0000001) < 0.10:
                    fit_ranks[i][2] = fit_ranks[0][2]

            # Check to see if we have ties.
            for i in range(1,len(fit_ranks)):
                if fit_ranks[0][2] == fit_ranks[i][2]:
                    tie = True
                    tie_index = i+1
                elif i == 1:
                    tie = False
                    break
                else:
                    tie_index = i
                    break
            if tie:
                sel_inds = [sel_inds[i[1]] for i in fit_ranks[:tie_index]]
            else:
                selected_individuals.append(sel_inds[fit_ranks[0][1]])
                tie = False
                break

        # If tie is True, we haven't selected an individual as we've reached a tie state.
        # Select randomly from the remaining individuals in that case.
        if tie:
            selected_individuals.append(random.choice(sel_inds))

    return selected_individuals

def evaluate_individual(individual,file_prefix=""):
    """ Wrapper to call Simulation which will evaluate an individual.  

    Args:
        individual: arguments to pass to the simulation
        file_prefix: prefix for output files if validating

    Returns:
        fitness of an individual
    """

    simulation = hopper_simulation.Simulation(log_frames=args.log_frames, run_num=args.run_num, eval_time=args.eval_time, dt=.02, n=4,file_prefix=file_prefix)
    return simulation.evaluate_individual(individual)

def evolutionary_run(**kwargs):
    """ Conduct an evolutionary run using DEAP.  
    
    Args:
        gens: generations of evolution
        pop_size: population size
        mut_prob: mutation probability
    """

    # Establish name of the output files and write appropriate headers.
    out_fit_file = args.output_path+str(args.run_num)+"_fitnesses.dat"
    #out_hof_file = args.output_path+str(args.run_num)+"_hof.dat"
    out_time_file = args.output_path+str(args.run_num)+"_timing.dat"
    geneaology_file = args.output_path+str(args.run_num)+"_geneaology.dat"
    writeHeaders(out_fit_file)

    creator.create("Fitness", base.Fitness, weights=(1.0,-1.0,1.0,1.0,)) # Maximize distance
    creator.create("Individual", kwargs['exp_class'], fitness=creator.Fitness)

    # Create the toolbox for setting up DEAP functionality.
    toolbox = base.Toolbox()

    # Create the toolbox for tracking history.
    history = tools.History()

    # Define an individual for use in constructing the population.
    toolbox.register("individual", hopper_utils.initIndividual, creator.Individual)
    toolbox.register("mutate", hopper_utils.mutate)
    toolbox.register("mate", tools.cxTwoPoint)

    # Decorate the variation operators
    toolbox.decorate("mate", history.decorator)
    toolbox.decorate("mutate", history.decorator)

    # Create a population as a list.
    toolbox.register("population", tools.initRepeat, list, toolbox.individual)

    # Register the evaluation function.
    toolbox.register("evaluate", evaluate_individual)

    # Register the selection function.
    toolbox.register("select", tools.selTournament, tournsize=2)

    # Create the Hall-of-Fame
    #hof = tools.HallOfFame(maxsize=100)

    # Multiprocessing component.
    cores = mpc.cpu_count()
    pool = mpc.Pool(processes=cores-2)
    toolbox.register("map", pool.map)

    # Crossover and mutation probability
    cxpb, mutpb = 0.5, 0.05

    # Set the mutation value for hopper utils
    hopper_utils.mutate_chance = mutpb

    # Setup the population.
    pop = toolbox.population(n=kwargs['pop_size'])
    history.update(pop)

    # Request new id's for the population.
    # for ind in pop:
    #     ind.get_new_id()

    # Run the first set of evaluations.
    fitnesses = toolbox.map(toolbox.evaluate, pop)
    for ind, fit in zip(pop, fitnesses):
        ind.fitness.values = fit

    # Log the progress of the population. (For Generation 0)
    writeGeneration(out_fit_file,0,pop)

    # writeTimeInformationHeaders(out_time_file)

    for g in range(1,args.gens):
        # select_time = time.time()
        # Pull out the elite individual to save for later.
        elite = tools.selBest(pop, k=1)

        pop = toolbox.select(pop, k=len(pop)-1)
        pop = [toolbox.clone(ind) for ind in pop]
        # select_time = time.time() - select_time

        # Update the Hall of Fame
        #hof.update(pop)

        # id_time = time.time()
        # Request new id's for the population.
        for ind in pop:
            ind.get_new_id()
        # id_time = time.time() - id_time


        # cross_time = time.time()
        for child1, child2 in zip(pop[::2], pop[1::2]):
            if random.random() < cxpb:
                # Must serialize and deserialize due to the type of object.
                # child1_serialized, child2_serialized = toolbox.mate(child1.serialize(), child2.serialize())
                child1, child2 = toolbox.mate(child1, child2)
                #child1.deserialize(child1_serialized)
                #child2.deserialize(child2_serialized)
                del child1.fitness.values, child2.fitness.values
        # cross_time = time.time() - cross_time

        # mut_time = time.time()
        #for mutant in pop:
        #    toolbox.mutate(mutant)
        #    del mutant.fitness.values
        for i in range(len(pop)):
        #for ind in pop:
           pop[i] = toolbox.mutate(pop[i])[0]
           del pop[i].fitness.values
        # mut_time = time.time() - mut_time

        # eval_time = time.time()
        invalids = [ind for ind in pop if not ind.fitness.valid]
        fitnesses = toolbox.map(toolbox.evaluate, invalids)
        for ind, fit in zip(invalids, fitnesses):
            ind.fitness.values = fit
        # eval_time = time.time() - eval_time

        # Check to see if we have a new elite individual.
        new_elite = tools.selBest(pop, k=1)
        elite = tools.selBest([elite[0],new_elite[0]],k=1)

        # Add the elite individual back into the population.
        pop = elite+pop

        print("Generation "+str(g))
        # Log the progress of the population.
        writeGeneration(out_fit_file,g,pop)
        #history.update(pop)

    writeGeneaology(geneaology_file,history.genealogy_tree)
    # Write the hall of fame out to a file.
    #writeHOF(out_hof_file,hof)

        # Log the timing of the run.
        # writeTimeInformation(out_time_file,g,select_time, id_time, cross_time, mut_time, eval_time)

def nsga_evolutionary_run(**kwargs):
    """ Conduct an evolutionary run using the snake and muscle model.  
    
    Args:
        gens: generations of evolution
        pop_size: population size
        mut_prob: mutation probability
    """

    # Establish name of the output files and write appropriate headers.
    out_fit_file = args.output_path+str(args.run_num)+"_fitnesses.dat"
    out_fronts_file = args.output_path+str(args.run_num)+"_fronts.dat"
    writeHeaders(out_fit_file)

    creator.create("FitnessMulti", base.Fitness, weights=(1.0, -1.0, 1.0, 1.0)) # Maximize distance, minimize touches, maximize time upright, maximize efficiency
    creator.create("Individual", kwargs['exp_class'], fitness=creator.FitnessMulti)

    # Create the toolbox for setting up DEAP functionality.
    toolbox = base.Toolbox()

    # Define an individual for use in constructing the population.
    toolbox.register("individual", hopper_utils.initIndividual, creator.Individual)
    toolbox.register("mutate", hopper_utils.mutate)
    toolbox.register("crossover", hopper_utils.crossover)

    # Create a population as a list.
    toolbox.register("population", tools.initRepeat, list, toolbox.individual)

    # Register the evaluation function.
    toolbox.register("evaluate", evaluate_individual)

    # Register the selection function.
    toolbox.register("select", tools.selNSGA2)

    # Multiprocessing component.
    cores = mpc.cpu_count()
    pool = mpc.Pool(processes=cores-2)
    toolbox.register("map", pool.map)

    # Setup the population.
    pop = toolbox.population(n=kwargs['pop_size'])

    # Run the first generation to establish what the initial population does.
    # Evaluate the individuals with an invalid fitness
    invalid_ind = [ind for ind in pop if not ind.fitness.valid]
    fitnesses = toolbox.map(toolbox.evaluate, invalid_ind)
    for ind, fit in zip(invalid_ind, fitnesses):
        ind.fitness.values = fit

    # Check to see if a timeout error occurred, if so kill and restart children.
    # timeout = False
    # for f in fitnesses:
    #     if f[0] == -1 and f[1] == 20000:
    #         timeout = True
    # if timeout:
    #     print(mpc.active_children())
    #     pool.terminate()
    #     print(mpc.active_children())
    #     pool = mpc.Pool(processes=cores-2)
    #     toolbox.register("map", pool.map)

    # This is just to assign the crowding distance to the individuals
    # no actual selection is done
    pop = toolbox.select(pop, len(pop))

    # Log the progress of the population. (For Generation 0)
    writeGeneration(out_fit_file,0,pop)

    # Track the progress of NSGA
    fronts = []
    #fronts.append(','.join(str(i) for i in ind.fitness.values) for ind in pop)
    fronts.append(str(ind.id)+','+','.join(str(i) for i in ind.fitness.values) for ind in tools.sortLogNondominated(pop, args.pop_size, first_front_only=True))

    # Request new id's for the population.
    for ind in pop:
        ind.get_new_id()

    for gen in range(1, args.gens):
        # Variate the population
        offspring = tools.selTournamentDCD(pop, len(pop))
        offspring = [toolbox.clone(ind) for ind in offspring]

        # Update the fronts information.
        #fronts.append(','.join(str(i) for i in ind.fitness.values) for ind in pop)
        fronts.append(str(ind.id)+','+','.join(str(i) for i in ind.fitness.values) for ind in tools.sortLogNondominated(pop, args.pop_size, first_front_only=True))

        # Mutate the population.
        for ind in offspring:
            hopper_utils.mutate(ind)
            del ind.fitness.values

        # Evaluate the individuals with an invalid fitness
        invalid_ind = [ind for ind in offspring if not ind.fitness.valid]
        fitnesses = toolbox.map(toolbox.evaluate, invalid_ind)
        for ind, fit in zip(invalid_ind, fitnesses):
            ind.fitness.values = fit

        # Select the next generation population
        pop = toolbox.select(pop + offspring, kwargs['pop_size'])

        # Request new id's for the population.
        for ind in pop:
            ind.get_new_id()
        
        print("Generation "+str(gen))
        # Log the progress of the population.
        writeGeneration(out_fit_file,gen,pop)

    # Write out the fronts data.
    writeFronts(out_fronts_file,fronts)

def lexicase_evolutionary_run(**kwargs):
    """ Conduct an evolutionary run using the snake and muscle model.  
    
    Args:
        gens: generations of evolution
        pop_size: population size
        mut_prob: mutation probability
    """

    # Establish name of the output files and write appropriate headers.
    out_fit_file = args.output_path+str(args.run_num)+"_fitnesses.dat"
    writeHeaders(out_fit_file)

    creator.create("FitnessMulti", base.Fitness, weights=(1.0, -1.0, 1.0, 1.0)) # Maximize distance, minimize touches, maximize time upright, maximize efficiency
    creator.create("Individual", kwargs['exp_class'], fitness=creator.FitnessMulti)

    # Create the toolbox for setting up DEAP functionality.
    toolbox = base.Toolbox()

    # Define an individual for use in constructing the population.
    toolbox.register("individual", hopper_utils.initIndividual, creator.Individual)
    toolbox.register("mutate", hopper_utils.mutate)
    toolbox.register("mate", tools.cxTwoPoint)

    # Create a population as a list.
    toolbox.register("population", tools.initRepeat, list, toolbox.individual)

    # Register the evaluation function.
    toolbox.register("evaluate", evaluate_individual)

    # Register the selection function.
    toolbox.register("select", lexicase_selection, tournsize=4)

    # Multiprocessing component.
    cores = mpc.cpu_count()
    pool = mpc.Pool(processes=cores-2)
    toolbox.register("map", pool.map)

    # Crossover and mutation probability
    cxpb, mutpb = 0.5, 0.2

    # Set the mutation value for hopper utils
    hopper_utils.mutate_chance = mutpb

    # Setup the population.
    pop = toolbox.population(n=kwargs['pop_size'])

    # Run the first set of evaluations.
    fitnesses = toolbox.map(toolbox.evaluate, pop)
    for ind, fit in zip(pop, fitnesses):
        ind.fitness.values = fit

    # Log the progress of the population. (For Generation 0)
    writeGeneration(out_fit_file,0,pop)

    for g in range(args.gens):
        pop = toolbox.select(pop, k=len(pop))
        pop = [toolbox.clone(ind) for ind in pop]

        # Request new id's for the population.
        for ind in pop:
            ind.get_new_id()

        for child1, child2 in zip(pop[::2], pop[1::2]):
            if random.random() < cxpb:
                # Must serialize and deserialize due to the type of object.
                child1_serialized, child2_serialized = toolbox.mate(child1.serialize(), child2.serialize())
                child1.deserialize(child1_serialized)
                child2.deserialize(child2_serialized)
                del child1.fitness.values, child2.fitness.values

        for mutant in pop:
            toolbox.mutate(mutant)
            del mutant.fitness.values
        
        invalids = [ind for ind in pop if not ind.fitness.valid]
        fitnesses = toolbox.map(toolbox.evaluate, invalids)
        for ind, fit in zip(invalids, fitnesses):
            ind.fitness.values = fit

        print("Generation "+str(g))
        # Log the progress of the population.
        writeGeneration(out_fit_file,g,pop)

######################################################################

# Process inputs.
parser = argparse.ArgumentParser()
parser.add_argument("--validator", action="store_true", help="Validate current results.")
parser.add_argument("--max_validator", action="store_true", help="Validate current results.")
parser.add_argument("--gens", type=int, default=100, help="Number of generations to run evolution for.")
parser.add_argument("--pop_size", type=int, default=100, help="Population size for evolution.")
parser.add_argument("--eval_time", type=float, default=10., help="Simulation time for an individual.")
parser.add_argument("--run_num", type=int, default=0, help="Run Number")
parser.add_argument("--output_path", type=str, default="./", help="Output path")
parser.add_argument("--log_frames",action="store_true",help="Save the frames to a folder.")
parser.add_argument("--debug_runtime",action="store_true",help="Evaluate the run time of a simulation.")
parser.add_argument("--no_periodic",action="store_true",help="Whether we're including a periodic signal or not.")
parser.add_argument("--val_ind",type=int, default=0, help="Individual to validate from a generation.")
parser.add_argument("--lexicase",action="store_true",help="Whether to do NSGA-II or Lexicase selection.")
parser.add_argument("--exp_num",type=int, default=0, help="What experiment to run.")
parser.add_argument("--tail_mass",type=float,default=0.2,help="What is the tail mass we're experimenting with?")
args = parser.parse_args()

running = True
eval_time = args.eval_time 

# Select the experiment to run.
hopper_classes = [
    hopper_utils.StraightTailControlHopperContainer, # 0
    hopper_utils.StraightActuatedTailControlHopperContainer, # 1
    hopper_utils.EvoTailStraightActuatedTailControlHopperContainer, # 2
    hopper_utils.StraightAngledTailControlHopperContainer, # 3
    hopper_utils.EvoAngledTailStraightActuatedTailControlHopperContainer, # 4
    hopper_utils.EvoTailFixedMassStraightActuatedTailControlHopperContainer, # 5
    hopper_utils.EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, # 6
    hopper_utils.EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, # 7
    hopper_utils.EvoAngledTailCurvedActuatedTailControlHopperContainer, # 8
]

output_path = args.output_path
hopper_simulation.output_path = output_path
run_num = args.run_num

# Seed only the evolutionary runs.
random.seed(run_num)

# Set friction based on experiment number.
#if args.exp_num == 5:
#    hopper_simulation.friction_coefficient = 0.8

hopper_simulation.mass_flag_fix = True

# Set the tail mass.
#hopper_utils.tail_seg_mass = args.tail_mass
hopper_utils.tail_seg_mass = 0.1

if args.debug_runtime:
    print(evaluate_individual(hopper_classes[args.exp_num](debug=True),file_prefix=str(args.output_path)+"/DEAP_Hopper_"+str(args.run_num)+"_Gen_"+str(args.gens)+"_"))
elif args.max_validator:
    fit_file = args.output_path+"/"+str(args.run_num)+"_max_fitness_validation.dat"
    genome_strs = getValIndGenomeStrs(fit_file)
    for genome in genome_strs:
        print(genome[0],genome[1],genome[2])
        print(evaluate_individual(hopper_classes[args.exp_num](validate=True,genome=genome[2]),file_prefix=str(args.output_path)+"/DEAP_Hopper_"+str(args.run_num)+"_Gen_"+str(genome[0])+"_Ind_"+str(genome[1])+"_"))
elif args.validator:
    fit_file = args.output_path+"/"+str(args.run_num)+"_fitnesses.dat"
    genome_str = getValIndGenomeStr(fit_file,args.gens,args.val_ind)
    print(genome_str)
    print(evaluate_individual(hopper_classes[args.exp_num](validate=True,genome=genome_str),file_prefix=str(args.output_path)+"/DEAP_Hopper_"+str(args.run_num)+"_Gen_"+str(args.gens)+"_Ind_"+str(args.val_ind)+"_"))
else:
    if args.lexicase:
        lexicase_evolutionary_run(gens=args.gens,pop_size=args.pop_size,exp_class=hopper_classes[args.exp_num])
    else:
        evolutionary_run(gens=args.gens,pop_size=args.pop_size,exp_class=hopper_classes[args.exp_num])
