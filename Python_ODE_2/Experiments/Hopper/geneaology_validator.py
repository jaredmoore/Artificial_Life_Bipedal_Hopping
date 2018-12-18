"""
	Read in the geneaology data from a run and figure out the lineage to validate an individual.
"""

import argparse

def read_fitness_data(input_file):
	""" Read in the fitness file information and store it in a dictionary with the genome id. """
	fitness_dict = {}
	with open(input_file,"r") as f:
		f.next()
		for line in f:
			split = line.split(",")
			fitness_dict[(int(split[2]),int(split[0]))] = {
				'gen':int(split[0]),
				'ind':int(split[1]),
				'genome_id':int(split[2]),
				'fit_data':split[3:7],
				'genome':split[7:]
			}
	return fitness_dict

def read_geneaology_data(input_file):
	""" Read the geneaology data into a dictionary. """
	geneaology_dict = {}
	with open(input_file,"r") as f:
		for line in f:
			split = line.split(":")
			geneaology_dict[int(split[0])] = [int(i) for i in split[1].split(",")]
	return geneaology_dict

def traverse_geneaology_tree(start_ind,start_ind_gen,gen_tree,fit_dict):
	""" Return the geneaology of the provided start_ind. """
	to_review = [(start_ind,start_ind_gen)]
	reviewed = [(start_ind,start_ind_gen)]
	gen_list = [(start_ind,start_ind_gen)]

	while(len(to_review)):
		ind = to_review.pop()
		print(ind)
		if ind[0] in gen_tree:
			gen = ind[1]-1
			if ind not in fit_dict:
				gen = ind[1] # Individual was not evaluated (Part of mutation/crossover)
			
			parents = []
			for i in gen_tree[ind[0]]:
				# Check if the parent was a clone from previous generation.
				tmp_gen = gen 
				while (i,tmp_gen-1) in fit_dict:
					tmp_gen -= 1
				parents.append((i,tmp_gen))

			# Check to see if one of the parents has the same fitness.
			# (Eliminates meaningless crossover events.)
			if len(parents) > 1:
				if ind in fit_dict:
					if fit_dict[parents[0]]['fit_data'][0] == fit_dict[ind]['fit_data'][0] and fit_dict[parents[1]]['fit_data'][0] != fit_dict[ind]['fit_data'][0]:
						del parents[1]
					elif fit_dict[parents[1]]['fit_data'][0] == fit_dict[ind]['fit_data'][0] and fit_dict[parents[0]]['fit_data'][0] != fit_dict[ind]['fit_data'][0]:
						del parents[0]
			elif len(parents) == 1 and parents[0] not in fit_dict:
				# Find parents of the individual not in the fit_dict.
				tmp_ind = parents[0]
				parents = []
				for i in gen_tree[tmp_ind[0]]:
					# Check if the parent was a clone from previous generation.
					tmp_gen = gen 
					while (i,tmp_gen-1) in fit_dict:
						tmp_gen -= 1
					parents.append((i,tmp_gen))
				if fit_dict[parents[0]]['fit_data'][0] == fit_dict[ind]['fit_data'][0] and fit_dict[parents[1]]['fit_data'][0] != fit_dict[ind]['fit_data'][0]:
					del parents[1]
				elif fit_dict[parents[1]]['fit_data'][0] == fit_dict[ind]['fit_data'][0] and fit_dict[parents[0]]['fit_data'][0] != fit_dict[ind]['fit_data'][0]:
					del parents[0]
			for p in parents:
				if p not in gen_list and p in fit_dict:
					gen_list.append(p)
				if p not in to_review and p not in reviewed and p in fit_dict:
					to_review.append(p)
		reviewed.append(ind)
		print("To Review: "+str(len(to_review))+"\t"+"Reviewing: "+str(ind))

	return gen_list

def write_geneaology_file(output_file,geneaology,gen_dict,fit_dict):
	""" Write a file containing the geneaology of an individual complete with information about the parents. """
	with open(output_file,"w") as f:
		f.write("Individual,Gen,Fitness,Par_1,Par_2\n")
		for g in geneaology:
			if g in fit_dict:
				f.write(str(g[0])+","+str(g[1])+","+fit_dict[g]['fit_data'][0]+",")

				# Check to see if the parents have a fitness or not, if not, replace parents of the
				# current individual with parents of the non-fitness individual.
				if g[0] in gen_dict:
					if len(gen_dict[g[0]]) == 1:
						par_1 = gen_dict[g[0]][0]
						if (par_1,g[1]-1) not in fit_dict:
							f.write(str(gen_dict[par_1][0])+",")
							if len(gen_dict[par_1]) > 1:
								f.write(str(gen_dict[par_1][1]))
							else:
								f.write("-1")	
						else:
							f.write(str(par_1)+",-1")	
				else:
					f.write("-1,-1")
			# if g in gen_dict:
			# 	f.write(str(gen_dict[g][0])+",")
			# 	if len(gen_dict[g]) > 1:
			# 		f.write(str(gen_dict[g][1]))
			# 	else:
			# 		f.write("-1")
			# else:
				# f.write("-1,-1")
				f.write("\n")

##############################################################################################################################

# Process inputs.
parser = argparse.ArgumentParser()
parser.add_argument("--run_num", type=int, default=0, help="Run Number")
parser.add_argument("--output_path", type=str, default="./", help="Output path")
args = parser.parse_args()

# Read in the test file.
base_path = args.output_path
fitness_file = str(args.run_num)+"_fitnesses.dat"
geneaology_file = str(args.run_num)+"_geneaology.dat"
out_geneaology_file = str(args.run_num)+"_geneaology_output.dat"

# Read in the geneaology information.
gen_dict = read_geneaology_data(base_path+geneaology_file)

# Read in the fitness/genome information.
fit_dict = read_fitness_data(base_path+fitness_file)

# Find the maximum fitness in the population with the lowest genome id.
max_fit = -1
max_ind = 100000000
max_gen = -1
for k,v in fit_dict.iteritems():
	fit = float(v['fit_data'][0])
	if fit > max_fit:
		max_fit = fit
		max_ind = k[0]
		max_gen = k[1]
	elif fit == max_fit:
		if max_gen > k[1]:
			max_ind = k[0]
			max_gen = k[1]

print(max_ind,max_gen,max_fit)

# Find the geneaology of the best individual.
geneaology = traverse_geneaology_tree(max_ind,max_gen,gen_dict,fit_dict)
write_geneaology_file(base_path+out_geneaology_file,geneaology,gen_dict,fit_dict)