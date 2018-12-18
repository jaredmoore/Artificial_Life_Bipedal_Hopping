""" File to encapsulate a hopping individual for use in DEAP. 

Base Template for a New Genome Class:

class TestContainer(ParentContainer):
    # Define a genome, all the parameters associated with an individual.

    def __init__(self,debug=False,validate=False,genome=""):
        # Initialize the genome with a set of values.
        super(TestContainer, self).__init__(debug=debug)

        self.__num_genes = 
        self.__genome_offset = super(TestContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):
            EvoTailStraightActuatedTailControlHopperContainer
            self.gene_1 = format_float(random.random())

        elif validate and genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        # Return a comma separated string of the parameters associated with an individual.
        head_str = super(TestContainer, cls).headers()
        head_str += ",Gene_1"
        return head_str

    def genome_length(self):
        # Return the number of genes in the class. 
        return self.__num_genes + super(TestContainer, self).genome_length()

    def __str__(self):
        # Define the to string method for the class. 
        out_str = super(TestContainer, self).__str__()
        out_str += ','+str(self.gene_1)
        return out_str

    def serialize(self):
        # Return a list representation of the genome for crossover.
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        # Set the genome to values provided in the genome.
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        # Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        #
        super(TestContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.gene_1 = float(genome[0+self.__genome_offset])

    def mutate(self, mut_prob=0.04):
        # Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        #
        super(TestContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.gene_1 = mutate_value(self.osc_freq,0.0,2.0)

"""

import math
import random
import warnings

mutate_chance = 0.04

tail_seg_mass = 0.2

def format_float(value):
    """ Return a formatted float value capable of being printed. """
    return float("{0:.6f}".format(value))


def mutate_value(value,low_lim,upp_lim):
    """ Mutate a value by a gaussian within the bounds.  

    Args:
        value: initial value of the parameter.
        upp_lim: upper limit of the parameter
        low_lim: lower limit of the parameter
    """
    value = format_float(random.gauss(value, (upp_lim-low_lim)*0.1)) # Mutate in the range of 10% SD of the value
    if(value > upp_lim):
        value = upp_lim
    elif(value < low_lim):
        value = low_lim
    return value


def initIndividual(ind_class):
    return ind_class()


def mutate(individual):
    """ Call the mutation method of the appropriate class. 

    Note: Wrapped to function with DEAP.
    """    
    individual.mutate(mut_prob=mutate_chance)
    return individual,


class BaseHopperContainer(object):
    """ An abstract hopper genome containing all configurable parameters. """
    _id = 0 # Global genome identifier

    @classmethod
    def __get_new_id(cls):
        cls._id += 1
        return cls._id

    def __init__(self,debug=False):
        """ Initialize a default configuration of the robot. """

        self._id = self.__get_new_id()

        # Control parameters
        self.osc_freq = 1.0
        self.limb_offsets = [
            0./8., # Right Rear Leg
            4./8., # Left Rear Leg
            0./8., # Right Front Leg
            0./8.  # Left Front Leg
        ]
        self.joint_offsets = [
            0./8., # Rear Hips
            4./8., # Rear Knee
            0./8., # Rear Ankle
            0./8., # Rear Toes
            0./8., # Front Hip
            0./8., # Front Knee
        ]

        self.joint_ranges = [
            # Fore/Aft, Side-to-Side (Low,High)
            [[0,0],[0,0]], # Rear - Mid Connection
            [[0,0],[0,0]], # Mid - Front Connection
            [[math.radians(-15),math.radians(15)],[0,0]], # Rear Hips (Negatives are backward?)
            [[math.radians(-45),math.radians(45)],[0,0]], # Rear Knees (Negatives are forward.)
            [[math.radians(-45),math.radians(45)],[0,0]], # Rear Ankles
            [[math.radians(-30),math.radians(30)],[0,0]], # Rear Toes
            [[math.radians(-22.5),math.radians(22.5)],[0,0]], # Front Shoulder
            [[math.radians(-22.5),math.radians(22.5)],[0,0]], # Front Knee
            [[math.radians(-45),math.radians(45)],[math.radians(-45),math.radians(45)]], # Tail Base
            [[math.radians(-45),math.radians(45)],[math.radians(-45),math.radians(45)]], # Tail Joints
            # [[math.radians(-45),math.radians(45)],[math.radians(-45),math.radians(45)]], # Head Base
            [[0,0],[0,0]], # Head Base
        ]

        self.MAX_FORCE_LIMIT = 35
        self.FORCE_MUT_RESOLUTION = 1 # Resolution in steps for forces.
        self.max_forces = [
            [self.MAX_FORCE_LIMIT*4.,self.MAX_FORCE_LIMIT*4.], # Back Joints 
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Rear Hips
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Rear Knees
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Rear Ankles
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Rear Toes
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Front Hips
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Front Knees
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Tail
            [self.MAX_FORCE_LIMIT,self.MAX_FORCE_LIMIT], # Head
        ]
        self.MAX_JOINT_VEL_LIMIT = 80000
        self.max_joint_vel = self.MAX_JOINT_VEL_LIMIT

        # Flexibility parameters
        self.erp = [
            [0.5,0.5], # Back Joints 
            [0.5,0.5], # Rear Hips
            [0.5,0.5], # Rear Knees
            [0.5,0.5], # Rear Ankles
            [0.5,0.5], # Rear Toes
            [0.5,0.5], # Front Hips
            [0.5,0.5], # Front Knees
            [0.5,0.5], # Tail
            [0.5,0.5], # Head
        ]
        self.cfm = [
            [0.0001,0.0001], # Back Joints 
            [0.0001,0.0001], # Rear Hips
            [0.0001,0.0001], # Rear Knees
            [0.0001,0.0001], # Rear Ankles
            [0.0001,0.0001], # Rear Toes
            [0.0001,0.0001], # Front Hips
            [0.0001,0.0001], # Front Knees
            [0.0001,0.0001], # Tail
            [0.0001,0.0001], # Head
        ]

        # Original Masses from ECAL 2013 Work
        # Total Mass without Tail: 2.9444
        # Torso:
        # Rear Upper Legs:
        # Rear Mid Legs:
        # Rear Lower Legs:
        # Rear Feet:
        # Front Upper Legs:
        # Front Lower Legs:
        # Head:
        self.body_masses = [
            2./3., # Rear Torso Segment     -  0
            2./3., # Mid Torso Segment      -  1
            2./3., # Front Torso Segment    -  2
            0.0828, # Rear Upper Legs       -  3
            0.0458, # Rear Mid Legs         -  4
            0.0152, # Rear Low Legs         -  5
            0.0056, # Rear Feet             -  6
            0.0528, # Front Upper Legs      -  7
            0.02, # Front Lower Legs        -  8
            0.0429/3., # Base of Tail       -  9
            0.0429/3., # Mid Tail           - 10
            0.0429/3., # End Tail           - 11
            0.5, # Head                     - 12
        ]
        
        # Dimensions from 6bm mass in ECAL 2013
        # const double BODY_LENGTH = .873; //Original: .773        
        # const double TAIL_LENGTH  = 0.065 + (0.065 * gen_vals.t_length);//1.95;        
        # const double FEMUR_LENGTH = .284;        
        # const double TIBIA_LENGTH = .471;        
        # const double MTARSALS_LENGTH = .231;
        # const double TOES_LENGTH = .179;
        # const double FR_UL_LENGTH = .19; //Two-thirds of the femur length        
        # const double FR_FT_LENGTH = .284; //Length of the femur

        # Base the simple dimensions off a base factor for scaling.
        self.bf = 2.0
        self.body_dimensions = [
            [.291*self.bf,.2*self.bf,0.2*self.bf], # Rear Segment
            [.291*self.bf,.2*self.bf,0.175*self.bf], # Mid Segment
            [.291*self.bf,.2*self.bf,0.2*self.bf],   # Front Segment
            [0.284*self.bf,0.025*self.bf], # Rear Upper Legs (Length, Radius)
            [0.471*self.bf,0.025*self.bf], # Rear Mid Legs (Length, Radius)
            [0.231*self.bf,0.025*self.bf], # Rear Low Legs (Length, Radius)
            [0.179*self.bf,0.025*self.bf], # Rear Feet (Length, Radius)
            [0.19*self.bf,0.025*self.bf], # Front Upper Legs (Length, Radius)
            [0.284*self.bf,0.025*self.bf], # Front Lower Legs (Length, Radius)
        ]

        self.tail_dimensions = [
            [0.65*self.bf,0.025*self.bf], # Base of Tail
            [0.65*self.bf,0.025*self.bf], # Mid Tail
            [0.65*self.bf,0.025*self.bf], # End Tail
        ]

        self.head_dimensions = [.252*self.bf, .08*self.bf]

        self.body_rotations = [
            [90,90,0], # Rear Upper Legs    0
            [0,-135,0], # Rear Mid Legs     1
            [0,90,0], # Rear Low Legs      2
            [0,20,0],  # Rear Feet          3
            [90,-62.5,0], # Front Upper Legs   4
            [0,112.5,0],  # Front Lower Legs   5
            [90,-60,0], # Base of Tail      6
            [0,-20,0],  # Mid Tail          7
            [0,-20,0],  # End Tail          8
        ]

    id = property(lambda self: self._id)

    def get_new_id(self):
        """ Get a new id for the genome. """
        self._id = BaseHopperContainer.__get_new_id()


class BaseEvolveHopperContainer(BaseHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. 

        This class forms the basis for all evolved hopper genomes.  Any genes that 
        are universal should be implemented here.
    """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(BaseEvolveHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 3
        self.__body_mass = 1.5

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):
            
            self.rear_mass = format_float(0.1+random.random()*(self.__body_mass*0.8-0.1)) # Ensures a minimum mass of 0.1 and max of 2.4 (80% of total)
            self.mid_mass  = format_float(0.1+random.random()*(((self.__body_mass-self.rear_mass)*0.8)-0.1)) # Ensures minimum mass of 0.1 and max of 80% remaining mass.
            self.front_mass = format_float(self.__body_mass-self.rear_mass-self.mid_mass) # This should have a minimum of 0.12 and should be stable.

            #if self.front_mass < 0.1:
            #    warning("WARNING: Front Mass Underweight! "+str(self.rear_mass)+","+str(self.mid_mass)+","+str(self.front_mass))

            # Set the parameters for the robot accordingly.
            self.set_morphology_parameters()

        elif genome != "":
            self.setGenomeValues(genome)

    def __eq__(self,other):
        """ Override the equality operator. """
        return True if self.serialize() == other.serialize() else False

    def __len__(self):
        """ Implement len operator. """
        return len(self.serialize())

    def __getitem__(self,key):
        """ Override the default list item getter. """
        genome_list = self.serialize()
        if isinstance(key, slice):
            return genome_list[key.start:key.stop]
        return genome_list[key]

    def __setitem__(self,key,value):
        """ Override the default list item setter. """
        genome_list = self.serialize()
        if isinstance(key, slice):
            genome_list[key.start:key.stop] = value
        else:
            genome_list[key] = value
        self.deserialize(genome_list)

    def __delitem__(self,key):
        """ Override the default list item deleter. """
        pass

    def set_morphology_parameters(self):
        """ Set the mass parameters associated with the robot. """
        self.body_masses[0] = self.rear_mass
        self.body_masses[1] = self.mid_mass
        self.body_masses[2] = self.front_mass

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        return "Rear_Torso_Mass,Mid_Torso_Mass,Front_Torso_Mass"

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = str(self.rear_mass)+","+str(self.mid_mass)+","+str(self.front_mass)
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        if type(genome) == str:
            genome = genome.split(',')

        # This is the base class so we don't need to have an offset.
        self.rear_mass = float(genome[0])
        self.mid_mass = float(genome[1])
        self.front_mass = float(genome[2])

        # Verify that the three components are within acceptable ranges.
        if round(self.rear_mass + self.mid_mass + self.front_mass,2) != 3.00:
            # Find the difference between the sum and 3, then equally distribute those differences among the masses?
            diff = self.__body_mass - self.rear_mass + self.mid_mass + self.front_mass

            if self.rear_mass < self.__body_mass*0.8:
                rear_mass_diff = self.__body_mass*0.8 - self.rear_mass
                if diff < rear_mass_diff:
                    self.rear_mass = self.rear_mass + diff
                    diff = 0
                else:
                    self.rear_mass = self.rear_mass + rear_mass_diff
                    diff -= rear_mass_diff
            else:
                self.rear_mass = self.__body_mass*0.8

            if self.mid_mass < ((self.__body_mass-self.rear_mass)*0.8) and diff > 0:
                mid_mass_diff = ((self.__body_mass-self.rear_mass)*0.8) - self.mid_mass
                if diff < mid_mass_diff:
                    self.mid_mass += diff
                    diff = 0
                else:
                    self.mid_mass += mid_mass_diff
                    diff -= mid_mass_diff
            elif self.mid_mass > ((self.__body_mass-self.rear_mass)*0.8):
                self.mid_mass = ((self.__body_mass-self.rear_mass)*0.8)

            if diff > 0:
                self.front_mass += diff

            if round(self.rear_mass + self.mid_mass + self.front_mass,2) != round(self.__body_mass,2):
                self.front_mass = self.__body_mass - self.rear_mass - self.mid_mass

        # Set the changes in the morphology itself.
        self.set_morphology_parameters()

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """

        # Mutate the rear segment.
        mutated = False
        if random.random() < mut_prob:
            self.rear_mass = mutate_value(self.rear_mass,0.1,self.__body_mass*0.8)
            mutated = True

        # Mutate the middle segment on random chance or if the rear mass was mutated (cascading relationship)
        if random.random() < mut_prob or mutated:
            self.mid_mass = mutate_value(self.mid_mass,0.1,((self.__body_mass-self.rear_mass)*0.8)-0.1)
            mutated = True

        # Change the front segment mass if mutation occurred.
        if mutated:
            self.front_mass = self.__body_mass - self.rear_mass - self.mid_mass

            # Ensure the masses are within the acceptatble ranges.
            #if self.front_mass < 0.1:
            #    warning("WARNING: Front Mass Underweight! "+str(self.rear_mass)+","+str(self.mid_mass)+","+str(self.front_mass))

            # Set the changes in the morphology itself.
            self.set_morphology_parameters()


class ControlHopperContainer(BaseEvolveHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(ControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 30
        self.__genome_offset = super(ControlHopperContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):
            
            self.osc_freq = format_float(random.random()*2.0)
            self.limb_offsets = [
                format_float(random.randint(0,15)/8.), # Right Rear Leg
                format_float(random.randint(0,15)/8.), # Left Rear Leg
                format_float(random.randint(0,15)/8.), # Right Front Leg
                format_float(random.randint(0,15)/8.),  # Left Front Leg
                format_float(0.),  # Tail (Not Actuated)
            ]
            self.joint_offsets = [
                format_float(random.randint(0,15)/8.), # Rear Hips
                format_float(random.randint(0,15)/8.), # Rear Knee
                format_float(random.randint(0,15)/8.), # Rear Ankle
                format_float(random.randint(0,15)/8.), # Rear Toes
                format_float(random.randint(0,15)/8.), # Front Hip
                format_float(random.randint(0,15)/8.), # Front Knee
                format_float(0.), # Tail (Not Actuated)
            ]
            self.max_forces = [
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Back Joints 
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Rear Hips
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Rear Knees
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Rear Ankles
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Rear Toes
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Front Hips
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Front Knees
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Tail
                [random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)]),random.choice([i for i in range(0,self.MAX_FORCE_LIMIT,self.FORCE_MUT_RESOLUTION)])], # Head
            ]
            self.max_joint_vel = format_float(random.randint(20,(self.MAX_JOINT_VEL_LIMIT/1000))*1000.) #180000 -> Original value
        elif genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(ControlHopperContainer,cls).headers()
        head_str += ",Osc_Freq,RRL_Off,LRL_Off,RFL_Off,LFL_Off,RH_Off,RK_Off,RA_Off,RT_Off,FH_Off,FK_Off,BJ_MF_1,BJ_MF_2,RH_MF_1,RH_MF_2,"
        head_str += "RK_MF_1,RK_MF_2,RA_MF_1,RA_MF_2,RT_MF_1,RT_MF_2,FH_MF_1,FH_MF_2,FK_MF_1,FK_MF_2,T_MF_1,T_MF_2,H_MF_1,H_MF_2,Max_Joint_Vel"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(ControlHopperContainer, self).genome_length()

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(ControlHopperContainer, self).__str__()
        out_str += ','+str(self.osc_freq)+","+(','.join(str(i) for i in self.limb_offsets[:-1]))+','+(','.join(str(i) for i in self.joint_offsets[:-1]))
        out_str += ','+','.join(str(int(i[0]))+','+str(int(i[1])) for i in self.max_forces)+','+str(self.max_joint_vel)
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(ControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.osc_freq = float(genome[0+self.__genome_offset])
        self.limb_offsets = [float(genome[i]) for i in range(1+self.__genome_offset,5+self.__genome_offset)]+[0]
        self.joint_offsets = [float(genome[i]) for i in range(5+self.__genome_offset,11+self.__genome_offset)]+[0]
        self.max_forces = [[int(float(genome[i])),int(float(genome[i+1]))] for i in range(11+self.__genome_offset,29+self.__genome_offset,2)]
        self.max_joint_vel = float(genome[29+self.__genome_offset])

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(ControlHopperContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.osc_freq = mutate_value(self.osc_freq,0.0,2.0)
            
        for i in range(len(self.limb_offsets[:-1])):
            if random.random() < mut_prob:
                self.limb_offsets[i] = float("{0:.6f}".format(round(mutate_value(self.limb_offsets[i],0.,15.))/8.))
        
        for i in range(len(self.joint_offsets[:-1])):
            if random.random() < mut_prob:
                self.joint_offsets[i] = float("{0:.6f}".format(round(mutate_value(self.joint_offsets[i],0.,15.))/8.))
            
        for i in range(len(self.max_forces)):
            for j in range(len(self.max_forces[i])):
                if random.random() < mut_prob:
                    self.max_forces[i][j] = int(round(mutate_value(self.max_forces[i][j],0,self.MAX_FORCE_LIMIT))) # TODO: Continue to mutate in steps of 20?
        
        if random.random() < mut_prob:
            self.max_joint_vel = round(mutate_value(self.max_joint_vel,20,(self.MAX_JOINT_VEL_LIMIT/1000)))*1000. # TODO: Mutate in whole number steps instead?


class StraightTailControlHopperContainer(ControlHopperContainer):
    """ Same as ControlHopperContainer but with a straight tail. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(StraightTailControlHopperContainer, self).__init__(debug=debug)

        # Set the tail range of motion to no range.
        self.joint_ranges[8] = [[0,0],[0,0]]
        self.joint_ranges[9] = [[0,0],[0,0]]
        
        # Set the initial rotation of the tail to neutral.
        self.body_rotations[6] = [90,-90,0]
        self.body_rotations[7] = [0,0,0]
        self.body_rotations[8] = [0,0,0]

        if genome != "":
            self.setGenomeValues(genome)

class StraightAngledTailControlHopperContainer(ControlHopperContainer):
    """ Same as ControlHopperContainer but with an evolved angle for the tail. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(StraightAngledTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 1
        self.__genome_offset = super(StraightAngledTailControlHopperContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if genome == "":

            # Tail Base Angle
            self.tail_base_angle = random.randint(-90,10)

        else:
            self.setGenomeValues(genome)

        # Set the tail range of motion to no range.
        # Set the tail range of motion to no range.
        self.joint_ranges[8] = [[0,0],[0,0]]
        self.joint_ranges[9] = [[0,0],[0,0]]
        
        # Set the initial rotation of the tail.
        self.body_rotations[6] = [90,-90+self.tail_base_angle,0]
        self.body_rotations[7] = [0,0,0]
        self.body_rotations[8] = [0,0,0]

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(StraightAngledTailControlHopperContainer, cls).headers()
        head_str += ",Base_Tail_Angle"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(StraightAngledTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(StraightAngledTailControlHopperContainer, self).__str__()
        out_str += ','+str(self.tail_base_angle) # Angle for the base of the tail.
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(StraightAngledTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.tail_base_angle = int(genome[0+self.__genome_offset])
        self.body_rotations[6] = [90,-90+self.tail_base_angle,0]

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(StraightAngledTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.tail_base_angle = int(mutate_value(self.tail_base_angle,-90,10))
            self.body_rotations[6] = [90,-90+self.tail_base_angle,0]


class StraightActuatedTailControlHopperContainer(ControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(StraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 2
        self.__genome_offset = super(StraightActuatedTailControlHopperContainer, self).genome_length()

        # Set the tail joints range of motion to no range.
        # Leave the base to be actuatable
        self.joint_ranges[9] = [[0,0],[0,0]]
        
        # Set the initial rotation of the tail to neutral.
        self.body_rotations[6] = [90,-90,0]
        self.body_rotations[7] = [0,0,0]
        self.body_rotations[8] = [0,0,0]

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):
            
            self.limb_offsets[4] = format_float(random.randint(0,15)/8.)  # Tail (All tail joints!)
            
            self.joint_offsets[6] = format_float(random.randint(0,15)/8.) # Tail
            
        elif genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(StraightActuatedTailControlHopperContainer,cls).headers()
        head_str += ",T_Off,TB_Off"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(StraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(StraightActuatedTailControlHopperContainer, self).__str__()
        out_str += ','+str(self.limb_offsets[4])+','+str(self.joint_offsets[6])
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(StraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.limb_offsets[4] = float(genome[0+self.__genome_offset])
        self.joint_offsets[6] = float(genome[1+self.__genome_offset])

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(StraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.limb_offsets[4] = float("{0:.6f}".format(round(mutate_value(self.limb_offsets[4],0.,15.))/8.))
        
        if random.random() < mut_prob:
            self.joint_offsets[6] = float("{0:.6f}".format(round(mutate_value(self.joint_offsets[6],0.,15.))/8.))


class EvoTailStraightActuatedTailControlHopperContainer(StraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoTailStraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 6
        self.__genome_offset = super(EvoTailStraightActuatedTailControlHopperContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):

            # Dimensions of the Tail
            self.tail_dimensions = [
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # Base of Tail
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # Mid Tail
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # End Tail
            ]

            # Masses of the Tail
            self.body_masses[9]  = format_float(0.05+random.random()*0.55)
            self.body_masses[10] = format_float(0.05+random.random()*0.55) 
            self.body_masses[11] = format_float(0.05+random.random()*0.55)

        elif genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoTailStraightActuatedTailControlHopperContainer, cls).headers()
        head_str += ",TS1_Len,TS2_Len,TS3_Len,TS1_Mass,TS2_Mass,TS3_Mass"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoTailStraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoTailStraightActuatedTailControlHopperContainer, self).__str__()
        out_str += ','+','.join(str(format_float(i[0]/self.bf)) for i in self.tail_dimensions) # Downscale the tail dimensions by self.bf
        out_str += ','+str(self.body_masses[9])  # Base of Tail Mass
        out_str += ','+str(self.body_masses[10]) # Mid Tail Mass
        out_str += ','+str(self.body_masses[11]) # End Tail Mass
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoTailStraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')

        self.tail_dimensions[0] = [float(genome[0+self.__genome_offset])*self.bf,0.025*self.bf]
        self.tail_dimensions[1] = [float(genome[1+self.__genome_offset])*self.bf,0.025*self.bf]
        self.tail_dimensions[2] = [float(genome[2+self.__genome_offset])*self.bf,0.025*self.bf]
        self.body_masses[9]  = float(genome[3+self.__genome_offset])
        self.body_masses[10] = float(genome[4+self.__genome_offset])
        self.body_masses[11] = float(genome[5+self.__genome_offset])

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoTailStraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        for i in range(len(self.tail_dimensions)):
            if random.random() < mut_prob:
                self.tail_dimensions[i][0] = format_float(mutate_value(self.tail_dimensions[i][0]/self.bf,0.1,0.85))*self.bf

        for i in range(3):
            if random.random() < mut_prob:
                self.body_masses[9+i] = format_float(mutate_value(self.body_masses[9+i],0.05,0.60))


class EvoTailFixedMassStraightActuatedTailControlHopperContainer(StraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 6
        self.__genome_offset = super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        if not debug and (not validate and genome == ""):

            # Dimensions of the Tail
            self.tail_dimensions = [
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # Base of Tail
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # Mid Tail
                [format_float(0.1+random.random()*0.75)*self.bf,0.025*self.bf], # End Tail
            ]

            # Masses of the Tail
            # Default is 0.0429/3.
            self.body_masses[9]  = tail_seg_mass 
            self.body_masses[10] = tail_seg_mass
            self.body_masses[11] = tail_seg_mass

        elif genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, cls).headers()
        head_str += ",TS1_Len,TS2_Len,TS3_Len,TS1_Mass,TS2_Mass,TS3_Mass"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).__str__()
        out_str += ','+','.join(str(format_float(i[0]/self.bf)) for i in self.tail_dimensions) # Downscale the tail dimensions by self.bf
        out_str += ','+str(self.body_masses[9])  # Base of Tail Mass
        out_str += ','+str(self.body_masses[10]) # Mid Tail Mass
        out_str += ','+str(self.body_masses[11]) # End Tail Mass
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')

        self.tail_dimensions[0] = [float(genome[0+self.__genome_offset])*self.bf,0.025*self.bf]
        self.tail_dimensions[1] = [float(genome[1+self.__genome_offset])*self.bf,0.025*self.bf]
        self.tail_dimensions[2] = [float(genome[2+self.__genome_offset])*self.bf,0.025*self.bf]
        self.body_masses[9]  = float(genome[3+self.__genome_offset])
        self.body_masses[10] = float(genome[4+self.__genome_offset])
        self.body_masses[11] = float(genome[5+self.__genome_offset])

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoTailFixedMassStraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        for i in range(len(self.tail_dimensions)):
            if random.random() < mut_prob:
                self.tail_dimensions[i][0] = format_float(mutate_value(self.tail_dimensions[i][0]/self.bf,0.1,0.85))*self.bf


class EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer(EvoTailFixedMassStraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 0
        self.__genome_offset = super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).genome_length()

        self.rear_mass  = 2.0/3.0
        self.mid_mass   = 2.0/3.0
        self.front_mass = 2.0/3.0

        if genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, cls).headers()
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).__str__()
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')

        self.rear_mass  = 2.0/3.0
        self.mid_mass   = 2.0/3.0
        self.front_mass = 2.0/3.0

        self.body_masses[0] = self.rear_mass
        self.body_masses[1] = self.mid_mass
        self.body_masses[2] = self.front_mass

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        self.rear_mass  = 2.0/3.0
        self.mid_mass   = 2.0/3.0
        self.front_mass = 2.0/3.0

        self.body_masses[0] = self.rear_mass
        self.body_masses[1] = self.mid_mass
        self.body_masses[2] = self.front_mass


class EvoAngledTailStraightActuatedTailControlHopperContainer(EvoTailStraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 1
        self.__genome_offset = super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).genome_length()

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        # Tail Base Angle
        self.tail_base_angle = random.randint(-90,10)
        if genome != "":
            self.setGenomeValues(genome)

        # Set the tail range of motion to no range.
        self.joint_ranges[9] = [[0,0],[0,0]]
        
        # Set the initial rotation of the tail.
        self.body_rotations[6] = [90,-90+self.tail_base_angle,0]
        self.body_rotations[7] = [0,0,0]
        self.body_rotations[8] = [0,0,0]

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoAngledTailStraightActuatedTailControlHopperContainer, cls).headers()
        head_str += ",Base_Tail_Angle"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).__str__()
        out_str += ','+str(self.tail_base_angle) # Angle for the base of the tail.
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.tail_base_angle = int(genome[0+self.__genome_offset])
        self.body_rotations[6] = [90,-90+self.tail_base_angle,0]

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoAngledTailStraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.tail_base_angle = int(mutate_value(self.tail_base_angle,-90,10))
            self.body_rotations[6] = [90,-90+self.tail_base_angle,0]


class EvoAngledTailCurvedActuatedTailControlHopperContainer(EvoTailFixedMassStraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 0
        self.__genome_offset = super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).genome_length()

        if genome != "":
            self.setGenomeValues(genome)

        # Set the tail range of motion to no range.
        self.joint_ranges[9] = [[0,0],[0,0]]
        
        # Set the initial rotation of the tail.
        #self.body_rotations[6] = [90,-90+self.tail_base_angle,0]
        self.body_rotations[7] = [0,-30,0]
        self.body_rotations[8] = [0,-30,0]

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoAngledTailCurvedActuatedTailControlHopperContainer, cls).headers()
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).__str__()
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).setGenomeValues(genome)

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoAngledTailCurvedActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)


class EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer(EvoTailStraightActuatedTailControlHopperContainer):
    """ Define a hopper genome, all the parameters associated with an individual. """

    def __init__(self,debug=False,validate=False,genome=""):
        """ Initialize the genome with a set of values. """
        super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).__init__(debug=debug)

        self.__num_genes = 1
        self.__genome_offset = super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).genome_length()

        self.delta_lims = [-10.0,1.0]

        # Check to see if we're just debugging or not.
        # Only initialize the parameters we're concerned with.
        # Tail Base Angle
        self.delta = format_float(self.delta_lims[0]+(random.random()*(self.delta_lims[1]-self.delta_lims[0])))
        if genome != "":
            self.setGenomeValues(genome)

    @classmethod
    def headers(cls):
        """ Return a comma separated string of the parameters associated with an individual. """
        head_str = super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, cls).headers()
        head_str += ",Delta"
        return head_str

    def genome_length(self):
        """ Return the number of genes in the class. """
        return self.__num_genes + super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).genome_length()    

    def __str__(self):
        """ Define the to string method for the class. """
        out_str = super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).__str__()
        out_str += ','+str(self.delta) # Delta for skew of the signal.
        return out_str

    def serialize(self):
        """ Return a list representation of the genome for crossover. """
        return [float(i) for i in (str(self)).split(',')]

    def deserialize(self,genome):
        """ Set the genome to values provided in the genome. """
        self.setGenomeValues(genome)

    def setGenomeValues(self,genome):
        """ Set the genome values associated with an individual. 

        Args:
            genome: string containing the various genome parameters.
        """
        super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).setGenomeValues(genome)

        if type(genome) == str:
            genome = genome.split(',')
        self.delta = format_float(float(genome[0+self.__genome_offset]))

    def mutate(self, mut_prob=0.04):
        """ Mutate an individual. 

        Args:
            mut_prob: mutation probability per element in the genome.
        """
        super(EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer, self).mutate(mut_prob=mut_prob)

        if random.random() < mut_prob:
            self.delta = format_float(mutate_value(self.delta,self.delta_lims[0],self.delta_lims[1]))            