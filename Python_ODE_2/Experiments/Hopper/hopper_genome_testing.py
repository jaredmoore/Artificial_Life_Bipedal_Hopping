"""
	Unit tests to ensure that the genomes are serialized and deserialized correctly.  Modifications to the base classes 
	could cause incorrect translation in the future.
"""

import unittest

import hopper_utils


class MutationOperatorTests(unittest.TestCase):

	def testZeroRange(self):
		self.failIf(hopper_utils.mutate_value(10,10,10) != 10)

	def testUpperOutOfBoundsStart(self):
		self.failIf(hopper_utils.mutate_value(20,0,10) != 10)

	def testLowerOutOfBoundsStart(self):
		self.failIf(hopper_utils.mutate_value(-10,0,10) != 0)	


def GenomeSerialization(genome_class):
	""" Test the serialization and deserialization of the provided class.

	Args:
		genome_class: what type of genome we are testing.

	Returns:
		Boolean whether the serialization produced the same or dissimilar genomes.
	"""
	test_genome = genome_class()
	orig_genome = str(test_genome)
	serialized_genome = test_genome.serialize()
	test_genome.deserialize(serialized_genome)
	processed_genome = str(test_genome)
	return (orig_genome != processed_genome)


class GenomeSerializationTests(unittest.TestCase):

	def testControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.ControlHopperContainer))

	def testStraightTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.StraightTailControlHopperContainer))

	def testStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.StraightActuatedTailControlHopperContainer))

	def testEvoTailStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.EvoTailStraightActuatedTailControlHopperContainer))

	def testStraightAngledTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.StraightAngledTailControlHopperContainer))

	def testEvoAngledTailStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.EvoAngledTailStraightActuatedTailControlHopperContainer))

	def testEvoTailFixedMassStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.EvoTailFixedMassStraightActuatedTailControlHopperContainer))		

	def testEvoShiftOscillatorTailStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer))	

	def testEvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer(self):
		self.failIf(GenomeSerialization(hopper_utils.EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer))		
		

def GenomeHeadItems(genome_class):
	""" Get the number of header items in a class. """
	gc = genome_class()
	return (len(gc.headers().split(',')),gc.genome_length())

class GenomeLengthTests(unittest.TestCase):
	""" Make sure the various genome header functions print out the correct number of items. """

	def testBaseEvolveHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.BaseEvolveHopperContainer)
		self.failIf(head_len != num_genes)

	def testControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.ControlHopperContainer)
		self.failIf(head_len != num_genes)

	def testStraightTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.StraightTailControlHopperContainer)
		self.failIf(head_len != num_genes)	

	def testStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.StraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)	

	def testEvoTailStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.EvoTailStraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)	

	def testStraightAngledTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.StraightAngledTailControlHopperContainer)
		self.failIf(head_len != num_genes)

	def testEvoAngledTailStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.EvoAngledTailStraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)

	def testEvoTailFixedMassStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.EvoTailFixedMassStraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)		

	def testEvoShiftOscillatorTailStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)

	def testEvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer(self):
		head_len, num_genes = GenomeHeadItems(hopper_utils.EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer)
		self.failIf(head_len != num_genes)
		

class GenomeEqualityTests(unittest.TestCase):
	""" Make sure the equality operator is overridden correctly. """

	def testBaseEvolveHopperContainerNotEqual(self):
		genome1 = hopper_utils.BaseEvolveHopperContainer()
		genome2 = hopper_utils.BaseEvolveHopperContainer()
		genome2.rear_mass = 4.0
		self.failIf(genome1 == genome2)

	def testBaseEvolveHopperContainerEqual(self):
		genome1 = hopper_utils.BaseEvolveHopperContainer()
		genome1.rear_mass = 4.0
		genome1.mid_mass = 2.0
		genome1.front_mass = 1.0
		genome2 = hopper_utils.BaseEvolveHopperContainer()
		genome2.rear_mass = 4.0
		genome2.mid_mass = 2.0
		genome2.front_mass = 1.0
		self.failIf(not(genome1 == genome2))

	def testControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.ControlHopperContainer()
		genome1.osc_freq = 4.0
		genome2 = hopper_utils.ControlHopperContainer()
		genome2.osc_freq = 5.0
		self.failIf(genome1 == genome2)

	def testControlHopperContainerEqual(self):
		""" 
			Set the Base Hopper Container components to equal so that it 
			only on components of the ControlHopperContainer. 
		"""
		genome1 = hopper_utils.ControlHopperContainer()
		genome1.rear_mass = 4.0
		genome1.mid_mass = 2.0
		genome1.front_mass = 1.0
		genome2 = hopper_utils.ControlHopperContainer()
		genome2.rear_mass = 4.0
		genome2.mid_mass = 2.0
		genome2.front_mass = 1.0
		self.failIf(genome1 == genome2)

	def testStraightAngledTailControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.StraightAngledTailControlHopperContainer()
		genome1.tail_base_angle = -90
		genome2 = hopper_utils.StraightAngledTailControlHopperContainer()
		genome2.tail_base_angle = 90
		self.failIf(genome1 == genome2)

	def testStraightAngledTailControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.EvoAngledTailStraightActuatedTailControlHopperContainer()
		genome1.tail_base_angle = -90
		genome2 = hopper_utils.EvoAngledTailStraightActuatedTailControlHopperContainer()
		genome2.tail_base_angle = 90
		self.failIf(genome1 == genome2)		

	def testEvoTailFixedMassStraightActuatedTailControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.EvoTailFixedMassStraightActuatedTailControlHopperContainer()
		genome1.tail_base_angle = -90
		genome2 = hopper_utils.EvoTailFixedMassStraightActuatedTailControlHopperContainer()
		genome2.tail_base_angle = 90
		self.failIf(genome1 == genome2)		

	def testEvoShiftOscillatorTailStraightActuatedTailControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer()
		genome1.delta = -0.5
		genome2 = hopper_utils.EvoShiftOscillatorTailStraightActuatedTailControlHopperContainer()
		genome2.delta = 1
		self.failIf(genome1 == genome2)

	def testEvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainerNotEqual(self):
		genome1 = hopper_utils.EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer()
		genome1.tail_base_angle = -90
		genome2 = hopper_utils.EvoBodyUniformMassTailFixedMassStraightActuatedTailControlHopperContainer()
		genome2.tail_base_angle = 90
		self.failIf(genome1 == genome2)		
		

def main():
	unittest.main()

if __name__ == '__main__':
	main()