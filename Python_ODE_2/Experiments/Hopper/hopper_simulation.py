"""
    Wrapper to conduct the actual simulation of a quadruped robot.  Access through methods: 
    evaluate individual, and physics only validation.
"""

import sys, os, random
import itertools
import math


sys.path.insert(0, '../../')

from Analysis import MOIAnalysis

from ODESystem import ODEManager
from ODESystem import Placement

from Robot import Sensors

import ode

man = 0
hopper = 0

# For tracking toe touching over time.
num_touches = 0
touch_logging = [] # For keeping track of when a toe touch is persistent or new.
output_path = ""

# Track collisions between bodies and ground over time.
log_body_touching = False

# Friction Coefficient
friction_coefficient = 100 # Default is high friction.

mass_flag_fix = False # Use to implement mass as total mass and not density.

##########################################################################################

def drange(start, stop, step):
    r = start
    while r < stop:
        yield r
        r += step
        
def shift_oscillator(t,freq,d,phase_shift):
    """ Implement the shifted oscillator from the paper:
        "An Improved Evolvable Oscillator and Basis Function Set for 
        Control of an Insect-Scale Flapping-Wing Micro Air Vehicle"
        
        Args:
            freq: Oscillation frequency
            d: parameter determining how the wave will be skewed. (-12.0 to 1.0 acceptable.)
            phase_shift: percentage to shift the phase by.
    """
    
    # Adjust t to fall within the bounds of the oscillation frequency.
    t = t + (1.0/freq) * phase_shift
    t = t -(int(t/(1.0/freq))*1.0/freq)
    
    omega = 2.0*math.pi*freq

    # delta is the frequency offset that deines how much the upstroke phase is 
    # impeded or advanced
    delta = d*3.0*freq

    # rho and xi characterize a downstroke consisten with the split-cycle philosophy
    rho = delta * omega / (omega - 2.0*delta)
    xi = -2.0*math.pi*delta / (omega - 2.0*delta)
    xiS = 0.0

    thalfU = 1.0/(omega - delta)*2.0*math.pi/2.0
    thalfD = 1.0/(omega + rho)*2.0*math.pi/2.0

    if t < thalfU:
        return math.cos((omega - delta)*t)
    else:
        return math.cos((omega + rho)*t + xi)

##########################################################################################        

class Hopper(object):
    """ Represent the hopper robot. """

    def __init__(self,man,genome,base_pos=[0,0,0],logging=False):
        """ Initialize the robot in the ODE environment. 

        Arguments:
            man: ODE Manager for the Physics Simulation
            genome: genome defining a robot
            base_pos: base position to start the robot from
        """
        global output_path

        self.man = man
        self.body_keys = []
        self.joint_feedback_range = [2,15]

        # Sensors for robot.
        self.sensor = Sensors(man,logging=logging,log_path=output_path)

        # Hardware Limits
        self.ref_moi_pivot = ""

        # Initialize the robot.
        self.__create_robot(genome,base_pos=base_pos)

    def __create_robot(self,genome,base_pos=[0,0,0]):
        """ Create the robot used in the experiment. 

        Arguments:
            base_pos: base position to start the robot from
            genome: defines the basic components of a robot.
        """
        global touch_logging,log_body_touching

        # Joint Power
        BACK_FORCE = genome.max_forces[0]
        RH_FORCE = genome.max_forces[1]
        RK_FORCE = genome.max_forces[2]
        RA_FORCE = genome.max_forces[3]
        RT_FORCE = genome.max_forces[4]
        FH_FORCE = genome.max_forces[5]
        FK_FORCE = genome.max_forces[6]
        T_FORCE = genome.max_forces[7]
        H_FORCE = genome.max_forces[8]

        # Flexibility for the joints.
        ERP = genome.erp

        CFM = genome.cfm

        joint_range = 1.0

        joint_ranges = genome.joint_ranges

        height = 0.75*genome.bf # Scale up by the genome base factor.

        # Torso body dimensions
        body_seg_dims = genome.body_dimensions[0:3]

        # Store main body dimensions for flipped checking.
        self.main_body_dimensions = [i for i in body_seg_dims[1]]

        # Torso body positions
        body_seg_pos = [
            [-body_seg_dims[1][0]/2.-body_seg_dims[0][0]/2.,height,0.], # Rear Segment
            [0,height,0.], # Mid Segment
            [body_seg_dims[1][0]/2.+body_seg_dims[2][0]/2.,height,0.]  # Front Segment
        ]

        # Masses of the segments.
        body_masses = genome.body_masses

        # Leg Dimensions 
        leg_dims = genome.body_dimensions[3:]

        # Leg Rotations
        leg_rotations = [
            genome.body_rotations[0], # Rear Upper Legs
            genome.body_rotations[1], # Rear Mid Legs
            genome.body_rotations[2], # Rear Low Legs
            genome.body_rotations[3],  # Rear Feet
            genome.body_rotations[4], # Front Upper Legs
            genome.body_rotations[5],  # Front Lower Legs
        ]

        # Body segement and upper leg joint positions
        fudge_factor = 0.0001 # Put the legs slightly outside the body, helps with stability.
        body_seg_joint_pos = [
            [-body_seg_dims[1][0]/2.,height,0.], # Rear - Mid Connection
            [body_seg_dims[1][0]/2.,height,0.],  # Mid - Front Connection
            [body_seg_pos[0][0]-body_seg_dims[0][0]/4., height, body_seg_dims[0][2]/2.+leg_dims[0][1]+fudge_factor],  # Rear Upper Left Leg
            [body_seg_pos[0][0]-body_seg_dims[0][0]/4., height, -body_seg_dims[0][2]/2.-leg_dims[0][1]-fudge_factor], # Rear Upper Right Leg
            [body_seg_pos[2][0]+body_seg_dims[0][0]/4., height, body_seg_dims[2][2]/2.+leg_dims[4][1]+fudge_factor], # Front Upper Left Leg
            [body_seg_pos[2][0]+body_seg_dims[0][0]/4., height, -body_seg_dims[2][2]/2.-leg_dims[4][1]-fudge_factor], # Front Upper Right Leg
            [body_seg_pos[0][0]-body_seg_dims[0][0]/2., height, 0.], # Tail Connection
            [body_seg_pos[2][0]+body_seg_dims[2][0]/2., height, 0.], # Head Connection
        ]

        # Tail Rotations
        tail_rotations = [
            genome.body_rotations[6], # Base of Tail
            genome.body_rotations[7],  # Mid Tail
            genome.body_rotations[8],  # End Tail
        ]

        # Tail Dimensions
        tail_dimensions = genome.tail_dimensions

        # Head Dimensions
        head_dims = genome.head_dimensions

        # Head Rotations
        head_rotations = [90,45,0]

        # Set the reference point for the MOI pivot.
        self.ref_moi_pivot = [-body_seg_dims[0][0]/4.,0.,0.]
        
        # Main Body (3 sections
        self.man.create_box(0,body_seg_dims[0],body_seg_pos[0],density=body_masses[0],mass_flag=mass_flag_fix) # Rear Segment
        self.man.create_box(1,body_seg_dims[1],body_seg_pos[1],density=body_masses[1],mass_flag=mass_flag_fix) # Mid Segment
        self.man.create_box(2,body_seg_dims[2],body_seg_pos[2],density=body_masses[2],mass_flag=mass_flag_fix) # Front Segment

        self.man.create_universal(0,body_seg_joint_pos[0],[0,1],axis1=[0,0,1],axis2=[0,1,0],loStop1=-joint_range,hiStop1=joint_range,loStop2=-joint_range,hiStop2=joint_range,fmax=BACK_FORCE[0],fmax2=BACK_FORCE[1])
        self.man.create_universal(1,body_seg_joint_pos[1],[1,2],axis1=[0,0,1],axis2=[0,1,0],loStop1=-joint_range,hiStop1=joint_range,loStop2=-joint_range,hiStop2=joint_range,fmax=BACK_FORCE[0],fmax2=BACK_FORCE[1])

        # Upper Legs
        self.man.create_capsule(3, body_masses[3], leg_dims[0][0], leg_dims[0][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[3],pos=body_seg_joint_pos[2],rot=leg_rotations[0])
        self.man.create_flexible_universal(2, body_seg_joint_pos[2],[0,3],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[2][1][0],hiStop1=joint_ranges[2][1][1],loStop2=joint_ranges[2][0][0],hiStop2=joint_ranges[2][0][1],fmax=RH_FORCE[0],fmax2=RH_FORCE[1],erp1=ERP[1][0],erp2=ERP[1][1],cfm1=CFM[1][0],cfm2=CFM[1][1])
        self.man.create_capsule(4, body_masses[3], leg_dims[0][0], leg_dims[0][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[4],pos=body_seg_joint_pos[3],rot=leg_rotations[0])
        self.man.create_flexible_universal(3, body_seg_joint_pos[3],[0,4],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[2][1][0],hiStop1=joint_ranges[2][1][1],loStop2=joint_ranges[2][0][0],hiStop2=joint_ranges[2][0][1],fmax=RH_FORCE[0],fmax2=RH_FORCE[1],erp1=ERP[1][0],erp2=ERP[1][1],cfm1=CFM[1][0],cfm2=CFM[1][1])

        # Mid Legs
        self.man.create_capsule(5, body_masses[4], leg_dims[1][0], leg_dims[1][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[3],self.man.bodies[5],rot=leg_rotations[1]) 
        self.man.create_flexible_universal(4, j_loc,[3,5],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[3][1][0],hiStop1=joint_ranges[3][1][1],loStop2=joint_ranges[3][0][0],hiStop2=joint_ranges[3][0][1],fmax=RK_FORCE[0],fmax2=RK_FORCE[1],erp1=ERP[2][0],erp2=ERP[2][1],cfm1=CFM[2][0],cfm2=CFM[2][1])
        self.man.create_capsule(6, body_masses[4], leg_dims[1][0], leg_dims[1][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[4],self.man.bodies[6],rot=leg_rotations[1]) 
        self.man.create_flexible_universal(5, j_loc,[4,6],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[3][1][0],hiStop1=joint_ranges[3][1][1],loStop2=joint_ranges[3][0][0],hiStop2=joint_ranges[3][0][1],fmax=RK_FORCE[0],fmax2=RK_FORCE[1],erp1=ERP[2][0],erp2=ERP[2][1],cfm1=CFM[2][0],cfm2=CFM[2][1])

        # Low Legs
        self.man.create_capsule(7, body_masses[5], leg_dims[2][0], leg_dims[2][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[5],self.man.bodies[7],rot=leg_rotations[2]) 
        self.man.create_flexible_universal(6, j_loc,[5,7],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[4][1][0],hiStop1=joint_ranges[4][1][1],loStop2=joint_ranges[4][0][0],hiStop2=joint_ranges[4][0][1],fmax=RA_FORCE[0],fmax2=RA_FORCE[1],erp1=ERP[3][0],erp2=ERP[3][1],cfm1=CFM[3][0],cfm2=CFM[3][1])
        self.man.create_capsule(8, body_masses[5], leg_dims[2][0], leg_dims[2][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[6],self.man.bodies[8],rot=leg_rotations[2]) 
        self.man.create_flexible_universal(7, j_loc,[6,8],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[4][1][0],hiStop1=joint_ranges[4][1][1],loStop2=joint_ranges[4][0][0],hiStop2=joint_ranges[4][0][1],fmax=RA_FORCE[0],fmax2=RA_FORCE[1],erp1=ERP[3][0],erp2=ERP[3][1],cfm1=CFM[3][0],cfm2=CFM[3][1])

        # Feet 
        self.man.create_capsule(9, body_masses[6], leg_dims[3][0], leg_dims[3][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[7],self.man.bodies[9],rot=leg_rotations[3]) 
        self.man.create_flexible_universal(8, j_loc,[7,9],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[5][1][0],hiStop1=joint_ranges[5][1][1],loStop2=joint_ranges[5][0][0],hiStop2=joint_ranges[5][0][1],fmax=RT_FORCE[0],fmax2=RT_FORCE[1],erp1=ERP[4][0],erp2=ERP[4][1],cfm1=CFM[4][0],cfm2=CFM[4][1])
        self.man.create_capsule(10, body_masses[6], leg_dims[3][0], leg_dims[3][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[8],self.man.bodies[10],rot=leg_rotations[3]) 
        self.man.create_flexible_universal(9, j_loc,[8,10],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[5][1][0],hiStop1=joint_ranges[5][1][1],loStop2=joint_ranges[5][0][0],hiStop2=joint_ranges[5][0][1],fmax=RT_FORCE[0],fmax2=RT_FORCE[1],erp1=ERP[4][0],erp2=ERP[4][1],cfm1=CFM[4][0],cfm2=CFM[4][1])

        # Upper Front Legs
        self.man.create_capsule(11, body_masses[7], leg_dims[4][0], leg_dims[4][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[11],pos=body_seg_joint_pos[4],rot=leg_rotations[4])
        self.man.create_flexible_universal(10, body_seg_joint_pos[4],[2,11],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[6][1][0],hiStop1=joint_ranges[6][1][1],loStop2=joint_ranges[6][0][0],hiStop2=joint_ranges[6][0][1],fmax=FH_FORCE[0],fmax2=FH_FORCE[1],erp1=ERP[5][0],erp2=ERP[5][1],cfm1=CFM[5][0],cfm2=CFM[5][1])
        self.man.create_capsule(12, body_masses[7], leg_dims[4][0], leg_dims[4][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[12],pos=body_seg_joint_pos[5],rot=leg_rotations[4])
        self.man.create_flexible_universal(11, body_seg_joint_pos[5],[2,12],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[6][1][0],hiStop1=joint_ranges[6][1][1],loStop2=joint_ranges[6][0][0],hiStop2=joint_ranges[6][0][1],fmax=FH_FORCE[0],fmax2=FH_FORCE[1],erp1=ERP[5][0],erp2=ERP[5][1],cfm1=CFM[5][0],cfm2=CFM[5][1])

        # Front Feet 
        self.man.create_capsule(13, body_masses[8], leg_dims[5][0], leg_dims[5][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[11],self.man.bodies[13],rot=leg_rotations[5]) 
        self.man.create_flexible_universal(12, j_loc,[11,13],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[7][1][0],hiStop1=joint_ranges[7][1][1],loStop2=joint_ranges[7][0][0],hiStop2=joint_ranges[7][0][1],fmax=FK_FORCE[0],fmax2=FK_FORCE[1],erp1=ERP[6][0],erp2=ERP[6][1],cfm1=CFM[6][0],cfm2=CFM[6][1])
        self.man.create_capsule(14, body_masses[8], leg_dims[5][0], leg_dims[5][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[12],self.man.bodies[14],rot=leg_rotations[5]) 
        self.man.create_flexible_universal(13, j_loc,[12,14],axis1=[1,0,0],axis2=[0,0,1],loStop1=joint_ranges[7][1][0],hiStop1=joint_ranges[7][1][1],loStop2=joint_ranges[7][0][0],hiStop2=joint_ranges[7][0][1],fmax=FK_FORCE[0],fmax2=FK_FORCE[1],erp1=ERP[6][0],erp2=ERP[6][1],cfm1=CFM[6][0],cfm2=CFM[6][1])

        # Tail
        
        self.man.create_capsule(15, body_masses[9], tail_dimensions[0][0], tail_dimensions[0][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[15],pos=body_seg_joint_pos[6],rot=tail_rotations[0])
        self.man.create_flexible_universal(14, body_seg_joint_pos[6],[0,15],axis1=[0,1,0],axis2=[0,0,1],loStop1=joint_ranges[8][1][0],hiStop1=joint_ranges[8][1][1],loStop2=joint_ranges[8][0][0],hiStop2=joint_ranges[8][0][1],fmax=T_FORCE[0],fmax2=T_FORCE[1],erp1=ERP[7][0],erp2=ERP[7][1],cfm1=CFM[7][0],cfm2=CFM[7][1])
        
        self.man.create_capsule(16, body_masses[10], tail_dimensions[0][0], tail_dimensions[0][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[15],self.man.bodies[16],rot=tail_rotations[1]) 
        self.man.create_flexible_universal(15, j_loc,[15,16],axis1=[0,1,0],axis2=[0,0,-1],loStop1=joint_ranges[9][1][0],hiStop1=joint_ranges[9][1][1],loStop2=joint_ranges[9][0][0],hiStop2=joint_ranges[9][0][1],fmax=T_FORCE[0],fmax2=T_FORCE[1],erp1=ERP[7][0],erp2=ERP[7][1],cfm1=CFM[7][0],cfm2=CFM[7][1])
        
        self.man.create_capsule(17, body_masses[11], tail_dimensions[0][0], tail_dimensions[0][1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        j_loc = Placement.place_capsule_trans(self.man.bodies[16],self.man.bodies[17],rot=tail_rotations[2]) 
        self.man.create_flexible_universal(16, j_loc,[16,17],axis1=[0,1,0],axis2=[0,0,1],loStop1=joint_ranges[9][1][0],hiStop1=joint_ranges[9][1][1],loStop2=joint_ranges[9][0][0],hiStop2=joint_ranges[9][0][1],fmax=T_FORCE[0],fmax2=T_FORCE[1],erp1=ERP[7][0],erp2=ERP[7][1],cfm1=CFM[7][0],cfm2=CFM[7][1])

        # Head
        self.man.create_capsule(18, body_masses[12], head_dims[0], head_dims[1],[0,0,0],rot=[0.,0.,0.],mass_flag=mass_flag_fix)
        Placement.place_capsule_at_trans(self.man.bodies[18],pos=body_seg_joint_pos[7],rot=head_rotations)
        self.man.create_flexible_universal(17, body_seg_joint_pos[7],[2,18],axis1=[0,1,0],axis2=[0,0,1],loStop1=joint_ranges[10][1][0],hiStop1=joint_ranges[10][1][1],loStop2=joint_ranges[10][0][0],hiStop2=joint_ranges[10][0][1],fmax=H_FORCE[0],fmax2=H_FORCE[1],erp1=ERP[8][0],erp2=ERP[8][1],cfm1=CFM[8][0],cfm2=CFM[8][1])

        # Add in information about the feet.
        self.sensor.add_touch_sensor([9,10,13,14])
        touch_logging.append({man.get_geom_by_key(9):0,man.get_geom_by_key(10):0,man.get_geom_by_key(13):0,man.get_geom_by_key(14):0})
        touch_logging.append({man.get_geom_by_key(9):0,man.get_geom_by_key(10):0,man.get_geom_by_key(13):0,man.get_geom_by_key(14):0})

        # Add body touch sensors (Only if logging.)
        if log_body_touching:
            self.sensor.add_body_touch_sensor([i for i in range(19)])

        # Add in joint positions sensors.
        self.sensor.register_joint_sensors([i for i in range(17)])

        # Turn feedback on for the actuated joints.
        for i in range(self.joint_feedback_range[0],self.joint_feedback_range[1]):
            self.man.joints[i].setFeedback()

    def actuate_joints_by_pos(self,positions=[[0.,0.] for i in xrange(8)]):
        """ Actuate the joints of the hopper to the specified position. 

        Arguments:
            positions: position of the joints to actuate to.
        """

        for i,p in enumerate(positions):
            self.man.actuate_universal(i,p[0],p[1])

    def get_sensor_states(self):
        """ Get the states of the various sensors on the robot. """
        sensors = [i for i in self.sensor.get_touch_sensor_states()]
        sensors += [i for i in self.sensor.get_joint_sensors()]
        return sensors

    def reset_touch_sensors(self):
        """ Reset the touch sensors. """
        self.sensor.clear_touching()

    def reset_body_touch_sensors(self):
        """ Reset the body touch sensors. """
        self.sensor.clear_body_touching()

    def get_touch_sensor_states(self):
        """ Get the state of the touch sensor for debugging. """
        return self.sensor.get_touch_sensor_states()

    def get_joint_feedback(self):
        """ Get the feedback from the joints. """

        joint_feedback = []
        for i in range(self.joint_feedback_range[0],self.joint_feedback_range[1]):
            joint_feedback.append(self.man.joints[i].getFeedback())
        return joint_feedback

    def get_flipped(self):
        """ Determine if the hopper flipped over or not based on the position of the main torso. """
        top_pos = (self.man.get_rel_position(1,(0.,self.main_body_dimensions[1]/2.,0.)))[1]
        bot_pos = (self.man.get_rel_position(1,(0.,-self.main_body_dimensions[1]/2.,0.)))[1]

        # Check to see if we've flipped to almost completely upside down.
        if (top_pos-bot_pos)/math.fabs(self.main_body_dimensions[1]) < -0.9:
            return True

        return False

    def step_sensors(self,cur_time):
        """ Step the sensor information. """
        self.sensor.clear_sensors(cur_time)

    def log_sensor_data(self,file_prefix):
        """ Log the sensor data. 

            Intended to use for validation.
        """
        self.sensor.dump_sensor_data(file_prefix)

    def get_moi_ref_point(self):
        """ Return the reference point for use in MOI calculation.  Rear hips in this case. """
        return self.ref_moi_pivot

############################################################################################################        

def vector_3d_magnitude(vec):
    """ Return the magnitude of a vector in 3D. """

    vec = [vec[0],vec[1],vec[2]]

    # Filter out low values in the vector.
    for i in range(len(vec)):
        if math.fabs(vec[i]) < 0.0001:
            vec[i] = math.copysign(0.0001,vec[i])
        elif math.fabs(vec[i]) > 1000.:
            vec[i] = math.copysign(1000.,vec[i])

    if sum(vec) < 0.01 or (math.isnan(vec[0]) or math.isnan(vec[1]) or math.isnan(vec[2])):
        return 0
    force_mag = 0    
    try:
        force_mag = math.sqrt(vec[0]**2+vec[1]**2+vec[2]**2)
    except OverflowError as e:
        print("\n\n\n")
        print("OverflowError: ",e,vec)
        print("\n\n\n")
    return force_mag

def distance_per_unit_of_power(p1,p2,forces):
    """ Calculate the distance per unit of power. """
    distance = math.sqrt((p1[0]-p2[0])**2 + (p1[2]-p2[2])**2)

    forces = [i for f in forces for i in f]
    forces = zip(*forces)
    total_power = sum([abs(i) for i in forces[0]]) + sum([abs(j) for j in forces[1]])

    # Division by Zero.
    if total_power == 0:
        total_power = 0.0000001

    return (distance)/total_power    

def euclidean_distance(p1,p2):
    """ Calculate the 2d Euclidean distance of two coordinates.
    
    Args:
        p1: position 1
        p2: position 2
    Returns:
        Euclidean distance between the two points.
    """
    return math.sqrt((p1[0]-p2[0])**2 + (p1[2]-p2[2])**2) 

############################################################################################################

# Collision callback
def near_callback(args, geom1, geom2):
    """Callback function for the collide() method.

    This function checks if the given geoms do collide and
    creates contact joints if they do.
    """
    global log_body_touching,touch_logging

    # Check to see if the two objects are connected.  Don't collide.
    if(man.are_connected(geom1.getBody(), geom2.getBody()) or (geom1 != man.floor and geom2 != man.floor)):
        return

    # Check if the objects do collide
    contacts = man.generate_contacts(geom1, geom2)

    # Check to see if one of the objects is a foot sensor and the other
    # is the ground.
    if ((geom1 == man.floor and hopper.sensor.is_touch_sensor(man.get_body_key(geom2.getBody()))) or
        (hopper.sensor.is_touch_sensor(man.get_body_key(geom1.getBody())) and geom2 == man.floor)):
        body_id = -1
        if geom1 == man.floor:
            body_id = man.get_body_key(geom2.getBody())
            touch_logging[1][geom2] = 1
        else:
            body_id = man.get_body_key(geom1.getBody())
            touch_logging[1][geom1] = 1
        hopper.sensor.activate_touch_sensor(body_id,[i for i in contacts[0].getContactGeomParams()[0]])

    # Log which body is contacting the ground.
    # Only when doing validation.
    if log_body_touching:
        body_id = -1
        if (geom1 == man.floor):
            body_id = man.get_body_key(geom2.getBody())
        elif (geom2 == man.floor):
            body_id = man.get_body_key(geom1.getBody())
        hopper.sensor.activate_body_touch_sensor(body_id,[i for i in contacts[0].getContactGeomParams()[0]])

    # Check to see if we are colliding between the floor and a body or bodies on the robot.
    # Set the friction accordingly.
    if not(geom1 == man.floor or geom2 == man.floor):
        mu = 10 # Low friction
    else:
        mu = friction_coefficient

    # Create contact joints
    man.world,man.contactgroup = args
    for c in contacts:
        c.setBounce(0.2)
        c.setMu(mu)
        j = man.create_contact_joint(c)
        j.attach(geom1.getBody(), geom2.getBody())

class Simulation(object):
    """ Define a simulation to encapsulate an ODE simulation. """

    def __init__(self, log_frames=0, run_num=0, eval_time=10., dt=.02, n=4,hyperNEAT=False,substrate=False,periodic=True,file_prefix=""):
        """ Initialize the simulation class. """
        global log_body_touching,simulate

        man = ""

        # Settings for the simulation.
        self.log_frames = log_frames
        self.run_num = run_num
        self.eval_time = eval_time
        self.elapsed_time = 0.
        self.dt = dt            # Timestep for simulation.
        self.n = n              # How many timesteps to simulate per callback.

        self.current_network = 0

        self.hyperNEAT = True if hyperNEAT else False
        self.substrate = substrate

        # Whether we include a periodic oscillating input signal.
        self.periodic = periodic

        # File prefix for logging
        self.file_prefix = ""

        # Genome for a hopper.
        self.genome = ""

        # Explosion condition.
        self.exploded = False

        # Flipping condition.
        self.flipped = False
        self.flipped_time = 0.
        self.flipped_dist = 0.

        # Joint Force Logging
        self.joint_feedback = []
        self.forces = []

        # Set the file prefix for validation purposes.
        self.file_prefix = file_prefix

        # MOI Analysis
        self.moi_analysis = ""

        # Turn on body touch logging if we're doing so.
        if self.log_frames:
            log_body_touching = True


    def update_callback(self):
        """ Function to handle updating the joints and such in the simulation. """

        # Record joint forces for calculation in the fitness function.
        jf = hopper.get_joint_feedback()
        
        self.forces.append([vector_3d_magnitude(jf[i][0]),vector_3d_magnitude(jf[i][2])] for i in range(len(jf)))
        
        if self.log_frames:
            # Record the joint feedback for validation.

            self.joint_feedback.append([self.elapsed_time,
                                        i,
                                        sum(jf[i][0]),
                                        sum(jf[i][1]),
                                        sum(jf[i][2]),
                                        sum(jf[i][3])] for i in range(len(jf)))

        if not hasattr(self.genome, 'delta'):
            positions = [
                         [0,0], # Rear Spine (Up/Down, sideways)
                         [0,0], # Front Spine (Up/Down, sideways)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[0]+self.genome.joint_offsets[0])))], # Rear Right Hip (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[1]+self.genome.joint_offsets[0])))], # Rear Left Hip (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[0]+self.genome.joint_offsets[1])))], # Rear Right Knee (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[1]+self.genome.joint_offsets[1])))], # Rear Left Knee (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[0]+self.genome.joint_offsets[2])))], # Rear Right Ankle (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[1]+self.genome.joint_offsets[2])))], # Rear Left Ankle (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[0]+self.genome.joint_offsets[3])))], # Rear Right Toe (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[1]+self.genome.joint_offsets[3])))], # Rear Left Toe (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[2]+self.genome.joint_offsets[4])))], # Front Right Hip (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[3]+self.genome.joint_offsets[4])))], # Front Left Hip (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[2]+self.genome.joint_offsets[5])))], # Front Right Knee (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[3]+self.genome.joint_offsets[5])))], # Front Left Knee (sideways, forward/back)
                         [0,-math.sin(self.genome.osc_freq*2.*math.pi*(self.elapsed_time)+(2.*math.pi*(self.genome.limb_offsets[4]+self.genome.joint_offsets[6])))], # Base Tail (rotation, up/down)
                         [0,0], # Mid Tail (rotation, up/down)
                         [0,0], # Rear Tail (rotation, up/down)
                         [0,0], # Head (rotation, up/down)
                        ]
        else:
            positions = [
                         [0,0], # Rear Spine (Up/Down, sideways)
                         [0,0], # Front Spine (Up/Down, sideways)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[0]+self.genome.joint_offsets[0]))], # Rear Right Hip (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[1]+self.genome.joint_offsets[0]))], # Rear Left Hip (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[0]+self.genome.joint_offsets[1]))], # Rear Right Knee (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[1]+self.genome.joint_offsets[1]))], # Rear Left Knee (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[0]+self.genome.joint_offsets[2]))], # Rear Right Ankle (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[1]+self.genome.joint_offsets[2]))], # Rear Left Ankle (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[0]+self.genome.joint_offsets[3]))], # Rear Right Toe (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[1]+self.genome.joint_offsets[3]))], # Rear Left Toe (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[2]+self.genome.joint_offsets[4]))], # Front Right Hip (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[3]+self.genome.joint_offsets[4]))], # Front Left Hip (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[2]+self.genome.joint_offsets[5]))], # Front Right Knee (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[3]+self.genome.joint_offsets[5]))], # Front Left Knee (sideways, forward/back)
                         [0,shift_oscillator(self.elapsed_time,self.genome.osc_freq,self.genome.delta,(self.genome.limb_offsets[4]+self.genome.joint_offsets[6]))], # Base Tail (rotation, up/down)
                         [0,0], # Mid Tail (rotation, up/down)
                         [0,0], # Rear Tail (rotation, up/down)
                         [0,0], # Head (rotation, up/down)
                        ]
        # positions = [
        #              [0,0], # Rear Spine (Up/Down, sideways)
        #              [0,0], # Front Spine (Up/Down, sideways)
        #              [0,0], # Rear Right Toe (sideways, forward/back)
        #              [0,0], # Rear Left Toe (sideways, forward/back)
        #              [0,0], # Rear Right Toe (sideways, forward/back)
        #              [0,0], # Rear Left Toe (sideways, forward/back)
        #              [0,-math.sin(0.5*2.*math.pi*(self.elapsed_time)+(2.*math.pi))], # Rear Right Ankle (sideways, forward/back)
        #              [0,-math.sin(0.5*2.*math.pi*(self.elapsed_time)+(2.*math.pi))], # Rear Left Ankle (sideways, forward/back)
        #              [0,0], # Front Right Hip (sideways, forward/back)
        #              [0,0], # Front Left Hip (sideways, forward/back)
        #              [0,0], # Front Right Knee (sideways, forward/back)
        #              [0,0], # Front Left Knee (sideways, forward/back)
        #              [0,0], # Rear Right Toe (sideways, forward/back)
        #              [0,0],
        #              [0,0],
        #              [0,0],
        #              #[0,-math.sin(0.5*2.*math.pi*(self.elapsed_time)+(2.*math.pi))], # Rear Left Knee (sideways, forward/back)
        #              [0,0], # Mid Tail (rotation, up/down)
        #              [0,0], # Rear Left Hip (sideways, forward/back)
        #              #[0,0], # Rear Left Hip (sideways, forward/back)
        #              ]
        hopper.actuate_joints_by_pos(positions=positions)

    def reset_simulation(self):
        """ Reset the simulation. """
        
        global num_touches, touch_logging

        man.delete_joints()
        man.delete_bodies()
        self.exploded = False
        self.flipped = False
        self.elapsed_time = 0.
        self.flipped_time = 0.
        self.flipped_dist = 0.
        self.forces = []
        self.joint_feedback = []
        num_touches = 0
        touch_logging = []

    def simulate(self):
        """ Perform physics simulation. """
        global num_touches, touch_logging

        if self.elapsed_time < self.eval_time: 
            self.update_callback()

            # Update the touching code.
            for k,v in touch_logging[0].iteritems():
                if touch_logging[1][k] != v and v == 0:
                    num_touches += 1
            touch_logging[0] = touch_logging[1].copy() # Ensure we don't copy objects.
            touch_logging[1] = dict.fromkeys( touch_logging[1].iterkeys(), 0 ) # Reset the touch sensors to null state.

            # Check to see if we flipped over.
            if not self.flipped and hopper.get_flipped():
                self.flipped = True
                self.flipped_time = self.elapsed_time
                self.flipped_dist = math.copysign(euclidean_distance(man.get_body_position(0),[0,0,0]),man.get_body_position(0)[0])

            # Periodically check to see if we exploded.
            # Check to see about explosions.
            pos = man.bodies[0].getPosition()
            ang_vel = man.bodies[0].getAngularVel()
            lin_vel = man.bodies[0].getLinearVel()
            arm_ang_vel = man.bodies[13].getAngularVel()
            arm_lin_vel = man.bodies[13].getLinearVel()
            #if int(self.elapsed_time*100.) % 2 == 0 and 
            if (math.fabs(pos[0]) > 150 or math.fabs(pos[1]) > 8. or math.fabs(pos[2]) > 150) \
                or (math.fabs(ang_vel[0]) > 20 or math.fabs(ang_vel[1]) > 20 or math.fabs(ang_vel[2]) > 20) or \
                (math.fabs(lin_vel[0]) > 30 or math.fabs(lin_vel[1]) > 30 or math.fabs(lin_vel[2]) > 30) \
                or (math.fabs(arm_ang_vel[0]) > 20 or math.fabs(arm_ang_vel[1]) > 20 or math.fabs(arm_ang_vel[2]) > 20) or \
                (math.fabs(arm_lin_vel[0]) > 30 or math.fabs(arm_lin_vel[1]) > 30 or math.fabs(arm_lin_vel[2]) > 30):
                # Dump explosion genome and logging information to file.
                # self.explosion_logging(pos,ang_vel,lin_vel)
                self.exploded = True 
        if self.elapsed_time >= self.eval_time or self.exploded:
            fit = [0,100000,0,0]
            if not self.exploded:
                fit[0] = math.copysign(euclidean_distance(man.get_body_position(0),[0,0,0]),man.get_body_position(0)[0]) if not self.flipped else self.flipped_dist
                fit[1] = num_touches
                fit[2] = self.flipped_time if self.flipped else self.elapsed_time
                fit[3] = distance_per_unit_of_power(man.get_body_position(0),[0,0,0],self.forces)
            
            if self.log_frames:
                self.moi_analysis.process_data()
                self.moi_analysis.write_data(self.file_prefix)

            self.reset_simulation()
            return False, fit
        hopper.step_sensors(self.elapsed_time)
        return True, 0 

    def explosion_logging(self, pos, ang_vel, lin_vel):
        """ Log the genome and conditions leading to an explosion for an individual. 

        Args:
            pos: position of an individual
            ang_vel: angular velocity of an individual
            lin_vel: linear velocity of an individual
        """
        # Check to see if explosion file has been created.
        with open(str(self.run_num)+"_explosion_log.dat","a") as f:
            f.write("Genome: "+str(self.genome)+"\n")
            f.write("Position:"+str(pos)+"\n")
            f.write("Ang Vel:"+str(ang_vel)+"\n")
            f.write("Lin Vel:"+str(lin_vel)+"\n")
            f.write("\n\n\n")

    def physics_only_simulation(self):
        """ Initialize and conduct a simulation. """
        global man, hopper

        # Initialize the manager to be unique to the process.
        man = ODEManager(near_callback, stepsize=self.dt/self.n, log_data=self.log_frames, run_num=self.run_num, eval_time=self.eval_time, max_joint_vel=self.genome.max_joint_vel,output_path=self.file_prefix, erp=0.2, cfm=1E-2)

        # Initialize the quadruped
        hopper = Hopper(man=man,genome=self.genome,logging=self.log_frames)

        # Initialize the MOI Analysis
        self.moi_analysis = MOIAnalysis(
            man,
            0,
            hopper.get_moi_ref_point(),
            {
                'torso':[0,1,2,18],
                'right_leg':[4,6,8,10],
                'left_leg':[3,5,7,9],
                'tail':[15,16,17],
                'both_rear_legs':[3,4,5,6,7,8,9,10],
                'front_right_leg':[11,13],
                'front_left_leg':[12,14]
            },
            self.dt
        )

        # If logging the output, tell manager to write the body type, dimensions, and position to the logging file.
        if self.log_frames:
            man.log_world_setup()
       
        go_on, fit = self.simulate()
        while go_on:
            # Simulate physics
            man.step(near_callback, self.n)
            self.moi_analysis.add_timestep()
            self.elapsed_time += self.dt
            go_on, fit = self.simulate()

        # Log the sensor data at the end of a run.
        if self.log_frames:
            hopper.log_sensor_data(self.file_prefix)

        return fit

    def evaluate_individual(self,genome):
        """ Evaluate an individual solution. 

        Args:
            genome: genome of the individual to evaluate

        Returns:
            fitness value of the individual
        """

        # Set the genome.
        self.genome = genome

        # Conduct the evaluation
        return self.physics_only_simulation() 
