# This script is intended to look at the profiles of the best individuals and present a statistical breakdown of the various parameters
# and performance.

# Constants for writing out data.
write_dest <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/best_individual_analysis/"

# Read in the data from the file.
all_data <- read.csv("/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/all_best_individuals_unified_data.dat",header=TRUE,sep=",")
all_data$Behavior <- as.factor(all_data$Behavior)
all_data$Exp <- as.factor(all_data$Exp)
all_data$Tail_Len <- all_data$TS1_Len + all_data$TS2_Len + all_data$TS3_Len
all_data$Total_Mass <- all_data$Rear_Torso_Mass + all_data$Mid_Torso_Mass + all_data$Front_Torso_Mass
all_data$Mass_Dist <- (all_data$Rear_Torso_Mass/all_data$Total_Mass*1.0 + all_data$Mid_Torso_Mass/all_data$Total_Mass*2.0 + all_data$Front_Torso_Mass/all_data$Total_Mass*3.0)/3.0
all_data$Tail_Offset <- all_data$T_Off + all_data$TB_Off
all_data$Rear_Hip_Offset <- all_data$RH_Off + (all_data$RRL_Off+all_data$LRL_Off)/2.0
all_data$Phase_Diff_Hips_vs_Tail <- abs(all_data$Rear_Hip_Offset - all_data$Tail_Offset)/2.0
summary(all_data)

# Subset the data to get bipedal hopper only results.
bipedal_hopper_data <- subset(all_data, Behavior == "Bipedal Hopping")
summary(bipedal_hopper_data)

#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Dist_Traveled,fill=Behavior)) +
	labs(title="Behavior versus Distance Traveled",x="Experiment",y="Distance Traveled") +
	geom_boxplot()
plt

save_plot(write_dest,"behavior_vs_dist_traveled",plt)

#######################################################################################################################
# Plot the oscillation frequency of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Osc_Freq,fill=Behavior)) +
	labs(title="Behavior versus Oscillation Frequency",x="Experiment",y="Oscillation Frequency") +
	ylim(min=0,max=2) +
	geom_boxplot()
plt

save_plot(write_dest,"behavior_vs_osc_freq",plt)

#######################################################################################################################
# Plot the tail length of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Tail_Len,fill=Behavior)) +
	labs(title="Behavior versus Tail Length",x="Experiment",y="Tail Length") +
	ylim(min=0,max=2.5) +
	geom_boxplot()
plt

save_plot(write_dest,"behavior_vs_tail_len",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Mass_Dist,fill=Behavior)) +
	labs(title="Behavior versus Mass Distribution",x="Experiment",y="Mass Dist(0 Rear, 1 Fore)") +
	geom_boxplot()
plt

save_plot(write_dest,"behavior_vs_mass_dist",plt)

#######################################################################################################################
# Plot tail length for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Tail_Len,fill=Exp)) +
	labs(title="Tail Length for Bipedal Hoppers Across Experiments",x="Experiment",y="Tail Length") +
	geom_boxplot()
plt

save_plot(write_dest,"tail_len_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot oscillation frequency for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Osc_Freq,fill=Exp)) +
	labs(title="Oscillation Frequency for Bipedal Hoppers Across Experiments",x="Experiment",y="Oscillation Frequency") +
	ylim(min=0,max=2) +
	geom_boxplot()
plt

save_plot(write_dest,"osc_freq_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot Distance Traveled for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Dist_Traveled,fill=Exp)) +
	labs(title="Distance Traveled for Bipedal Hoppers Across Experiments",x="Experiment",y="Distance Traveled") +
	ylim(min=0,max=120) +
	geom_boxplot()
plt

save_plot(write_dest,"dist_traveled_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot tail length versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Tail_Len,y=Dist_Traveled,color=Exp)) +
	geom_point()
plt

#######################################################################################################################
# Plot oscillation frequency versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Osc_Freq,y=Dist_Traveled,color=Exp)) +
	ylim(min=0,max=120) +
	xlim(min=0,max=2) +
	geom_point() +
	facet_grid(. ~ Exp)
plt

#######################################################################################################################
# Plot Rear Hip Offset versus Tail Offset for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Rear_Hip_Offset,y=Tail_Offset,color=Exp)) +
	ylim(min=0,max=2) +
	xlim(min=0,max=2) +
	geom_point(aes(size=Dist_Traveled)) +
	facet_wrap(~ Exp,ncol=3) +
	coord_fixed()
plt

#######################################################################################################################
# Plot Rear Hip Offset versus Tail Offset for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Phase_Diff_Hips_vs_Tail,y=Dist_Traveled,color=Exp)) +
	ylim(min=0,max=120) +
	xlim(min=0,max=1) +
	geom_point() +
	facet_wrap(~ Exp,ncol=3)
plt