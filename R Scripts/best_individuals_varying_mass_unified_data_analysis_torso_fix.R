# This script is intended to look at the profiles of the best individuals and present a statistical breakdown of the various parameters
# and performance.

library(plyr)

# Constants for writing out data.
write_dest <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/torso_fix/best_individual_analysis/"

# Read in the data from the file.
all_data <- read.csv("/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/torso_fix/all_best_individuals_unified_data.dat",header=TRUE,sep=",")
all_data$Behavior <- as.factor(all_data$Behavior)
all_data$Exp <- as.factor(all_data$Exp)
all_data$Tail_Len <- all_data$TS1_Len * 3.0
all_data$Tail_Mass <- all_data$TS1_Mass + all_data$TS2_Mass + all_data$TS3_Mass
all_data$Total_Mass <- all_data$Rear_Torso_Mass + all_data$Mid_Torso_Mass + all_data$Front_Torso_Mass + 0.5 # Head mass = 0.5
all_data$Mass_Ratio <- all_data$Tail_Mass/all_data$Total_Mass
all_data$Tail_Mass_TBM <- all_data$Tail_Mass/(all_data$Tail_Mass + all_data$Total_Mass + 0.6028) # Remaining mass in legs is .6028
all_data$Mass_Dist <- (all_data$Rear_Torso_Mass/all_data$Total_Mass*1.0 + all_data$Mid_Torso_Mass/all_data$Total_Mass*2.0 + all_data$Front_Torso_Mass/all_data$Total_Mass*3.0)/3.0
all_data$Tail_Offset <- all_data$T_Off + all_data$TB_Off
all_data$Rear_Hip_Offset <- all_data$RH_Off + (all_data$RRL_Off+all_data$LRL_Off)/2.0
all_data$Phase_Diff_Hips_vs_Tail <- all_data$Rear_Hip_Offset - all_data$Tail_Offset
summary(all_data)

# Change the order of experiment factor levels.
all_data$Exp <- factor(all_data$Exp, levels = c("2.48% TBM","4.85% TBM","9.25% TBM","9.25% TBMC","16.93% TBM","16.93% TBMC","23.41% TBM","23.41% TBMC","37.94% TBM"))
all_data <- subset(all_data, Exp %in% c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))

# Calculate the mean of the tail length for all data.
mean_tail_len<-ddply(all_data, .(Exp), summarize, mean=mean(Tail_Len))
mean_tail_len

# Calculate the mean of the oscillation frequency for all data.
mean_osc_freq<-ddply(all_data, .(Exp), summarize, mean=mean(Osc_Freq))
mean_osc_freq

# Calculate the mean of the tail length for all data.
mean_dist_traveled<-ddply(all_data, .(Exp), summarize, mean=mean(Dist_Traveled))
mean_dist_traveled

# Subset the data to get bipedal hopper only results.
bipedal_hopper_data <- subset(all_data, Behavior == "Bipedal Hopping")
summary(bipedal_hopper_data)

bipedal_hopper_data$Exp <- factor(bipedal_hopper_data$Exp, levels = c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM","9.25% TBMC","16.93% TBMC","23.41% TBMC"))

levels(all_data$Exp)

#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Dist_Traveled,fill=Behavior)) +
	labs(title="Behavior versus Distance Traveled",x="Experiment",y="Distance Traveled") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0, max=90) +
	geom_boxplot() #+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_dist_traveled",plt)

#######################################################################################################################
# Plot the performance of the individuals versus Tail TBM Mass
plt <- ggplot(subset(all_data,Behavior == "Bounding" | Behavior == "Bipedal Hopping"),aes(x=Tail_Mass_TBM,y=Dist_Traveled,color=Behavior,shape=Experiment)) +
	labs(title="Tail Mass TBM versus Distance Traveled",x="Tail Mass (TBM)",y="Distance Traveled") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0, max=90) +
	geom_point() +
	scale_color_manual(values=c(cbPalette[2],cbPalette[4]))#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tail_mass_tbm_vs_dist_traveled",plt)

#######################################################################################################################
# Plot the performance of the individuals versus Tail TBM Mass
plt <- ggplot(subset(all_data,Behavior == "Bounding" | Behavior == "Bipedal Hopping"),aes(x=Total_Mass,y=Dist_Traveled,color=Behavior,shape=Experiment)) +
	labs(title="Torso Mass versus Distance Traveled",x="Torso Mass",y="Distance Traveled") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0, max=90) +
	xlim(min=0, max=4.0) +
	geom_point() +
	scale_color_manual(values=c(cbPalette[2],cbPalette[4]))#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"torso_mass_vs_dist_traveled",plt)

#######################################################################################################################

# Plot the oscillation frequency of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Osc_Freq,fill=Behavior)) +
	labs(title="Behavior versus Oscillation Frequency",x="Experiment",y="Oscillation Frequency") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=2) +
	geom_boxplot()# +
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_osc_freq",plt)

#######################################################################################################################
# Plot the tail length of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Tail_Len,fill=Behavior)) +
	labs(title="Behavior versus Tail Length",x="Experiment",y="Tail Length") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=2.5) +
	geom_boxplot() #+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_tail_len",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Exp,y=Mass_Dist,fill=Behavior)) +
	labs(title="Behavior versus Mass Distribution",x="Experiment",y="Mass Dist(0 Rear, 1 Fore)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_boxplot() #+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_mass_dist",plt)

#######################################################################################################################

# Plot the mass distribution of the individuals versus Dist_Traveled separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Mass_Dist, y=Dist_Traveled,color=Behavior)) +
	labs(title="Mass Distribution versus Performance",x="Mass Distribution(0 Rear, 1 Fore)",y="Distance Traveled") +
	xlim(min=0,max=1) +
	ylim(min=0,max=90) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_point() +
	facet_wrap(~ Exp, ncol=2)
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"mass_dist_vs_performance",plt)

#######################################################################################################################

# Plot the total mass of the individuals versus Dist_Traveled separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Total_Mass, y=Dist_Traveled,color=Behavior)) +
	labs(title="Torso Mass versus Performance",x="Torso Mass",y="Distance Traveled") +
	xlim(min=0,max=5) +
	ylim(min=0,max=90) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_point() +
	facet_wrap(~ Exp, ncol=2)
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"torso_mass_vs_performance",plt)

#######################################################################################################################

# Plot the total mass of the individuals versus Dist_Traveled separated by behavior and grouped by experiment.
plt <- ggplot(all_data,aes(x=Mass_Ratio, y=Dist_Traveled,color=Behavior)) +
	labs(title="Torso Tail Mass Ratio versus Performance",x="Mass Ratio",y="Distance Traveled") +
	xlim(min=0,max=1.) +
	ylim(min=0,max=90) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_point() +
	facet_wrap(~ Exp, ncol=2)
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"torso_tail_mass_ratio_vs_performance",plt)

#######################################################################################################################

plt <- ggplot(all_data,aes(x=Tail_Len,y=Dist_Traveled,color=Exp,shape=Exp)) +
	labs(title="Tail Length versus Distance Traveled",x="Tail Length",y="Distance Traveled") +
	ylim(min=0,max=90) +
	xlim(min=0,max=2.55) +
	geom_point(size=2) +
	facet_wrap(~ Behavior, ncol=2)
plt

save_plot(write_dest,"dist_traveled_versus_tail_length_exp",plt)

#######################################################################################################################

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# Plot tail length for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Tail_Len,fill=Exp)) +
	labs(title="Tail Length for Bipedal Hoppers Across Experiments",x="Experiment",y="Tail Length") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0, max=2.55) +
#	geom_boxplot() +
	geom_point(aes(color=Exp, shape=Exp), size=3) +
	geom_point(data=mean_tail_len, aes(x=Exp, y=mean), size=4) +
#	stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tail_len_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot oscillation frequency for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Osc_Freq,fill=Exp)) +
	labs(title="Osc. Freq. for Bipedal Hoppers Across Experiments",x="Experiment",y="Oscillation Frequency") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=2) +
	geom_point(aes(color=Exp, shape=Exp), size=3) +
	geom_point(data=mean_osc_freq, aes(x=Exp, y=mean), size=4) +
	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"osc_freq_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot Distance Traveled for the different bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Exp,y=Dist_Traveled,fill=Exp)) +
	labs(title="Distance Traveled for Bipedal Hoppers Across Experiments",x="Experiment",y="Distance Traveled") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=90) +
	geom_point(aes(color=Exp, shape=Exp), size=3) +
	geom_point(data=mean_dist_traveled, aes(x=Exp, y=mean), size=4) +
	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"dist_traveled_across_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot tail length versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Tail_Len,y=Dist_Traveled,color=Exp,shape=Exp)) +
	labs(title="Tail Length versus Distance Traveled",x="Tail Length",y="Distance Traveled") +
	ylim(min=0,max=90) +
	xlim(min=0,max=2.55) +
	geom_point(size=2)
plt

save_plot(write_dest,"dist_traveled_versus_tail_length_exp_bipedal_hoppers",plt)

#######################################################################################################################
# Plot oscillation frequency versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Osc_Freq,y=Dist_Traveled,color=Exp)) +
	labs(title="Oscillation Frequency versus Distance Traveled",x="Oscillation Frequency",y="Distance Traveled") +
	ylim(min=0,max=90) +
	xlim(min=0,max=2) +
	geom_point() +
	facet_wrap(~ Exp,ncol=3)
plt

#######################################################################################################################
# Plot Rear Hip Offset versus Tail Offset for bipedal hoppers.
plt <- ggplot(bipedal_hopper_data,aes(x=Rear_Hip_Offset,y=Tail_Offset,color=Exp)) +
	labs(title="Rear Hip versus Tail Offset",x="Rear Hip Offset",y="Tail Offset") +
	ylim(min=0,max=2) +
	xlim(min=0,max=2) +
	geom_point(aes(size=Dist_Traveled)) +
	facet_wrap(~ Exp,ncol=3) +
	coord_fixed()
plt

save_plot(write_dest,"hip_vs_tail_offset_across_exp_bipedal_hoppers",plt)

plt <- ggplot(all_data,aes(x=Rear_Hip_Offset,y=Tail_Offset,color=Behavior)) +
	labs(title="Rear Hip versus Tail Offset",x="Rear Hip Offset",y="Tail Offset") +
	ylim(min=0,max=2) +
	xlim(min=0,max=2) +
	geom_point(aes(size=Dist_Traveled)) +
	facet_wrap(~ Exp,ncol=3) +
	coord_fixed()
plt

save_plot(write_dest,"hip_vs_tail_offset",plt)

#######################################################################################################################
# Plot Phase Difference versus Distance Traveled.
plt <- ggplot(bipedal_hopper_data,aes(x=Phase_Diff_Hips_vs_Tail,y=Dist_Traveled,color=Exp)) +
	ylim(min=0,max=120) +
	xlim(min=-0.5,max=0.5) +
	geom_point() +
	facet_wrap(~ Exp,ncol=3)
plt

plt <- ggplot(all_data,aes(x=Phase_Diff_Hips_vs_Tail,y=Dist_Traveled,color=Behavior)) +
	ylim(min=0,max=120) +
	xlim(min=-1.0,max=1.0) +
	geom_point() +
	facet_wrap(~ Exp,ncol=3)
plt

subset(bipedal_hopper_data, Exp == "9.25% TBM")$Dist_Traveled
subset(bipedal_hopper_data, Exp == "9.25% TBM")[which.max(subset(bipedal_hopper_data, Exp == "9.25% TBM")$Dist_Traveled),]

subset(bipedal_hopper_data, Exp == "2.48% TBM")

#######################################################################################################################

