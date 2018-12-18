# This script is intended to look at the profiles of the best individuals and present a statistical breakdown of the various parameters
# and performance for the evolvable mass and tail length runs.

library(plyr)

# Constants for writing out data.
write_dest <- "/Volumes/Public/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/best_individual_evo_tail_analysis/"

base_path <- "/Volumes/Public/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/"

exp_num <- 2
experiments <- c("evo_tail_evo_mass_straight_actuated_tail_4000_gens",
	"evo_tail_evo_mass_straight_actuated_tail_low_lim_0_15")
exp_name <- c("Evo Len/Mass",
	"Evo Len/Mass Low Lim 0.015")

all_data <- data.frame()

for (i in 1:2) {
	# Read in the data from the file.
	fit_data <- read.csv(paste(base_path,experiments[i],"/best_individuals_fit_data.dat",sep=""),header=TRUE,sep=",")
	behavior_data <- read.csv(paste(base_path,"experiment_overview_best_individuals_evo_mass_evo_length.csv",sep=""),header=TRUE,sep=",")
	behavior_data <- subset(behavior_data, Exp == exp_name[i])
	
	tmp_data <- merge(fit_data, behavior_data)
	all_data <- rbind(all_data, tmp_data)
}

all_data$Behavior <- as.factor(all_data$Behavior)
all_data$Exp <- as.factor(all_data$Exp)
all_data$Tail_Len <- (all_data$TS1_Len * 3)/1.373
all_data$Tail_Mass <- all_data$TS1_Mass + all_data$TS2_Mass + all_data$TS3_Mass
all_data$Total_Mass <- all_data$Rear_Torso_Mass + all_data$Mid_Torso_Mass + all_data$Front_Torso_Mass + 0.5 # Head mass = 0.5
all_data$TBM_Tail_Mass <- all_data$Tail_Mass/(all_data$Tail_Mass + all_data$Total_Mass + 0.6028) # Remaining mass in legs is .6028
all_data$Tail_Mass_Dist <- ((all_data$TS3_Mass/all_data$Tail_Mass)*1.0 + (all_data$TS2_Mass/all_data$Tail_Mass)*2.0 + (all_data$TS1_Mass/all_data$Tail_Mass)*3.0)/3.0
avg_tail_mass <- all_data$Tail_Mass/3.0
all_data$Tail_Var <- ((all_data$TS1_Mass - avg_tail_mass)**2 + (all_data$TS2_Mass - avg_tail_mass)**2 + (all_data$TS3_Mass - avg_tail_mass)**2)/3
abs(all_data$TS1_Mass - (all_data$Tail_Mass/3.0)) + abs(all_data$TS2_Mass - (all_data$Tail_Mass/3.0)) + abs(all_data$TS3_Mass - (all_data$Tail_Mass/3.0))
all_data$Mass_Dist <- (all_data$Rear_Torso_Mass/all_data$Total_Mass*1.0 + all_data$Mid_Torso_Mass/all_data$Total_Mass*2.0 + all_data$Front_Torso_Mass/all_data$Total_Mass*3.0 + 0.5/all_data$Total_Mass*4.0)/4.0
all_data$Tail_Offset <- all_data$T_Off + all_data$TB_Off
all_data$Rear_Hip_Offset <- all_data$RH_Off + (all_data$RRL_Off+all_data$LRL_Off)/2.0
all_data$Phase_Diff_Hips_vs_Tail <- all_data$Rear_Hip_Offset - all_data$Tail_Offset
all_data$Dist_Traveled <- all_data$Dist_Traveled/1.373
summary(all_data)
	
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

# Fix treatment names for plotting.
all_data$Exp <- revalue(all_data$Exp, c("Evo Len/Mass"="ELM-3%", "Evo Len/Mass Low Lim 0.015"="ELM-13.3%"))

save_plot <- function(filepath,filename,data_plot) {
     png(paste(filepath,filename,".png",sep=""), width=5, height=3.75, units="in", res=300)
     print(data_plot)
     dev.off()

     pdf(paste(filepath,filename,".pdf",sep=""), width=5, height=3.75)
     print(data_plot)
     dev.off()
}

title.size<-12
theme_white <- function() {
   theme_update(
   plot.title = element_text(size=title.size,hjust = 0.5),
   panel.background = element_blank(),
   panel.grid.major = element_line(colour = '#00000022', size = .25, linetype = 'dashed'),
   axis.title.x = element_text(size=title.size),
   axis.title.y = element_text(size=title.size, angle=90),
   strip.background = element_rect(fill = 'white',colour='white'))
}
theme_set(theme_bw())
theme_white()

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
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=TBM_Tail_Mass,y=Dist_Traveled,color=Behavior,fill=Behavior,shape=Exp)) +
	labs(title="Tail Mass versus Performance",x="Tail Mass as % of Total Body Mass",y="Distance Traveled (Body Lengths)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	ylim(min=0, max=75) +
	xlim(min=0, max=0.2) +	
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +#+
	scale_shape_discrete(name="Treatment (Symbol)")#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tbm_tail_mass_vs_dist_traveled",plt)

max(all_data$Dist_Traveled)
#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Tail_Len,y=Dist_Traveled,shape=Exp,color=Behavior,fill=Behavior)) +
	labs(title="Tail Length versus Performance",x="Tail Length (Body Lengths)",y="Distance Traveled (Body Lengths)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	ylim(min=0, max=75) +
	xlim(min=0, max=2) +	
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +#+
	scale_shape_discrete(name="Treatment (Symbol)")
	#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tail_len_vs_dist_traveled",plt)

#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Tail_Len,y=TBM_Tail_Mass,shape=Exp,color=Behavior)) +
	labs(title="Tail Length versus Tail Mass",x="Tail Length",y="TBM Tail Mass") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0, max=0.25) +
	xlim(min=0, max=2.55) +	
	geom_point(aes(size=Dist_Traveled)) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]))#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tail_len_vs_tail_mass",plt)

#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Tail_Len,y=Tail_Mass_Dist,shape=Exp,color=Behavior,fill=Behavior)) +
	labs(title="Tail Length versus Tail Mass Distribution",x="Tail Length (Body Lengths)",y="Tail Mass Dist(0 - Rear, 1 - Fore)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=6)), shape = guide_legend(override.aes = list(size=3))) +
	ylim(min=0, max=1.0) +
	xlim(min=0, max=2) +	
	geom_point(aes(size=Dist_Traveled)) +
	scale_size_continuous(guide=FALSE) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +#+
	scale_shape_discrete(name="Treatment (Symbol)")#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"tail_len_vs_tail_mass_dist",plt)

#######################################################################################################################
# Plot the performance of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Total_Mass,y=Dist_Traveled,shape=Exp,color=Behavior)) +
	labs(title="Torso Mass versus Performance",x="Torso Mass",y="Distance Traveled") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#	ylim(min=0, max=0.25) +
	xlim(min=0, max=5) +	
	geom_point(size=3) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]))#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"torso_mass_vs_dist_traveled",plt)

#######################################################################################################################
# Plot the oscillation frequency of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Exp,y=Osc_Freq,fill=Behavior)) +
	labs(title="Behavior versus Oscillation Frequency",x="Experiment",y="Oscillation Frequency") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=2) +
	geom_boxplot()# +
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_osc_freq",plt)

#######################################################################################################################
# Plot the tail length of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Exp,y=Tail_Len,fill=Behavior)) +
	labs(title="Behavior versus Tail Length",x="Experiment",y="Tail Length") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	ylim(min=0,max=2.5) +
	geom_boxplot() #+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_tail_len",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Exp,y=Mass_Dist,fill=Behavior)) +
	labs(title="Behavior versus Mass Distribution",x="Experiment",y="Mass Dist(0 Rear, 1 Fore)") +
	ylim(min=0,max=1) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_boxplot() #+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_mass_dist",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Mass_Dist,y=Tail_Mass_Dist,color=Behavior,fill=Behavior,shape=Exp)) +
	labs(title="Torso versus Tail Mass Distribution",x="Torso Mass Dist (O Rear, 1 Fore)",y="Tail Mass Dist (0 Rear, 1 Fore)") +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=6)), shape = guide_legend(override.aes = list(size=3))) +
	xlim(min=0,max=1) +
	ylim(min=0,max=1) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_point(aes(size=Dist_Traveled)) +
	scale_size_continuous(guide=FALSE) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +#+
	scale_shape_discrete(name="Treatment (Symbol)")#+#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"torso_vs_tail_mass_dist",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Exp,y=Tail_Mass_Dist,fill=Behavior)) +
	labs(title="Behavior versus Mass Distribution",x="Experiment",y="Tail Mass Dist(0 Rear, 1 Fore)") +
	ylim(min=0,max=1) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_boxplot() +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]))#+#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_tail_mass_dist",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Exp,y=Tail_Var,fill=Behavior)) +
	labs(title="Behavior versus Tail Mass Variance",x="Experiment",y="Tail Mass Var(0 Avg)") +
	ylim(min=0,max=0.05) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_boxplot() +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]))#+
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_tail_mass_var",plt)

#######################################################################################################################
# Plot the mass distribution of the individuals versus Dist_Traveled separated by behavior and grouped by experiment.
plt <- ggplot(subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"),aes(x=Mass_Dist, y=Dist_Traveled,color=Exp,shape=Behavior)) +
	labs(title="Mass Distribution versus Performance",x="Mass Distribution(0 Rear, 1 Fore)",y="Distance Traveled") +
	xlim(min=0,max=1) +
	ylim(min=0,max=90) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	geom_point() +
	facet_wrap(~ Exp, ncol=2)
#	scale_x_discrete(limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM"))
plt

save_plot(write_dest,"behavior_vs_mass_dist",plt)

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
	geom_point(data=mean_tail_len, aes(x=Exp, y=mean), size=4)
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