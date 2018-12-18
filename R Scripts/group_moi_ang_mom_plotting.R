# Plot the MOI and Angular Momentum data of the entire group over time.

# Constants for writing out data.
write_dest <- "/Volumes/Public/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/best_individual_analysis/"

# Read in the data file.
base_path <- "/Volumes/Public/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/"

data_file <- "all_best_individuals_ang_mom_moi_data.dat"

all_data <- read.csv(paste(base_path,data_file,sep=""),head=TRUE,sep=",")
# Remove curved results for now.
all_data$Exp <- factor(all_data$Exp, levels = c("2.48% TBM","4.85% TBM","9.25% TBM","9.25% TBMC","16.93% TBM","16.93% TBMC","23.41% TBM","23.41% TBMC","37.94% TBM")) 
all_data$Dist_Traveled <- all_data$Dist_Traveled/1.373
all_data <- subset(all_data, ! Exp %in% c("9.25% TBMC", "16.93% TBMC", "23.41% TBMC"))
summary(all_data)

bipedal_data <- subset(all_data, Behavior == "Bipedal Hopping")
summary(bipedal_data)

bipedal_data

# Remove Other and Unsteady Bipedal Hopping
all_data <- subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding")

# Rename experiments from TBM to Mass
legend_limits=c("2.48% TBM","4.85% TBM","9.25% TBM","16.93% TBM","23.41% TBM","37.94% TBM")
legend_labels=c("FM-0.075","FM-0.15","FM-0.3","FM-0.6","FM-0.9","FM-1.8")
legend_labels_2=c("FM-2.5%","FM-4.9%","FM-9.3%","FM-17%","FM-23.5%","FM-38.1%")

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

#subset(all_data, Exp == "9.25% TBM" & Trial == 13)

######################################################################################################

# Plot the torso/tail angular momentum versus distance traveled colored by behavior.
plt <- ggplot(all_data, aes(x=no_transient_torso_tail_ang_mom,y=Dist_Traveled,color=Behavior,shape=Exp)) +
	labs(title="Torso-Tail Ang. Mom. versus Performance", x="Torso-Tail Total Ang. Mom.", y="Distance Traveled") +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	xlim(min=-2500,max=2500) +
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_torso_tail_ang_mom_vs_dist_traveled",plt)

######################################################################################################

# Plot the torso/tail angular momentum versus distance traveled colored by behavior.
plt <- ggplot(all_data, aes(x=no_transient_tail_rear_legs_ang_mom,y=Dist_Traveled,color=Behavior,shape=Exp)) +
	labs(title="Tail-Rear Legs Ang. Mom. versus Performance", x="Tail-Rear Legs Summed Ang. Mom.", y="Distance Traveled") +
#	xlim(min=-2500,max=2500) +
	geom_point() +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2])) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels)
plt

save_plot(write_dest,"no_transient_tail_rear_legs_ang_mom_vs_dist_traveled",plt)

######################################################################################################

# Plot the torso/tail angular momentum versus distance traveled colored by behavior.
plt <- ggplot(all_data, aes(x=no_transient_all_ang_mom,y=Dist_Traveled,color=Behavior,shape=Exp)) +
	labs(title="Ang. Mom. versus Performance", x="Total Angular Momentum", y="Distance Traveled") +
#	xlim(min=-2500,max=2500) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_all_ang_mom_vs_dist_traveled",plt)

######################################################################################################

# Plot the angular momentum versus behavior
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_all_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

# Plot the flight angular momentum versus behavior.
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_flight_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

# Plot the ground contact angular momentum versus behavior.
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_ground_contact_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

# Plot the ground contact angular momentum versus flight angular momentum grouped by behavior.
plt <- ggplot(all_data, aes(x=no_transient_flight_ang_mom,y=no_transient_ground_contact_ang_mom,color=Behavior,fill=Behavior,shape=Exp)) +
	labs(title="Flight Phase vs. Ground Phase Ang. Mom.", x="Flight Phase Angular Momentum", y="Ground Phase Angular Momentum") +
	theme(legend.key=element_rect(color=NA)) +
	guides(color = guide_legend(override.aes = list(shape = 22, size=6))) +
	geom_segment(aes(x=-15000,y=15000,xend=15000,yend=-15000),color="black",alpha=0.5,linetype=2) +
	geom_point(size=3) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2]),name="Behavior (Color)") +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_flight_vs_ground_ang_mom",plt)

######################################################################################################

# Plot the torso/tail angular momentum versus behavior.
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_torso_tail_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

# Plot the torso/tail/rear_legs angular momentum versus behavior.
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_torso_tail_rear_legs_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

# Plot the torso/tail angular momentum versus behavior.
plt <- ggplot(all_data, aes(x=Behavior,y=no_transient_torso_tail_ang_mom)) +
	geom_boxplot()
plt

######################################################################################################

plt <- ggplot(all_data, aes(x=no_transient_ground_contact_time,y=Dist_Traveled)) +
	ylim(min=0,max=90) +
	xlim(min=0,max=10) +
	geom_point() +
	facet_wrap(~ Behavior, ncol=2)
plt

######################################################################################################

plt <- ggplot(all_data, aes(x=no_transient_leg_taps,y=no_transient_tail_taps)) +
	ylim(min=0,max=50) +
	xlim(min=0,max=400) +
	geom_point(aes(size=Dist_Traveled)) +
	facet_wrap(~ Behavior, ncol=2)
plt

######################################################################################################

plt <- ggplot(all_data, aes(x=no_transient_tail_taps,y=Dist_Traveled,color=Exp,shape=Exp)) +
	labs(title="Tail Taps Versus Dist Traveled",x="# Tail Taps",y="Distance Traveled") +
	ylim(min=0,max=100) +
	xlim(min=0,max=100) +
	geom_point() +
	facet_wrap(~ Behavior, ncol=2) + 
	scale_color_discrete(limits=legend_limits,labels=legend_labels) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels)	
plt

save_plot(write_dest,"tail_taps_vs_dist_traveled",plt)

######################################################################################################

# Plot the torso/tail angular momentum versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_data, aes(x=no_transient_torso_tail_ang_mom,y=Dist_Traveled)) +
	geom_point()
plt

plt <- ggplot(bipedal_data, aes(x=no_transient_torso_tail_rear_legs_ang_mom,y=Dist_Traveled)) +
	geom_point()
plt

######################################################################################################

# Plot the ground_contact_time versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_data, aes(x=no_transient_ground_contact_time,y=Dist_Traveled)) +
	ylim(min=0,max=90) +
	xlim(min=0,max=10) +
	geom_point()
plt

######################################################################################################

# Plot the number of tail taps versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_data, aes(x=no_transient_tail_taps,y=Dist_Traveled)) +
	ylim(min=0,max=90) +
#	xlim(min=0,max=10) +
	geom_point()
plt

######################################################################################################

# Plot the number of leg taps versus distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_data, aes(x=no_transient_leg_taps,y=Dist_Traveled)) +
	ylim(min=0,max=90) +
#	xlim(min=0,max=10) +
	geom_point()
plt

######################################################################################################

# Plot the number of leg taps versus tail taps and point size as distance traveled for bipedal hoppers.
plt <- ggplot(bipedal_data, aes(x=no_transient_leg_taps,y=no_transient_tail_taps)) +
	ylim(min=0,max=30) +
	xlim(min=0,max=300) +
	geom_point(aes(size=Dist_Traveled))
plt

######################################################################################################

# Plot the phase offset between torso and tail.

abs((subset(all_data,Behavior == "Bipedal Hopping" | Behavior == "Bounding"))$no_transient_torso_tail_phase_diff)

plt <- ggplot(subset(all_data,Behavior == "Bipedal Hopping" | Behavior == "Bounding"), 	
	aes(x=abs(no_transient_torso_tail_phase_diff),y=Dist_Traveled,group=Behavior,color=Behavior,shape=Exp,fill=Behavior)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	labs(title="Torso-Tail Phase Difference",x="Phase Difference",y="Distance Traveled") +
	ylim(min=0,max=65) +
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +	
	scale_x_continuous(labels = c(expression(-pi),expression(-pi*"/2"),0,expression(pi*"/2"),expression(pi)), breaks = c(-pi,-pi/2,0,pi/2,pi)) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_torso_tail_phase_diff_vs_performance",plt)

# Plot the phase offset between torso and rear Legs.
plt <- ggplot(subset(all_data,Behavior == "Bipedal Hopping" | Behavior == "Bounding"), 	
	aes(x=abs(no_transient_torso_rear_legs_phase_diff),y=Dist_Traveled,group=Behavior,color=Behavior,shape=Exp,fill=Behavior)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	labs(title="Torso-Rear Legs Phase Difference",x="Phase Difference",y="Distance Traveled") +
	ylim(min=0,max=65) +
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +	
	scale_x_continuous(labels = c(expression(-pi),expression(-pi*"/2"),0,expression(pi*"/2"),expression(pi)), breaks = c(-pi,-pi/2,0,pi/2,pi)) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_torso_rear_legs_phase_diff_vs_performance",plt)

# Plot the phase offset between tail and rear Legs.
plt <- ggplot(subset(all_data,Behavior == "Bipedal Hopping" | Behavior == "Bounding"), 	
	aes(x=abs(no_transient_tail_rear_legs_phase_diff),y=Dist_Traveled,group=Behavior,color=Behavior,shape=Exp,fill=Behavior)) +
	theme(legend.key=element_rect(color=NA)) +
	guides(fill = guide_legend(override.aes = list(shape = 22, size=5))) +
	labs(title="Tail-Rear Legs Phase Difference",x="Phase Difference",y="Distance Traveled") +
	ylim(min=0,max=65) +
	geom_point(size=2) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4]),name="Behavior (Color)") +	
	scale_x_continuous(labels = c(expression(-pi),expression(-pi*"/2"),0,expression(pi*"/2"),expression(pi)), breaks = c(-pi,-pi/2,0,pi/2,pi)) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels,name="Treatment (Symbol)")
plt

save_plot(write_dest,"no_transient_tail_rear_legs_phase_diff_vs_performance",plt)

######################################################################################################

# Read in the genome data for comparing phase and torso mass.

data_file <- "all_best_individuals_unified_data.dat"

genetic_data <- read.csv(paste(base_path,data_file,sep=""),head=TRUE,sep=",")
genetic_data$Exp <- factor(genetic_data$Exp, levels = c("2.48% TBM","4.85% TBM","9.25% TBM","9.25% TBMC","16.93% TBM","16.93% TBMC","23.41% TBM","23.41% TBMC","37.94% TBM")) 
# Remove curved results for now.
genetic_data <- subset(genetic_data, ! Exp %in% c("9.25% TBMC", "16.93% TBMC", "23.41% TBMC"))
genetic_data$Torso_Mass <- genetic_data$Rear_Torso_Mass + genetic_data$Mid_Torso_Mass + genetic_data$Front_Torso_Mass

# Drop the Description column
genetic_data <- genetic_data[,!(names(genetic_data) %in% c("Description"))]
summary(genetic_data)

every_data <- merge(all_data,genetic_data)
summary(every_data)

######################################################################################################

plt <- ggplot(subset(every_data,Behavior == "Bipedal Hopping" | Behavior == "Bounding"), 	
	aes(x=abs(no_transient_torso_tail_phase_diff),y=Torso_Mass,group=Behavior,color=Behavior,shape=Exp,fill=Behavior)) +
	labs(title="Torso-Tail Phase Difference vs Torso Mass",x="Phase Difference",y="Torso Mass") +
	ylim(min=0,max=4) +
	geom_point(size=3) +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4])) +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2],cbPalette[3],cbPalette[4])) +	
	scale_x_continuous(labels = c(expression(-pi),expression(-pi*"/2"),0,expression(pi*"/2"),expression(pi)), breaks = c(-pi,-pi/2,0,pi/2,pi)) +
	scale_shape_discrete(limits=legend_limits,labels=legend_labels)
plt

save_plot(write_dest,"no_transient_torso_tail_phase_diff_vs_torso_mass",plt)