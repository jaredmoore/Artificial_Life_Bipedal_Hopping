# Compare the different parameters and performance across the treatments where tail mass is fixed at various values.

collect_fit_data <- function() {
	
}

#############################################################################################################################
# Read in the data

base_path <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/"
sec_path <- c(
	"evo_tail_fixed_mass_straight_actuated_tail_0_3_per_seg_mass/", # 1
	"evo_tail_fixed_mass_straight_actuated_tail_0_6_per_seg_mass/", # 2
	"evo_tail_fixed_mass_straight_actuated_tail_0_15_per_seg_mass/" # 3
	)
plot_titles <- c(
	"Evolved Straight Actuated Tail 0.1 Mass Per Tail Seg",
	"Evolved Straight Actuated Tail 0.2 Mass Per Tail Seg",
	"Evolved Straight Actuated Tail 0.05 Mass Per Tail Seg"
	)
data_file <- "best_individuals_fit_data.dat"


data_3_per_seg <- read.csv(file=paste(base_path,sec_path[1],data_file,sep=""),head=TRUE,sep=",",comment.char="#")
data_3_per_seg$Exp <- as.factor("0.3 Tail Mass")
summary(data_3_per_seg)

data_6_per_seg <- read.csv(file=paste(base_path,sec_path[2],data_file,sep=""),head=TRUE,sep=",",comment.char="#")
data_6_per_seg$Exp <- as.factor("0.6 Tail Mass")
summary(data_6_per_seg)

data_15_per_seg <- read.csv(file=paste(base_path,sec_path[3],data_file,sep=""),head=TRUE,sep=",",comment.char="#")
data_15_per_seg$Exp <- as.factor("0.15 Tail Mass")
summary(data_15_per_seg)

all_data <- rbind(data_3_per_seg, data_6_per_seg)
all_data <- rbind(all_data, data_15_per_seg)
summary(all_data)

# Add aggregate data columns.
all_data$Tail_Len <- all_data$TS1_Len + all_data$TS2_Len + all_data$TS3_Len
all_data$COM_Avg <- ((1.0*all_data$Rear_Torso_Mass/2.0)+(2.0*all_data$Mid_Torso_Mass/2.0)+(3.0*all_data$Front_Torso_Mass/2.0))/3.0

#############################################################################################################################

# Plot fitness versus experiment
wilcox.test(subset(all_data, Exp=="0.3 Tail Mass")$Fit_1,subset(all_data, Exp=="0.6 Tail Mass")$Fit_1)
wilcox.test(subset(all_data, Exp=="0.3 Tail Mass")$Fit_1,subset(all_data, Exp=="0.15 Tail Mass")$Fit_1)
wilcox.test(subset(all_data, Exp=="0.6 Tail Mass")$Fit_1,subset(all_data, Exp=="0.15 Tail Mass")$Fit_1)

plt <- ggplot(all_data, aes(x=Exp,y=Fit_1,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Distance Traveled versus Tail Mass",x="Experiment",y="Distance Traveled") +
	theme(legend.position="None") +
	ylim(min=0,max=85) +
#	ylim(min=0,max=90) +
	geom_boxplot(notch=TRUE) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_fitness_boxplot",plt)

# Plot the Tail Length versus Performance Grouped by Trial
plt <- ggplot(all_data, aes(x=Tail_Len,y=Fit_1,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Tail Length versus Fitness",x="Tail Length",y="Distance Traveled") +
	theme(legend.position=c(0.85,0.15)) +
	xlim(min=0,max=2.5) +
	ylim(min=0,max=90) +
	geom_point(size=4) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_tail_length_versus_fitness",plt)

wilcox.test(subset(all_data, Exp=="0.3 Tail Mass")$Tail_Len,subset(all_data, Exp=="0.6 Tail Mass")$Tail_Len)
wilcox.test(subset(all_data, Exp=="0.3 Tail Mass")$Tail_Len,subset(all_data, Exp=="0.15 Tail Mass")$Tail_Len)
wilcox.test(subset(all_data, Exp=="0.6 Tail Mass")$Tail_Len,subset(all_data, Exp=="0.15 Tail Mass")$Tail_Len)

plt <- ggplot(all_data, aes(x=Exp,y=Tail_Len,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Tail Length",x="Experiment",y="Tail Length") +
	theme(legend.position="None") +
	ylim(min=0,max=2.5) +
#	ylim(min=0,max=90) +
	geom_boxplot(notch=TRUE) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_tail_length_boxplot",plt)

# Plot the tail length versus oscillation frequency Grouped by Trial
plt <- ggplot(all_data, aes(x=Tail_Len,y=Osc_Freq,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Tail Length versus Oscillation Frequency",x="Tail Length",y="Oscillation Frequency") +
	theme(legend.position=c(0.85,0.15)) +
	xlim(min=0,max=2.5) +
	ylim(min=0,max=2.5) +
	geom_point(aes(size=Fit_1)) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_tail_length_versus_osc_freq",plt)

# Plot the rear torso mass versus fitness
plt <- ggplot(all_data, aes(x=Rear_Torso_Mass,y=Fit_1,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Rear Torso Mass versus Fitness",x="Rear Torso Mass",y="Distance Traveled") +
	theme(legend.position=c(0.85,0.15)) +
	xlim(min=0,max=1.2) +
	ylim(min=0,max=90) +
	geom_point(size=4) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_rear_torso_mass_versus_fitness",plt)

# Plot the COM_Avg versus fitness
# Plot the rear torso mass versus fitness
plt <- ggplot(all_data, aes(x=COM_Avg,y=Fit_1,group=Exp,fill=Exp,shape=Exp)) +
	labs(title="Avg COM X Position versus Fitness",x="COM Avg Position",y="Distance Traveled") +
	theme(legend.position=c(0.85,0.15)) +
	xlim(min=0.5,max=3.0) +
	ylim(min=0,max=90) +
	geom_point(size=4) +
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7]))	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6],cbPalette[7])) +	
	scale_shape_manual(values=c(21,22,23))
plt

save_plot(paste(base_path,"comparisons/fixed_tail_mass_comparisons/",sep=""),"fixed_mass_com_x_position_versus_fitness",plt)