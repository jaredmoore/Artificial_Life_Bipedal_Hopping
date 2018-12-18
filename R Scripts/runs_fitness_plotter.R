# Plotting function
save_plot <- function(filepath,filename,data_plot) {
	png(paste(filepath,filename,".png",sep=""), width=7, height=5.25, units="in", res=300)
	print(data_plot)
	dev.off()
	
	pdf(paste(filepath,filename,".pdf",sep=""), width=7, height=5.25)
	print(data_plot)
	dev.off()
}

#############################################################################################################################
# Python Implementation

# Using the compiled_fitness_stats.dat files.
base_path <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/"
sec_path <- c("straight_tail_hopper/","evo_straight_actuated_tail/","straight_angled_tail_hopper/","evo_angled_straight_actuated_tail/",
	"evo_tail_fixed_mass_straight_actuated_tail_0_3_per_seg_mass/","evo_tail_fixed_mass_straight_actuated_tail_0_6_per_seg_mass/",
	"evo_tail_fixed_mass_straight_actuated_tail_0_15_per_seg_mass/","evo_angled_curved_actuated_tail/")
plot_titles <- c("Straight Tail Hopper","Evolved Straight Actuated Tail","Straight Angled Tail Hopper","Evolved Angled Straight Actuated Tail",
	"Evolved Straight Actuated Tail 0.1 Mass Per Tail Seg","Evolved Straight Actuated Tail 0.2 Mass Per Tail Seg",
	"Evolved Straight Actuated Tail 0.05 Mass Per Tail Seg","Evolved Angled Curved Actuated Tail")
treatment_num <- 8
data_file <- "compiled_stats_data.dat"

input_file <- paste(base_path,sec_path[treatment_num],data_file,sep="")

input_file

all_data <- read.csv(file = input_file,head=TRUE, sep=",",comment.char="#")
all_data <- na.omit(all_data)
all_data$Generation <- as.integer(all_data$Generation)
summary(all_data)

# Subset the data every 50 generations (smoothing)
plot_data <- subset(all_data, Generation %% 50 == 0 | Generation == 999 | Generation == 0)
points_data <- subset(all_data, Generation %% 200 == 0 | Generation == 999 | Generation == 0)

summary(plot_data)

# Plot the average of the fitnesses
data_plot <- ggplot(data=plot_data, aes(x=Generation)) +
	labs(title=plot_titles[treatment_num], x="Generation", y="Fitness") +
	xlim(min=0,max=2000) +
	ylim(min=0,max=100) +
	theme(legend.position=c(.85,.15)) +
	geom_ribbon(aes(x=Generation,ymin=Max_Fitness_Low_CI,ymax=Max_Fitness_Upp_CI),alpha=0.4) +
	geom_ribbon(aes(x=Generation,ymin=Avg_Avg_Fitness_Low_CI,ymax=Avg_Avg_Fitness_Upp_CI),alpha=0.4) +
	geom_line(aes(y=Avg_Max_Fitness,color="Max")) +
  	geom_line(aes(y=Avg_Avg_Fitness,color="Mean")) +
  	geom_point(data=points_data,aes(y=Avg_Max_Fitness,fill="Max",shape="Max"),size=3) +
  	geom_point(data=points_data,aes(y=Avg_Avg_Fitness,fill="Mean",shape="Mean"),size=3) +  	
	scale_fill_manual(values=c(cbPalette[5],cbPalette[6]), name="Fitness")	+
	scale_color_manual(values=c(cbPalette[5],cbPalette[6]), name="Fitness")	+	
	scale_shape_manual(values=c(21,22), name="Fitness")
data_plot

base_path

save_plot(paste(base_path,sec_path[treatment_num],sep=""),"all_trials_combined_fitness_plot",data_plot)

#############################################################################################################################