# Plot the normalized genetic variance information.
save_plot_custom <- function (filepath, filename, data_plot, w, h) 
{
    png(paste(filepath, filename, ".png", sep = ""), width = w, 
        height = h, units = "in", res = 300)
    print(data_plot)
    dev.off()
    pdf(paste(filepath, filename, ".pdf", sep = ""), width = w, 
        height = h)
    print(data_plot)
    dev.off()
}


base_path <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/"
write_dest <- base_path

# Read in the data file.
all_data <- read.csv(paste(base_path,"aggregated_max_fitness_genomic_distances_time_normalized.dat",sep=""))

# Remove Other and Unsteady Bipedal Hopping Data
all_data <- subset(all_data, Behavior != "Other" & Behavior != "Unsteady Bipedal Hopping")

summary(all_data)

# Categorize by normalized age.
all_data$Cat <- cut(all_data$Norm_Age, breaks=20)

# Categorize by generation.
all_data$Cat <- cut(all_data$Gen, breaks=40)

################################################################################################################################################################

# Create boxplots of the data grouped by the normalization score.
filtered_cols <- c("Rear_Torso_Mass","Mid_Torso_Mass","Front_Torso_Mass","Osc_Freq","RRL_Off","LRL_Off","RFL_Off",
	"LFL_Off","RH_Off","RK_Off","RA_Off","RT_Off","FH_Off","FK_Off","RH_MF_2","RK_MF_2","RA_MF_2",
	"RT_MF_2","FH_MF_2","FK_MF_2","T_MF_2","Max_Joint_Vel","T_Off","TB_Off","Tail_Len")

# for (i in filtered_cols) {
	# plt <- ggplot(all_data, aes_string(x="Cat",y=i,color="Behavior",fill="Behavior")) +
		# ylim(min=0,max=1) +
		# geom_boxplot()
	
	# save_plot("/Users/JMoore/Desktop/test_plots/",paste("boxplot_",i,sep=""),plt)
# }

plt <- ggplot(all_data, aes(x=Cat,y=Rear_Torso_Mass,color=Behavior)) +
	ylim(min=0,max=1) +
	geom_boxplot()
plt

################################################################################################################################################################


bipedal_hopping_data <- subset(all_data, Behavior == "Bipedal Hopping")

bipedal_hopper_cat_data <- data.frame("norm_age_cat" = unique(all_data$Cat))

for (i in names(all_data)[7:45]) {
	bipedal_hopper_cat_data[i] <- aggregate(bipedal_hopping_data[,i], list(bipedal_hopping_data$Cat),mean)$x
}
bipedal_hopper_cat_data$Behavior <- "Bipedal Hopping"

summary(bipedal_hopper_cat_data)

bounding_data <- subset(all_data, Behavior == "Bounding")

bounding_cat_data <- data.frame("norm_age_cat" = unique(all_data$Cat))

for (i in names(all_data)[7:45]) {
	bounding_cat_data[i] <- aggregate(bounding_data[,i], list(bounding_data$Cat),mean)$x
}
bounding_cat_data$Behavior <- "Bounding"

summary(bounding_cat_data)

all_cat_data <- rbind(bipedal_hopper_cat_data, bounding_cat_data)
summary(all_cat_data)

################################################################################################################################################################

# Create a heatmap showing the progpression over time for three variables. 
library(plyr)
library(reshape2)
library(scales)

behavior <- "Bipedal Hopping"

sub_filtered_cols <- filtered_cols[1:3]
sub_filtered_cols[length(sub_filtered_cols)+1] <- "norm_age_cat"
sub_filtered_cols

all_cat_data.m <- melt(subset(subset(all_cat_data,Behavior == behavior)[,sub_filtered_cols]))
summary(all_cat_data.m)

# Setup the y-axis labeling.
cats <- unique(all_cat_data.m$norm_age_cat)
#labs <- seq(from=0,to=2000,by=(2000/length(cats)))
labs <- seq(from=0, to=1, by=(1/length(cats)))
labs

all_cat_data.m$norm_age_cat <- as.double(all_cat_data.m$norm_age_cat)

plt <- ggplot(all_cat_data.m, aes(variable, norm_age_cat)) + 
	labs(title=paste("Genetic Distance for ",behavior," Max Individuals",sep=""),x="Gene",y="Individual") +
	theme(#axis.text.y=element_blank(),
		axis.text.x = element_text(angle = 45, hjust = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		#axis.ticks.y = element_blank()
		legend.position="bottom"
		) +
	geom_tile(aes(fill = value),colour = "white") + 
	scale_fill_gradient(low = "white",high = "steelblue",limits=c(0,0.5)) +
	scale_y_continuous(labels=c("0.0","0.25","0.50","0.75","1.0"),breaks=seq(0,40,10))
plt

write_dest

save_plot_custom(write_dest,paste(behavior,"_gen_dist_for_max_individuals_three_columns",sep=""),plt,7,10)

################################################################################################################################################################

# Create a heatmap showing the progpression over time. 
library(plyr)
library(reshape2)
library(scales)

behavior <- "Bounding"

filtered_cols[length(filtered_cols)+1] <- "norm_age_cat"
filtered_cols

all_cat_data.m <- melt(subset(subset(all_cat_data,Behavior == behavior)[,filtered_cols]))
summary(all_cat_data.m)

# Setup the y-axis labeling.
cats <- unique(all_cat_data.m$norm_age_cat)
#labs <- seq(from=0,to=2000,by=(2000/length(cats)))
labs <- seq(from=0, to=1, by=(1/length(cats)))
labs

all_cat_data.m$norm_age_cat <- as.double(all_cat_data.m$norm_age_cat)

plt <- ggplot(all_cat_data.m, aes(variable, norm_age_cat)) + 
	labs(title=paste("Genetic Distance for ",behavior," Max Individuals",sep=""),x="Gene",y="Individual") +
	theme(#axis.text.y=element_blank(),
		axis.text.x = element_text(angle = 45, hjust = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		#axis.ticks.y = element_blank()
		legend.position="bottom"
		) +
	geom_tile(aes(fill = value),colour = "white") + 
	scale_fill_gradient(low = "white",high = "steelblue",limits=c(0,0.5)) +
	scale_y_continuous(labels=c("0.0","0.25","0.50","0.75","1.0"),breaks=seq(0,40,10))
plt

save_plot(write_dest,paste(behavior,"_gen_dist_for_max_individuals",sep=""),plt)

################################################################################################################################################################

names(all_data)[7:45]

par(ask = TRUE)
for (i in names(all_data)[7:45]) {
	plt <- ggplot(all_cat_data, aes_string(x="norm_age_cat",y=i,color="Behavior",group="Behavior")) +
		ylim(min=0,max=1) +
		geom_line()
	plt	
	
	save_plot("/Users/JMoore/Desktop/test_plots/",i,plt)
}

names(all_cat_data)[7][1]

plt <- ggplot(all_cat_data, aes_string(y="Rear_Torso_Mass",x="norm_age_cat",color="Behavior",group="Behavior")) +
	ylim(min=0,max=1) +
	geom_line()
plt	


# Plot the categorical data for oscillation frequency over time.
plt <- ggplot(all_cat_data, aes(x=norm_age_cat,y=Rear_Torso_Mass,color=Behavior,group=Behavior)) +
	ylim(min=0,max=1) +
	geom_line()
plt

################################################################################################################################################################

# Plot the fixation of oscillation frequency over time.
plt <- ggplot(all_data, aes(x=Norm_Age,y=Osc_Freq,group=interaction(Exp,Trial),color=Behavior)) +
	geom_line()
plt

plt <- ggplot(all_data, aes(x=Norm_Age,y=Osc_Freq,group=Behavior,color=Behavior)) +
	geom_histogram()
plt