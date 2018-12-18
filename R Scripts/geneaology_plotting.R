# Script to generate the geneaology plots from the hopping runs.

base_path = "/Users/JMoore/Desktop/"
gen_file = "99_geneaology_output.dat"
gen_paired_file = "99_paired_geneaology.dat"

base_path = "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/evo_tail_fixed_mass_straight_actuated_tail_0_3_total_mass/13/"
write_dest = paste(base_path,"best_individual_lineage/",sep="")
fit_file = "13_fitnesses.dat"
gen_file = "13_geneaology_output.dat"
gen_paired_file = "13_paired_geneaology.dat"
gen_dist_file = "13_geneaology_genomic_distances.dat"

fit_data <- read.csv(paste(base_path,fit_file,sep=""),head=TRUE,sep=",")
summary(fit_data)

geneaology_data <- read.csv(paste(base_path,gen_file,sep=""),head=TRUE,sep=",")
geneaology_data$Gen <- as.integer(geneaology_data$Gen)
summary(geneaology_data)

length(geneaology_data[,1])

(geneaology_data[order(-geneaology_data$Gen),])[1:50,]

paired_data <- read.csv(paste(base_path,gen_paired_file,sep=""),head=TRUE,sep=",")
paired_data$Ind_Gen <- as.integer(paired_data$Ind_Gen)
summary(paired_data)

gen_dist_data <- read.csv(paste(base_path,gen_dist_file,sep=""),head=TRUE,sep=",")
gen_dist_data$Tail_Len <- gen_dist_data$TS1_Len + gen_dist_data$TS2_Len + gen_dist_data$TS3_Len
summary(gen_dist_data)

geneaology_data[geneaology_data$Par_1 == 332243,]
geneaology_data[geneaology_data$Par_2 == 332243,]

plot(aggregate(Fitness ~ Gen, geneaology_data, mean))
plot(aggregate(Fitness ~ Gen, geneaology_data, max))

max_inds <- aggregate(Fitness ~ Gen, geneaology_data, max)
summary(max_inds)

last_index <- 1

for (i in 2:length(max_inds[,1])) {
	if (max_inds[i,2] <= max_inds[last_index,2]) {
		max_inds[i,1] <- NA
		max_inds[i,2] <- NA		
	} else {
		last_index <- i
	}
}
max_inds <- na.omit(max_inds)
max_inds[1:50,]

plot(max_inds)

max_inds

max_inds_data <- merge(geneaology_data,max_inds, by=c("Gen","Fitness"))
max_inds_data <- na.omit(max_inds_data)
summary(max_inds_data)
length(max_inds_data[,1])

# Get the geneaology_data for the max individuals.
max_inds_gen_dist_data <- subset(gen_dist_data, Ind_ID %in% max_inds_data$Individual)
summary(max_inds_gen_dist_data)

max_inds_data[1:69,]

length(max_inds_data[,1])

#################################################################################

# Plot the fitness and generation of the maximum individuals.
plt <- ggplot() +
	labs(title="Maximum Fitness Over Time",x="Generation",y="Distance Traveled") +
	geom_point(data=max_inds_data, aes(x=Gen,y=Fitness))
plt

save_plot(write_dest,"max_fit_over_time",plt)

#################################################################################

# Plot the fitness and generation of the maximum individuals and the population as a whole.
plt <- ggplot() +
	labs(title="Maximum Fitness Over Time",x="Generation",y="Distance Traveled") +
	geom_point(data=fit_data, aes(x=Gen,y=Fit_1)) +
	geom_point(data=max_inds_data, aes(x=Gen,y=Fitness), color="#00FFAA")
plt

save_plot(write_dest,"max_fit_over_time",plt)

#################################################################################

# Plot the parent and child data by generation.
plt <- ggplot() +
	labs(title="Geneaology",x="Generation",y="Distance Traveled") +
	xlim(min=0,max=2000) +
	ylim(min=-10,max=95) +
	geom_point(data=geneaology_data, aes(x=Gen,y=Fitness),size=1)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

save_plot(write_dest,"lineage",plt)

#################################################################################

# Plot the parent and child data by generation including line segements 
# connecting parents to child.
plt <- ggplot() +
	labs(title="Geneaology",x="Generation",y="Distance Traveled") +
	xlim(min=1850,max=1876) +
	ylim(min=-10,max=95) +
	geom_segment(data=paired_data, aes(x=Par_Gen,y=Par_Fit,xend=Ind_Gen,yend=Fit)) +
	geom_point(data=geneaology_data, aes(x=Gen,y=Fitness),color="#00FFFF",size=1)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

save_plot(write_dest,"parentage",plt)

#################################################################################

# Plot the genetic distance versus fitness over time.
plt <- ggplot() +
	labs(title="Genetic Distance over Time",x="Generation",y="Scaled Genetic Distance") +
	xlim(min=0,max=2000) +
	ylim(min=0,max=18) +
	geom_point(data=gen_dist_data, aes(x=Gen,y=Tot_Distance),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

save_plot(write_dest,"gen_dist_over_time",plt)

#################################################################################

# Plot the genetic distance versus fitness.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Scaled Genetic Distance",y="Distance Traveled") +
	xlim(min=0,max=18) +
	ylim(min=-10,max=95) +
	geom_point(data=gen_dist_data, aes(x=Tot_Distance,y=Fit_1),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

save_plot(write_dest,"gen_dist_versus_dist_traveled",plt)

#################################################################################

# Plot the genetic distance of the oscillation frequency versus fitness.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Scaled Genetic Distance",y="Distance Traveled") +
	xlim(min=0,max=1) +
	ylim(min=-10,max=95) +
	geom_point(data=gen_dist_data, aes(x=Osc_Freq,y=Fit_1),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

#################################################################################

# Plot the genetic distance of the tail length versus fitness.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Scaled Genetic Distance",y="Distance Traveled") +
	xlim(min=0,max=3) +
	ylim(min=-10,max=95) +
	geom_point(data=gen_dist_data, aes(x=Tail_Len,y=Fit_1),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

#################################################################################

# Plot the genetic distance of the tail length versus fitness.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Scaled Genetic Distance",y="Distance Traveled") +
	xlim(min=0,max=1) +
	ylim(min=-10,max=95) +
	geom_point(data=gen_dist_data, aes(x=Rear_Torso_Mass,y=Fit_1),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

#################################################################################

# Plot the genetic distance of the oscillation frequency over time.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Time",y="Scaled Genetic Distance") +
	xlim(min=0,max=2000) +
	ylim(min=0,max=1) +
	geom_point(data=gen_dist_data, aes(x=Gen,y=Osc_Freq),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

#################################################################################

# Plot the genetic distance of the tail length over time.
plt <- ggplot() +
	labs(title="Genetic Distance versus Performance",x="Time",y="Scaled Genetic Distance") +
	xlim(min=0,max=2000) +
	ylim(min=0,max=3) +
	geom_point(data=gen_dist_data, aes(x=Gen,y=Tail_Len),size=1,alpha=0.5)
#	geom_text(data=geneaology_data, aes(x=Gen,y=Fitness,label=Individual))
plt

#################################################################################

# Test for correlation on the entire data set.
all_cor <- cor(gen_dist_data)
summary(all_cor)
fit_cor_df <- data.frame(variable=names(gen_dist_data),values=all_cor[,4])
fit_cor_df$variable <- factor(fit_cor_df$variable, levels=names(gen_dist_data))

plt <- ggplot(subset(fit_cor_df, !variable %in% c("Gen","Ind","Ind_ID","Fit_1","Fit_2","Fit_3","Fit_4")), aes(x=variable,y=values)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	ylim(min=-1,max=1) +
	geom_point()
plt

lm.out <- lm(data=gen_dist_data, Fit_1 ~ Rear_Torso_Mass + Osc_Freq + T_Off + Front_Torso_Mass + RH_MF_2 + RH_Off + RRL_Off + LRL_Off + Max_Joint_Vel)
summary(lm.out)
par(mfrow=c(2,2))
plot(lm.out)
anova(lm.out)

lm.out <- lm(data=max_inds_gen_dist_data, Fit_1 ~ Rear_Torso_Mass + Osc_Freq + T_Off + Front_Torso_Mass + RH_MF_2 + RH_Off + RRL_Off + LRL_Off + Max_Joint_Vel)
summary(lm.out)

#################################################################################

# Calculate the variance per variable in the data.
variances <- c(rep(0,length(gen_dist_data[1,])-8+1))
for (i in 8:length(gen_dist_data[1,])+1) {
	variances[i-7] <- var(gen_dist_data[,i])
}
variances

length(variances)
names(gen_dist_data)[8:length(names(gen_dist_data))]

variance_df <- data.frame("var"=names(gen_dist_data)[8:length(names(gen_dist_data))],
	"val"=variances)
variance_df$var <- as.factor(variance_df$var)
variance_df$var <- factor(variance_df$var, levels=names(gen_dist_data)[8:length(names(gen_dist_data))])
summary(variance_df)

# Plot the variances.
plt <- ggplot(variance_df, aes(x=var,y=val)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	labs(title="Variance in Each Parameter",x="Parameter",y="Variance") +
	geom_point()
plt

# Create a heatmap of the genomes to view distances.
library(plyr)
library(reshape2)

cols = (names(gen_dist_data)[8:46])
cols [length(cols)+1] = (names(gen_dist_data)[length(names(gen_dist_data))])
cols[length(cols)+1] = "Ind_ID"
cols

gen_dist_data$Ind_ID <- as.factor(gen_dist_data$Ind_ID)
gen_dist_data.m <- melt(subset(gen_dist_data[,cols]))
summary(gen_dist_data.m)

# Create a boxplot of each parameter.
plt <- ggplot(gen_dist_data.m, aes(x=variable,y=value)) +
	geom_boxplot()
plt

gen_dist_data.m <- ddply(gen_dist_data.m, .(variable), transform)
gen_dist_data.m$Ind_ID <- as.factor(gen_dist_data.m$Ind_ID)
gen_dist_data.m$Ind_ID <- as.integer(gen_dist_data.m$Ind_ID)
gen_dist_data.m$Ind_ID <- with(gen_dist_data.m, reorder(Tot_Distance, Ind_ID))
summary(gen_dist_daa.m)

max(gen_dist_data.m$Ind_ID)

plt <- ggplot(subset(gen_dist_data.m, Ind_ID > 58200), aes(variable, Ind_ID)) + 
	theme(axis.text.y=element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1),
		axis.ticks.y = element_blank()
		) +
	geom_tile(aes(fill = value),colour = "white") + 
	scale_fill_gradient(low = "white",high = "steelblue",limits=c(0,0.3))
plt

subset(gen_dist_data.m, Ind_ID > 58000 & variable == "Tot_Distance")

#############################################################################################

# Create a heatmap of the genomes to view distances.
library(plyr)
library(reshape2)

cols = (names(max_inds_gen_dist_data)[8:46])
cols [length(cols)+1] = (names(max_inds_gen_dist_data)[length(names(max_inds_gen_dist_data))])
cols[length(cols)+1] = "Index"
cols

max_inds_gen_dist_data[1:10,]
max_inds_gen_dist_data$Index <- 1:nrow(max_inds_gen_dist_data)
max_inds_gen_dist_data$Index <- as.factor(max_inds_gen_dist_data$Index)
summary(max_inds_gen_dist_data)
max_inds_gen_dist_data$Ind_ID <- as.factor(max_inds_gen_dist_data$Ind_ID)
max_inds_gen_dist_data.m <- melt(subset(max_inds_gen_dist_data[,cols]))
summary(max_inds_gen_dist_data.m)

# Create a boxplot of each parameter.
plt <- ggplot(max_inds_gen_dist_data.m, aes(x=variable,y=value)) +
	geom_boxplot()
plt

max_inds_gen_dist_data.m <- ddply(max_inds_gen_dist_data.m, .(variable), transform)
max_inds_gen_dist_data.m$Ind_ID <- as.factor(max_inds_gen_dist_data.m$Ind_ID)
max_inds_gen_dist_data.m$Ind_ID <- as.integer(max_inds_gen_dist_data.m$Ind_ID)
max_inds_gen_dist_data.m$Ind_ID <- with(max_inds_gen_dist_data.m, reorder(Tot_Distance, Ind_ID))
summary(max_inds_gen_dist_data.m)

max(max_inds_gen_dist_data.m$Ind_ID)

plt <- ggplot(subset(max_inds_gen_dist_data.m,!variable == "Tot_Distance"), aes(variable, Index)) + 
	labs(title="Genetic Distance for Max Individuals",x="Gene",y="Individual") +
	theme(axis.text.y=element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()
		#axis.ticks.y = element_blank()
		) +
	geom_tile(aes(fill = value),colour = "white") + 
	scale_fill_gradient(low = "white",high = "steelblue",limits=c(0,1.0))
plt

save_plot(write_dest,"gen_dist_for_max_individuals",plt)

subset(max_inds_gen_dist_data.m, Ind_ID > 58000 & variable == "Tot_Distance")
