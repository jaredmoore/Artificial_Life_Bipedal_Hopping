# Analyze the variance in parameters by behavior and see if they correlate to fitness at all.

save_plot_tall <- function (filepath, filename, data_plot) 
{
    png(paste(filepath, filename, ".png", sep = ""), width = 7, 
        height = 10, units = "in", res = 300)
    print(data_plot)
    dev.off()
    pdf(paste(filepath, filename, ".pdf", sep = ""), width = 7, 
        height = 10)
    print(data_plot)
    dev.off()
}

# Constants for writing out data.
write_dest <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/best_individual_analysis/"

# Read in the data file.
base_path <- "/Users/JMoore/Documents/Publications/hopping_journal/Experiments/ankle_fix/mass_runs/"

data_file <- "all_best_individuals_unified_data.dat"

all_data <- read.csv(paste(base_path,data_file,sep=""),head=TRUE,sep=",")
all_data$Exp <- factor(all_data$Exp, levels = c("2.48% TBM","4.85% TBM","9.25% TBM","9.25% TBMC","16.93% TBM","16.93% TBMC","23.41% TBM","23.41% TBMC","37.94% TBM")) 
all_data$Exp_Labels <- revalue(all_data$Experiment, c("0.075 Mass"="FM-0.075","0.15 Mass"="FM-0.15","0.3 Mass"="FM-0.3",
	"0.3 Mass Curved"="FM-0.3C","0.6 Mass"="FM-0.6","0.6 Mass Curved"="FM-0.6C","0.9 Mass"="FM-0.9","0.9 Mass Curved"="FM-0.9C","1.8 Mass"="FM-1.8"))
# Remove curved results for now.
all_data <- subset(all_data, ! Exp %in% c("9.25% TBMC", "16.93% TBMC", "23.41% TBMC"))

# Drop the Description column
all_data <- all_data[,!(names(all_data) %in% c("Description"))]
summary(all_data)

bipedal_data <- subset(all_data, Behavior == "Bipedal Hopping")
summary(bipedal_data)

bounding_data <- subset(all_data, Behavior == "Bounding")
summary(bounding_data)

##############################################################################################################################

# Test correlation for bipedal data. (Across all treatments)

columns <- c("Fit_1","Rear_Torso_Mass","Mid_Torso_Mass","Front_Torso_Mass","Osc_Freq","Max_Joint_Vel","RH_Off","RK_Off","RA_Off","RT_Off","FH_Off",
	"FK_Off","LRL_Off","RRL_Off","LFL_Off","RFL_Off","BJ_MF_1","BJ_MF_2","RH_MF_1","RH_MF_2","RK_MF_1","RK_MF_2","RA_MF_1","RA_MF_2",
	"RT_MF_1","RT_MF_2","FH_MF_1","FH_MF_2","FK_MF_1","FK_MF_2","T_MF_1","T_MF_2","H_MF_1","H_MF_2","T_Off","TB_Off","TS1_Len","TS2_Len",
	"TS3_Len","TS1_Mass","TS2_Mass","TS3_Mass")

fit_1_cor <- cor(bipedal_data[,(names(bipedal_data) %in% columns)])
fit_1_cor
fit_1_cor <- cor(bipedal_data[,(names(bipedal_data) %in% columns)])[,1]
fit_1_cor
length(fit_1_cor)
length(columns)

cor_df <- data.frame(variable=columns,value=fit_1_cor)
summary(cor_df)
#cor_df <- subset(cor_df, !(variable %in% c("Gen","Ind","Dist_Traveled","Fit_1","Fit_2","Fit_3","Fit_4","Trial")))
cor_df$variable <- factor(cor_df$variable, level=columns)

plt <- ggplot(cor_df, aes(x=variable,y=value)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	labs(title="Correlation in Each Parameter",x="Parameter",y="Variance") +
	geom_point()
plt

##############################################################################################################################

# Test correlation for bounding data. (Across all treatments)
columns <- c("Fit_1","Rear_Torso_Mass","Mid_Torso_Mass","Front_Torso_Mass","Osc_Freq","Max_Joint_Vel","RH_Off","RK_Off","RA_Off","RT_Off","FH_Off",
	"FK_Off","LRL_Off","RRL_Off","LFL_Off","RFL_Off","BJ_MF_1","BJ_MF_2","RH_MF_1","RH_MF_2","RK_MF_1","RK_MF_2","RA_MF_1","RA_MF_2",
	"RT_MF_1","RT_MF_2","FH_MF_1","FH_MF_2","FK_MF_1","FK_MF_2","T_MF_1","T_MF_2","H_MF_1","H_MF_2","T_Off","TB_Off","TS1_Len","TS2_Len",
	"TS3_Len","TS1_Mass","TS2_Mass","TS3_Mass")

fit_1_cor <- cor(bounding_data[,(names(bounding_data) %in% columns)])
fit_1_cor
fit_1_cor <- cor(bounding_data[,(names(bounding_data) %in% columns)])[,1]
fit_1_cor
length(fit_1_cor)
length(columns)

cor_df <- data.frame(variable=columns,value=fit_1_cor)
summary(cor_df)
#cor_df <- subset(cor_df, !(variable %in% c("Gen","Ind","Dist_Traveled","Fit_1","Fit_2","Fit_3","Fit_4","Trial")))
cor_df$variable <- factor(cor_df$variable, level=columns)

plt <- ggplot(cor_df, aes(x=variable,y=value)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	labs(title="Correlation in Each Parameter",x="Parameter",y="Variance") +
	geom_point()
plt

##############################################################################################################################

# Measure the variance by each parameter and group by behavior.
library(plyr)
library(reshape2)

names(all_data)[!(names(all_data) %in% columns)]

summary(all_data)

max_column_values <- c(
	1.0, #Trial
	1.0, #Gen
	1.0, #Ind
	1.0, #Fit_1
	1.0, #Fit_2
	1.0, #Fit_3
	1.0, #Fit_4
	2.4, # Rear_Torso_Mass
	2.1505, # Mid_Torso_Mass
	2.8, # Front_Torso_Mass
	2.0, # Osc_Freq
	 80000, # Max_Joint_Vel
	 2.0, # RRL_Off
	 2.0, # LRL_Off
	 2.0, # RFL_Off
	 2.0, # LFL_Off
	 2.0, # RH_Off
	 2.0, # RK_Off
	 2.0, # RA_Off
	 2.0, # RT_Off
	 2.0, # FH_Off
	 2.0, # FK_Off
	 35, # BJ_MF_1
	 35, # BJ_MF_2
	 35, # RH_MF_1
	 35, # RH_MF_2
	 35, # RK_MF_1
	 35, # RK_MF_2
	 35, # RA_MF_1
	 35, # RA_MF_2
	 35, # RT_MF_1
	 35, # RT_MF_2
	 35, # FH_MF_1
	 35, # FH_MF_2
	 35, # FK_MF_1
	 35, # FK_MF_2
	 35, # T_MF_1
	 35, # T_MF_2
	 35, # H_MF_1
	 35, # H_MF_2
	 2.0, # T_Off
	 2.0, # TB_Off
	 0.85, # TS1_Len
	 0.85, # TS2_Len
	 0.85, # TS3_Len
	 1, # TS1_Mass
	 1, # TS2_Mass
	 1, # TS3_Mass
	 1, # Exp
	 1, # Experiment
	 1, # Dist_Traveled
	 1 # Behavior
)

all_data.s <- sweep(all_data,2,max_column_values,"/")
all_data.s$Exp <- all_data$Exp
all_data.s$Experiment <- all_data$Experiment
all_data.s$Behavior <- all_data$Behavior
summary(all_data.s)

names(all_data)

all_data.m <- melt(subset(all_data.s,Behavior == "Bipedal Hopping" | Behavior == "Bounding"), id=names(all_data)[!(names(all_data) %in% columns)])
summary(all_data.m)

names(all_data.m)

plt <- ggplot(all_data.m, aes(x=variable,y=value,fill=Behavior)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	ylim(min=0,max=1) +
	geom_boxplot()
plt

##############################################################################################################################

# Read in the melted and scaled data file.
melted_scaled_data_file <- "all_best_individuals_unified_data_scaled_melted.dat"
all_data.m.s <- read.csv(paste(base_path,melted_scaled_data_file,sep=""),head=TRUE,sep=",")
all_data.m.s$Exp_Labels <- revalue(all_data.m.s$Experiment, c("0.075 Mass"="FM-0.075","0.15 Mass"="FM-0.15","0.3 Mass"="FM-0.3",
	"0.3 Mass Curved"="FM-0.3C","0.6 Mass"="FM-0.6","0.6 Mass Curved"="FM-0.6C","0.9 Mass"="FM-0.9","0.9 Mass Curved"="FM-0.9C","1.8 Mass"="FM-1.8"))
	
summary(all_data.m.s)

# Remove curved tail treatments.
all_data.m.s <- subset(all_data.m.s, ! Exp %in% c("9.25% TBMC", "16.93% TBMC", "23.41% TBMC"))

# Remove any lines with Other or Unsteady Bipedal Hopping
all_data.m.s <- subset(all_data.m.s, Behavior != "Other" & Behavior != "Unsteady Bipedal Hopping")

columns <- c("Rear_Torso_Mass","Mid_Torso_Mass","Front_Torso_Mass","Osc_Freq","Max_Joint_Vel","RH_Off","RK_Off","RA_Off","RT_Off","FH_Off",
	"FK_Off","LRL_Off","RRL_Off","LFL_Off","RFL_Off","RH_MF_2","RK_MF_2","RA_MF_2",
	"RT_MF_2","FH_MF_2","FK_MF_2","T_MF_2","T_Off","TB_Off","Tail_Len")

all_data.m.s$variable <- factor(all_data.m.s$variable, levels=columns)
all_data.m.s <- na.omit(all_data.m.s)

give.n <- function(x){
   return(c(y = mean(x), label = length(x)))
}

plt <- ggplot(all_data.m.s, aes(x=variable,y=value,fill=Behavior)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	labs(title="Scaled Genome Values",x="Gene",y="Scaled Value") +
	ylim(min=0,max=1) +
	geom_boxplot() +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2])) + 
   	stat_summary(aes(x=variable,y=value,fill=Behavior),fun.data = give.n, geom = "text")
plt

save_plot(write_dest,"best_individuals_scaled_genome_comparison",plt)

# Plot the variance of each column.
plt <- ggplot(all_data.m.s, aes(x=variable,y=value,color=Behavior)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	labs(title="Variance in Genome Values",x="Gene",y="Variance") +
	ylim(min=0,max=1.0) +
	stat_summary(fun.y = var, geom = "point") +
	stat_summary(fun.data = function(y) {
    data.frame(y = var(y),
               ymin = ((length(y)-1)*var(y))/qchisq(0.025,length(y)-1),
               ymax = ((length(y)-1)*var(y))/qchisq(0.975,length(y)-1))
  }, geom = "errorbar") +
	scale_color_manual(values=c(cbPalette[4],cbPalette[2]))
plt
save_plot(write_dest,"best_individuals_scaled_genome_comparison_variance",plt)

all_data.num_behavior <- ddply(.data=subset(all_data, Behavior == "Bipedal Hopping" | Behavior == "Bounding"), 
                 .(Exp_Labels, Behavior), 
                 summarize, 
                 n=paste(Behavior,"=", length(Behavior)))
all_data.num_behavior <- unique(all_data.num_behavior)
all_data.num_behavior <- subset(all_data.num_behavior, Exp_Labels %in% c("FM-0.075","FM-0.15","FM-0.3","FM-0.6"))
all_data.num_behavior              

plt <- ggplot(subset(all_data.m.s,Exp_Labels %in% c("FM-0.075","FM-0.15","FM-0.3","FM-0.6")), aes(x=variable,y=value,fill=Behavior)) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
	labs(title="Scaled Genome Values",x="Gene",y="Scaled Value") +
	ylim(min=0,max=1.3) +
	geom_boxplot() +
	scale_fill_manual(values=c(cbPalette[4],cbPalette[2])) +
#	stat_summary(aes(x=variable,y=value,fill=Behavior),fun.data = give.n, geom = "text") +
	facet_wrap(~ Exp_Labels, ncol=1) +
	geom_text(data=subset(all_data.num_behavior,Behavior == "Bipedal Hopping"), aes(x=10,y=1.2,label=n),colour="black", inherit.aes=FALSE, parse=FALSE, check_overlap=TRUE, size=3) +
	geom_text(data=subset(all_data.num_behavior,Behavior == "Bounding"), aes(x=20,y=1.2,label=n),colour="black", inherit.aes=FALSE, parse=FALSE, check_overlap=TRUE, size=3)
plt

save_plot_tall(write_dest,"best_individuals_scaled_genome_comparison_faceted",plt)