# Script for tuto 3
# Alina Zeng
# May-15-2021


# Load libraries ----
library(dplyr)  # For data manipulation
library(ggplot2)  # For data visualisation
library(tidyr)
library(readr)
library(gridExtra)
library(Cairo)
library(RColorBrewer)

setwd("C:/Users/alina/Documents/git/Git_Tutorials/CC_course_stream2/03_Data_vis_2")


# Read in data ----
magic_veg <- read.csv("magic_veg.csv")


str(magic_veg)

# land - the location within the land of magic (two possible lands: Narnia and Hogsmeade)
# plot - the plot number within each land
# year - the year the measurement was taken
# species - the species name (or code), Note that these are fake species!
# height - the imaginary canopy height at that point
# id - the id of each observation


species_counts <- magic_veg %>%
  group_by(land, plot) %>%
  summarise(Species_number = length(unique(species)))


(hist <- ggplot(species_counts, aes(x = plot)) +
    geom_histogram())

# This is the common way of making a histogram, when you have one observation 
# per row and the histogram tallies them for you. But you can immediately see 
# that it doesn’t look right, because we are working with summarised data. You 
# therefore need to tell R that you already know how many species are in each plot. 
# You do that by specifying the stat argument:

png(filename="barplot_confused.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity"))
dev.off()

# Note: an equivalent alternative is to use geom_col (for column), which takes a y value and displays it
png(filename="barplot_confused2.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
col <- ggplot(species_counts, aes(x = plot, y = Species_number, color=plot)) +
    geom_col()+
    scale_color_brewer(palette = "Set2")
dev.off()


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity"))

# Remember that any aesthetics that are a function of your data (like fill here)
# need to be INSIDE the aes() brackets.


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge"))


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_y_continuous(limits = c(0, 50)))


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot", 
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic", 
         x = "\n Plot number", y = "Number of species \n"))     
# \n adds space before x and after y axis text



(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))


We will use the scale_...() functions to customise both the color code AND the 
legend at once.

# The scale_fill_manual(values = c("your-colour-1", "your-colour-2", ...)) function 
# lets you decide on custom colour values for solid elements (bars, boxplots, 
# ribbons, etc.), and its counterpart scale_colour_manual() works exactly the 
# same for line elements (points in a scatter plot, regression lines, box or 
# column outlines, etc.). You need to make sure you put in 
# as many colours as there are factor levels in your data.

png(filename="barplot_pretty_color.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                      name = "Land of Magic") +                # specifying title of legend
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))
dev.off()

ggsave("magical-sp-rich-hist.png", width = 7, height = 5, dpi = 300)
# Create vectors with land names and species counts
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))

# Create the new data frame from the vectors
more_magic <- data.frame(land, counts)

# We'll need as many colours as there are factor levels
length(levels(more_magic$land))    # that's 7 levels 

# CREATE THE COLOUR PALETTE
magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900", "#5F9EA0", "#6CA6CD")    # defining 7 colours
names(magic.palette) <- levels(more_magic$land)                                                    # linking factor names to the colours

# Bar plot with all the factors

(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                        # using our palette here
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


# See how consistent the colour scheme is if you drop some factors (using filter in the first line)

(hist <- ggplot(filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                       # using our palette ensures that colours with no corresponding factors are dropped
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))



yearly_counts <- magic_veg %>%
  group_by(land, plot, year) %>%                             # We've added in year here
  summarise(Species_number = length(unique(species))) %>%
  ungroup() %>%
  mutate(plot = as.factor(plot))


(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Saving the boxplot
ggsave("magical-sp-rich-boxplot1.png", width = 7, height = 5, dpi = 300)


# Create the summarised data
summary <- species_counts %>%  group_by(land) %>% summarise(mean = mean(Species_number),
                                                            sd = sd(Species_number))

# Make a dot plot
(dot <- ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 3) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'), 
                        labels = c('HOGSMEADE', 'NARNIA'), 
                        name = 'Land of Magic') +                   
    labs(title = 'Average species richness', 
         x = '', y = 'Number of species \n') + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom', 
          legend.box.background = element_rect(color = 'grey', size = 0.3)))


