##%######################################################%##
#                                                          #
####       Notes on ggplot2 for my own awareness        ####
####  # Alina Zeng # May-07-2021 # alinazeng(at)ubc.ca  ####
#                                                          #
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(Cairo)
library(RColorBrewer)##%######################################################%##


# Set working directory ----
setwd("C:/Users/alina/Documents/git/Git_Tutorials/CC_course_stream2/02_Data_vis_1")

# Libraries ----
# if you haven't installed them before, run the code install.packages("package_name")


# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
LPI <- read.csv("LPIdata_CC.csv")

# reformat the data and make it long using gather from tidyr package
# Reshape data into long form
# By adding 9:53, we select columns 9 to 53
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)

# Drop the Xs in front of the years using parse_number() from the readr package.
LPI2$year <- parse_number(LPI2$year)

# When manipulating data it's always good check if the variables have stayed 
# how we want them
# Use the str() function
str(LPI2)

# Abundance is a character variable when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

# change all the column names to lower cased
names(LPI2)  # Check what each column is called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case


# To see what species are available, use the following code to get a list:
unique(LPI2$common.name)

# filter out just the records for that species
# substituting common.name for the name of your chosen species.

vulture <- filter(LPI2, common.name == "Griffon vulture / Eurasian griffon")
head(vulture)

# There are a lot of NAs in this dataframe, so we will get rid of the empty rows using na.omit()
vulture <- na.omit(vulture)


# With base R graphics
base_hist <- hist(vulture$abundance)

# With ggplot2
vulture_hist <- ggplot(vulture, aes(x = abundance))  +
  geom_histogram() 

# Calling the object to display it in the plot viewer
vulture_hist

# With brackets: you create and display the graph at the same time
(vulture_hist <- ggplot(vulture, aes(x = abundance))  +
    geom_histogram())

png(filename="Temp_complete_2015.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6, 
    res=300)

# For another way to check whether your data is normally distributed
# you can either create density plots using package ggpubr and command ggdensity(), OR use functions qqnorm() and qqline()

png(filename="vulture_histogram.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6, 
    res=300)
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +                
    geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(abundance)),                       # Adding a line for mean abundance
               colour = "red", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_bw() +                                                      # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nGriffon Vulture Abundance")  +                              # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Putting a 1 cm margin around the plot
dev.off()
# We can see from the histogram that the data are very skewed - a typical distribution of count abundance data


# Filtering the data to get records only from Croatia and Italy using the `filter()` function from the `dplyr` package
vultureITCR <- filter(vulture, country.list %in% c("Croatia", "Italy"))

# Using default base graphics
plot(vultureITCR$year, vultureITCR$abundance, col = c("#1874CD", "#68228B"))

# Using default ggplot2 graphics
png(filename="Test2.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = country.list)) +  # linking colour to a factor inside aes() ensures that the points' colour will vary according to the factor levels
    geom_point())
dev.off()


# pamper it up
png(filename="vulture_scatter.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +                # Adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#EE7600", "#00868B"),                # Adding custom colours for lines and points
                        labels = c("Croatia", "Italy")) +                # Adding labels for the legend
    ylab("Griffon Vulture Abundance\n") +                             
    xlab("\nYear")  +
    coord_cartesian(xlim = c(1985, 2010))+                               # x limits
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.9, 0.9)))                                 # Setting legend position - 0 is left/bottom, 1 is top/right
dev.off()


(vulture_boxplot <- ggplot(vultureITCR, aes(country.list, abundance)) + geom_boxplot())

# Beautifying
png(filename="vulture_boxplot.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
(vulture_boxplot <- ggplot(vultureITCR, aes(country.list, abundance)) + 
    geom_boxplot(aes(fill =country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B")) +             # Adding custom colours
    ylab("Griffon Vulture Abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none"))                                    # Removing legend - not needed with only 2 factors
dev.off()
# Calculating species richness using pipes %>% from the dplyr package
richness <- LPI2 %>% filter (country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(country.list) %>%
  mutate(richness = (length(unique(common.name)))) # create new column based on how many unique common names (or species) there are in each country 

# Plotting the species richness
png(filename="spp_richness_barplot.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=8, 
    res=300)
richness_barplot <- ggplot(richness, aes(x = country.list, y = richness,colour = country.list)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    scale_fill_brewer(
      palette = "Blues")+  
    scale_color_brewer(
        palette = "Blues")+
    ylab("Species Richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
dev.off()


scale_fill_brewer(palette = "Set3")+   ## use this to change color
  scale_color_brewer(palette = "Set3")+
  
# Plot the population change for all countries
  png(filename="population_change_all_countries.png", 
      type="cairo", ### this helps with resolution, love it!!
      units="in", 
      width=8, 
      height=8, 
      res=300)
(vulture_scatter_all <- ggplot(vulture, aes (x = year, y = abundance, colour = country.list)) +
  geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))  
dev.off()


# Plot the population change for countries individually
png(filename="population_change_individual_default.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=16, 
    height=10, 
    res=300)
(vulture_scatter_facets <- ggplot(vulture, aes (x = year, y = abundance, colour = country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    facet_wrap(~ country.list, scales = "free_y") +                      # THIS LINE CREATES THE FACETTING
    theme_bw() +
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   
dev.off()

grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)

# This doesn't look right - the graphs are too stretched, the legend and text are all messed up, the white margins are too big

# Fixing the problems - adding ylab() again overrides the previous settings
png(filename="panel.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=12, 
    height=20, 
    res=300)
(panel <- grid.arrange(
  vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.85, 0.85)), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have
dev.off()