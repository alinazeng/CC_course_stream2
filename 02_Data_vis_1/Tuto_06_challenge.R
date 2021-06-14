# To practice making graphs, go back to the original LPI dataset that you imported
# at the beginning of the tutorial. Now, can you:
  
  # 1 - Choose TWO species from the LPI data and display their population trends
  # over time, using a scatterplot and a linear model fit?
  
  # 2 - Using the same two species, filter the data to include only records from
  # FIVE countries of your choice, and make a boxplot to compare how the abundance 
  # of those two species varies between the five countries?


# Filter Mediterranean gull ----
gull <- filter(LPI2, common.name == "Mediterranean gull" )

# get rid of NAs
gull <- na.omit(gull)

# With base R graphics ----
base_hist <- hist(gull$abundance)

# With ggplot2: creating graph with no brackets ----
gull_hist <- ggplot(gull, aes(x = abundance))  +
  geom_histogram() 

# Calling the object to display it in the plot viewer
gull_hist


# For another way to check whether your data is normally distributed, you can 
# either create density plots using package ggpubr and command ggdensity(), 
# OR use functions qqnorm() and qqline()

# beautification
(gull_hist <- ggplot(gull, aes(x = abundance)) +                
    geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(abundance)),                       # Adding a line for mean abundance
               colour = "red", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_bw() +                                                      # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nMediterranean gull abundance")  +                              # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Putting a 1 cm margin around the plot


# Scatter plot to examine population change over time

# Filtering the data to get records only from UK and Italy using the `filter()` function from the `dplyr` package
gullITUK <- filter(gull, country.list %in% c("United Kingdom", "Italy"))

# Using default base graphics
plot(gullITUK$year, gullITUK$abundance, col = c("#1874CD", "#68228B"))

# Using default ggplot2 graphics
(gull_scatter <- ggplot(gullITUK, aes(x = year, y = abundance, colour = country.list)) +  # linking colour to a factor inside aes() ensures that the points' colour will vary according to the factor levels
    geom_point())

(gull_scatter <- ggplot(gullITUK, aes (x = year, y = abundance, colour = country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +                # Adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#EE7600", "#00868B"),                # Adding custom colours for lines and points
                        labels = c("Italy","United Kingdom")) +                # Adding labels for the legend
    ylab("Mediterranean gull abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.9, 0.9)))                                 # Setting legend position - 0 is left/bottom, 1 is top/right





# Boxplot to examine whether gull abundance differs between Croatia and Italy


(gull_boxplot <- ggplot(gullITUK, aes(country.list, abundance)) + geom_boxplot())

# Beautifying

(gull_boxplot <- ggplot(gullITUK, aes(country.list, abundance)) + 
    geom_boxplot(aes(fill = country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B")) +             # Adding custom colours
    ylab("Mediterranean gull abundance\n") +                             
    xlab("\ncountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none"))                                    # Removing legend - not needed with only 2 factors



# Barplot to compare species richness of a few European countries

# Calculating species richness using pipes %>% from the dplyr package
richness2 <- LPI2 %>% filter (country.list %in% c("Zimbabwe", "Denmark", "Spain", "Austria", "Algeria")) %>%
  group_by(country.list) %>%
  mutate(richness = (length(unique(common.name)))) # create new column based on how many unique common names (or species) there are in each country 

# Plotting the species richness
(richness2_barplot <- ggplot(richness2, aes(x = country.list, y = richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Using facets and creating panels ----

# Plot the population change for all countries
(gull_scatter_all <- ggplot(gull, aes (x = year, y = abundance, colour = country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    ylab("Mediterranean gull abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   



# Plot the population change for countries individually
png("Tuto6challenge.png", width=1500, height=900)  
(gull_scatter_facets <- ggplot(gull, aes (x = year, y = abundance, colour = country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = country.list)) +               # Adding linear model fit, colour-code by country
    facet_wrap(~ country.list, scales = "free_y") +                      # THIS LINE CREATES THE FACETTING
    theme_bw() +
    ylab("Mediterranean gull abundance\n") +                             
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