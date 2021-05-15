# Script for Tuto 1 challenge
# May-15-2021

(1)
# The Craigmillar Castle team would like a summary of the different species 
# found within its grounds, but broken down in four quadrants (NE, NW, SE, SW). 
      # You can start from the trees.genus object created earlier.


# First of all, we need to create the four quadrants. This only requires simple 
# maths and the use of mutate to create a new factor.

## Calculate the quadrants

# Find the center coordinates that will divide the data (adding half of the 
# range in longitude and latitude to the smallest value)

lon <- (max(trees.genus$Easting) - min(trees.genus$Easting))/2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing))/2 + min(trees.genus$Northing)

trees.genus <- trees.genus %>% 
  mutate(quadrant = case_when(Easting > lon & Northing > lat ~ "NE",
                              Easting > lon & Northing < lat ~ "SE",
                              Easting <= lon & Northing > lat ~ "NW",
                              Easting <= lon & Northing < lat ~ "SW"))


# Can you calculate the species richness (e.g. the number of different species) 
# in each quadrant?

trees.genus <- trees.genus %>% 
  group_by(quadrant) %>% 
  mutate("richness" = length(unique(CommonName)))

# equivalent to
trees.genus.grouped <- group_by(trees.genus, quadrant)
trees.genus$richness <- length(unique(CommonName))

#
sp.richness <- trees.genus %>%
  group_by(quadrant) %>%
  summarise(richness = length(unique(LatinName)))

(2)
# They would also like to know how abundant the genus Acer is (as a % of the 
# total number of trees) in each quadrant.
acer_abundance <- trees.genus %>% 
  group_by(quadrant) %>% 
  summarize(abundance = length(filter(Genus == "Acer"))/length(trees.genus$Genus))
# need to revisit this


acer.percent <- trees.genus %>%
  group_by(quadrant, Genus) %>%
  tally() %>%                      # get the count of trees in each quadrant x genus
  group_by(quadrant) %>%           # regroup only by quadrant 
  mutate(total = sum(n)) %>%       # sum the total of trees in a new column
  filter(Genus == 'Acer') %>%      # keep only acer
  mutate(percent = n/total)        # calculate the proportion

# We can make a plot representing the %

ggplot(acer.percent) +
  geom_col(aes(x = quadrant, y = percent)) +
  labs(x = 'quadrant', y = 'Proportion of Acer') +
  theme_bw()

(3)
# Finally, they would like, for each quadrant separately, a bar plot showing 
# counts of Acer trees in the different age classes, ordered so they read from 
# Young (lumping together juvenile and semi-mature trees), Middle Aged, and 
# Mature.


# Create an Acer-only data frame 

acer <- trees.genus %>% 
  filter(Genus == 'Acer')


# Rename and reorder age factor

acer$AgeGroup <- factor(acer$AgeGroup,
                        levels = c('Juvenile', 'Semi-mature', 'Middle Aged', 'Mature'),
                        labels = c('Young', 'Young', 'Middle Aged', 'Mature'))


# Plot the graphs for each quadrant

acer.plots <- acer %>%
  group_by(quadrant) %>%
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_bar(aes(x = AgeGroup)) +
       labs(title = paste('Age distribution of Acer in ', .$quadrant, ' corner', sep = ''),
            x = 'Age group', y = 'Number of trees') +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.title = element_text(size = 14),
             axis.text = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))
  ) 

# View the plots (use the arrows on the Plots viewer)
acer.plots$plots
