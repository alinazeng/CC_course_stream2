# Script for Tuto 1 challenge
# May-15-2021

(1)
# The Craigmillar Castle team would like a summary of the different species 
# found within its grounds, but broken down in four quadrants (NE, NW, SE, SW). 
      # You can start from the trees.genus object created earlier.

# Can you calculate the species richness (e.g. the number of different species) 
# in each quadrant?



# First of all, we need to create the four quadrants. This only requires simple 
# maths and the use of mutate to create a new factor.

lon <- (max(trees.genus$Easting) - min(trees.genus$Easting))/2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing))/2 + min(trees.genus$Northing)


trees.genus <- trees.genus %>% 
  mutate(quadrant = case_when(Easting > lon & Northing > lat ~ "NE",
                              Easting > lon & Northing < lat ~ "SE",
                              Easting < lon & Northing > lat ~ "NW",
                              Easting < lon & Northing < lat ~ "SW"))


## Calculate the quadrants

# Find the center coordinates that will divide the data (adding half of the range in longitude and latitude to the smallest value)


(2)
# They would also like to know how abundant the genus Acer is (as a % of the 
# total number of trees) in each quadrant.

(3)
# Finally, they would like, for each quadrant separately, a bar plot showing 
# counts of Acer trees in the different age classes, ordered so they read from 
# Young (lumping together juvenile and semi-mature trees), Middle Aged, and 
# Mature.
