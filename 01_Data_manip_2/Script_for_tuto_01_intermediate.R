# Script for intermediate tuto 1 ----
# May-15-2021

# The pipe operator %>% is a funny little thing that serves as a channel for the 
# output of a command to be passed to another function seamlessly, i.e., without 
# creating intermediary objects. It really makes your code flow, and avoids 
# repetition. Let’s first import the data, and then we’ll see what pipes are all 
# about.


# LIBRARIES
library(dplyr)     # for data manipulation
library(ggplot2)   # for making graphs; make sure you have it installed, or install it now

# Set your working directory
setwd("C:/Users/alina/Documents/git/Git_Tutorials/CC_course_stream2/01_Data_manip_2")

# LOAD DATA ----
trees <- read.csv(file = "trees.csv", header = TRUE)

head(trees)  # make sure the data imported OK, familiarise yourself with the variables

# Count the number of trees for each species ----

trees.grouped <- group_by(trees, CommonName)    # create an internal grouping structure, so that the next function acts on groups (here, species) separately. 

trees.summary <- summarise(trees.grouped, count = length(CommonName))   # here we use length to count the number of rows (trees) for each group (species). We could have used any row name.

# Alternatively, dplyr has a tally function that does the counts for you!
trees.summary <- tally(trees.grouped)


# This is where the pipe comes in to save the day. It takes the data frame created 
# on its left side, and passes it to the function on its right side. This saves 
# you the need for creating intermediary objects, and also avoids repeating the 
# object name in every function: the tidyverse functions “know” that the object 
# that is passed through the pipe is the data = argument of that function.


# Count the number of trees for each species, with a pipe!

trees.summary <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # see how we don't need to name the object, just the grouping variable?
  tally()                     # and we don't need anything at all here, it has been passed through the pipe!


trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()


# 2a. summarise_all() - quickly generate a summary dataframe
summ.all <- summarise_all(trees, mean)


# The super useful case_when() is a generalisation of ifelse() that lets you 
# assign more than two outcomes. All logical operators are available, and you 
# assign the new value with a tilde ~. For instance:

vector2 <- c("What am I?", "A", "B", "C", "D")

case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")


# addingg a Genus column ----

unique(trees$LatinName)  # Shows all the species names

# Create a new column with the tree genera

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )

# In our specific case, we could have achieved the same result much quicker. The 
# genus is always the first word of the LatinName column, and always separated 
# from the next word by a space. We could use the separate() function from the 
# tidyr package to split the column into several new columns filled with the words 
# making up the species names, and keep only the first one.

library(tidyr)
trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)

# we're creating two new columns in a vector (genus name and species name), 
# "sep" refers to the separator, here space between the words, and remove = FALSE 
# means that we want to keep the original column LatinName in the data frame


# Mind blowing! Of course, sometimes you have to be typing more, so here is 
# another example of how we can reclassify a factor. The Height factor has 5 
# levels representing brackets of tree heights, but let’s say three categories 
# would be enough for our purposes. We create a new height category variable 
# Height.cat:

trees.genus <- trees.genus %>%   # overwriting our data frame 
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )




## Reordering a factor's levels

levels(trees.genus$Height.cat)  # shows the different factor levels in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

levels(trees.genus$Height.cat)  # a new order and new names for the levels



library(ggplot2)



# Subset data frame to fewer genera

trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Map all the trees

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)


# The do() function allows us to use pretty much any R function within a pipe 
# chain, provided that we supply the data as data = . where the function requires 
# it.

# Plotting a map for each genus

tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  ) 

# You can view the graphs before saving them
tree.plots$plots

# Saving the plots to file

tree.plots %>%              # the saving call withisn the do function
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", 
                                      sep = ""), device = "png", height = 12, 
            width = 16, units = "cm"))



# Important notes: Pipes only work on data frame objects, and functions outside 
# the tidyverse often require that you specify the data source with a full stop 
# dot "."



# Phew! This could even be chained in one long call without creating the 
# tree.plots object, but take a moment to explore this object: the plots are 
# saved as lists within the plots column that we created. The do() function 
# allows to use a lot of external functions within dplyr pipe chains. However, 
# it is sometimes tricky to use and is becoming deprecated. This page shows an 
# alternative solution using the purr package to save the files.

