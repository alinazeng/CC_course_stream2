# script_to_animate_ggplots
# alina.zeng(at)ubc.ca
# June-14, 2021
# learned from Dr. Chanin Nantasenamat

# https://towardsdatascience.com/how-to-create-animated-plots-in-r-adf53a775961


# Libraries
install.packages(c('gapminder','gganimate','gifski'))
library(gapminder) # contains an excerpt of the Gapminder time series dataset that we are using in this tutorial.
library(ggplot2)
library(gganimate)# allows us to add animation to the plots
library(gifski)# allows us to render the animation as a GIF file format 
library(Cairo)

gapminder <- gapminder

# produce a static plot as reference
png(filename="plot_gdpPercap_lifeExp_static.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +  # the data point will be translucent as defined by the alpha parameter of 0.7
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() + # function logarithmically transforms the data in the X axis via log10.
  facet_wrap(~continent) +
  labs(title = 'Year: 1952-2007', x = 'GDP per capita', y = 'Life expectancy')
dev.off()

ggsave('plot_gdpPercap_lifeExp_static.png', width=8, height=8)


# animate
p1 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # animating the plot
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Life expectancy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1)
anim_save('plot_gdpPercap_lifeExp.gif')



gif(filename="plot_gdpPercap_lifeExp_static.gif", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # animating the plot
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Life expectancy') +    # {frame_time} will dynamically display the changing years as the data points move across the plot.
  transition_time(year) +
  ease_aes('linear')
dev.off()