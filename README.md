# Portfolio

## About Me
I'm a Mathematics and Data Science student at Truman State University, minoring in Statistics, Computer Science, and Astronomy. I'm pursuing a career in data analytics and have experience with statistical analysis, data wrangling and visualization, machine learning, and programming using R, Python, C++, and Java.

## Projects

<!--
### Interactive HR-Diagram
-->

### Playfair Recreation
<!--
[Click here](https://joeregina.github.io/Portfolio/playfair) to see the full project.
-->
#### Part 1: Critique
> ![Playfair's original visualization](/assets/img/playfair graph.png)
> Individually, the representations of the individual variables work well. The sizes of the circles
are useful for comparing land area, and are scaled correctly. The lines are also helpful for
comparing population and tax revenue for each country. The diagonal lines connecting the
vertical lines help the viewer see each as a group, and guides the eye to make the comparison
between the two for each country. Having the lines be different colors helps distinguish them
from each other as well.
However, there is simply too much going on in the visualization for it to be as effective as
it could. The complex shapes and lines look nice but make it harder to figure out what the
visualization is trying to communicate. The lack of labeling is confusing as well, since you
cannot tell right away what the different lines or colors mean (for example the population vs
tax revenue line, or why the circles are different colors).
In the end, with some work from the viewer, you can understand what the graph wants to
communicate. However, it is not immediately obvious, so the graph does not communicate it
well. It most obviously shows the comparisons of land areas, and with a bit of digging you
can see the population and tax revenue comparison.

#### Part 2: Re-creation
```r
get_circle_x <- function(radius_vector) {
  position_sum <- 0
  x_vector <- numeric(length(radius_vector))
  for(i in 1:length(radius_vector)) {
    # Add radius of circle to get center position
    position_sum <- position_sum + radius_vector[i]
    # Save center position
    x_vector[i] <- position_sum
    # Add other half of circle and add spacing
    position_sum <- position_sum + radius_vector[i] + 2
  }
  x_vector
}

get_pop_x <- function(radius_vector) {
  position_sum <- 0
  x_vector <- numeric(length(radius_vector))
  for(i in 1:length(radius_vector)) {
    # Save left x coordinate of circle
    x_vector[i] <- position_sum
    # Add circle diameter and spacing
    position_sum <- position_sum + 2 * radius_vector[i] + 2
  }
  x_vector
}

get_tax_x <- function(radius_vector) {
  position_sum <- 0
  x_vector <- numeric(length(radius_vector))
  for(i in 1:length(radius_vector)) {
    # Add circle diameter
    position_sum <- position_sum + 2 * radius_vector[i]
    # Save right x coordinate of circle
    x_vector[i] <- position_sum
    # Add spacing
    position_sum <- position_sum + 2
  }
  x_vector
}

# Load packages and data
library(tidyverse)
library(ggforce)
europe <- read.csv("playfair_european_nations.csv")

# Change Country and Power to factors for ease of graphing
europe$Country <- fct_reorder(europe$Country, europe$Area, .desc = TRUE)
europe$Power <- factor(europe$Power)

# Calculate and scale radii of circles
europe$r <- sqrt(europe$Area / pi) / 115

# Calculate proper x coordinates for each circle, each has y coordinate 0
europe$x0 <- get_circle_x(europe$r)
europe$y0 <- 0

# Calculate proper x coordinates for population and tax lines
europe$pop_x <- get_pop_x(europe$r)
europe$tax_x <- get_tax_x(europe$r)

ggplot(data = europe) +
  # Create circles
  geom_circle(mapping = aes(x0 = x0, y0 = y0, r = r, fill = Power), 
              show.legend = FALSE) +
  scale_fill_manual(values = c("Maritime" = "#BCAC92", "Land" = "#D39679")) +
  
  # Create vertical population and tax revenue lines, add connecting line
  geom_segment(mapping = aes(x = pop_x, y = 0, yend = Population), 
               color = "#D78768", linewidth = 1.5) +
  geom_segment(mapping = aes(x = pop_x, y = 0, yend = Population), 
               linewidth = 0.2) +
  geom_segment(mapping = aes(x = tax_x, y = 0, yend = Taxation),
               color = "#E1D587", linewidth = 1.5) +
  geom_segment(mapping = aes(x = tax_x, y = 0, yend = Taxation),
               linewidth = 0.2) +
  geom_segment(mapping = aes(x = pop_x, y = Population, 
                             xend = tax_x, yend = Taxation), linetype = 2) +
  
  # Add thicker lines at intervals of 10
  geom_hline(yintercept = c(0, 10, 20, 30)) +
  
  # Customize y-axis
  scale_y_continuous(breaks = c(1:30),
                     minor_breaks = NULL, # Remove secondary tick lines
                     name = "Millions",
                     expand = c(0,0), # Cuts off top at exactly 30, no padding
                     sec.axis = dup_axis()) + # Create axis on right side
  
  # Hide x-axis breaks
  scale_x_continuous(breaks = c()) +
  
  # Set plot to fixed 1:1 ratio to display circles properly, adjust plot size
  coord_fixed(ylim = c(-22, 30)) +
  
  # Label each circle with country
  geom_text(mapping = aes(x = x0, y = (-r - 1), label = Country, 
                          angle = 90, hjust = "right"), 
            size = 3, family = "serif", fontface = "bold") +
  
  # Label each circle with land area
  geom_text(mapping = aes(x = x0, y = -0.5, label = Area), 
            size = 2, family = "serif") +
  
  # Add label for land area units
  annotate("text", x = europe$x0[1], y = -1.5, label = "Square miles", 
           size = 2, family = "serif") +
  
  # Set plot title and hide x-axis label
  labs(x = NULL, title = "Chart Representing the Extent, Population & Revenues, of the Principal Nations in Europe, after the Division of Poland & Treaty of Luneville") + 
  
  # Set theme, adjust text options, set background and grid line colors
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif", face = "bold"),
        axis.text.y = element_text(size = 7, family = "serif", face = "bold"),
        axis.title.y = element_text(family = "serif", face = "bold"),
        plot.background = element_rect(fill = "#EFCFA8"),
        panel.background = element_rect(fill = "#EFCFA8"),
        panel.grid.major = element_line(color = "#666666"))
```

![Recreation of Playfair's visualization](/assets/img/playfair recreation.png)

<!--
<details>
  <summary>Click to view code</summary>
  
```r
# Code block
```
</details>
-->

#### Part 3: New Data Visualization
```r
# Combine Population and Taxation into 1 variable
europe %>% pivot_longer(cols = c(Population, Taxation), 
                        names_to = "Count_Type", 
                        values_to = "Value") %>% 
  ggplot() +
  
  # Create column chart, use interactions for color coding
  geom_col(mapping = aes(x = Country, y = Value, 
                         fill = interaction(Count_Type, Power)), 
           width = 0.5, position = "dodge") +
  
  # Leave out one taxation entry from legend since redundant
  scale_fill_manual(name = "Color Guide",
                    values = c("Population.Maritime" = "#4287F5",
                               "Population.Land" = "#20916F",
                               "Taxation.Maritime" = "#F5C542",
                               "Taxation.Land" = "#F5C542"),
                    breaks = c("Population.Maritime", 
                               "Population.Land", 
                               "Taxation.Maritime"),
                    labels = c("Population.Maritime" = 
                                 "Maritime Power Population",
                               "Population.Land" = 
                                 "Land Power Population",
                               "Taxation.Maritime" = 
                                 "Tax Revenue (pounds Sterling)")) +
  
  # Add country names and populations
  geom_text(mapping = aes(x = Country, y = -1, 
              label = paste(str_wrap(Country, width = 10), 
                            paste("(", 
                                  prettyNum(Area, big.mark = ","), 
                                  ")",
                                  sep = ""),
                            sep = "\n")),
              vjust = "top") +
  
  # Hide x-axis breaks
  scale_x_discrete(breaks = NULL) +
  
  # Properly scale and label y-axis
  scale_y_continuous(name = "Millions",
                     limits = c(-7, 30),
                     breaks = seq(from = 0, to = 30, by = 5),
                     minor_breaks = c(1:30)) +
  
  # Set x-axis title and graph title
  labs(x = "Country\n(Land Area in square miles)",
       title = "Population vs. Tax Revenue of Principal Nations of Europe") +
  
  # Edit theme for readability
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        panel.grid.major = element_line(linewidth = 1),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
```

![New visualization based on Playfair's data](/assets/img/playfair new.png)

#### Part 4: Concluding Explanation
> I chose to map population and taxation amounts to bar size because it makes for easy, immediate comparisons. You can clearly see which bar is higher for each country, especially since they are grouped together. I used color to distinguish between the taxation and population bars, as well as to note if the population was for a country that was a maritime power or land power. The two population bars are both cooler colors (blue and green) so they are grouped together, while taxation is a warm yellow which contrasts sharply. Since there are only 3 colors it is also easy to remember which color goes with which variable. I mapped country name and land area to text labels since it is easy to digest and keeps the main graph simple. 

> I chose to group the population and taxation bars for each country together, and I put the country name and land area labels right next to them so they are easy to separate. Each country has its own slice of the graph and all of its information is in one place. I put land area in the text label since it is not really the focus of the comparison the graph is trying to show, so it is not as relevant and does not need to be highlighted as much. We want to compare tax revenues and populations to determine comparative taxation (i.e. per-capita taxation) for European countries. Thus we want to highlight the difference between tax revenue and population for each country, which is easy to do with a bar graph. I purposefully chose similar colors for the population bars since they are both representing population, just for different types of nations.

> I think the graph could be improved by implementing the maritime vs. land power variable in a different way, color made sense to me but it may not be as evident to the viewer right away. Perhaps different fill strokes (solid/lined/checkered) could be used to differentiate the type of power. Land area could also be implemented better if we wanted to focus more on comparisons of it, since it is a bit cumbersome to have to read all of the numbers to make comparisons.

> As presented, I think that the data does support Playfair's point about taxes. Britain and Ireland has by far the highest per-capita tax revenue, with tax revenue being about double the population. However, this data does not tell the full story since different groups of people within the country may bear more or less of the tax burden.




<!--
**JoeRegina/JoeRegina** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.

Here are some ideas to get you started:

- 🔭 I’m currently working on ...
- 🌱 I’m currently learning ...
- 👯 I’m looking to collaborate on ...
- 🤔 I’m looking for help with ...
- 💬 Ask me about ...
- 📫 How to reach me: ...
- 😄 Pronouns: ...
- ⚡ Fun fact: ...
-->
