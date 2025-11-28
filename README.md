# Portfolio

## About Me
I'm a Mathematics and Data Science student at Truman State University, minoring in Statistics, Computer Science, and Astronomy. I'm pursuing a career in data analytics and have experience with statistical analysis, data wrangling and visualization, machine learning, and programming using R, Python, C++, and Java.

## Projects

<!--
### Interactive HR-Diagram
-->

### Playfair Recreation
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
<details>
  <summary>Click to view code</summary>
  
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

</details>

#### Part 3

<!--
[Click here](https://joeregina.github.io/Portfolio/playfair) to see the full project.
-->


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
