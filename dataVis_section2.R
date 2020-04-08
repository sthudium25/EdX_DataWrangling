library(tidyverse)
library(dslabs)
data("murders")

str(murders)
head(murders)

r <- murders %>%
  summarise(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

p <- murders %>%
      mutate(pop_mils = population/1000000) %>%
      ggplot(aes(x = pop_mils, 
                 y = total, 
                 color = region,
                 label = abb)) +
      geom_point(size = 3) +
      geom_abline(aes(slope = 1, intercept = log10(r)), 
                  lty = 2, color = "grey10") +
      geom_text_repel(color = "black")+
      scale_x_continuous(trans = "log10") +
      scale_y_continuous(trans = "log10") +
      ggtitle("US Gun Murders in 2010") +
      xlab('Population in millions (log10)')+
      ylab("Total murders (log10)") +
      scale_color_discrete(name = "Region") +
      theme_gdocs()
      
p

