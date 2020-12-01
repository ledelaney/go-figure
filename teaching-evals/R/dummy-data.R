### DUMMY DATA FOR EXPLANATION GRAPHICS ###

library(tidyverse)
library(viridis)
library(viridisLite)


# From https://ourcodingclub.github.io/tutorials/dataviz-beautification/
theme_niwot <- function(){
  theme_bw() +
    theme(
      axis.text = element_text(size = 16), 
      axis.title = element_text(size = 18),
      axis.line.x = element_line(color="black"), 
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),                                          
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 18, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),          
      legend.title = element_blank(),                              
      legend.position = c(0.95, 0.15), 
      legend.key = element_blank(),
      legend.background = element_rect(color = "black", 
                                       fill = "transparent", 
                                       size = 2, linetype = "blank"))
}




### Get some mostly normally distributed data 

scores <- rnorm(n = 250, mean = 2.5, sd = 1) %>%
  enframe(name = NULL, value = "scores") %>%
  mutate(semester = rep("semester", 250)) %>%
  filter(scores > 1 & scores <= 5)


pdf(file = "key-box-plot.pdf",
    bg = "transparent",
    width = 5,
    height = 6)

ggplot(data = scores, aes(x = scores, y = 1)) +
  #geom_violin(aes(fill=semester),show.legend = FALSE, alpha = 0.3) +
  geom_boxplot(aes(fill=semester),alpha=0.2, show.legend=FALSE, width = 0.2) +
  #geom_jitter(position = position_jitter(0.1), 
  #           alpha = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = "#000000") +
  coord_flip() +
  theme_niwot() +
  # annotate("text", x = 1, y = 0.75, label = "hello") +
  # "\n" creates a line break
  # geom_curve(aes(x = 50, y = 60, xend = mean(avgs$avg) + 2, yend = 60),
  #            arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
  #            color = "grey30", curvature = 0.3) +
  theme(axis.text.x = element_blank(), 
        axis.line.x = element_blank(), axis.ticks = element_blank()) +
  labs(title = "") +
  ylab("") + xlab("Overall Average")

dev.off()


pdf(file = "key-violin-plot.pdf",
    bg = "transparent",
    width = 5,
    height = 6)

ggplot(data = scores, aes(x = scores, y = 1)) +
  geom_violin(aes(fill=semester),show.legend = FALSE, alpha = 0.3) +
  #geom_boxplot(aes(fill=semester),alpha=0.2, show.legend=FALSE, width = 0.2) +
  geom_jitter(position = position_jitter(0.1), 
             alpha = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = "#000000") +
  coord_flip() +
  theme_niwot() +
  # annotate("text", x = 1, y = 0.75, label = "hello") +
  # "\n" creates a line break
  # geom_curve(aes(x = 50, y = 60, xend = mean(avgs$avg) + 2, yend = 60),
  #            arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
  #            color = "grey30", curvature = 0.3) +
  theme(axis.text.x = element_blank(), 
        axis.line.x = element_blank(), axis.ticks = element_blank()) +
  labs(title = "") +
  ylab("") + xlab("Overall Average")

dev.off()


pdf(file = "key-data-dist.pdf",
    bg = "transparent",
    width = 6,
    height = 6)

ggplot(data = scores, aes(x = scores)) +
  geom_histogram(binwidth = 0.5, alpha=0.5) +
  theme_niwot() +
  ylab("Frequency") + xlab("Overall Average")

dev.off()

