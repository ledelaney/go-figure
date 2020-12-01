### TEACHING EVAL GRAPHICS ###

# Load Libraries
library(tidyverse)
library(reshape)
library(reshape2)
library(viridis)
library(viridisLite)

# Source for flat violin plots
## https://ourcodingclub.github.io/tutorials/dataviz-beautification/
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

### CUSTOM THEME FROM ABOVE
#text = element_text(family = "Helvetica Light") -- not working with PDF export
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


###### This function takes the following: #######
## 3 dataframes containing: the rating and the percentage of each student that selected that rating
## name of semester
## number of students in class

make.class.data <- function(myoverall, myquestions, myexplain, n, name){
  
  # Overall rating
  
  x <- rep(myoverall[1,1], round(myoverall[1,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  y <- rep(myoverall[2,1], round(myoverall[2,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  z <- rep(myoverall[3,1], round(myoverall[3,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  w <- rep(myoverall[4,1], round(myoverall[4,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  s <- rep(myoverall[5,1], round(myoverall[5,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  overall <- bind_rows(x, y, z, w, s) %>%
    setNames(., "overall") 
  
  # Answer questions rating
  
  x <- rep(myquestions[1,1], round(myquestions[1,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  y <- rep(myquestions[2,1], round(myquestions[2,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  z <- rep(myquestions[3,1], round(myquestions[3,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  w <- rep(myquestions[4,1], round(myquestions[4,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  s <- rep(myquestions[5,1], round(myquestions[5,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  questions <- bind_rows(x, y, z, w, s) %>%
    setNames(., "questions")
  
  # Explain rating
  
  x <- rep(myexplain[1,1], round(myexplain[1,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  y <- rep(myexplain[2,1], round(myexplain[2,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  z <- rep(myexplain[3,1], round(myexplain[3,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  w <- rep(myexplain[4,1], round(myexplain[4,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  s <- rep(myexplain[5,1], round(myexplain[5,2]*n)) %>%
    enframe(name = NULL, value = "score")
  
  explain <- bind_rows(x, y, z, w, s) %>%
    setNames(., "explain")
  
  # Bind together for full results
  myresult <- bind_cols(questions, overall, explain) %>%
    mutate(semester = name) %>%
    select(semester, questions:explain) %>%
    return()
  
  
}

# Summer 2020: BIOS220 GENETICS
score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.926, 0.074, 0, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.963, .037, 0, 0, 0)
questions <- cbind(score, perc)

score <- c(5, NaN, NaN, NaN, NaN)
perc <- c(1, 0, 0, 0, 0)
explain <- cbind(score, perc)

su.2020 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 27, 
                           name = "summ2020")

## Spring 2020: BIOS220 GENETICS

score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.897, .103, 0, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.897, .103, 0, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.966, .034, 0, 0, 0)
explain <- cbind(score, perc)

sp.2020 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 29, 
                           name = "spring2020")

## Fall 2019: BIOS220 GENETICS

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.593, .259, .148, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 3, 2, NaN)
perc <- c(0.444, .370, .111, .074, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, 2, NaN)
perc <- c(0.593, .185, .148, .074, 0)
explain <- cbind(score, perc)

fa.2019 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 27, 
                           name = "fall2019")

# Summer 2019: BIOS331 ECOLOGY

score <- c(5, 4, 2, NaN, NaN)
perc <- c(0.5, .25, .25, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.5, .25, .25, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.25, .5, .25, 0, 0)
explain <- cbind(score, perc)

su.2019 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 4, 
                           name = "summ2019")



# Spring 2019: BIOS230 ECO-EVO

score <- c(5, 4, 3, 2, 1)
perc <- c(0.423, .202, .269, .067, .038)
overall <- cbind(score, perc)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.452, .202, .279, .038, .029)
questions <- cbind(score, perc)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.437, .214, .272, .049, .029)
explain <- cbind(score, perc)

sp.2019 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 107, 
                           name = "spring2019")


# Fall 2018: BIOS430 EVOLUTION

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.5, 0.2, 0.3, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.4, .5, .1, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.3, .6, .1, 0, 0)
explain <- cbind(score, perc)

fa.2018 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 10, 
                           name = "fall2018")


# Summer 2018: BIOS220 GENETICS

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.5, .357, .143, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.571, .357, .071, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.5, .286, .214, 0, 0)
explain <- cbind(score, perc)

su.2018 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 14, 
                           name = "summ2018")


# Spring 2018: BIOS430 EVOLUTION

score <- c(5, 4, NaN, NaN, NaN)
perc <- c(0.556, .444, 0, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.556, .333, .111, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, NaN, NaN)
perc <- c(0.444, .333, .222, 0, 0)
explain <- cbind(score, perc)

sp.2018 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 9, 
                           name = "spring2018")


# Fall 2017: BIOS430 EVOLUTION

score <- c(5, 4, 2, NaN, NaN)
perc <- c(0.75, .125, .125, 0, 0)
overall <- cbind(score, perc)

score <- c(5, 4, 2, NaN, NaN)
perc <- c(0.75, .125, .125, 0, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 2, NaN, NaN)
perc <- c(0.625, .25, .125, 0, 0)
explain <- cbind(score, perc)

fa.2017 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 8, 
                           name = "fall2017")


# Spring 2017: BIOS120 INTRO (add sections together)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.318, 0.132, 0.132, 0.09, 0.318)
overall <- cbind(score, perc)

score <- c(5, 4, 3, 1, NaN)
perc <- c(0.364, 0.273, 0.091, 0.273, 0)
questions <- cbind(score, perc)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.409, 0.136, 0.182, 0.091, 0.182)
explain <- cbind(score, perc)

sp.2017 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 22, 
                           name = "spring2017")

# Fall 2016: BIOS130 INTRO (add sections together)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.167, 0.133, 0.2, 0.233, 0.267)
overall <- cbind(score, perc)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.4, 0.167, 0.133, 0.1, 0.2)
questions <- cbind(score, perc)

score <- c(5, 4, 3, 2, 1)
perc <- c(0.276, 0.241, 0.12, 0.138, 0.241)
explain <- cbind(score, perc)

fa.2016 <- make.class.data(myoverall = overall, 
                           myquestions = questions, 
                           myexplain = explain, 
                           n = 30, 
                           name = "fall2016")


rm(overall, explain, questions, perc, score)


# Bind all averages together for plotting averages..

alldata <- bind_rows(fa.2016, sp.2017, fa.2017, sp.2018, su.2018, 
                  fa.2018, sp.2019, su.2019, fa.2019, sp.2020, su.2020) %>%
  mutate(semester = factor(semester, levels = c("fall2016", "spring2017", 
                                                 "fall2017", "spring2018", 
                                                 "summ2018", "fall2018", "spring2019", 
                                                 "summ2019", "fall2019", "spring2020", "summ2020")))


# Take average across three components for composite score
avgs <- alldata %>%
  rowwise() %>%
  mutate(avg = mean(questions:explain)) %>%
  select(semester, avg)

# Single average for each course
single.avgs <- alldata %>%
  rowwise() %>%
  mutate(avg = mean(questions:explain)) %>%
  select(semester, avg) %>%
  group_by(semester) %>%
  summarise(all.avg = mean(avg))



###### LINE GRAPH SHOWING CHANGE IN OVERALL COURSE RATING AVERAGE THROUGH TIME ######
pdf(file = "overall-averges.pdf",
    bg = "transparent",
    width = 18,
    height = 7)

ggplot(data = single.avgs, aes(x=1:11, y = all.avg)) +
  geom_line(size=1.2) +
  ylim(1,5.5) +
  theme_niwot() +
  theme(axis.text.x = element_blank()) +
  ylab("Overall Course Average") + xlab("")

dev.off()


##### PLOT WITH ALL DATA, ALL SEMESTERS #####

pdf(file = "overall-scores.pdf",
    bg = "transparent",
    width = 18,
    height = 7)

ggplot(data = avgs, aes(x = avg, y = 1)) +
  geom_violin(show.legend = FALSE, aes(color = semester)) +
  geom_boxplot(aes(fill = semester), alpha=0.5, show.legend=FALSE, width = 0.2) +
  geom_jitter(aes(color = semester), position = position_jitter(0.1), 
              alpha = 0.4, show.legend = FALSE) +
  facet_grid(. ~ semester) +
  coord_flip() +
  theme_niwot() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Summary of Teaching Evaluation Scores") +
  ylab("") + xlab("Overall Average")

dev.off()


#### EXAMPLE DATA AND PLOTS ####

scores <- rbinom(100, 1:5, 0.5) %>%
  enframe(name = NULL, value = "scores") %>%
  mutate(semester = rep("semester", 100))

pdf(file = "key-plot.pdf",
    bg = "transparent",
    width = 3,
    height = 5)

ggplot(data = scores, aes(x = scores, y = 1)) +
  geom_violin(aes(fill=semester),show.legend = FALSE, alpha = 0.3) +
  geom_boxplot(aes(fill=semester),alpha=0.2, show.legend=FALSE, width = 0.2) +
  geom_jitter(position = position_jitter(0.1), 
              alpha = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = "#000000") +
  coord_flip() +
  theme_niwot() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Key") +
  ylab("") + xlab("Overall Average")

dev.off()


pdf(file = "key-data-dist.pdf",
    bg = "transparent",
    width = 3,
    height = 5)

ggplot(data = scores, aes(x = scores)) +
  geom_histogram(binwidth = 0.5, alpha=0.5) +
  theme_niwot() +
  ylab("") + xlab("Overall Average")

dev.off()



#### ATTEMPT AT FLAT VIOLIN, DOES NOT LOOK GREAT ####
pdf(file = "overall-rain.pdf",
    bg = "transparent",
    width = 18,
    height = 12)

ggplot(data = avgs, 
       aes(x = semester, y = avg, fill = semester)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8, width = 3) +
  geom_point(aes(y = avg, color = semester), 
             position = position_jitter(width = 0.15), size = 1, alpha = 0.5) +
  geom_boxplot(width = 0.3, outlier.shape = NA, alpha = 0.6) +
  labs(y = "\nOverall Average", x = "") +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 5.5)) +
  coord_flip() +
  theme_niwot()

dev.off()