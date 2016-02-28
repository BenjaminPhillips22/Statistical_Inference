


library(ggplot2)
library(dplyr)
library(gridExtra)

set.seed(103)

num_per_group <- 40
num_groups <- 1000

my_exponentials <- rexp(num_per_group*num_groups,0.2) 

row_means <- my_exponentials %>%
    matrix(nrow = num_groups) %>%
    rowMeans() %>% 
    data_frame() %>%
    `names<-`("means")

t.test(row_means$means, conf.level = 0.95)

g1 <- ggplot(data = row_means, aes(means)) +
    geom_histogram(binwidth = 1/10) + 
    ggtitle("Distribution of the Means of 40 Exponentials") +
    geom_vline(xintercept = 5, color = "red")
    
mean(row_means$means)
sd(row_means$means)

my_exponentials <- data_frame(my_exponentials)
g2 <- ggplot(data = my_exponentials, aes(my_exponentials)) +
    geom_histogram(binwidth = 1) + 
    ggtitle("Distribution of 40000 Exponentials") +
    geom_vline(xintercept = 5, color = "green")

print(g2)

grid.arrange(g1, g2, nrow = 1)




    
