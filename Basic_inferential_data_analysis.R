


data("ToothGrowth")
str(ToothGrowth)
table(ToothGrowth$supp)
table(ToothGrowth$dose)


library(ggplot2)
library(dplyr)

g1 <- ggplot(data = ToothGrowth, aes(dose, len, fill = interaction(dose, supp))) +
    geom_boxplot() + 
    labs(title = "Len by Dose and Supp")
print(g1)


# Lets consider the effect of dose first. H0 is that dose have no effect on len
# and Ha is that is does have an effect. We will need to test for both supps seperately.
# alpha = 0.05

# # supp = OJ 
# OJ_0.5 <- ToothGrowth %>% filter(supp=="OJ", dose==0.5) %>% select(len)
# OJ_1.0 <- ToothGrowth %>% filter(supp=="OJ", dose==1.0) %>% select(len)
# OJ_2.0 <- ToothGrowth %>% filter(supp=="OJ", dose==2.0) %>% select(len)
# 
# t.test(OJ_0.5,OJ_1.0)$p.value
# t.test(OJ_0.5,OJ_2.0)$p.value
# t.test(OJ_1.0,OJ_2.0)$p.value
# 
# # supp = VC
# VC_0.5 <- ToothGrowth %>% filter(supp=="VC", dose==0.5) %>% select(len)
# VC_1.0 <- ToothGrowth %>% filter(supp=="VC", dose==1.0) %>% select(len)
# VC_2.0 <- ToothGrowth %>% filter(supp=="VC", dose==2.0) %>% select(len)
# 
# t.test(VC_0.5, VC_1.0)$p.value
# t.test(VC_0.5, VC_2.0)$p.value
# t.test(VC_1.0, VC_2.0)$p.value

supps <- c("OJ", "VC")
doses <- c(0.5, 1, 2)
doses_p.values <- numeric(length = length(supps)*length(doses))
i <- 1
for(s in supps){
    for(d1 in doses){
        for(d2 in doses[doses>d1]){
            doses_p.values[i] <- t.test(ToothGrowth %>% filter(supp==s, dose==d1) %>% select(len),
                                        ToothGrowth %>% filter(supp==s, dose==d2) %>% select(len)
                                        )$p.value
            i <- i+1
        }
    }
}

# Due to multiple testing, we should control for False Discovery Rate (FDR, Benjamin Hochberg)
p.adjust(doses_p.values, method = "BH")
# All of these values are below alpha, we can reject the null hypothesis that dose has no effect







# Now look at supplements. H0 is that supps have no effect and Ha is that supps do have an effect.

supps_p.values <- numeric(length = length(doses))
i <- 1
for(d in doses){
    supps_p.values[i] <- t.test(ToothGrowth %>% filter(supp=="OJ", dose==d) %>% select(len),
           ToothGrowth %>% filter(supp=="VC", dose==d) %>% select(len)
           )$p.value
    i <- i+1
}
p.adjust(supps_p.values, method = "BH")

# for doses 0.5 and 1.0, it appears that supplement does have an effect, but for dose 2.0 
 # we have a p-value of 0.95 and cannot reject the null hypothesis that the supplement has no effect





