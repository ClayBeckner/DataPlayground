options(scipen = 999) #<-Use this code if you want decimal p-values --.000006 rather than 6e-6

install.packages("RCurl")#<--DO THIS if you haven't previously. 
#RCurl should allow you to access data directly from a URL. 
library(RCurl)
library(ggplot2)

#############################################
#TEXTING SPEED DATA. 
textingRawDat<- getURL("https://raw.githubusercontent.com/ClayBeckner/DataPlayground/master/textDat.txt")
textingDat<-read.delim(text= textingRawDat, sep = "\t", header = T)

#See how the categories intersect
table(textingDat$condition, textingDat$language)

#visualise the data with a density plot or boxplot. 
boxplot(textingDat$text.speed ~ textingDat$condition)


#You can check normality like this: 
shapiro.test(subset(textDat, condition == "keypad")$text.speed)

#Now check it when the condition is "touchscreen" 

# Dependent variable is text.speed: number of characters per second
# State a hypothesis for the type of phone interface (touchscreen vs. keypad)
#H0: The type of phone interface makes no difference in speed. The mean speed will 
# be the same for touchscreens and keypads. 
#H1: The mean texting speed will be faster for touchscreens. 

#Test this hypothesis with a t-test. 

t.test(textDat$text.speed ~ textDat$condition)
# p-value is < .05, so we reject the null hypothesis. 
# The t-test reports the means: .797 characters per second for keypad, 1.26 for touchscreen.
#This confirms that indeed people are faster on touchscreens. 

# **** TRY THIS****
#How do we find out if participant AGES are different in the two conditions?
#What do the results mean?

# **** TRY THIS****
#How do we find out if language status (L1 or L2) affects the results? 
#State a formal hypothesis and test it. 
#What do the results mean?


################################################
#SIMULATED VS. REAL COIN FLIPS

flippinRawDat<- getURL("https://raw.githubusercontent.com/ClayBeckner/DataPlayground/master/SimulatedAndRealFlips215.txt")
simRealFlips<-read.delim(text= flippinRawDat, sep = "\t", header = T)

#Maxrun column: maximum number of heads or tails in this run.
#Are the distributions different for real vs. simulated flips? 

table(simRealFlips$Type) #21 datapoints for each condition

ggplot(simRealFlips, aes(x= Maxrun, fill= Type)) + geom_density(alpha = 0.3)
#Data looks very non-normal

#Check normality of simulated data:
simDat <-subset(simRealFlips, Type == "Simulated")
shapiro.test(simDat$Maxrun) #p-value is less than .05. Not normal. 
#Mean Maxrun value is 4.33. (*********See if you can remember how to find this.) 

realDat<-subset(simRealFlips, Type == "Real")
shapiro.test(realDat$Maxrun) #Even the real data is non-normal.
#Mean Maxrun value is 5.29. (**********How do you find this?) 

#Because of non-normality, we need to use a non-parametric test.  Wilcoxon: 
wilcox.test(simRealFlips$Maxrun ~ simRealFlips$Type, exact = FALSE)
#The exact = FALSE command is to turn off a warning about ties.

#Test result: p-value = 0.001738
#Therefore, reject the null hypothesis. The categories are different. 
#Our mean values above, and the density plots, show that 'Maxrun' is shorter for simulated flips than
# for real flips. The Wilcoxon test confirms that the difference is significant. 

#Note that above we created data subsetted by Type.
#We can use this for an alternate wilcox.test call:   

wilcox.test(realDat$Maxrun, simDat$Maxrun, exact = FALSE)
#*********************
#Try to understand why this does the exact same thing as the original wilcox.test above.  
