#Exploratory analysis:

#Trying to figure out which mean to use for representing different variables:

#Variable 1 : sleep hrs earlier

min(data$`How many hours you used to sleep before pandemic ?`)
max(data$`How many hours you used to sleep before pandemic ?`)


hist(data$`How many hours you used to sleep before pandemic ?`,
     xlim = c(0,15),breaks = 2)

data$`How many hours you used to sleep before pandemic ?`[data$`How many hours you used to sleep before pandemic ?`>10]

#A 9 y/o child took the survey too, 11-30 yrs is our target population, so removing
#that data point now, we see that now we can use mean for representing 
#this variable rather than using trimmed mean.

#variable 2 : sleep hrs now

min(data$`How many hours do you sleep now ?`)
max(data$`How many hours do you sleep now ?`)


hist(data$`How many hours do you sleep now ?`,
     xlim = c(0,15),breaks = 2)
data$`How many hours do you sleep now ?`[data$`How many hours do you sleep now ?`>10]

#Thus, we can use mean here too, as there isn't a lot of outliers.

#variable 3 : online/play hrs earlier

min(data$`How many hours you used to give for playing or being online before pandemic ?`)
max(data$`How many hours you used to give for playing or being online before pandemic ?`)

hist(data$`How many hours you used to give for playing or being online before pandemic ?`,
     xlim = c(0,15),breaks = 2)
data$`How many hours you used to give for playing or being online before pandemic ?`[data$`How many hours you used to give for playing or being online before pandemic ?`>10]

#Natural variation in the target population. 

data$`How many hours you used to give for playing or being online before pandemic ?`[data$`How many hours you used to give for playing or being online before pandemic ?`>5]

#Natural variation in the target population.

#We'll use mean for representing this.

#variable 4 : online/play hrs now

min(data$`How many hours do you give for playing or being online now ?`)
max(data$`How many hours do you give for playing or being online now ?`)

hist(data$`How many hours do you give for playing or being online now ?`,
     xlim = c(0,20),breaks = 3)
data$`How many hours do you give for playing or being online now ?`[data$`How many hours do you give for playing or being online now ?`>15]

#Natural variation, will you mean only.


#variable 5 : study/work earlier

min(data$`How many hours you used to study/work before pandemic ?`)
max(data$`How many hours you used to study/work before pandemic ?`)


hist(data$`How many hours you used to study/work before pandemic ?`,
     xlim = c(0,15),breaks = 2)
data$`How many hours you used to study/work before pandemic ?`[data$`How many hours you used to study/work before pandemic ?`>10]

#Natural variation in target population, so will use mean only.


#variable 6 : study/work now

min(data$`How many hours do you study/work now ?`)
max(data$`How many hours do you study/work now ?`)

hist(data$`How many hours do you study/work now ?`,
     xlim=c(0,15),breaks = 2) 
data$`How many hours do you study/work now ?`[data$`How many hours do you study/work now ?`>10]

#Natural variation, will use mean.

#We also come to know from the histograms above that, distributions of sleep 
#durations, online/playing durations and study/work durations are not normally 
#distributed.

##Next part is to calculate pearson's correlation coefficient for given data


#Renaming data variables to make this easier to read:
data1 = data
names(data1) = c("Age","sleep b","sleep n","online b","online n","study b","study n")

cormat = cor(data1)

corrplot(cormat)

#Before & now pairs show some correlation, let's check whether this is significant
#at 5% level of significance.

cor.test(data1$`sleep b`,data1$`sleep n`)

#As this comes out to be significantly different from zero, as indicated by the 
#pvalue < 0.05 and the 95% CI not containing zero, we can conclude that,
#sleep before and after have significant positive correlation with each other.

cor.test(data1$`online b`,data1$`online n`)

#As this comes out to be significantly different from zero, as indicated by the 
#pvalue < 0.05 and the 95% CI not containing zero, we can conclude that,
#online before and after have significant positive correlation with each other.


cor.test(data1$`study b`,data1$`study n`)

#As this comes out to be significantly different from zero, as indicated by the 
#pvalue < 0.05 and the 95% CI not containing zero, we can conclude that,
#study/work before and after have significant positive correlation with each other.






