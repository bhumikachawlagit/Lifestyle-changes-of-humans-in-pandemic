#Cleaning data:

data = Humans_in_pandemic_edited
data = data[-c(25,2,61),]

#Desriptive analysis:

min(data$Age)
max(data$Age)

#Conc : People from age 10 to 44 have filled the survey.

## Drawing a pie chart to get further insight into age distribution :

r = c()
p = 1
age = as.vector(data$Age)

for(i in c(0,10,20,30,40))
{ 
 r[p] = length(age[age > i  & age <= (i + 10)]);
 p = 1 + p
}

r
sum(r)

percent = round((r/length(age)),2)
percent

pie(r,labels = percent,col = c("grey","light blue","lavender",rainbow(length(r-3))))

legend("topright",c("0-10","11-20","21-30","31-40","41-50"),
       fill = c("grey","light blue","lavender",rainbow(length(r-3))))

#conc : 79% of the people in our population are between 11 - 20 and 17% are between
#       21 - 30 , rest 4% belong to other age groups but have a negligible 
#       representativeness in the sample, hence we can only generalize our results
#       in future to population aged between 11-30 years. 


#Calculating mean,median,mode,trimmed mean,variance of our mean,
#variance of trimmed mean:


#Analysis regarding the most frequent age of our data:

d = c()
p = 1
for(i in 11:30)
{  
 d[p] = length(age[age == i]);
 p = 1 + p
}
d
v = data.frame(ages = c(11:30),d)
attach(v)
mode = v[d == max(d),1]
mode

detach(v)

#Conc : The mode of our sample is 18 years.

#Means:

mean(age)
mean(age,trim = 0.1)

se_of_mean = sd(age)/sqrt(length(age))
library(WRS2)

se_of_trimmed_mean = trimse(age,tr = 0.1)

#Thus, se of mean is larger as compared to trimmed mean.
#Thus, according to the trimmed mean the average age of our sample is
#19.175 ~ 19 yrs.

#Variance:

var(age)

sd(age)/mean(age)
#less than 1, implies variance isn't very large.

#Finally, median:

median(age)

#As expected.
#This is because our sample is mostly distributed in that region only.

#Thus, concluding the descriptive analysis of our data here, we have 
#successfully described the age factor of our sample, further, in 
#the next section of analysis i.e. exploratory data analysis, we'll
#try to find relationships between age and all other variables in our
#survey.

#Thank You.
















