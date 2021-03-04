#Cleaning data:

data = Humans_in_pandemic_edited
data = data[-25,]

#Desriptive analysis:

min(data$Age)
max(data$Age)

#Conc : People from age 9 to 69 have filled the survey.

## Drawing a pie chart to get further insight into age distribution :

r = c()
p = 1
age = as.vector(data$Age)

for(i in c(0,10,20,30,40,50,60))
{ 
 r[p] = length(age[age > i  & age <= (i + 10)]);
 p = 1 + p
}

r
sum(r)

percent = (r/length(age))
percent

pie(r,labels = percent,col = c("grey","light blue","lavender",rainbow(length(r-3))))

legend("topright",c("0-10","11-20","21-30","31-40","41-50","51-60","61-70"),
       fill = c("grey","light blue","lavender",rainbow(length(r-3))))

#conc : 77% of the people in our population are between 11 - 20 and 17% are between
#       21 - 30 , rest 6% belong to other age groups but have a negligible 
#       representativeness in the sample, hence we can only generalize our results
#       in future to population aged between 11-30 years. 


#Calculating mean,median,mode,trimmed mean,variance of our mean,
#variance of trimmed mean:


#Analysis regarding the most frequent age of our data:

d = c()
p = 1
for(i in 11:30)
{  
 d[p] = length(age[age == 18]);
 p = 1 + p
}
d
rm(d)
v = data.frame(ages = c(11:30),d)
attach(v)
mode = v[d == max(d),1]
mode

detach(v)

#Conc : The mode of our sample is 18 years.

#Means:

mean(age)
mean(age,trim = 0.1)

library(WRS2)
















