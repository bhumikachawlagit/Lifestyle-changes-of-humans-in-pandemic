#Proceeding towards inferential analysis:

#We can use the t-test in the case where n >= 30 (for non-normal data too;
#as mentioned in theory).
#So we will use the paired t-test to verify whether there has been a 
#significant change in the lifestyle of people in pandemic. 
#Later we will also use a non-parametric test to check whether our results 
#remain the same or not ?!

#Testing assumptions: 

hist(-data1$`sleep b`+data1$`sleep n`,probability = T)
m = mean(-data1$`sleep b`+data1$`sleep n`)
sd = sd(-data1$`sleep b`+data1$`sleep n`)
curve(dnorm(x,m,sd),add = T,col = "Red" )
qqnorm(data1$`sleep b`-data1$`sleep n`)
qqline(data1$`sleep b`-data1$`sleep n`)

#Approximately normal.


#For sleep times before and now:


t.test(data1$`sleep b`,data1$`sleep n`,paired = TRUE,"less")
t.test(data1$`sleep n`,data1$`sleep b`,paired = TRUE,"greater")

#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean sleep durations before might be significantly lower than during pandemic 
#at 5% level of testing.

#Testing assumptions: 

hist(-data1$`online b`+data1$`online n`,probability = T)
m = mean(-data1$`online b`+data1$`online n`)
sd = sd(-data1$`online b`+data1$`online n`)
curve(dnorm(x,m,sd),add = T,col = "Red" )
qqnorm(data1$`online b`-data1$`online n`)
qqline(data1$`online b`-data1$`online n`)

#Approximately normal.

#For being online/playing durations before and now:

t.test(data1$`online b`,data1$`online n`,paired = T,"less")
t.test(data1$`online n`,data1$`online b`,paired = T,"greater")


#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean durations for being online/playing before might be significantly lower 
#than during pandemic at 5% level of testing.

#Testing assumptions: 

hist(data1$`study b`-data1$`study n`,probability = T)
m = mean(data1$`study b`-data1$`study n`)
sd = sd(data1$`study b`-data1$`study n`)
curve(dnorm(x,m,sd),add = T,col = "Red" )
qqnorm(data1$`online b`-data1$`online n`)
qqline(data1$`online b`-data1$`online n`)

#Approximately normal.


#For work or study durations:

t.test(data1$`study b`,data1$`study n`,paired = T,"greater")

#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean study/work durations before might be significantly higher than during 
#pandemic at 5% level of testing.

#Wohoo! We've answered our prime question finally! YAY!

#But but, before you get too excited we need to check this using non-parametric
#test as well, because you never know :(

#Verifying our results using non-parametric test:

#For sleep durations before & now:

#We are planning on re-sampling 100000 times :

set.seed(500)
B = 100000
dif = rep(0,B)
signs = c(-1,1)
D1 = data1$`sleep b`-data1$`sleep n`
obsD1 = mean(D1)

for(i in 1:B)
{
  p = sample(signs,length(D),replace = TRUE);
  dif[i] = mean(abs(D1)*p)
}

length(dif[dif <= obsD1])/length(dif)

#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean sleep durations before might be significantly lower than during pandemic 
#at 5% level of testing.

#For being online/playing durations :

#Re-sampling 100000 times :

B = 100000
set.seed(500)
dif = rep(0,B)
D2 = data1$`online b` - data1$`online n`
obsD2 = mean(D2)

for(i in 1:B)
{
  p = sample(signs,length(D2),replace = T);
  dif[i] = mean(abs(D2)*p)
}

length(dif[dif <= obsD2])/length(dif)

#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean durations for being online/playing before might be significantly lower 
#than during pandemic at 5% level of testing.

#For study/work durations :

#Re-sampling = 100000

set.seed(500)
B = 100000
dif = rep(0,B)
D3 = data1$`study b` - data1$`study n`
obsD3 = mean(D3)

for(i in 1:B)
{
  p = sample(sign,length(D3),replace = T);
  dif[i] = mean(abs(D3)*p)
}

length(dif[dif >= obsD3])/length(dif)


#As p-value < 5% we have sufficient evidence to reject H0, we can conclude that, 
#mean study/work durations before might be significantly higher than during 
#pandemic at 5% level of testing.


#Hence verified!

#THUS, YES, THERE MIGHT HAVE BEEN A SIGNIFICANT CHANGE IN THE LIFESTYLE OF
#OF HUMANS DURING PANDEMIC.

#THE END.
#THANK YOU.












