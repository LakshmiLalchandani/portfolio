# Sample of statistical and modeling knowledge


# Create data
userID <- 1:100
cogLoad <- rep(c("high","low"), times = 50)
wordLength <- sample(3:7, 100, replace = T)
responseTime <- runif(100, min = 1200, max = 1900)
df <- data.frame(userID,cogLoad,wordLength,responseTime)

# This data is simulated data for a 2
# (cognitive load: high or low) x continuous (word length) between-
# participant experiment. The dependent variable is response time.


## 1) Run a linear regression to analyze the experiment. Interpret the meaning
## of the parameters and the statistical tests.

mod1 <- lm(responseTime ~ cogLoad*wordLength, data = df)
summary(mod1)

# The omnibus test of the effects of cognitive load and word length on
# response time is significant,F(3,96) = 41.01, p < .001.  The predicted 
# response time for the high cognitive load condition, when word length 
# is 0, is 1116.028 ms.  In the low cognitve load condition, this 
# response time is expected to increase by 147.312 ms, 
# t(96) = 2.242, p = .027 (again, for a theoretical word length of 0).  
# The effect of word length, in the high cognitive load condition, is 
# predicted to increase the response time by 87.766 ms for a 1 unit 
# increase in word length, t(96) = 9.368, p < .001.  However, in the 
# low cognitve load condition, this increase in response time due to 
# word length is predicted to be 48.528 ms less than for the high load 
# condition, t(96) = -3.731, p < .001.

## 2) Conduct a spotlight analysis looking at the simple effect of
## cognitive load at the mean word length and at plus and minus one
## standard deviations of word length from the mean. Interpret the
## simple effects. Why is it strange to use this approach (mean and
## +1/-1 sd) in this case?

WL.sd <- sd(df$wordLength)
mod2.a <- lm(responseTime ~ cogLoad * I(wordLength - mean(wordLength)), data = df)
mod2.b <- lm(responseTime ~ cogLoad * I(wordLength - (mean(wordLength) - WL.sd)), data = df)
mod2.c <- lm(responseTime ~ cogLoad * I(wordLength - (mean(wordLength) + WL.sd)), data = df)

summary(mod2.a)
summary(mod2.b)
summary(mod2.c)

# At the average word length, people in the high cognitive load condition 
# have a predicted response time of 1542.57 ms.  Those in the low cognitive
# load condition are predicted to have a significantly faster response
# time, approximately 88.53 ms faster than the high cognitive load
# condition, t(96) = -4.865, p < .001.

# The effect of cognitive load remains when word length is 1 standard
# deviation above the mean, t(96) = -156.823, p < .001.  When word 
# length is 1 standard deviation above the mean, the predicted response 
# time for the high cognitive load condition is 1666.08 ms.  For this 
# same word length, the low cognitive load condition is predicted to 
# perform approximately 156.82 ms faster than the high load condition.

# However, when the word length is 1 standard deviation below the mean, 
# there are no predicted differences in response time between the two 
# cognitive load conditions, t(96) = -.786, p = .434.

# These particular focal points, specifically 1 standard deviation above 
# and below the mean, seem odd because there is no theoretical reason 
# as to why these word lengths would impact the effects of cognitive load 
# on the given task. While the analyses do give some idea as to the shape 
# of the data, it is convoluted. When looking at the simple effects, it 
# makes sense to look as points of theoretical interest.  In this case, 
# it might make more sense to look at the agreed upon boundaries of working 
# memory capacity, for instance.

## 3) Conduct a floodlight analysis. Find BOTH Johnson-Neyman points (to 2 decimal 
## places), but then report your results in a way that's sensible based on the 
## experimental context.

summary(lm(responseTime ~ cogLoad * I(wordLength - 3.93), data = df)) # 3.93 is upper point
summary(lm(responseTime ~ cogLoad * I(wordLength - .71), data = df)) # .71 is the lower point

# A significant interaction between cognitive load and word length was found on 
# response time. The Johnson-Neyman technique revealed that the effect of cognitve
# load only had a significant effect on response times when word length was greater
# than 3.93. Similarly, the effect of cognitive load reverses when word length is
# smaller than .71.  Because word length cannot be less than 1 letter, the upper 
# bound of the Johnson-Neyman region is most informative. Above 4 letters, a high 
# cognitive load will lead to significantly slower reaction times when compared to 
# a low cognitive load.


## 4) Plot your results in a sensible way (does not need to be in R).

p <- ggplot(data = df, aes(x=wordLength, y=responseTime, color = cogLoad))
p + geom_point()+
  xlab("Word Length")+
  ylab("Response Time")+
  geom_smooth(method = "lm", fullrange = TRUE)+
  scale_color_manual(name= "Cognitive Load",
                     values=c("red",'green'),
                     labels=c("High","Low"))+
  theme_bw()
