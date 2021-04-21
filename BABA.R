library(quantmod)
library(lmtest)
install.packages("lmtest")
install.packages("plm")
library(plm)

# 1
getSymbols("BABA")

# 2
# BABA
BABA.o<-as.vector(BABA$BABA.Open)
BABA.h<-as.vector(BABA$BABA.High)
BABA.l<-as.vector(BABA$BABA.Low)
BABA.c<-as.vector(BABA$BABA.Close)
BABA.v<-as.vector(BABA$BABA.Volume)
BABA.a<-as.vector(BABA$BABA.Adjusted)

####
# 3 # Dont use
BABA.open.r<-dailyReturn(BABA$BABA.Open)
BABA.high.r<-dailyReturn(BABA$BABA.High)
BABA.low.r<-dailyReturn(BABA$BABA.Low)
BABA.close.r<-dailyReturn(BABA$BABA.Close)
BABA.volume.r<-dailyReturn(BABA$BABA.Volume)
BABA.adjusted.r<-dailyReturn(BABA$BABA.Adjusted)
####

# 3
# Vectors
BABA.open_r<-diff(BABA.o)/BABA.o[-length(BABA.o)]
BABA.high_r<-diff(BABA.h)/BABA.h[-length(BABA.h)]
BABA.low_r<-diff(BABA.l)/BABA.l[-length(BABA.l)]
BABA.close_r<-diff(BABA.c)/BABA.c[-length(BABA.c)]
BABA.volume_r<-diff(BABA.v)/BABA.v[-length(BABA.v)]
BABA.adjusted_r<-diff(BABA.a)/BABA.a[-length(BABA.a)]

# 4

# Before
BABA.open.r_before<-BABA.open_r[-length(BABA.open_r)]
BABA.high.r_before<-BABA.high_r[-length(BABA.high_r)]
BABA.low.r_before<-BABA.low_r[-length(BABA.low_r)]
BABA.close.r_before<-BABA.close_r[-length(BABA.close_r)]
BABA.volume.r_before<-BABA.volume_r[-length(BABA.volume_r)]
BABA.adjusted.r_before<-BABA.adjusted_r[-length(BABA.adjusted_r)]

# Current
BABA.open.r_current<-BABA.open_r[-1]
BABA.high.r_current<-BABA.high_r[-1]
BABA.low.r_current<-BABA.low_r[-1]
BABA.close.r_current<-BABA.close_r[-1]
BABA.volume.r_current<-BABA.volume_r[-1]
BABA.adjusted.r_current<-BABA.adjusted_r[-1]

# Ignore
#####
# Create the data frame :
BABA.open.r_lagged <-data.frame(BABA.open.r_before,BABA.open.r_current)
BABA.high.r_lagged <-data.frame(BABA.high.r_before,BABA.high.r_current)
BABA.low.r_lagged <-data.frame(BABA.low.r_before,BABA.low.r_current)
BABA.close.r_lagged <-data.frame(BABA.close.r_before,BABA.close.r_current)
BABA.volume.r_lagged <-data.frame(BABA.volume.r_before,BABA.volume.r_current)
BABA.adjusted.r_lagged <-data.frame(BABA.adjusted.r_before,BABA.adjusted.r_current)
#####

# 5

# Before 400
BABA.open.r_before_last400 <- tail(BABA.open.r_before, 400)
BABA.high.r_before_last400 <- tail(BABA.high.r_before, 400)
BABA.low.r_before_last400 <- tail(BABA.low.r_before, 400)
BABA.close.r_before_last400 <- tail(BABA.close.r_before, 400)
BABA.volume.r_before_last400 <- tail(BABA.volume.r_before, 400)
BABA.adjusted.r_before_last400 <- tail(BABA.adjusted.r_before, 400)

# Current 400
BABA.open.r_current_last400 <- tail(BABA.open.r_current, 400)
BABA.high.r_current_last400 <- tail(BABA.high.r_current, 400)
BABA.low.r_current_last400 <- tail(BABA.low.r_current, 400)
BABA.close.r_current_last400 <- tail(BABA.close.r_current, 400)
BABA.volume.r_current_last400 <- tail(BABA.volume.r_current, 400)
BABA.adjusted.r_current_last400 <- tail(BABA.adjusted.r_current, 400)

c_open <- BABA.open.r_current_last400
c_high <- BABA.high.r_current_last400
c_low <- BABA.low.r_current_last400
c_close <- BABA.close.r_current_last400
c_volume <- BABA.volume.r_current_last400
c_adjusted <- BABA.adjusted.r_current_last400

# 6 & 7
# Empirical model was created for the closing returns of "current" days.
# Each model was tested for heteroscdasticity and the p-values were found to find which variables were significant for # 8.

#a# (c_close~c_open+c_high)

model_1 <- lm(c_close~c_open)
model_2 <- lm(c_close~c_high)
model_3 <- lm(c_close~c_open+c_high)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_3

bptest(model_3) # p-value = 0.08051
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#b# (c_close~c_open+c_low)

model_1 <- lm(c_close~c_open)
model_2 <- lm(c_close~c_low)
model_3 <- lm(c_close~c_open+c_low)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_3

bptest(model_3) # p-value = 4.005e-07
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#c# (c_close~c_open+c_volume)

model_1 <- lm(c_close~c_open)
model_2 <- lm(c_close~c_volume)
model_3 <- lm(c_close~c_open+c_volume)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_3

bptest(model_3) # p-value < 2.2e-16
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#d# (c_close~c_high+c_low)

model_1 <- lm(c_close~c_high)
model_2 <- lm(c_close~c_low)
model_3 <- lm(c_close~c_high+c_low)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_3

bptest(model_3) # p-value = 0.01103*
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#e# (c_close~c_high+c_volume)

model_1 <- lm(c_close~c_high)
model_2 <- lm(c_close~c_volume)
model_3 <- lm(c_close~c_high+c_volume)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_3

bptest(model_3) # p-value = 2.451e-09
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#f# (c_close~c_low+c_volume)

model_1 <- lm(c_close~c_low)
model_2 <- lm(c_close~c_volume)
model_3 <- lm(c_close~c_high+c_volume)
lrtest(model_1, model_3)
lrtest(model_2, model_3)
# Choose model_1

bptest(model_1) # p-value = 4.794e-05
coeftest(model_1, vcov=vcovHC(model_1, "HC1"))
summary(model_1)

# 7

# Test conducted along with #6, but will be rewritten to show steps of test.

#a# (c_close~c_open+c_high)
bptest(model_3) # p-value = 0.08051
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#b# (c_close~c_open+c_low)
bptest(model_3) # p-value: 4.005e-07
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#c# (c_close~c_open+c_volume)
bptest(model_3) # p-value < 2.2e-16
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#d# (c_close~c_high+c_low)
bptest(model_3) # P-value: 0.01103*
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#e# (c_close~c_high+c_volume)
bptest(model_3) # p-value = 2.451e-09
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)

#f# (c_close~c_low+c_volume)
bptest(model_3) # P-value: 4.794e-05
coeftest(model_3, vcov=vcovHC(model_3, "HC1"))
summary(model_3)


# 8

# The high + low was the variable with the most significance.
# Additionally, it (high + low) has the most most positive effect on the close (dependent variable). 
# In support of this point, it has the lowest p-value when tested using the bptest.
# The open + high, open + low, open + volume, high + volume, and low + volume have a negative effect of the close (dependent variable), as they have p-values greater than 0.05, when conducting the test.

