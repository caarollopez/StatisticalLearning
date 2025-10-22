# RESAMPLING

# given test sample x0
# prediction y^0
# MSE DECOMPOSITION **

# through a simulation, we are going to illustrate the decomposition

# data generating process: 
# y = 10x^2 - 2x + 5 + €
# € ~ N(0, 100^2)

# SIMULATION OF 100 SAMPLES FROM THE MODEL
f = function(x) {
  10*x^2-2*x+5
}

get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = -6, max = 6) # x is assumed uniform
  y = f(x) + rnorm(n = sample_size, mean = 0, sd = 100)
  data.frame(x, y)
}


########################################################################################
# 4 polynomial models (all linear regressions)
# to fit the train a data
sim_data = get_sim_data(f, sample_size = 100)

# 1 naive model
fit_1 = lm(y ~ 1, data = sim_data)

# 2 linear model
fit_2 = lm(y ~ poly(x, degree = 1), data = sim_data)

# 3 quadratic model
fit_3 = lm(y ~ poly(x, degree = 2), data = sim_data)

# 4 overfitted model
fit_4 = lm(y ~ poly(x, degree = 9), data = sim_data)

# plot of the models + real output
plot(y ~ x, data = sim_data)
grid = seq(from = -6, to = 6, by = 0.1)

# naive model plot (red)
lines(grid, predict(fit_1, newdata = data.frame(x = grid)), 
      col = "red", lwd = 2, lty = 2)
# linear model plot (blue)
lines(grid, predict(fit_2, newdata = data.frame(x = grid)), 
      col = "blue", lwd = 2, lty = 3)
# quadratic model plot (green)
lines(grid, predict(fit_3, newdata = data.frame(x = grid)), 
      col = "green", lwd = 2, lty = 4)
# overfitted model (orange)
lines(grid, predict(fit_4, newdata = data.frame(x = grid)), 
      col = "orange", lwd = 2, lty = 5)
# original plot (black)
lines(grid, f(grid), col = "black", lwd = 5)

legend(x = 3.2, y = -40, 
       c("y ~ 1", "y ~ poly(x, 1)", "y ~ poly(x, 2)",  "y ~ poly(x, 9)", "real"), 
       col = c("red", "blue", "green", "orange", "black"), lty = c(2, 3, 4, 5, 1), lwd = 2)

# naive (red) and linear (blue) models are bad approximations
# quadratic (green) and high-order polynomials (orange) are good ones

###########################################################################################
# MSE DECOMPOSITION for a text point x0 = 5
# for each of the 4 models
# we simulate the experiment 1000 times

n_sims = 1000
n_models = 4
x0 = 5.0
predictions = matrix(0, nrow = n_sims, ncol = n_models)

for (i in 1:n_sims) {
  
  sim_data = get_sim_data(f, sample_size = 100)
  
  fit_1 = lm(y ~ 1, data = sim_data)
  fit_2 = lm(y ~ poly(x, degree = 1), data = sim_data)
  fit_3 = lm(y ~ poly(x, degree = 2), data = sim_data)
  fit_4 = lm(y ~ poly(x, degree = 9), data = sim_data)
  
  predictions[i, ] = c(
    predict(fit_1, newdata = data.frame(x = x0)),
    predict(fit_2, newdata = data.frame(x = x0)),
    predict(fit_3, newdata = data.frame(x = x0)),
    predict(fit_4, newdata = data.frame(x = x0))
  )
}

# out of sample evaluation
eps = rnorm(n = n_sims, mean = 0, sd = 100)
y0 = f(x0) + eps

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

bias = apply(predictions, 2, get_bias, f(x0))
variance = apply(predictions, 2, var)
mse = apply(predictions, 2, get_mse, y0)

mse

bias ^ 2 + variance + var(eps)

# each number corresponds to each of the 4 fits (models)
# the best one is of course the quadratic model

# as we increase the complexity of the model (overfitting)
# the MSE decreases in the testing set

# because we are simulating data to estimate the bias and variance
# the MSE decomposition is not exact
# if we increase the number of replications in the simulation, the formula would become exact

##################################################################################
# RESAMPLING TOOLS
#####################################################################################
# 1 DATA SPLITTING (validation set approach)
n = 100
data = get_sim_data(f, sample_size = n)
n.train = 0.8*n
ind.train = sample(1:n, n.train, replace = F)

train = data[ind.train,]
test = data[-ind.train,]

fit_1 = lm(y ~ 1, data = train)
fit_2 = lm(y ~ poly(x, degree = 1), data = train)
fit_3 = lm(y ~ poly(x, degree = 2), data = train)
fit_4 = lm(y ~ poly(x, degree = 9), data = train)

pred.test.1 = predict(fit_1, newdata = test)
pred.test.2 = predict(fit_2, newdata = test)
pred.test.3 = predict(fit_3, newdata = test)
pred.test.4 = predict(fit_4, newdata = test)

MSE.test.1 = mean((pred.test.1 - test$y)^2)
MSE.test.2 = mean((pred.test.2 - test$y)^2)
MSE.test.3 = mean((pred.test.3 - test$y)^2)
MSE.test.4 = mean((pred.test.4 - test$y)^2)

MSE.test.1 

MSE.test.2

MSE.test.3

MSE.test.4

# for just a given sample, as it was real data set
# we can see how the testing MSE increases with overfitted models, as expected

# 2 CROSS VALIDATION (leave-one-out)
MSE_test = matrix(0, nrow = 4, ncol = n)
for (i in 1:n){
  train = data[-i,]
  test = data[i,]
  
  fit_1 = lm(y ~ 1, data = train)
  fit_2 = lm(y ~ poly(x, degree = 1), data = train)
  fit_3 = lm(y ~ poly(x, degree = 2), data = train)
  fit_4 = lm(y ~ poly(x, degree = 9), data = train)
  
  pred.test.1 = predict(fit_1, newdata = test)
  pred.test.2 = predict(fit_2, newdata = test)
  pred.test.3 = predict(fit_3, newdata = test)
  pred.test.4 = predict(fit_4, newdata = test)
  
  MSE_test[1,i] = (pred.test.1 - test$y)^2
  MSE_test[2,i] = (pred.test.2 - test$y)^2
  MSE_test[3,i] = (pred.test.3 - test$y)^2
  MSE_test[4,i] = (pred.test.4 - test$y)^2 
}

# MSE:
apply(MSE_test, 1, mean)

# advantage of the LOOCV (Leave-One-Out Cross Validation) method 
# is that we make use all data points reducing potential bias


# the process is repeated as many times as there are data points
# resulting to higher execution time when n is extremely large

# additionally, we test the mode performance against one data point at each iteration
# this might result to higher variation in the prediciton error
# if some data points are outliers


# 3 K-FOLD CROSS-VALIDATION
# more efficient than LOOCV
# it often gives more accurate estimates of the test error than LOOCV

n.fold = 10
MSE_test = matrix(0, nrow = 4, ncol = n.fold)
data = data[sample(1:n),] 
folds = sample(rep(1:n.fold,length.out=nrow(data)),size=nrow(data),replace=T) # define the random folds

for (i in 1:n.fold){
  train = data[folds!=i,]
  test = data[folds==i,]
  
  fit_1 = lm(y ~ 1, data = train)
  fit_2 = lm(y ~ poly(x, degree = 1), data = train)
  fit_3 = lm(y ~ poly(x, degree = 2), data = train)
  fit_4 = lm(y ~ poly(x, degree = 9), data = train)
  
  pred.test.1 = predict(fit_1, newdata = test)
  pred.test.2 = predict(fit_2, newdata = test)
  pred.test.3 = predict(fit_3, newdata = test)
  pred.test.4 = predict(fit_4, newdata = test)
  
  MSE_test[1,i] = mean((pred.test.1 - test$y)^2)
  MSE_test[2,i] = mean((pred.test.2 - test$y)^2)
  MSE_test[3,i] = mean((pred.test.3 - test$y)^2)
  MSE_test[4,i] = mean((pred.test.4 - test$y)^2) 
}

# MSE:
apply(MSE_test, 1, mean)

# similar conclussions but more efficient


# 4 BOOTSTRAP
## 1- resample data with replacement
## 2- calculate the statistic of interest for each resample
## 3- repeat 1 and 2B times
## 4- use the bootstrap distribution inference

if (!require("bootstrap")){
  install.packages("bootstrap")
}
library(bootstrap)

if (!require("boot")){
  install.packages("boot")
}
library(boot)

# example:
# small randomized experiment were done with 16 mice
# 7 tro treatment group
# 9 to control group
# treatment was intended to prolong survival after a test surgery
# output = days of survival following surgery

mouse.c

mouse.t

mean(mouse.t)

mean(mouse.c)

dif_means = mean(mouse.t) - mean(mouse.c)

# difference in means is 30.6 days

# the treatmant has a higher survival rate then the control
# it is significant or noise?? sample is very small...

# CLASSIC T-TEST 
# assuming normal distribution and similar variances
t.test(mouse.t ,mouse.c ,alternative="greater", var.equal=TRUE)

# can we trust in those assumptions?
# what about the difference between the medians? 

median(mouse.t)

median(mouse.c)

dif_med = median(mouse.t) - median(mouse.c)
dif_med

# the difference in medians is 48 days

# dif_med > dif_means -> significant???

MedianDiff.boot = replicate(1000, median(sample(mouse.t,replace=TRUE)) - median(sample(mouse.c,replace=TRUE)))

hist(MedianDiff.boot)
abline(v=0, col="red2")


# standard deviation of median difference
sd(MedianDiff.boot)

# 95% Percentile CI
quantile(MedianDiff.boot, c(.025, .975))

# NOT SIGNIFICANT!