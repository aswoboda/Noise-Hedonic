
# How much do our noise coefficients change as we increase the bandwidth to something "much larger" if we are 
# concerned with inference/accuracy of marginal effects.

# Correlation of coefficients by bandwidth? 500, 650, 1000, 2000

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/TimeLag12months/Sales20052010LWRmodelAirMean3-2014-03-19.RData")

# make a dataset of the regression coefficients for noise with different bandwidths

noisebetabyk = data.frame(output[["beta.Air_Mean"]][, c("k500", "k650", "k1000", "k2000")])
names(noisebetabyk)

cor(noisebetabyk)

require(graphics)
smoothScatter(noisebetabyk$k650, noisebetabyk$k500,
              xlim = c(-0.015, 0.005), ylim = c(-0.015, 0.005))
abline(0, 1)
abline(lm(k500 ~ k650, data = noisebetabyk), col = "red")

smoothScatter(noisebetabyk$k1000, noisebetabyk$k500,
              xlim = c(-0.015, 0.005), ylim = c(-0.015, 0.005))
abline(0, 1)
abline(lm(k500 ~ k1000, data = noisebetabyk), col = "red")

smoothScatter(noisebetabyk$k2000, noisebetabyk$k500,
              xlim = c(-0.015, 0.005), ylim = c(-0.015, 0.005))
abline(0, 1)
abline(lm(k500 ~ k2000, data = noisebetabyk), col = "red")
