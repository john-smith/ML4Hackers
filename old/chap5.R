library(ggplot2)
ages <- read.csv("data/longevity.csv")

ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) + geom_density() + facet_grid(Smokes ~ .)

guess <- 73
mean((ages$AgeAtDeath - guess) ^ 2)

guess.accuracy <- data.frame()
for(guess in seq(63, 83)) {
    prediction.error <- mean((ages$AgeAtDeath - guess) ^ 2)
    guess.accuracy <- rbind(guess.accuracy, data.frame(Guess = guess, Error = prediction.error))
}
ggplot(guess.accuracy, aes(x = Guess, y = Error)) + geom_point() + geom_line()

constant.guess <- mean(ages$AgeAtDeath)
sqrt(mean((ages$AgeAtDeath - constant.guess) ^ 2))
mean((ages$AgeAtDeath - constant.guess) ^ 2)

smokers.guess <- mean(ages[which(ages$Smokes == 1), 2])
non.smokers.guess <- mean(ages[which(ages$Smokes == 0), 2])

ages <- transform(ages, NewPrediction = ifelse(Smokes == 0, non.smokers.guess, smokers.guess))
sqrt(mean((ages$AgeAtDeath - ages$NewPrediction) ^ 2))

heights.weights <- read.csv("data/01_heights_weights_genders.csv", header = T, sep = ",")
#heights.weights <- read.csv("data/01_heights_weights_genders.csv")

ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth(method = 'lm')

fitted.regression <- lm(Weight ~ Height, data = heights.weights)
coef(fitted.regression)

intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]

predict(fitted.regression)

true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)

residuals(fitted.regression)

plot(fitted.regression, which = 1)
#plot(fitted.regression)
#plot(fitted.regression, which = 6)


x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)
plot(fitted.regression, which = 1)

x.y <- data.frame(X = x, Y = y)
ggplot(x.y, aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm')

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
sum(squared.errors)

mse <- mean(squared.errors)
mse
rmse <- sqrt(mse)
rmse

hw.mean <- mean(heights.weights$Weight)
mean.rmse <- sqrt(mean((heights.weights$Weight - hw.mean) ^ 2))
mean.rmse

fitted.regression <- lm(Weight ~ Height, data = heights.weights)
errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
model.rmse <- sqrt(mean(squared.errors))
model.rmse

r2 <- 1 - (model.rmse / mean.rmse)
r2

top.1000.sites <- read.csv('data/top_1000_sites.tsv', sep='\t', stringsAsFactors=F)
ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) + geom_point()
ggplot(top.1000.sites, aes(x = PageViews)) + geom_density()
ggplot(top.1000.sites, aes(x = log(PageViews))) + geom_density()
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) + geom_point()

ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) + geom_point() + geom_smooth(method = 'lm', se = F)

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
summary(lm.fit)

lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish, data = top.1000.sites)
summary(lm.fit)

lm.fit <- lm(log(PageViews) ~ HasAdvertising, data = top.1000.sites)
summary(lm.fit)$r.squared
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
summary(lm.fit)$r.squared
lm.fit <- lm(log(PageViews) ~ InEnglish, data = top.1000.sites)
summary(lm.fit)$r.squared

x <- 1:10
y <- x ^ 2

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm', se = F)

cor(x, y)
coef(lm(scale(y) ~ scale(x)))
