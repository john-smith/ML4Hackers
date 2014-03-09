data.file <- file.path("data/01_heights_weights_genders.csv")
heights.weights <- read.csv(data.file, header=T)

head(heights.weights)
summary(heights.weights$Weight)
summary(heights.weights$Height)

heights <- with(heights.weights, Height)
head(heights)
summary(heights)

heights2 <- heights.weights$Height
summary(heights2)


my.mean <- function(x) {
    return(sum(x) / length(x))
}
round(my.mean(heights), digits=2)

my.median <- function(x) {
    sorted.x <- sort(x)

    if(length(x) %% 2 == 0) {
        indices <- c(length(x) / 2, length(x) / 2 + 1)
        return(mean(sorted.x[indices]))
    } else {
        index <- ceiling(length(x) / 2)
        return(sorted.x[index])
    }
}
round(my.median(heights), digits=2)

my.vector <- c(0, 100)
my.vector
mean(my.vector)
median(my.vector)

my.vector <- c(0, 0, 100)
my.vector
mean(my.vector)
median(my.vector)

my.mean(heights)
my.median(heights)
mean(heights) - my.mean(heights)
median(heights) - my.median(heights)

min(heights)
max(heights)
c(min(heights), max(heights))
range(heights)

quantile(heights)
quantile(heights, probs=seq(0, 1, 0.01))

c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))
c(quantile(heights, probs=0.025), quantile(heights, probs=0.975))

my.var <- function(x) {
    m <- mean(x)
    return(sum((x - m) ^ 2) / length(x))
}
my.var(heights)
var(heights)
my.var(heights) - var(heights)

my.var <- function(x) {
    m <- mean(x)
    return(sum((x - m) ^ 2) / (length(x) - 1))
}
my.var(heights)
var(heights)
my.var(heights) - var(heights)

c(mean(heights) - var(heights), mean(heights) + var(heights))
range(heights)

my.sd <- function(x) {
    return(sqrt(my.var(x)))
}
my.sd(heights)
sd(heights)
my.sd(heights) - sd(heights)

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
range(heights)
c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))

library("ggplot2")
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 5)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 0.001)
ggplot(heights.weights, aes(x = Height)) + geom_density()
ggplot(heights.weights, aes(x = Height, fill=Gender)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill=Gender)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill=Gender)) + geom_density() + facet_grid(Gender ~ .)

m <- 0
s <- 1
ggplot(data.frame(X=rnorm(100000, m, s)), aes(x = X)) + geom_density()

m <- 2
s <- 10
ggplot(data.frame(X=rnorm(100000, m, s)), aes(x = X)) + geom_density()

set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)

range(normal.values)
range(cauchy.values)

ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

n <- data.frame(X = normal.values)
n$type = "norm"
head(n)

c <- data.frame(X = cauchy.values)
c$type = "cauchy"
head(c)

n.c <- rbind(n, c)
ggplot(n.c, aes(x = X, fill=type)) + geom_density() + facet_grid(type ~ .)

gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()

ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

ggplot(heights.weights[1:20, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:200, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:2000, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point()

heigths.weights <- transform(heights.weights, Male = ifelse(Gender == 'Male', 1, 0))
head(heigths.weights)

logit.model <- glm(Male ~ Weight + Height, data = heigths.weights, family = binomial(link = 'logit'))

ggplot(heigths.weights, aes(x = Height, y = Weight, color = Gender))+
    geom_point()+
    stat_abline(intercept =  - coef(logit.model)[1] / coef(logit.model)[2],
                slope = - coef(logit.model)[3] / coef(logit.model)[2],
                geom = 'abline',
                color = 'black')
