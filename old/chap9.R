set.seed(851982)
ex.matrix <- matrix(sample(c(-1, 0, 1), 24 ,replace = T), nrow = 4, ncol = 6)
row.names(ex.matrix) <- c('A', 'B', 'C', 'D')
colnames(ex.matrix) <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')

t(ex.matrix)
ex.mult <- ex.matrix %*% t(ex.matrix)
ex.mult

t(ex.matrix) %*% ex.matrix

sqrt(sum((ex.mult[1, ] - ex.mult[4, ]) ^ 2))

ex.dist <- dist(ex.mult)
ex.dist

dist(ex.mult, upper = T)
dist(t(ex.matrix))

ex.mds <- cmdscale(ex.dist)
plot(ex.mds, type = 'n')
text(ex.mds, c('A', 'B', 'C', 'D'))

#cmdscale(ex.dist, k = 3)

library(foreign)
library(ggplot2)

data.dir <- "data/roll_call/"
data.files <- list.files(data.dir)
data.files

rollcall.data <- lapply(data.files, function(f) read.dta(paste(data.dir, f, sep = ''), convert.factors = F))

dim(rollcall.data[[1]])
head(rollcall.data[[1]])
head(rollcall.data[[1]][, 1:10])

rollcall.simplified <- function(df) {
    no.pres <- subset(df, state < 99)
    for(i in 10:ncol(no.pres)) {
        no.pres[, i] <- ifelse(no.pres[, i] > 6, 0, no.pres[, i])
        no.pres[, i] <- ifelse(no.pres[, i] > 0 & no.pres[, i] < 4 , 1, no.pres[, i])
        no.pres[, i] <- ifelse(no.pres[, i] > 3, -1, no.pres[, i])
    }
    return(as.matrix(no.pres[, 10:ncol(no.pres)]))
}

rollcall.simple <- lapply(rollcall.data, rollcall.simplified)
rollcall.dist <- lapply(rollcall.simple, function(m) dist(m %*% t(m)))
rollcall.mds <- lapply(rollcall.dist, function(d) as.data.frame((cmdscale(d, k = 2)) * -1))

congresses <- 101:111

for(i in 1:length(rollcall.mds)) {
    names(rallcall.mds[[i]]) <- c('x', 'y')
    congress <- subset(rollcall.data[[i]], state < 99)
    congress.party <- as.factor(congress$party)
    congress.names <- sapply(as.character(congress$name), function(n) strsplit(n, "[, ]")[[1]][1])
    rollcall.mds[[i]] <- transform(rollcall.mds[[i]], name = congress.names,
                                   party = congress.party, congress = congresses[i])
}



head(rollcall.mds[[1]])

cong.110 <- rollcall.mds[[9]]

#うまく動かなかった
#base.110 <- ggplot(cong.110, aes(x = x, y = y)) +
#    geom_point(aes(shape = party, alpha = 0.75, size = 2)) +
#    scale_size(to = c(2, 2), legend = F) +
#    scale_alpha(legend = F) +
#    theme_bw() +
#    opts(axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.text.y = theme_blank(),
#         title = "Roll Call Vote MDS Clustering for 110th U.S. Senate",
#         panel.grid.major = theme_blank()) +
#    xlab("") +
#    ylab("") +
#    scale_shape(name =  "Party", breaks = c("100", "200", "328"), labels = c("Dem.", "Rep.", "Ind."), solid = F) +
#    scale_color_manual(name = "Party", values = c("100" = "black", "200" = "dimgray", "328" = "grey"),
                       breaks = c("100", "200", "328"), labels = c("Dem.", "Rep.", "Ind."))

print(base.110 + geom_point(aes(shape = party, alpha = 0.75, size = 2)))
print(base.110 + geom_text(aes(color = party, alpha = 0.75, label = cong.110$name, size = 2)))

ggplot(cong.110, aes(x = x, y = y)) + geom_text(aes(color = party, alpha = 0.75, label = cong.110$name, size = 2))


all.mds <- do.call(rbind, rollcall.mds)
ggplot(all.mds, aes(x = x, y = y)) +
    geom_point(aes(color = party)) +
    facet_wrap(~congress)
    #geom_text(aes(color = party, alpha = 0.75, label = cong.110$name, size = 2))
