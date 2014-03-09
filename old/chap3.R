library(tm)
library(ggplot2)

spam.path <- 'data/spam/'
spam2.path <- 'data/spam_2/'
easyham.path <- 'data/easy_ham/'
easyham2.path <- 'data/easy_ham_2/'
hardham.path <- 'data/hard_ham/'
hardham2.path <- 'data/hard_ham2/'

get.msg <- function(path) {
    con <- file(path, open="rt", encoding="latin1")
    text <- readLines(con)
    msg <- text[seq(which(text=='')[1] + 1, length(text), 1)]
    close(con)
    return(paste(msg, collapse="\n"))
}

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep="")))

get.tdm <- function(doc.vec) {
    doc.corpus <- Corpus(VectorSource(doc.vec))
    control <- list(stopwords=T, removePunctuation=T, removeNumbers=T, minDocFreq=2)
    doc.tdm <- TermDocumentMatrix(doc.corpus, control)
    return(doc.tdm)
}

spam.tdm <-get.tdm(all.spam)

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors=F)
names(spam.df) <- c('term', 'frequency')
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) {
                              length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
spam.density <- spam.df$frequency / sum(spam.df$frequency)
spam.df <- transform(spam.df, density=spam.density, occurrence=spam.occurrence)

head(spam.df[with(spam.df, order(-occurrence)), ])

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)], function(p) get.msg(paste(easyham.path, p, sep="")))

easyham.tdm <-get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts), as.numeric(easyham.counts)), stringsAsFactors=F)
names(easyham.df) <- c('term', 'frequency')
easyham.df$frequency <- as.numeric(easyham.df$frequency)

easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                          function(i) {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                          })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)
easyham.df <- transform(easyham.df, density=easyham.density, occurrence=easyham.occurrence)

head(easyham.df[with(easyham.df, order(-occurrence)), ])

classify.email <- function(path, training.df, prior = 0.5, c=1e-6) {
    prior <- log(prior)
    c <- log(c)
    
    msg <- get.msg(path)
    msg.tdm <- get.tdm(msg)
    msg.freq <- rowSums(as.matrix(msg.tdm))
    msg.match <- intersect(names(msg.freq), training.df$term)
    if(length(msg.match) < 1) {
        return(prior + (c * length(msg.freq)))
    } else {
        match.probs <- log(training.df$occurrence[match(msg.match, training.df$term)])
        return(prior + sum(match.probs) + (c * (length(msg.freq) - length(msg.match))))
    }
}


hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(paste(hardham.path, p, sep=""), training.df=spam.df))

hardham.hamtest <- sapply(hardham.docs,
                           function(p) classify.email(paste(hardham.path, p, sep=""), training.df=easyham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest, T, F)
summary(hardham.res)

spam.classifier <- function(path) {
    pr.spam <- classify.email(path, spam.df)
    pr.ham <- classify.email(path, easyham.df)
    return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}


