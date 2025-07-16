### R code from vignette source 'ground-control-to-ruimtehol.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+   ")
options(prompt = " ", continue = "   ")
set.seed(123456789)


###################################################
### code chunk number 2: ground-control-to-ruimtehol.Rnw:81-84
###################################################
library(ruimtehol)
data("dekamer", package = "ruimtehol")
str(dekamer)


###################################################
### code chunk number 3: ground-control-to-ruimtehol.Rnw:99-107
###################################################
dekamer$x <- strsplit(dekamer$question, "\\W")
dekamer$x <- lapply(dekamer$x, FUN = function(x) x[x != ""])
dekamer$x <- sapply(dekamer$x, FUN = function(x) paste(x, collapse = " "))
dekamer$x <- tolower(dekamer$x)
dekamer$y <- strsplit(dekamer$question_theme, split = ",")
dekamer$y <- lapply(dekamer$y, FUN=function(x) gsub(" ", "-", x))
dekamer$x[1:2]
dekamer$y[1:2]


###################################################
### code chunk number 4: ground-control-to-ruimtehol.Rnw:115-123
###################################################
set.seed(123456789)
model <- embed_tagspace(x = dekamer$x, y = dekamer$y, 
                        early_stopping = 0.8, validationPatience = 10,
                        dim = 50, 
                        lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                        similarity = "cosine", negSearchLimit = 50,
                        ngrams = 2, minCount = 2, bucket = 100000,
                        maxTrainTime = 2 * 60)


###################################################
### code chunk number 5: ground-control-to-ruimtehol.Rnw:125-126
###################################################
model


###################################################
### code chunk number 6: ground-control-to-ruimtehol.Rnw:146-147
###################################################
plot(model)


###################################################
### code chunk number 7: ground-control-to-ruimtehol.Rnw:153-156
###################################################
dict <- starspace_dictionary(model)
str(dict)
length(dict$labels)


###################################################
### code chunk number 8: ground-control-to-ruimtehol.Rnw:165-167
###################################################
emb <- as.matrix(model)
dim(emb)


###################################################
### code chunk number 9: ground-control-to-ruimtehol.Rnw:172-175
###################################################
emb_words  <- as.matrix(model, type = "words")
emb_labels <- as.matrix(model, type = "labels", prefix = FALSE)
e <- starspace_embedding(model, x = c("__label__VERVOERBELEID", "geld"), type = "ngram")


###################################################
### code chunk number 10: ground-control-to-ruimtehol.Rnw:180-181
###################################################
e <- starspace_embedding(model, c("nationale loterij"), type = "ngram")


###################################################
### code chunk number 11: ground-control-to-ruimtehol.Rnw:188-192
###################################################
text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...",
          "de migranten komen naar europa de asielcentra ...")
emb_text <- starspace_embedding(model, text)
dim(emb_text)


###################################################
### code chunk number 12: ground-control-to-ruimtehol.Rnw:199-200
###################################################
predict(model, "de migranten komen naar europa de asielcentra ...")


###################################################
### code chunk number 13: ground-control-to-ruimtehol.Rnw:205-206
###################################################
embedding_similarity(emb_text, emb_labels, type = "cosine", top_n = 5)


###################################################
### code chunk number 14: ground-control-to-ruimtehol.Rnw:211-212
###################################################
starspace_knn(model, "de migranten komen naar europa de asielcentra ...", k = 5)


###################################################
### code chunk number 15: ground-control-to-ruimtehol.Rnw:219-232
###################################################
targetdocs <- c("__label__FISCALITEIT", 
                "__label__OVERHEIDSADMINISTRATIE", 
                "__label__MIGRATIEBELEID", 
                "__label__POLITIE", 
                "__label__BUITENLANDS-BELEID", 
                "__label__ECONOMISCH-BELEID", 
                "de migranten komen naar europa ZZZ", 
                "__label__PERSONEEL")
predict(model, "de migranten komen naar europa de asielcentra ...", 
        basedoc = targetdocs)
embedding_similarity(
  starspace_embedding(model, "de migranten komen naar europa de asielcentra ..."),
  starspace_embedding(model, targetdocs), top_n = 3)


###################################################
### code chunk number 16: ground-control-to-ruimtehol.Rnw:240-242
###################################################
starspace_save_model(model, file = "textspace.ruimtehol")
model <- starspace_load_model("textspace.ruimtehol")


###################################################
### code chunk number 17: ground-control-to-ruimtehol.Rnw:245-246
###################################################
invisible(file.remove("textspace.ruimtehol"))


###################################################
### code chunk number 18: ground-control-to-ruimtehol.Rnw:262-275
###################################################
set.seed(321)
dekamer <- dekamer[order(rnorm(n = nrow(dekamer))), ]
X <- dekamer$x
Y <- dekamer$y
X[1:250]   <- NA
Y[251:500] <- NA
model <- embed_tagspace(x = X, y = Y, 
                        early_stopping = 0.8, validationPatience = 10,
                        dim = 50, 
                        lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                        similarity = "cosine", negSearchLimit = 50,
                        ngrams = 2, minCount = 2,
                        maxTrainTime = 2 * 60)


###################################################
### code chunk number 19: ground-control-to-ruimtehol.Rnw:298-303
###################################################
pretrained <- matrix(data = rnorm(1000 * 100), nrow = 1000, ncol = 100, 
                     dimnames = list(term = sprintf("word%s", 1:1000)))
model <- starspace(embeddings = pretrained, 
                   similarity = "cosine", p = 0.5, ngrams = 1, trainMode = 5)
predict(model, newdata = c("word5 word1 word5 word3"), type = "knn")


###################################################
### code chunk number 20: ground-control-to-ruimtehol.Rnw:309-315
###################################################
set.seed(321)
model <- embed_wordspace(dekamer$x, 
                         dim = 50, ws = 7, epoch = 5, ngrams = 2, adagrad = FALSE,
                         margin = 0.8, negSearchLimit = 10,
                         maxTrainTime = 2 * 60)
pretrained_words  <- as.matrix(model)


###################################################
### code chunk number 21: ground-control-to-ruimtehol.Rnw:320-328
###################################################
labels            <- sort(unique(unlist(dekamer$y)))
pretrained_labels <- matrix(data = rnorm(n = length(labels) * 50, 
                                         mean = mean(pretrained_words), 
                                         sd = sd(pretrained_words)), 
                            nrow = length(labels), 
                            ncol = 50, 
                            dimnames = list(term = sprintf("__label__%s", labels)))
pretrained        <- rbind(pretrained_words, pretrained_labels)


###################################################
### code chunk number 22: ground-control-to-ruimtehol.Rnw:334-344
###################################################
set.seed(321)
model <- embed_tagspace(x = dekamer$x, y = dekamer$y, 
                        embeddings = pretrained,
                        early_stopping = 0.8, validationPatience = 10,
                        dim = 50, 
                        lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                        similarity = "cosine", negSearchLimit = 50,
                        ngrams = 2, minCount = 2,
                        maxTrainTime = 2 * 60)
embedding <- as.matrix(model)


###################################################
### code chunk number 23: ground-control-to-ruimtehol.Rnw:347-349
###################################################
plot(model)
starspace_knn(model, "__label__FISCALITEIT", k = 10)


