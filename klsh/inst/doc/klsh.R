## ---- echo=TRUE, message=FALSE, knitr::opts_chunk$set(cache=TRUE)-------------
library(blink)
library(plyr)
library(klsh)
data(RLdata500)
head(RLdata500)
data.500 <- RLdata500[-c(2,4)]
head(data.500)

## -----------------------------------------------------------------------------
set.seed(1234)
klsh.blocks <- klsh(data.500, p=100, num.blocks=5, k=2)

## -----------------------------------------------------------------------------
confusion.from.blocking(klsh.blocks, true_ids = identity.RLdata500)
confusion.from.blocking(klsh.blocks, recall.only=TRUE, true_ids = identity.RLdata500)
reduction.ratio.from.blocking(klsh.blocks)

## -----------------------------------------------------------------------------
twohundred_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=200,k=4)
onehundred_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=100,k=4)
fifty_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=50,k=4)
twentyfive_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=25,k=4)
ten_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=10,k=4)
five_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=5,k=4)
three_blocks_for_2e4_recs <- klsh(p=100,data.500, num.blocks=3,k=4)

blockings_k4 <- list(
						twohundred_blocks_for_2e4_recs,
						onehundred_blocks_for_2e4_recs,
						fifty_blocks_for_2e4_recs,
						twentyfive_blocks_for_2e4_recs,
						ten_blocks_for_2e4_recs,
						five_blocks_for_2e4_recs,
						three_blocks_for_2e4_recs)
confusions_k4 <- sapply(blockings_k4, confusion.from.blocking, recall.only=TRUE, true_ids = identity.RLdata500)	
reduction.ratio.from.blocking_k4 <- sapply(blockings_k4, reduction.ratio.from.blocking)

## -----------------------------------------------------------------------------
twohundred_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=200,k=3)
onehundred_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=100,k=3)
fifty_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=50,k=3)
twentyfive_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=25,k=3)
ten_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=10,k=3)
five_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=5,k=3)
three_blocks_for_2e4_recs_3 <- klsh(p=100,data.500, num.blocks=3,k=3)

blockings_k3 <- list(
						twohundred_blocks_for_2e4_recs_3,
						onehundred_blocks_for_2e4_recs_3,
						fifty_blocks_for_2e4_recs_3,
						twentyfive_blocks_for_2e4_recs_3,
						ten_blocks_for_2e4_recs_3,
						five_blocks_for_2e4_recs_3,
						three_blocks_for_2e4_recs_3)

confusions_k3 <- sapply(blockings_k3, confusion.from.blocking, recall.only=TRUE, true_ids = identity.RLdata500)
reduction.ratio.from.blocking_k3 <- sapply(blockings_k3, reduction.ratio.from.blocking)

## -----------------------------------------------------------------------------
twohundred_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=200,k=2)
onehundred_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=100,k=2)
fifty_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=50,k=2)
twentyfive_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=25,k=2)
ten_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=10,k=2)
five_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=5,k=2)
three_blocks_for_2e4_recs_2 <- klsh(p=100,data.500, num.blocks=3,k=2)

blockings_k2 <- list(
						twohundred_blocks_for_2e4_recs_2,
						onehundred_blocks_for_2e4_recs_2,
						fifty_blocks_for_2e4_recs_2,
						twentyfive_blocks_for_2e4_recs_2,
						ten_blocks_for_2e4_recs_2,
						five_blocks_for_2e4_recs_2,
						three_blocks_for_2e4_recs_2)	

confusions_k2 <- sapply(blockings_k2, confusion.from.blocking, recall.only=TRUE, true_ids = identity.RLdata500)
reduction.ratio.from.blocking_k2 <- sapply(blockings_k2, reduction.ratio.from.blocking)

## -----------------------------------------------------------------------------
twohundred_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=200,k=1)
onehundred_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=100,k=1)
fifty_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=50,k=1)
twentyfive_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=25,k=1)
ten_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=10,k=1)
five_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=5,k=1)
three_blocks_for_2e4_recs_1 <- klsh(p=100,data.500, num.blocks=3,k=1)

blockings_k1 <- list(
						twohundred_blocks_for_2e4_recs_1,
						onehundred_blocks_for_2e4_recs_1,
						fifty_blocks_for_2e4_recs_1,
						twentyfive_blocks_for_2e4_recs_1,
						ten_blocks_for_2e4_recs_1,
						five_blocks_for_2e4_recs_1,
						three_blocks_for_2e4_recs_1)

confusions_k1 <- sapply(blockings_k1, confusion.from.blocking, recall.only=TRUE, true_ids = identity.RLdata500)
reduction.ratio.from.blocking_k1 <- sapply(blockings_k1, reduction.ratio.from.blocking)

## ---- fig.show="hold", fig.cap="The recall versus the total number of blocks after running KLSH using k=1, 2, 3, 4.", fig.height = 4, fig.width = 5, fig.align = "center"----
library(ggplot2)

plot_dat <- rbind(
  data.frame(k = "4", block_length = unlist(lapply(blockings_k4, length)), recall = confusions_k4, reduction_ratio = reduction.ratio.from.blocking_k4),
  data.frame(k = "3", block_length = unlist(lapply(blockings_k3, length)), recall = confusions_k3, reduction_ratio = reduction.ratio.from.blocking_k3),
  data.frame(k = "2", block_length = unlist(lapply(blockings_k2, length)), recall = confusions_k2, reduction_ratio = reduction.ratio.from.blocking_k2),
  data.frame(k = "1", block_length = unlist(lapply(blockings_k1, length)), recall = confusions_k1, reduction_ratio = reduction.ratio.from.blocking_k1)
)

ggplot(plot_dat) +
  geom_point(aes(block_length, recall, colour = k)) +
  geom_line(aes(block_length, recall, colour = k, group = k)) +
  xlab("Total Number of Blocks") +
  ylab("Recall") +
  theme_bw(base_family = "serif") +
  ylim(c(0.4, 1))

## ---- fig.show="hold", fig.cap="The recall versus reduction ratio after running KLSH using k=1,2,3,4.", fig.height = 4, fig.width = 5, fig.align = "center"----
ggplot(plot_dat) +
  geom_point(aes(block_length, reduction_ratio, colour = k)) +
  geom_line(aes(block_length, reduction_ratio, colour = k, group = k)) +
  xlab("Total Number of Blocks") +
  ylab("Reduction Ratio") +
  theme_bw(base_family = "serif")

