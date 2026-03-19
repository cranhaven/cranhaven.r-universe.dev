library(blockcluster)

library(data.table)

set.seed(3101)

data("binarydata", package = "blockcluster")
clustering <- cocluster(binarydata, datatype = "binary", nbcocluster =
				c(5, 3))
colclasses <- data.table(
		RK = colnames(clustering@data),
		colclass = clustering@colclass + 1
)
data_slot <- as.data.table(clustering@data)

for (rownum in 1:clustering@nbcocluster[1]) {
	row_idx <- which(clustering@rowclass == (rownum - 1))
	for (colnum in 1:clustering@nbcocluster[2]) {
		col_names <- colclasses[colclass ==
						colnum]$RK#which(clustering@colclass == colnum)
		temp <- data_slot[row_idx, ..col_names]
		rs <- rowSums(temp)
		
		# check: fraction of rows should equal
		clusteringqrowproportions[rownum - 1]
		dim(temp)[1] / dim(data_slot)[1]
		clustering@rowproportions
		clustering@rowproportions[rownum]
		# doesn't fit
		
		# check: fraction columns should equal
		clustering@colproportions[colnum - 1]
		dim(temp)[2] / dim(data_slot)[2]
		clustering@columnproportions[colnum]
		# fits
		
		
		# print checks:
		writeLines(paste0("for ", rownum, " and ", colnum,"\n",
						" fraction of rows is ", dim(temp)[1] /
								dim(data_slot)[1],
						" and should be ",
						clustering@rowproportions[rownum], "\n",
						", fraction of cols is ", dim(temp)[2] /
								dim(data_slot)[2],
						" and should be ",
						clustering@columnproportions[colnum]
				))
	}
}