# Creating compound string 's'
stretch1 <- paste0(rep("AATG", 10), collapse = "")
stretch2 <- paste0(rep("ATCG", 4), collapse = "")

s <- paste0(stretch1, stretch2)

# Return BLMM only
BLMM(s, motifLength = 4, returnType = "numeric")

# Return BLMM and motif of stretch
BLMM(s, motifLength = 4, returnType = "string")

# Return all blocks of 's'
BLMM(s, motifLength = 4, returnType = "fulllist")
