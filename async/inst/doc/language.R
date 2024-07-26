## -----------------------------------------------------------------------------
library(async)
pausables() |> vapply(as.character, "") |> sort()

## ---- error=TRUE--------------------------------------------------------------
L <- run( list( if (x) yield(y) else yield(x) ) )

## -----------------------------------------------------------------------------
# given a sequence of sequences, concatenate them like c() into one sequence
chain <- function(sequences) {
  force(sequences)
  gen({
    for (seq in sequences) {
      yieldFrom(seq)
    }
  })
}

## -----------------------------------------------------------------------------
chain <- gen(function(sequences) {
  for(seq in sequences) {
    yieldFrom(seq)
  }
})

## ---- error=TRUE--------------------------------------------------------------
names <- c("Ana", "Bob", "Charlie")
g <- gen({
  greet <- function(name) yield(paste0("Good morning, ", name))
  for (i in names) {
    greet(name)
  }
})
as.list(g)

## -----------------------------------------------------------------------------
names <- c("Ana", "Bob", "Charlie")
greet <- gen(function(name) {
  yield(paste0("Good morning, ", name))
})
run(type="", {
  for (n in names) {
    yieldFrom(greet(n))
  }
})

## ---- echo=FALSE--------------------------------------------------------------
download_file <- function(url) async:::mock_promise()

## -----------------------------------------------------------------------------
file_dataset <- async({
  filename <- await( download_file(url) )
  switch(getExtension(filename),
      "txt"=readTxt(filename),
      "csv"=tryCatch(readCSV(filename), error=goto("txt")),
      "json"={
          if (!await(validateSchema(filename))) goto("txt")
          tryCatch(read_json(filename), error=goto("txt"))
      },
      "zip"= {
        unzipped <- unzip_async(filename) |> await()
        filename <- unzipped
        on.exit(unlink(unzipped))
        goto(getExtension(unzipped))
      }
  )
})

## ----eval=FALSE---------------------------------------------------------------
#  graphAsync(file_dataset, type="svg")

## ---- echo=FALSE--------------------------------------------------------------
with_timeout <- function(...)async:::mock_promise()

## -----------------------------------------------------------------------------
  as <- async({
    tryCatch(
      await(with_timeout(x, 10)),
      error=return("Value was not available"))
  })

## -----------------------------------------------------------------------------
as <- async({
  await(with_timeout(promise()),
        "Value was not available")
})

## -----------------------------------------------------------------------------
fileLines <- gen(function(...) {
  f <- file(..., open="rt")
  on.exit(close(f))
  while (length(line <- readLines(f, 1)) > 0) yield(line)
})
length(as.list(fileLines("language.Rmd")))

## ---- error=TRUE--------------------------------------------------------------
x <- run({
    on.exit({return("this instead")})
    stop("an error")
})
x

## -----------------------------------------------------------------------------
finished <- FALSE
g <- gen({
  on.exit(finished <<- TRUE)
  for (i in 1:10) yield(i)
})
for (j in 1:10) nextOr(g, break)
finished
nextOr(g, "stop")
finished

