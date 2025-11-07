#' @title Tie removal
#' @description Removes ties from by adding noise at tied times
#' @param fr Data on long format
#' @param id Name of column in \code{dataFr} that identifies individuals
#' @param from Name of the variable that contains the name of start state for
#' each at-risk interval
#' @param to Name of column with stop time of the at risk period
#' @author Pål Christie Ryalen <p.c.ryalen@medisin.uio.no>
#' @export
#' @example /inst/examples/addNoiseAtEventTimes_example.R
addNoiseAtEventTimes <- function(fr, id, from, to){

        if(!("data.table" %in% class(fr))){
                stop("Error! Try again with input data as data.table")
        }

        if(!all(c("id","from","to") %in% names(fr)) ){
                stop("Error! Try again with input data on long format
                     with 'id', 'from', and  'to' in the column names.")
        }

        fr[,"incrementTimes":=diff(c(0,to)),by="id"]
        fr[,"nrws":=.N,by="id"]
        maxRows = max(fr$nrws)
        minIncrement = min(fr$incrementTimes)


        fr$noise <- runif(nrow(fr)) * minIncrement/(maxRows * 2)
        with(fr,
                fr[,"noise":=cumsum(noise),by="id"]
        )
        fr$noiseFrom <- fr$from
        fr$noiseTo <- fr$to
        with(fr, {
                fr[,"noiseFrom":= noiseFrom + c(0,noise[-length(noise)]),by="id"]
                fr[,"noiseTo" := noiseTo + noise,by="id"]
        })
        fr$from <- fr$noiseFrom
        fr$to <- fr$noiseTo

        fr <- subset(fr,select = !(names(fr) %in% c("noise","noiseFrom","noiseTo","incrementTimes","nrws") ))


        return(fr)

}
