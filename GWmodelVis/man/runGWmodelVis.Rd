\name{runGWmodelVis}
\alias{runGWmodelVis}
\title{Launch the interactive visualization application provided by GWmodelVis.}
\usage{
runGWmodelVis(host = "127.0.0.1", port = NULL, launch.browser = TRUE)
}
\arguments{
\item{host}{Specifies the IPv4 address on which the application should listen. 
            If the shiny.host option is set, its value will be used;
             otherwise, the default is "127.0.0.1".}

\item{port}{Defines the TCP port on which the application will listen. 
            If no port is specified but the shiny.port option is set 
            (e.g., via options(shiny.port = XX)), 
            that port will be used; otherwise, a random port will be chosen.}

\item{launch.browser}{If true, the system's default web browser will automatically 
                     launch once the application starts (this is the default behavior in interactive sessions). 
                     Alternatively, this parameter can be assigned a function 
                     that will be called with the application's URL as its argument.}
}
\value{
No return value
}
\description{
Loads interactive user interface built using R 'shiny'.
}
\details{
Provide users with an interactive, multi-dimensional visualization of the geographically weighted models,
 enabling them to explore spatial relationships and gain insights from multiple perspectives.}
\examples{
    if(interactive()){
        runGWmodelVis()
    }
}
