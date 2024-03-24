

## ##' @title is history tracking on
## ##' @description Returns a logical value indicating whether history is
## ##'     currently being tracked by the histry package
## ##' @return logical indicating whether history is currently being
## ##'     tracked.
## ##' @export
## trackingHistory = function() {
##     opts = histropts()
##     is(opts$history, "VirtHistoryTracker") && opts$history$tracking
## }

## ##' @title Automatically track history within an R session
## ##' @param tracker VirtHistoryTracker subclass or NULL. For NULL, if
## ##' a default tracker is set, toggle tracking with default tracker, otherwise
## ##' set a default tracker. For a VirtHistoryTracker (subclass) set as
## ##' the default tracker and turn it on.
## ##' @export
## trackHistory = function( tracker = NULL) {
##     opts = histropts()
##     if(!missing(tracker) && is(tracker, "VirtHistoryTracker")) {
##         set_histropts(tracker)
##         if(!trackingHistory())
##             tracker$toggleTracking()
##         message("Tracking session history. To turn off tracking call trackHistory().")
##     } else if (is.null(tracker) && is.null(opts$history)) {
##         message("Starting automatic session history tracking. To turn off tracking call trackHistory().")
##         opts$history = historyTracker("auto_history")
##     } else {

##         if(trackingHistory()) {
##             message("Suspending automatic history tracking. To turn it back on call trackHistory() again.")
##         } else {
##             message("Reinstating history tracking with existing default tracker.")
##         }
##         opts$history$toggleTracking()
##     }
##     invisible(NULL)

## }

