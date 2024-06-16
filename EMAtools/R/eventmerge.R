#' Merge Mobile EMA (mEMA) event-level data into momentary data
#'
#' This allows you to merge event-level data (e.g., yes/no to an event) into momentary data, placing the event with the most recent momentary datapoint before the event
#' @param MOMENTARY a dataframe with momentary (i.e., level-1) data exported from mEMA, should have the following columns (all mEMA default names): KEY, instance_key, subject_id,timestamp
#' @param EVENT a dataframe with event data (i.e., level-2) that should have the following columns (all mEMA default): instance_key	subject_id	respondent_id	timestamp	local_date	survey_id	timezone_offset	as well as an "event" column in the last column of the dataframe (can be any name)
#' @param eventNAME variable name for your event in the final merged dataset (does not have to match last column in EVENT dataset, but can). Defaults to "eventYN".
#' @return A dataframe that contains event data merged into your momentary data. It will have N rows = N rows in the momentary dataset.
#' @keywords merging
#' @examples
#' \dontrun{newDATA<-eventmerge(MOMENTARYdata,EVENTdata,eventNAME="eventYN")}



eventmerge=function(MOMENTARY,EVENT,eventNAME="eventYN"){



##### USED IN FUNCTION #####

  #MOMENTARY<-load(MOMENTARY)

  eventNAME<-eventNAME


  get(respondent_id)
  get(EVENT[[survey_id]])
#remove columns in EVENT dataset that aren't necessary, and rename timestamp to indicate that it's EVENT timestamp
EVENT[[respondent_id]]<-NULL; EVENT[[survey_id]]<-NULL; EVENT[[timezone_offset]]<-NULL
EVENT[[timestamp_event]]<-EVENT[[timestamp]]
colnames(EVENT)<-c("instance_key","subject_id_event","timestamp","date_event",eventNAME,"timestamp_event")


#merge datasets

#setup

EVENT$EVENT_KEY<-paste(EVENT$subject_id_event,EVENT$timestamp_event,sep="0")
EVENT$EVENT_KEY<-as.numeric(as.character(EVENT$EVENT_KEY))


MOMENTARY_EVENT <- merge(MOMENTARY, EVENT, by.x=c("KEY","instance_key","subject_id","timestamp"), by.y=c("EVENT_KEY","instance_key","subject_id_event","timestamp"), all=TRUE)

#slide up one
MOMENTARY_EVENT <- DataCombine::slide(MOMENTARY_EVENT, Var = eventNAME, GroupVar="subject_id",TimeVar="timestamp", slideBy = 1,NewVar = eventNAME)
MOMENTARY_EVENT <- DataCombine::slide(MOMENTARY_EVENT, Var = "timestamp_event", GroupVar="subject_id",TimeVar="timestamp", slideBy = 1,NewVar = "timestamp_event")
MOMENTARY_EVENT <- DataCombine::slide(MOMENTARY_EVENT, Var = "date_event", GroupVar="subject_id",TimeVar="timestamp", slideBy = 1,NewVar = "date_event")


MOMENTARY_EVENT[[eventNAME]][is.na(MOMENTARY_EVENT[[eventNAME]])]<-0 #see if var can work here
MOMENTARY_EVENT<-MOMENTARY_EVENT[complete.cases(MOMENTARY_EVENT$respondent_id),]

return(MOMENTARY_EVENT)
}



