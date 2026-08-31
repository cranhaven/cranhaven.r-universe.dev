## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- error=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='hide'----
library(zoomGroupStats)
batchOut = invisible(batchProcessZoomOutput(batchInput=system.file('extdata', 'myMeetingsBatch.xlsx', package = 'zoomGroupStats')))

## ---- eval=TRUE---------------------------------------------------------------
# Three records from the sample transcript dataset
 head(batchOut$transcript, 3)

## ---- eval=TRUE---------------------------------------------------------------
# Three records from the sample transcript dataset
 head(batchOut$chat, 3)

## ---- eval=FALSE--------------------------------------------------------------
#  # You can request both sentiment analysis methods by including them in sentMethods
#   transcriptSent = textSentiment(inputData=batchOut$transcript, idVars=c('batchMeetingId','utteranceId'), textVar='utteranceMessage', sentMethods=c('aws', 'syuzhet'), appendOut=FALSE, languageCodeVar='utteranceLanguage')
#  
#  # This does only the aws sentiment analysis on a chat file
#   chatSent = textSentiment(inputData=batchOut$chat, idVars=c('batchMeetingId', 'messageId'), textVar='message', sentMethods=c('aws'), appendOut=FALSE, languageCodeVar='messageLanguage')

## ---- eval=TRUE---------------------------------------------------------------
# This does only the syuzhet analysis on the transcript and appends does not append it to the input dataset
 transcriptSent = textSentiment(inputData=batchOut$transcript, idVars=c('batchMeetingId','utteranceId'), textVar='utteranceMessage', sentMethods=c('syuzhet'), appendOut=FALSE, languageCodeVar='utteranceLanguage')

head(transcriptSent$syuzhet)


## ---- eval=TRUE---------------------------------------------------------------
# This does only the syuzhet sentiment analysis on a chat file and appends it to the input dataset
 chatSent = textSentiment(inputData=batchOut$chat, idVars=c('batchMeetingId', 'messageId'), textVar='message', sentMethods=c('syuzhet'), appendOut=TRUE, languageCodeVar='messageLanguage')
  head(chatSent$syuzhet)

## ---- eval=TRUE---------------------------------------------------------------
# Analyze the transcript, without the sentiment metrics
convoTrans = textConversationAnalysis(inputData=batchOut$transcript, inputType='transcript', meetingId='batchMeetingId', speakerId='userName')

## ---- eval=TRUE---------------------------------------------------------------
# This is output at the meeting level. (Note that the values across meetings are equivalent because the sample dataset is a replication of the same meeting multiple times.)
head(convoTrans$transcriptlevel)


## ---- eval=TRUE---------------------------------------------------------------
# This is output at the speaker level
head(convoTrans$speakerlevel)


## ---- eval=TRUE---------------------------------------------------------------
# Analyze the conversation within the chat file, including the sentiment metrics
convoChat = textConversationAnalysis(inputData=chatSent$syuzhet, inputType='chat', meetingId='batchMeetingId', speakerId='userName', sentMethod="syuzhet")

## ---- eval=TRUE---------------------------------------------------------------
# This is output at the meeting level
head(convoChat$chatlevel)


## ---- eval=TRUE---------------------------------------------------------------
# This is output at the speaker level
head(convoChat$userlevel)


## ---- eval=TRUE---------------------------------------------------------------
 win.convo.out = windowedTextConversationAnalysis(inputData=batchOut$transcript, inputType='transcript', meetingId='batchMeetingId', speakerId='userName', sentMethod="none", timeVar="utteranceStartSeconds", windowSize=300)

## ---- eval=TRUE---------------------------------------------------------------
# View the window-level output
head(win.convo.out$windowlevel)


## ---- eval=TRUE---------------------------------------------------------------
# View the output for speakers within windows
head(win.convo.out$speakerlevel)


