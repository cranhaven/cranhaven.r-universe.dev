#' audio Class
#'
#'
#' @description To turn audio into text or text into audio
#'
#'
audio <- R6Class(
  "audio",
  inherit = base_api,
  public = list(
    #' @description Generates audio from the input text.
    #' @param model character Required. One of the available TTS models: tts-1 or tts-1-hd
    #' @param input character Required. The text to generate audio for. The maximum length is 4096 characters.
    #' @param voice character Required. The voice to use when generating the audio. Supported voices are alloy, echo, fable, onyx, nova, and shimmer.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param stream logical. Using the stream call, it will return raw data of the specified length,
    #'               which can be saved in the set format such as mp3, etc. For details, please see the examples.
    #' @param num The num parameter controls the number of raw entries returned by a stream in one go.
    #'            Note that this is different from the n parameter, which specifies the number of results returned.
    #'            For detailed information on the n parameter, please refer to OpenAI's API documentation.
    #' @param ... Additional parameters as required by the OpenAI API.For example:response_format;speed....
    #' @return The audio file content.
    speech=function(model="tts-1",input,voice="alloy",stream=F,num=100,...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$input <- input
      option$voice <- voice
      if (stream) {
        handle <- private$handle_call("audio", body=option,path="/speech", headers=list(Accept="text/event-stream", `Content-Type` = "application/json"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("audio", body = option,"/speech",method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(result$data)
        }
      }
    },
    #' @description Transcribes audio into the input language.
    #' @param path character Required. The audio file object (not file name) to transcribe,
    #'             in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    #' @param model character Required. ID of the model to use. Only whisper-1 is currently available.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:language;prompt;response_format;temperature....
    #' @return The transcribed text.
    transcription=function(path,model="whisper-1",...,verbosity=0){
      ff<-private$check_path(path)
      if(!ff$success){return(ff)}
      option <- list(...)
      option$model <- model
      option$file <- curl::form_file(path)
      result<-private$file_call(endpoint = "audio",path = "/transcriptions",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Translates audio into English.
    #' @param path character Required. The audio file object (not file name) to transcribe,
    #'             in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    #' @param model character Required. ID of the model to use. Only whisper-1 is currently available.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:prompt;response_format;temperature....
    #' @return The transcribed text.
    translation=function(path,model="whisper-1",...,verbosity=0){
      ff<-private$check_path(path)
      if(!ff$success){return(ff)}
      option <- list(...)
      option$model <- model
      option$file <- curl::form_file(path)
      result<-private$file_call(endpoint = "audio",path = "/translations",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)