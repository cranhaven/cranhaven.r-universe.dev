#' WechatHelper
#'
#' @param userid Please follow the official WeChat account "shengxintongzhizhushou", reply with id, and scan the QR code to obtain the userid. View "http://bj.s1f.ren/gzh/home.html" and "https://mp.weixin.qq.com/s/tiedh3FWTA4hOhut5Useew" for more information.
#' @param msg  Content of the notification to be sent. Limit to 20 characters.
#' @param status Current data status. Limit to 5 Chinese characters.
#' @param name Project name. Limit to 5 characters.
#' @param number Notification number.
#'
#' @return response of the request
#' @export
#'
#' @examples
#' WechatMsg(userid = "123", msg = "test---SaveRds Success!")
WechatMsg <- function(userid, msg, status = NULL, name = NULL, number = NULL) {
  base_url <- "http://bj.s1f.ren/gzh/sendMsg"

  query_params <- list(
    userid = userid,
    text = msg
  )

  if (!is.null(status)) {
    query_params$status <- status
  }
  if (!is.null(name)) {
    query_params$name <- name
  }
  if (!is.null(number)) {
    query_params$number <- number
  }

  response <- httr::GET(base_url, query = query_params)

  if (httr::status_code(response) == 200) {
    return(rawToChar(response$content))
  } else {
    stop("Request failed with status code: ", httr::status_code(response))
  }
  return(response)
}


#' WechatRobot
#'
#' @param key The key for the WeChat Work robot.
#' @param msg Text message.
#' @param qyapi WeChat Work API.
#' @param msgtype Type of text message being sent.
#'
#' @return response of the request
#' @export
#'
#' @examples
#' WechatRobot(key = "79bdbb23-7f7e-4fb6-b669-97f7e744755e", msg = "test---SaveRds Success!")
WechatRobot <- function(key, msg = "", qyapi = "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=", msgtype = "text") {
  headers <- c("Content-Type" = "application/json")
  url <- paste0(qyapi, key)
  mypayload <- list(
    "msgtype" = msgtype,
    "text" = list("content" = msg)
  )
  response <- httr::POST(url, httr::add_headers(.headers = headers),
                         body = mypayload,
                         encode = "json"
  )
  return(rawToChar(response$content))

}


