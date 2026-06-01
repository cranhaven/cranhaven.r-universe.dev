#' Get stat by clients, campaigns or banners from 'API MyTarget'
#'
#' @param date_from Start date
#' @param date_to End date
#' @param object_type API object type, character value, one of campaigns, banners, users.
#' @param object_id ID of API object (id campaign or any object).
#' @param stat_type Breakdown by day, possible values: day, summary.
#' @param metrics Set of metrics or fields, see metrics section for more ditali, default "base", but you can load any of all, base, events, video, viral, uniques, tps, or go this \href{https://target.my.com/adv/api-marketing/doc/stat-v2}{MyTarget Stat API v2} or \href{https://target.my.com/adv/api-marketing/doc/stat-v2#statisticsv3}{MyTarget Stat API v3}.
#' @param package_id List of package identifiers. Available for banner statistics.
#' @param attribution Attributing by event time or impression time. Available options: conversion, impression, default.
#' @param banner_status Banner statuses list, available for campaign and banner statistics. Possible values: all, active, blocked, deleted.
#' @param campaign_status List of campaign statuses, available for campaign and banner statistics. Possible values: all, active, blocked, deleted.
#' @param sort_by The field by which identifiers of campaigns, banners or users will be sorted. Available metrics: base, events, video, viral, carousel, tps, moat, playable, romi.
#' @param sort_direction Sorting direction. Available options: asc, desc
#' @param auth MyTarget Autherization R object. See \code{\link{myTarAuth}}
#' @param login Path to directory where you save credential data
#' @param token_path Your login, or client name in MyTarget account
#' @param api_version MyTarget Statistic API version: v2, v3
#'
#' @return Data frame with statistic.
#' 
#' @section Sets of metrics:
#' You can load different sets of metrics, for this use metrics arguments, and set a vector containing the names of the desired metric sets.
#' For more details of v2 API go \href{https://target.my.com/adv/api-marketing/doc/stat-v2}{this link}. 
#' For more details of v3 API go \href{https://target.my.com/adv/api-marketing/doc/stat-v2#statisticsv3}{this link}. 
#' For example: metrics = c("base", "video", "viral")
#'   
#'   \strong{base - based metrcis.}
#'   \describe{
#'     \item{shows}{Number of impressions}
#'     \item{clicks}{Number of clicks}
#'     \item{goals}{Number of goals achieved}
#'     \item{spent}{Cost sum}
#'     \item{cpm}{Average cost per 1000 views}
#'     \item{cpc}{Average cost per click}
#'     \item{cpa}{Average cost per goals}
#'     \item{ctr}{Percentage of clicks to views}
#'     \item{cr}{Percentage ratio of the number of goals achieved to the number of clicks.}
#'   }
#'   
#'   \strong{events - metrics for advertised posts on social media feeds.}
#'   \describe{
#'     \item{opening_app}{Number of discoveries of the advertised social networks application}
#'     \item{opening_post}{Number of discoveries of the advertised message in the social media feed}
#'     \item{moving_into_group}{Number of transitions to the group page from the advertised message}
#'     \item{clicks_on_external_url}{Number of clicks on the external link in the advertised message}
#'     \item{launching_video}{Number of video launches in advertised message}
#'     \item{comments}{Number of comments left in the advertised message}
#'     \item{joinings}{Number of joining the group through the advertised message}
#'     \item{likes}{Number of likes of the advertised message}
#'     \item{shares}{Number of action "Share" for the advertised message.}
#'     \item{votings}{Number of voting actions in the advertised message}
#'   }
#'   
#'   \strong{uniques - metrics by the number of unique users.}
#'   \describe{
#'     \item{reach}{Number of unique users who saw the ad for the specified period}
#'     \item{total}{Number of unique users who saw an ad for all time}
#'     \item{increment}{Number of new unique users who saw the ad for the specified period}
#'     \item{frequency}{Average frequency of displaying ads to one unique user}
#'   }
#'   
#'   \strong{video - metrics for video ads.}
#'   \describe{
#'     \item{started}{Number of video playback starts}
#'     \item{paused}{Number of pauses of video playback}
#'     \item{resumed_after_pause}{Number of video playback after pause}
#'     \item{fullscreen_on}{Number of full-screen video playbacks}
#'     \item{fullscreen_off}{Number of shutdowns of full-screen video playback}
#'     \item{sound_turned_off}{Number of video mute}
#'     \item{sound_turned_on}{Number of video sound starts}
#'     \item{viewed_10_seconds}{Number of views of the first 10 seconds of the video}
#'     \item{viewed_25_percent}{Number of views of the first 25 percent of the video duration}
#'     \item{viewed_50_percent}{Number of views of the first 50 percent of the video duration}
#'     \item{viewed_75_percent}{Number of views of the first 75 of the video duration}
#'     \item{viewed_100_percent}{Number of views 100 percent of the video duration}
#'     \item{viewed_10_seconds_rate}{Percentage of views with the achievement of the first 10 seconds of the video}
#'     \item{viewed_25_percent_rate}{Percentage of views with the achievement of the first 25 percent of the video duration}
#'     \item{viewed_50_percent_rate}{Percentage of views with the achievement of the first 50 percent of the video duration}
#'     \item{viewed_75_percent_rate}{Percentage of views with the achievement of the first 75 percent of the video duration}
#'     \item{viewed_100_percent_rate}{Percentage of views with the achievement of the first 100 percent of the video duration}
#'     \item{depth_of_view}{Average video viewing depth (percent)}
#'     \item{view_10_seconds_cost}{Average cost of watching the first 10 seconds of a video}
#'     \item{viewed_25_percent_cost}{Average viewing cost of the first 25 percent of video length}
#'     \item{viewed_50_percent_cost}{Average viewing cost of the first 50 percent of video length}
#'     \item{viewed_75_percent_cost}{Average viewing cost of the first 75 percent of video length}
#'     \item{viewed_100_percent_cost}{Average viewing cost of the first 100 percent of video length}
#'   }
#'   
#'   \strong{viral - metrics of viral events.}
#'   \describe{
#'     \item{viral_impressions}{Number of impressions of the shared advertising message in social networks}
#'     \item{viral_reach}{Number of unique users who saw the shared advertising message for the specified period}
#'     \item{viral_total}{Total number of unique users who have seen the shared advertising message for all time}
#'     \item{viral_increment}{Number of new unique users who saw the shared advertising message for the specified period}
#'     \item{viral_frequency}{Average frequency of displaying a shared advertising message to one unique user}
#'     \item{viral_opening_app}{Number of openings of the advertised application from the shared advertising message}
#'     \item{viral_opening_post}{Number of discoveries of the shared advertised message in the social media feed}
#'     \item{viral_moving_into_group}{number of transitions to the group page from the shared advertised message}
#'     \item{viral_clicks_on_external_url}{Number of clicks on the external link in the shared advertised message}
#'     \item{viral_launching_video}{Number of video launches in the shared advertised message}
#'     \item{viral_comments}{Number of comments left in the shared advertised message}
#'     \item{viral_joinings}{Number of joining the group through the shared advertised message}
#'     \item{viral_likes}{Number of likes of the shared advertised message}
#'     \item{viral_shares}{Number of actions "Share" for the shared advertised message}
#'     \item{viral_votings}{Number of voting actions in the shared advertised message}
#'   }
#'   
#'   \strong{carousel - statistics on individual slides of the advertising carousel (N - from 1 to the number of slides).}
#'   \describe{
#'     \item{slide_N_shows}{Number of N slide shows}
#'     \item{slide_N_clicks}{Number of clicks on slide N}
#'     \item{slide_N_ctr}{Percentage ratio of clicks to the number of views on slide N}
#'   }
#'   
#'   \strong{tps - additional write-off statistics.}
#'   \describe{
#'     \item{tps}{Additional charges for using the moat service}
#'     \item{tpd}{Additional charges for using third-party data (from dmp).}
#'   }
#'   
#'   \strong{moat - statistics according to the moat service.}
#'   \describe{
#'     \item{impressions}{Number of impressions}
#'     \item{in_view}{Number of visible shows}
#'     \item{never_focused}{Number of impressions in the inactive tab}
#'     \item{never_visible}{Number of impressions out of sight}
#'     \item{never_50_perc_visible}{Additional charges for using third-party data (from dmp).}
#'     \item{never_1_sec_visible}{Number of impressions with visibility duration less than 1 second}
#'     \item{human_impressions}{Number of verified impressions}
#'     \item{impressions_analyzed}{number of impressions analyzed}
#'     \item{in_view_percent}{Number of impressions analyzed}
#'     \item{human_and_viewable_perc}{Percentage of visible hits}
#'     \item{never_focused_percent}{Verified impression percentage}
#'     \item{never_visible_percent}{Impression percentage in inactive tab}
#'     \item{never_50_perc_visible_percent}{The percentage of orders with a zone of visibility of the ad is less than 50 percent}
#'     \item{never_1_sec_visible_percent}{Percentage of impressions with visibility duration less than 1 second}
#'     \item{in_view_diff_percent}{Visible impression difference}
#'     \item{active_in_view_time}{Average time the ad is in view}
#'     \item{attention_quality}{Engagement level}
#'   }
#'   
#'   \strong{playable - Playable Ads metrics.}
#'   \describe{
#'     \item{playable_game_open}{Opening the game}
#'     \item{playable_game_close}{Closing the game}
#'     \item{playable_call_to_action}{Clicks}
#'   }
#'   
#'   \strong{romi - Playable Ads metrics.}
#'   \describe{
#'     \item{value}{The given value of the event}
#'     \item{romi}{Return on investment}
#'     \item{adv_cost_share}{Ad spend share}
#'   }
#' 
#' @export
#' @seealso \href{https://target.my.com/adv/api-marketing/doc/stat-v2}{MyTarget API documentation}
#'
#' @examples
#' \dontrun{
#' # base metrics by campaigns
#' base_data <- myTarGetStats(date_from   = Sys.Date() - 7,
#'                            date_to     = Sys.Date(),
#'                            object_type = "campaigns",
#'                            metrics = "base",
#'                            stat_type = "day",
#'                            login = "client_login", 
#'                            token_path = "D:\\mytarget_token")
#' 
#' # all metrics by campaigns
#' all_data <- myTarGetStats(date_from   = Sys.Date() - 7,
#'                           date_to     = Sys.Date(),
#'                           object_type = "campaigns",
#'                           metrics = "all",
#'                           stat_type = "day",
#'                           login = "client_login", 
#'                           token_path = "D:\\mytarget_token")
#' 
#' # custom set of metric by campaigns
#' custom_data <- myTarGetStats(date_from   = Sys.Date() - 7,
#'                              date_to     = Sys.Date(),
#'                              object_type = "campaigns",
#'                              metrics = c("base", "tps", "viral"),
#'                              stat_type = "day",
#'                            login = "client_login", 
#'                              token_path = "D:\\mytarget_token")
#' 
#' 
#' # if have note objects id, base metrics by ads
#' base_data2 <- myTarGetStats(date_from   = as.Date("2013-01-01"),
#'                             date_to     = Sys.Date(),
#'                             object_type = "banners",
#'                             metrics = "base",
#'                             stat_type = "day",
#'                             login = "client_login", 
#'                             token_path = "D:\\mytarget_token")
#' 
#' # get all stats group by clients, only for agency account
#' client_stat <-  myTarGetStats(date_from   = Sys.Date() - 7,
#'                               date_to     = Sys.Date(),
#'                               object_type = "users",
#'                               metrics     = "all",
#'                               login       = "agency_login")
#' }
myTarGetStats <- 
  function(date_from       = Sys.Date() - 7,
           date_to         = Sys.Date(), 
           object_type     = "campaigns",
           object_id       = NULL, 
           stat_type       = "day",
           metrics         = "base",
           package_id      = NULL,
           attribution     = c("conversion", "impression", "default"),
           banner_status   = NULL, 
           campaign_status = NULL, 
           sort_by         = NULL,
           sort_direction  = c("asc", "desc"),
           auth            = NULL,
           login           = getOption('rmytarget.login'), 
           token_path      = myTarTokenPath(),
           api_version     = getOption('rmytarget.stat_api_version')
  ) {
    
    start_time <- Sys.time()
    
    # check api version
    api_version <- match.arg(api_version, c('v2', 'v3'))
    
    if (api_version == 'v2') {
      
      # check period
      if ( as.Date(date_from) < (Sys.Date() -  92) ) {
        message("You can get data only by last 92 days.")
        message("Date from convert to ", Sys.Date() -  92)
        date_from <- Sys.Date() -  92
      }
      
      message("Authorize.")
      
      if (is.null(auth)) {
        auth <- myTarAuth(login = login, token_path = token_path)
      }
      
      # if null obj id
      if ( is.null(object_id)) {
        message("Loading object list.")
        # define function
        f <- switch(object_type,
                    "campaigns" = "myTarGetCampaignList",
                    "banners"   = "myTarGetAdList",
                    "users"     = "myTarGetClientList")
        # load obj
        objects <- do.call(f, 
                           list(login = login,
                                token_path = token_path))
        
        object_id <- objects$id
        
        Sys.sleep(1)
      }
      
      # create tables
      # table list
      if ("all" %in% metrics) {
        tables_name <- c("base", "events", "uniques", "video", "viral", "carousel", "ad_offers", "tps", "moat", "romi", "playable")
      } else {
        tables_name <- metrics
      }
      # create table
      for (t in tables_name) {
        assign(t, data.frame())
      } 
      
      # attrib
      attribution     <- match.arg(attribution)
      if ( ! attribution %in% c("impression", "default") ) {
        attribution <- NULL
      }
      
      # pars of object
      nparts <- ceiling( length(object_id) / 200 )
      objects_parts <- suppressWarnings( split(object_id, 1:nparts) )
      
      message("start-loading------------->")
      # start progressbar
      if ( nparts > 1 ) {
        pb_step <- 1
        pb <- utils::txtProgressBar(pb_step, nparts, style = 3)}
      
      for (p in objects_parts) {
        
        ans <- GET(url = str_interp("https://target.my.com/api/v2/statistics/${object_type}/${stat_type}.json"),
                   query = list(date_from   = date_from,
                                date_to     = date_to,
                                id          = paste0(p, collapse = ","),
                                metrics     = paste0(metrics, collapse = ","), 
                                attribution = attribution),
                   add_headers(Authorization = paste0("Bearer ",auth$access_token)))
        
        temp_all_data <- content(ans, as = "parsed")
        
        if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
        
        if ( !is.null(temp_all_data$error) ) {
          stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
        }
        
        # daily stat
        if ( stat_type == "day" ) {
          # start cycle by each element, object
          for ( i in 1:length(temp_all_data$items) ) {
            
            # get id of current obj
            id <- temp_all_data$items[[i]]$id
            
            # cycle by each rows in obj
            for( r in 1:length(temp_all_data$items[[i]]$rows) ) {
              # get date
              date <- temp_all_data$items[[i]]$rows[[r]]$date
              # get metrics list
              m <- names(temp_all_data$items[[i]]$rows[[r]])[names(temp_all_data$items[[i]]$rows[[r]]) != "date"]
              
              for ( cur_metric in m ) {
                assign(cur_metric, 
                       bind_rows(get(cur_metric), 
                                 bind_cols(c(id = id, 
                                             date = date, 
                                             temp_all_data$items[[i]]$rows[[r]][[cur_metric]] ))))
                
              }
            }
          } 
        } else {
          
          # summary stat
          
          # start cycle by each element, object
          for ( i in 1:length(temp_all_data$items) ) {
            
            # get id of current obj
            id <- temp_all_data$items[[i]]$id
            
            
            # get metrics list
            m <- names(temp_all_data$items[[i]][[2]])
            
            for ( cur_metric in m ) {
              assign(cur_metric, 
                     bind_rows(get(cur_metric), 
                               bind_cols(c(id = id, 
                                           temp_all_data$items[[i]][[2]][[cur_metric]] ))))
              
            }
          }
        }
        #Progresbar step
        if ( nparts > 1 ) {
          pb_step <- pb_step + 1
          utils::setTxtProgressBar(pb, pb_step)}
      }    
      
      if ( exists("pb") ) {
        close(pb)
      }
      
      #Progresbar step
      if ( nparts > 1 ) {
        pb_step <- pb_step + 1
        utils::setTxtProgressBar(pb, pb_step)}
      
      
      if ( exists("pb") ) {
        close(pb)
      }
      
      message("end-loading--------------->")
      message("Create result table.")
      
      # joined all data
      assign("result", get(m[1])) 
      
      for ( j in m ) {
  
        if ( j == m[1] ) next
        
        if ( stat_type == "day" ) {
          
          if ( j == "viral") {
            assign(j, setNames(get(j),  c("id", "date", paste0( "viral_", names(get(j))[ !names(get(j)) %in% c("id", "date") ] ))))
          } 
        } else if ( j == "viral") {
          assign(j, setNames(get(j),  c("id", paste0( "viral_", names(get(j))[ !names(get(j)) %in% "id" ] ))))
        }
        
        # join to result
        if ( stat_type == "day" ) {
          assign("result", left_join(result, get(j), by = c("id", "date")))
        } else {
          assign("result", left_join(result, get(j), by = "id"))
        }
      }
    
    }
    
    if (api_version == 'v3') {

      message("Authorize.")

      if (is.null(auth)) {
        auth <- myTarAuth(login = login, token_path = token_path)
      }

      # if null obj id
      if ( is.null(object_id)) {
        message("Loading object list.")
        # define function
        f <- switch(object_type,
                    "campaigns" = "myTarGetCampaignList",
                    "banners"   = "myTarGetAdList",
                    "users"     = "myTarGetClientList")
        # load obj
        objects <- do.call(f,
                           list(login = login,
                                token_path = token_path))

        object_id <- objects$id

        Sys.sleep(1)
      }

      # match args
      attribution     <- match.arg(attribution)
      sort_direction  <- match.arg(sort_direction)
      
      if ( ! attribution %in% c("impression", "conversion") ) {
        attribution <- NULL
      }

      if ( !is.null(banner_status) ) {
        banner_status <- match.arg(banner_status, c("all", "active", "blocked", "deleted"))
      }

      if ( !is.null(campaign_status) ) {
        campaign_status <- match.arg(campaign_status, c("all", "active", "blocked", "deleted"))
      }

      fields <- paste0(metrics, collapse = ",")
      #stat_type <- "day"

      # pkg id
      if ( is.null(package_id) ) {
        package_id <- NULL
      } else {
        package_id <- paste0(package_id, collapse = ",")
      }

      # result obj
      result <- list()

      message("start-loading------------->")
      
      if ( stat_type == "summary" ) {
      
        ans <- GET(url = str_interp("https://target.my.com/api/v3/statistics/${object_type}/day.json"),
                   query = list(date_from       = date_from,
                                date_to         = date_to,
                                id              = paste0(object_id, collapse = ","),
                                fields          = paste0(fields, collapse = ","),
                                limit           = 250,
                                attribution     = attribution,
                                banner_status   = banner_status,
                                campaign_status = campaign_status,
                                package_id      = package_id,
                                sort_by         = sort_by,
                                d               = sort_direction,
                                offset          = 0),
                   add_headers(Authorization = paste0("Bearer ",auth$access_token)))
  
        # get answer content
        temp_all_data <- content(ans, as = "parsed")
        length(temp_all_data$items)
  
        # Check limits
        if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
  
        # check for error
        if ( !is.null(temp_all_data$error) ) {
          stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
        }
  
        # pagination
        obj_per_call <- 250
        start        <- temp_all_data$offset
        total_obj    <- temp_all_data$count
        r_offset     <- start + obj_per_call
  
        # main data
        mt_data <- tibble( data = temp_all_data$items ) %>%
          unnest_wider('data') %>%
          unnest_wider('total')
  
        # metrics group
        metrics_groups <- names(mt_data)[ ! names(mt_data) %in% c("id", "user_id") ]
  
        # unnest metrics group fields
        for ( metric_group in metrics_groups ) {
  
          mt_data <- unnest_wider(mt_data, metric_group, names_sep = "_")
  
        }
  
        # add to result
        result <- append(result, list(mt_data))
  
        # paginations
        while (r_offset <= total_obj) {
  
          ans <- GET(url = str_interp("https://target.my.com/api/v3/statistics/${object_type}/${stat_type}.json"),
                     query = list(date_from       = date_from,
                                  date_to         = date_to,
                                  id              = paste0(object_id, collapse = ","),
                                  fields          = paste0(fields, collapse = ","),
                                  limit           = 250,
                                  attribution     = attribution,
                                  banner_status   = banner_status,
                                  campaign_status = campaign_status,
                                  package_id      = package_id,
                                  sort_by         = sort_by,
                                  d               = sort_direction,
                                  offset          = r_offset),
                     add_headers(Authorization = paste0("Bearer ",auth$access_token)))
  
          # get answer content
          temp_all_data <- content(ans, as = "parsed")
  
          # Check limits
          if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
  
          # check for error
          if ( !is.null(temp_all_data$error) ) {
            stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
          }
  
          # paginations
          start        <- temp_all_data$offset
          total_obj    <- temp_all_data$count
          r_offset     <- start + obj_per_call
  
          # main data
          mt_data <- tibble( data = temp_all_data$items ) %>%
                      unnest_wider('data') %>%
                      unnest_wider('total')
  
          # metrics group
          metrics_groups <- names(mt_data)[ ! names(mt_data) %in% c("id", "user_id") ]
  
          # unnest metrics group fields
          for ( metric_group in metrics_groups ) {
  
            mt_data <- unnest_wider(mt_data, metric_group, names_sep = "_")
  
          }
  
          # add to result
          result <- append(result, list(mt_data))
  
          # pause between calls
          Sys.sleep(1)
        }
        
      } else {
        
        for ( dt in as.character(seq(as.Date(date_from), as.Date(date_to), by = 'day')) ) {
          
          ans <- GET(url = str_interp("https://target.my.com/api/v3/statistics/${object_type}/day.json"),
                     query = list(date_from       = dt,
                                  date_to         = dt,
                                  id              = paste0(object_id, collapse = ","),
                                  fields          = paste0(fields, collapse = ","),
                                  limit           = 250,
                                  attribution     = attribution,
                                  banner_status   = banner_status,
                                  campaign_status = campaign_status,
                                  package_id      = package_id,
                                  sort_by         = sort_by,
                                  d               = sort_direction,
                                  offset          = 0),
                     add_headers(Authorization = paste0("Bearer ",auth$access_token)))
          
          # get answer content
          temp_all_data <- content(ans, as = "parsed")
          length(temp_all_data$items)
          
          # Check limits
          if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
          
          # check for error
          if ( !is.null(temp_all_data$error) ) {
            stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
          }
          
          # pagination
          obj_per_call <- 250
          start        <- temp_all_data$offset
          total_obj    <- temp_all_data$count
          r_offset     <- start + obj_per_call
          
          # main data
          mt_data <- tibble( data = temp_all_data$items ) %>%
            unnest_wider('data') %>%
            unnest_wider('total') %>% 
            mutate(date = dt)
          
          # metrics group
          metrics_groups <- names(mt_data)[ ! names(mt_data) %in% c("id", "user_id") ]
          
          # unnest metrics group fields
          for ( metric_group in metrics_groups ) {
            
            mt_data <- unnest_wider(mt_data, metric_group, names_sep = "_")
            
          }
          
          # add to result
          result <- append(result, list(mt_data))
          
          # paginations
          while (r_offset <= total_obj) {
            
            ans <- GET(url = str_interp("https://target.my.com/api/v3/statistics/${object_type}/${stat_type}.json"),
                       query = list(date_from       = dt,
                                    date_to         = dt,
                                    id              = paste0(object_id, collapse = ","),
                                    fields          = paste0(fields, collapse = ","),
                                    limit           = 250,
                                    attribution     = attribution,
                                    banner_status   = banner_status,
                                    campaign_status = campaign_status,
                                    package_id      = package_id,
                                    sort_by         = sort_by,
                                    d               = sort_direction,
                                    offset          = r_offset),
                       add_headers(Authorization = paste0("Bearer ",auth$access_token)))
            
            # get answer content
            temp_all_data <- content(ans, as = "parsed")
            
            # Check limits
            if ( ! myTarCheckLimits(temp_all_data) ) stop("Limit error")
            
            # check for error
            if ( !is.null(temp_all_data$error) ) {
              stop( temp_all_data$error$code, ": ", temp_all_data$error$message)
            }
            
            # paginations
            start        <- temp_all_data$offset
            total_obj    <- temp_all_data$count
            r_offset     <- start + obj_per_call
            
            # main data
            mt_data <- tibble( data = temp_all_data$items ) %>%
              unnest_wider('data') %>%
              unnest_wider('total') %>% 
              mutate(date = dt)
            
            # metrics group
            metrics_groups <- names(mt_data)[ ! names(mt_data) %in% c("id", "user_id") ]
            
            # unnest metrics group fields
            for ( metric_group in metrics_groups ) {
              
              mt_data <- unnest_wider(mt_data, metric_group, names_sep = "_")
              
            }
            
            # add to result
            result <- append(result, list(mt_data))
            
            # pause between calls
            Sys.sleep(1)
          }
          
          Sys.sleep(1)
          
        }
        
      }


      result <- bind_rows(result) 

    }
    
    stop_time <- Sys.time()
    message("Success.")
    message("Total time: ", as.numeric(difftime(stop_time, start_time), units = "secs"), " sec.")
    # return total result
    return(result)
  }
