## ----include = FALSE----------------------------------------------------------

knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE,
                      fig.pos = "!H",
                      echo = TRUE,
                      eval = TRUE)          # this vignette by default does not evaluate the code snippets 


## -----------------------------------------------------------------------------

pkgs = c('IceSat2R', 'sf', 'mapview', 'data.table', 'stargazer', 'glue', 'utils',
         'reshape2', 'plotly', 'magrittr', 'geodist', 'CopernicusDEM', 'terra')
load_pkgs = lapply(pkgs, require, character.only = TRUE)  # load required R packages

sf::sf_use_s2(use_s2 = FALSE)                        # disable 's2' in this vignette
mapview::mapviewOptions(leafletHeight = '600px', 
                        leafletWidth = '700px')      # applies to all leaflet maps


## ----echo = F-----------------------------------------------------------------

#.........................................................................................
# For reproducibility purposes save only the 'date' and 'RGT' matches of winter and summer
#.........................................................................................

# data.table::fwrite(x = rgts_ws, file = system.file('data_files', 'vignette_data', 'winter_summer_inters.csv', package = "IceSat2R"), row.names = F)


## ----echo = FALSE-------------------------------------------------------------

subs_join_s = readRDS(file = system.file('data_files', 'vignette_data', 'Greenland_grid_cells.RDS', package = "IceSat2R"))


## -----------------------------------------------------------------------------

mapview::mapview(subs_join_s, legend = F)


## -----------------------------------------------------------------------------

join_geoms = 1:5
subs_join_reduced = subs_join_s[join_geoms]

mapview::mapview(subs_join_reduced, legend = F)


## ----echo = F, results = 'asis'-----------------------------------------------

pth_subs_RGTs = system.file('data_files', 'vignette_data', 'Greenland_RGTs_grid_cells.csv', package = "IceSat2R")
dtbl_RGTs = data.table::fread(pth_subs_RGTs, header = T, stringsAsFactors = F)
dtbl_RGTs = dtbl_RGTs[order(dtbl_RGTs$RGT, decreasing = F), ]

stargazer::stargazer(dtbl_RGTs,
                     summary = FALSE, 
                     rownames = FALSE, 
                     header = FALSE, 
                     float = FALSE,
                     table.placement = 'h', 
                     title = 'Greenland Grid Cells')


# #.................................................................
# # I came to these RGT's after processing the next for-loop for
# #   - all 9 Greenland geoms 
# #   - all RGT's (of the 'rgts_ws' variable)
# #.................................................................
# 
# require(magrittr)
# logs_out_dtbl = data.table::rbindlist(logs_out)         # the output of the 'logs_out' list is required!
# logs_out_dtbl$index = names(dat_out_w)                  # the output of the 'dat_out_w' list is required!
# logs_out_dtbl = logs_out_dtbl[order(logs_out_dtbl$N_rows_winter, decreasing = T), ]
# logs_out_dtbl_subs = subset(logs_out_dtbl, N_rows_winter > 0 & N_rows_summer > 0)
# vec_grid = as.vector(unlist(lapply(strsplit(logs_out_dtbl_subs$index, '_'), function(x) x[3])))
# logs_out_dtbl_subs$greenland_cell = vec_grid
# spl_rgt = split(logs_out_dtbl_subs, by = 'greenland_cell')
# spl_subs = spl_rgt[as.character(1:5)] %>%
#   lapply(function(x) x[1:4, ]) %>%
#   data.table::rbindlist()


## ----echo = FALSE-------------------------------------------------------------

pth_logs = system.file('data_files', 'vignette_data', 'LOGs.csv', package = "IceSat2R")
logs_out_dtbl = data.table::fread(file = pth_logs, stringsAsFactors = F, header = T)


## ----results = 'asis'---------------------------------------------------------

logs_out_dtbl = logs_out_dtbl[order(logs_out_dtbl$N_rows_winter, decreasing = T), ]

stargazer::stargazer(logs_out_dtbl, 
                     summary = FALSE, 
                     rownames = FALSE, 
                     header = FALSE, 
                     float = FALSE,
                     table.placement = 'h', 
                     title = 'LOGs')


## ----echo = FALSE-------------------------------------------------------------

ws_vis = readRDS(file = system.file('data_files', 'vignette_data', 'plotly_beams.RDS', package = "IceSat2R"))


## -----------------------------------------------------------------------------

ws_vis_mlt = reshape2::melt(ws_vis, id.vars = c('segment_id_winter', 'beam_winter'))
ws_vis_mlt = data.table::data.table(ws_vis_mlt, stringsAsFactors = F)
ws_vis_mlt_spl = split(ws_vis_mlt, by = 'beam_winter')
# BEAMS = names(ws_vis_mlt_spl)       # plot all beams


## -----------------------------------------------------------------------------

#...................................
# function to plot each subplot beam
#...................................

plotly_beams = function(spl_data, 
                        beam,
                        left_width,
                        left_height,
                        right_width,
                        right_height) {
  
  subs_iter = spl_data[[beam]]
  
  cat(glue::glue("Plot for Beam '{beam}' will be created ..."), '\n')
  
  #......................
  # plot for all segments
   #......................
  
    fig_lns = plot_ly(data = subs_iter,
                      x = ~segment_id_winter,
                      y = ~value,
                      color = ~variable,
                      colors = c("blue", "red"), 
                      line = list(width = 2),
                      text =~glue::glue("land-ice-height: {value} Segment-id: {segment_id_winter}"),
                      hoverinfo = "text",
                      width = left_width,
                      height = left_height) %>%
    
    plotly::layout(xaxis = list(gridcolor = "grey", showgrid = T),
                   yaxis = list(gridcolor = "grey", showgrid = T)) %>%
    
    plotly::add_lines()
    
  #..............................
  # plot for a subset of segments
  #..............................
  
  segm_ids = 588326:588908     # this subset of segments show a big difference betw. summer and winter
  plt_title = glue::glue("Beam: '{beam}' ( Segments: from {min(segm_ids)} to {max(segm_ids)} )")
  subs_iter_segm = subset(subs_iter, segment_id_winter %in% segm_ids)
  
    fig_spl = plot_ly(data = subs_iter_segm,
                  x = ~segment_id_winter,
                  y = ~value,
                  color = ~variable,
                  colors = c("blue", "red"), 
                  line = list(width = 2),
                  text =~glue::glue("land-ice-height: {value} Segment-id: {segment_id_winter}"),
                  hoverinfo = "text",
                  width = right_width,
                  height = right_height) %>%
    
    plotly::layout(xaxis = list(gridcolor = "grey", showgrid = T),
                   yaxis = list(gridcolor = "grey", showgrid = T)) %>%
    
    plotly::add_lines(showlegend = FALSE)
  
  both_plt = plotly::subplot(list(fig_lns, fig_spl), nrows=1, margin = 0.03, widths = c(0.7, 0.3)) %>% 
    plotly::layout(title = plt_title)
  # plotly::export(p = both_plt, file = glue::glue('{beam}.png'))
  
  return(both_plt)
}


## -----------------------------------------------------------------------------

plt_gt1l = plotly_beams(spl_data = ws_vis_mlt_spl, 
                        beam = "gt1l",
                        left_width = 1800,
                        left_height = 800,
                        right_width = 900,
                        right_height = 400)

plt_gt1l


## -----------------------------------------------------------------------------

plt_gt1r = plotly_beams(spl_data = ws_vis_mlt_spl, 
                        beam = "gt1r",
                        left_width = 1800,
                        left_height = 800,
                        right_width = 900,
                        right_height = 400)
plt_gt1r


## ----echo = F-----------------------------------------------------------------

# #......................................................
# # save all images for all beams in a separate directory
# #......................................................
# 
# nams_ws = names(dat_out_w)
# save_summary = save_dat = list()
# 
# for (nam_iter in nams_ws) {
#   
#   cat("-----------------\n")
#   cat(nam_iter, '\n')
#   cat("-----------------\n")
# 
#   w_subs = dat_out_w[[nam_iter]]
#   s_subs = dat_out_s[[nam_iter]]
#   
#   cols_keep = c('date', 'segment_id', 'longitude', 'latitude', 'h_li', 'beam')
#   
#   w_subs_hq = subset(w_subs, atl06_quality_summary == 0)
#   w_subs_hq = w_subs_hq[, ..cols_keep]
#   colnames(w_subs_hq) = glue::glue("{cols_keep}_winter")
#   
#   s_subs_hq = subset(s_subs, atl06_quality_summary == 0)
#   s_subs_hq = s_subs_hq[, ..cols_keep]
#   colnames(s_subs_hq) = glue::glue("{cols_keep}_summer")
#   
#   sw_hq_merg = merge(x = w_subs_hq, 
#                      y = s_subs_hq, 
#                      by.x = c('segment_id_winter', 'beam_winter'), 
#                      by.y = c('segment_id_summer', 'beam_summer'))
#   
#   if (nrow(sw_hq_merg) > 0) {
#     sw_hq_merg$dif_height = sw_hq_merg$h_li_winter - sw_hq_merg$h_li_summer
#     
#     save_dat[[nam_iter]] = sw_hq_merg
#     save_summary[[nam_iter]] = data.table::setDT(list(name_iter = nam_iter,
#                                                       min = min(sw_hq_merg$dif_height), 
#                                                       mean = mean(sw_hq_merg$dif_height),
#                                                       median = median(sw_hq_merg$dif_height),
#                                                       max = max(sw_hq_merg$dif_height),
#                                                       N_rows = nrow(sw_hq_merg)))
#     #.......................................
#     # save the plots for visual verification
#     #.......................................
#     
#     cols_viz = c('segment_id_winter', 'beam_winter', 'h_li_winter', 'h_li_summer')
#     ws_vis = sw_hq_merg[, ..cols_viz]
#     
#     ws_vis_mlt = reshape2::melt(ws_vis, id.vars = c('segment_id_winter', 'beam_winter'))
#     ws_vis_mlt = data.table::data.table(ws_vis_mlt, stringsAsFactors = F)
#     ws_vis_mlt_spl = split(ws_vis_mlt, by = 'beam_winter')
#     
#     dir_save = file.path('all_beams_all_RGTs', nam_iter)           # !! create the 'all_beams_all_RGTs' directory first
#     if (!dir.exists(dir_save)) dir.create(dir_save)
#     
#     BEAMS = names(ws_vis_mlt_spl)       # plot all beams
#     
#     for (beam in BEAMS) {
#       
#       subs_iter = ws_vis_mlt_spl[[beam]]
#       
#       cat(glue::glue("Plot for Beam '{beam}' will be saved ..."), '\n')
#       
#       #......................
#       # plot for all segments
#       #......................
#       
#       fig_lines = plotly::plot_ly(data = subs_iter,
#                                   x = ~segment_id_winter,
#                                   y = ~value,
#                                   color = ~variable,
#                                   colors = c("blue", "red"), 
#                                   line = list(width = 2),
#                                   text = ~glue::glue("land-ice-height: {value}  Segment-id: {segment_id_winter}"),
#                                   hoverinfo = "text",
#                                   width = 1800,
#                                   height = 1000) %>%
#         
#         plotly::layout(xaxis = list(gridcolor = "grey", showgrid = T),
#                        yaxis = list(gridcolor = "grey", showgrid = T)) %>%
#         
#         plotly::add_lines()
#       
#       plotly::export(p = fig_lines, file = file.path(dir_save, glue::glue('{beam}.png')))
#     }
#   }
#   else {
#    message(glue::glue("Empty data table after merging for idx and RGT: '{nam_iter}'"))
#   }
# }
# 
# save_summary = data.table::rbindlist(save_summary)
# save_summary = save_summary[order(save_summary$max, decreasing = T), ]
# save_summary


## ----echo = FALSE-------------------------------------------------------------

# merg_cells_viz = readRDS(file = system.file('data_files', 'vignette_data', '3d_plot.RDS', package = "IceSat2R"))


