##########################################################
#####################NETWORKS SUBTAB######################
##########################################################

# Function that get the needed igraph object to use to make a network
# This function takes in a (sparse) matrix and returns an igraph object
get_g_complete <- function(vals, mat){
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  # In gxe mode traits and marker nodes have different sizes
  # if(vals$mode == "gxe" && !is.null(vals$map_nodes)){
  #   labs <- vals$map_nodes[vals$map_nodes$node %in% igraph::vertex_attr(g, "name"), ]
  #   traits_mat <- sum(labs$node_group == "Trait")
  # }

  #Assign color to edges depending if it is negative (red) or (positive) blue
  if(!is.null(igraph::E(g)$weight)){
    edge_cols <- rep("blue", length(igraph::E(g)$weight))
    edge_cols[igraph::E(g)$weight < 0] <- "red"
    value <- abs(igraph::E(g)$weight)
    igraph::E(g)$color <- edge_cols
    igraph::E(g)$width <- value
  }

  return(g)
}

# Function that gets the proper layout for the networks
# This function takes a (sparse) matrix as an argument and
# returns a dataframe with the coordinates layout
get_igraph_lay <- function(vals, input, mat){
  #Get an initial set of coordinates that later will be matched
  #with the coordinates of the global network
  mat@x[abs(mat@x) > 0] <- 1
  g_lay <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")
  lay <- igraph::layout_nicely(g_lay)
  row.names(lay) <- names(igraph::V(g_lay))
  # Changing coordinates to match global network
  row_lay <- rownames(lay)
  row_coords <- rownames(vals$coords)
  for (i in 1:nrow(lay)){
    if(is.element(row_lay[i], row_coords)){
      lay[rownames(lay)[i], ] <- vals$coords[row_lay[i], ]
    }
  }
  # Make sure that isolated nodes are not too far away from the rest of the nodes
  if (shiny::isolate(vals$layout == "Tree")){
    s <- which(igraph::degree(g_lay) == 0)
    s <- names(s)
    lay_y <- sort(vals$coords[, 2])
    new_y <- lay_y[length(s) + 1]
    new_xs <- vals$coords[, 1][vals$coords[, 2] == new_y]
    new_X <- max(new_xs)
    for (i in s){
      new_X <- new_X + 3
      lay[i, ][1] <- new_X
      lay[i, ][2] <- new_y
    }
  }
  return(lay)
}

# his function gets a visnetwork to be shown in the app
# This function takes in a (sparse) matrix, igraph object, and layout dataframe
# and returns a visnetwork
get_vis_net <- function(vals, input, mat, g, lay){
  shiny::validate(shiny::need(!is.null(vals$map_nodes$node), "Getting proper data"))
  sel_nodes <- dimnames(mat)[[1]]
  test.visn <- visNetwork::toVisNetworkData(g)
  sel_by <- NULL
  if(shiny::isTruthy(vals$map_nodes)){
    if(vals$mode == "gxe"){
      sel_by <- "Chromosome"
      #test.visn$nodes$Chromosome <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_group
      test.visn$nodes$Chromosome <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    else{
      sel_by <- "Group"
      #test.visn$nodes$Group <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_group
      test.visn$nodes$Group <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    #drtest.visn$nodes$color.background <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_color
    test.visn$nodes$color.background <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
    #igraph::V(g)$color <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_color
  }
  if(isTruthy(vals$map_nodes$node_size)){
    #test.visn$nodes$value <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_size
    test.visn$nodes$value <- vals$map_nodes$node_size[match(sel_nodes, vals$map_nodes$node)]
  }
  else{
    if(vals$mode == "gxe"){
      #n_traits <- sum(test.visn$nodes$id %in% vals$map_nodes$node[1:vals$n_traits])
      test.visn$nodes$size <- ifelse(sel_nodes %in% vals$trait_nodes, 30, 20)
      #test.visn$nodes$size <- c(rep(30, n_traits), rep(20, nrow(test.visn$nodes) - n_traits))
    }
    else{
      test.visn$nodes$size <- rep(25, nrow(test.visn$nodes))
    }
  }
  if(!is.null(igraph::E(g)$weight)){
    test.visn$edges$value <- abs(igraph::E(g)$weight)
  }
  if(shiny::isTruthy(vals$map_nodes$font_size)){
    #test.visn$nodes$font.size <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$font_size
    test.visn$nodes$font.size <- vals$map_nodes$font_size[match(sel_nodes, vals$map_nodes$node)]
  }
  else{
    test.visn$nodes$font.size <- 17
  }
  visNetwork::visNetwork(test.visn$nodes, test.visn$edges, height = '100%', width = '100%') %>%
    visNetwork::visIgraphLayout(layout = "layout.norm", layoutMatrix = lay, randomSeed = vals$rseed) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), selectedBy = sel_by) %>%
    visNetwork::visInteraction(multiselect = TRUE) %>%
    visNetwork::visEvents(doubleClick =  "function(nodes){Shiny.onInputChange('click', nodes.nodes[0]);;}") %>%
    visNetwork::visEdges(smooth = list(enabled = TRUE, type = "curvedCCW", roundness = vals$roundness)) %>%
    return()
}

##########################################################
#####################MAT PLOTS SUBTAB#####################
##########################################################

# Function to create interactive matrices plot
get_mat_plots <- function(vals, input) {
  mat_plots <- list()
  for (i in 1:length(input$mat_sel)) {
    show_scale <- ifelse(i == 1, TRUE, FALSE)
    curr_env <- vals$networks[[input$mat_sel[[i]]]]
    nms <- dimnames(curr_env)[[1]]
    diag(curr_env) <- 0
    fig <-
      plotly::plot_ly(
        x = nms,
        y = nms,
        z = as.matrix(curr_env),
        zmin = -1,
        zmid = 0,
        zmax = 1,
        type = "heatmap",
        showscale = show_scale,
        colors = grDevices::colorRamp(c("blue", "#F5F2F2", "red2")),
        hovertemplate = paste(' x: %{x} <br>',
                              'y: %{y} <br>',
                              'value: %{z}',
                              ' <extra></extra>')
      )
    fig <- plotly::layout(fig,  yaxis = list(autorange = "reversed", tickfont = list(size = 7), showline = TRUE, linecolor = plotly::toRGB("black")), xaxis = list(tickfont = list(size = 7), showline = TRUE, linecolor = plotly::toRGB("black")))
    mat_plots[[i]] <- fig
  }

  shiny::validate(shiny::need(length(mat_plots) > 0, 'No Networks Chosen'))

  if (length(input$mat_sel) > 1) {
    plt <- plotly::subplot(mat_plots, nrows = 2, shareY = TRUE, shareX = TRUE)
  }
  else {
    plt <- mat_plots[[1]]
  }

  return(plt)

}

##########################################################
#################WEIGHTS ANALYSIS TAB#####################
##########################################################

get_con <- function(mat, sett, node){
  dimnms <- dimnames(mat)[[2]]
  node_ind <- which(dimnms == node) #Get index of chosen node
  summ <- Matrix::summary(mat)
  summ <- summ[summ$i == node_ind | summ$j == node_ind, ]
  summ <- summ[!(summ$i == node_ind & summ$j == node_ind), ]
  summ$setting <- rep(sett, nrow(summ))
  summ$cons <- ifelse(summ$i == node_ind, dimnms[summ$j], dimnms[summ$i])
  summ <- summ[c("cons", "x", "setting")]
  colnames(summ) <- c("cons", "value", "setting")
  return(summ)
}

# Function that gets the weights analysis plot
# This function takes in no argument and returns a ggplot object
get_weights_analysis_plot <- function(vals, input){
  df_conns <- mapply(get_con, mat = vals$networks, sett = vals$sett_names, node = input$marker, SIMPLIFY = FALSE)
  df_conns <- do.call("rbind", df_conns)
  if(shiny::isTruthy(vals$map_nodes)){
    df_conns <- merge(df_conns, vals$map_nodes[c("node", "node_group")], by.x = "cons", by.y = "node", all.x = TRUE)
    df_conns <- df_conns[order(factor(df_conns$cons, levels = vals$map_nodes$node)),]
    df_conns$cons <- factor(df_conns$cons, levels = vals$map_nodes$node)
    df_conns$node_group <- factor(df_conns$node_group, levels = unique(vals$map_nodes$node_group))
    row.names(vals$map_nodes) <- vals$map_nodes$node
    frame_colors <- unique(vals$map_nodes[unique(as.character(df_conns$cons)), ]$node_color)
  }
  else{
    df_conns$node_group <- df_conns$setting
  }

  lgnd.pos <- ifelse(is.null(vals$map_nodes), "none", "right")
  if(vals$mode == "gxe"){
    node_text <- "Marker: "
    con_text <- "Par. Cor.: "
    group_text <- "Chromosome: "
    y_lab <- "Partial Correlation"
  }
  else{
    node_text <- "Node: "
    con_text <- "Weight: "
    group_text <- "Group: "
    y_lab <- "Weight"
  }
  p <- ggplot2::ggplot(data = df_conns, ggplot2::aes(x = cons, text = paste0(node_text, cons, "\n",
                                                                             con_text, round(value, 3), "\n",
                                                                             group_text, node_group))) +
    ggplot2::geom_point(ggplot2::aes_(y = ~value), color = "red") +
    #geom_line(aes(y = values, group = 1)) +
    ggplot2::geom_bar(ggplot2::aes_(weight = ~value, fill = ~node_group), show.legend = FALSE) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::facet_wrap(~setting, nrow = length(vals$networks)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1, size = 10),
                   legend.text = ggplot2::element_text(size = 15)) + #, axis.title.y = element_text(size = 15)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme(legend.position = lgnd.pos)
  if(shiny::isTruthy(vals$map_nodes)){
    p <- p +
      ggplot2::scale_fill_manual(values = frame_colors)
  }

  return(p)
}

##########################################################
#############WEIGHTS DISTRIBUTION SUBTAB##################
##########################################################

# Function to get the plot for the distribution of the weights of the settings
# This function takes no arguments and returns a ggplot object
get_par_cor_plot <- function(vals, input){
  dat <- data.frame(type = factor(),
                    weights = numeric())
  if(vals$mode == "gxe"){
    x_lab <- "Distribution Partial Correlations"
  }
  else{
    x_lab <- "Distribution Edge Weights"
  }
  for (i in 1:length(vals$networks)){
    data <- vals$networks[[i]]
    data <- getNZ(vals = vals, input = input, mat = vals$networks[[i]])
    data <- get_subnet(vals = vals, mat = data)

    shiny::validate(shiny::need(nrow(data) > 0, 'No Connections'))

    diag(data) <- 0
    data <- Matrix::drop0(data, tol = 0, is.Csparse = TRUE)
    lst <- as.vector(data)
    lst <- lst[lst != 0]
    dat <- rbind(dat, data.frame(type = rep(vals$sett_names[i], length(lst)), weights = lst))
  }

  p <- ggplot2::ggplot(data = dat, ggplot2::aes_(x = ~weights, fill = ~type)) +
    ggplot2::geom_histogram(colour = "black", fill = "#007c00", bins = input$par_cor_bins, ggplot2::aes(y = ggplot2::after_stat(width * density))) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab("Proportion") +
    ggplot2::facet_wrap(~type) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   plot.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   panel.background = ggplot2::element_rect(fill = "#F5F2F2"))
  return(p)
}

##########################################################
##############DIFFERENCE NETWORKS SUBTAB##################
##########################################################



# Function to get difference between to matrices
# 3 = gained connection from mat1 -> mat2, 2 = lost connection from mat1 -> mat2, 1 = sign change from mat1 -> mat2
get_diff_mat <- function(mat1, mat2){
  if(dim(mat1)[[1]] != dim(mat2)[[1]]){
    shiny::showNotification("WARNING: Dimension of Networks Are Different. Only Common Nodes Will Be Used.", type = "warning", duration = NULL)
    mat1_nodes <- dimnames(mat1)[[2]]
    mat2_nodes <- dimnames(mat2)[[2]]
    intersection <- intersect(mat1_nodes, mat2_nodes)
    mat1_ind <- which(mat1_nodes %in% intersection)
    mat2_ind <- which(mat2_nodes %in% intersection)
    mat1 <- mat1[mat1_ind, mat1_ind]
    mat2 <- mat2[mat2_ind, mat2_ind]
  }

  newobj <- list()

  mat1@x[mat1@x < 0] <- -1
  mat1@x[mat1@x > 0] <- 4
  mat2@x[mat2@x < 0] <- -1
  mat2@x[mat2@x > 0] <- 4

  diff <- mat1 - mat2

  diff@x[diff@x %in% c(-4, 1)] <- 3
  diff@x[diff@x %in% c(-1, 4)] <- 2
  diff@x[diff@x %in% c(-5, 5)] <- 1
  diff <- Matrix::drop0(diff)

  return(diff)
}

get_dif_net_compl <- function(vals, input){
  mat1 <- vals$networks[[input$net1]]
  mat2 <- vals$networks[[input$net2]]

  diff <- get_diff_mat(mat1, mat2)
  diff <- getNZ(vals = vals, input = input, mat = diff, diff = TRUE)

  diff_compl <- mat1 - mat2
  diff_compl <- getNZ(vals = vals, input = input, mat = diff_compl, diff = TRUE)

  shiny::validate(
    shiny::need(dim(diff_compl)[[1]] != 0, "No Difference Between Networks")
  )

  g1 <- igraph::graph_from_adjacency_matrix(diff, mode = "undirected", weighted = TRUE)
  g2 <- igraph::graph_from_adjacency_matrix(diff_compl, mode = "undirected", weighted = TRUE)

  df_g2 <- data.frame(igraph::get.edgelist(g2))
  colnames(df_g2) <- c("a", "b")

  df_g1 <- data.frame(igraph::get.edgelist(g1))
  colnames(df_g1) <- c("a", "b")
  clrs <- c("blue", "red", "green")
  df_g1$color <- clrs[igraph::E(g1)$weight]

  merge_df <- merge(df_g2, df_g1, all = TRUE)
  merge_df$color[is.na(merge_df$color)] <- "black"
  igraph::E(g2)$color <- merge_df$color
  test.visn <- visNetwork::toVisNetworkData(g2)
  test.visn$edges$value <- abs(igraph::E(g2)$weight)

  sel_nodes <- dimnames(diff_compl)[[1]]
  sel_by <- NULL
  if(!is.null(vals$map_nodes)){
    if(vals$mode == "gxe"){
      sel_by <- "Chromosome"
      test.visn$nodes$Chromosome <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    else{
      sel_by <- "Group"
      test.visn$nodes$Group <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    test.visn$nodes$color.background <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
    igraph::V(g2)$color <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
  }

  test.visn$nodes$font.size <- 20

  ledges <- data.frame(color = c("blue", "green", "red", "black"),
                       label = c("Sign Change", "Gained", "Lost", "Same Direction"), arrows = c("unidrected", "undirected", "undirected", "undirected"))

  visNetwork::visNetwork(test.visn$nodes, test.visn$edges, height = '1000px', width = '1000px') %>%
    visNetwork::visIgraphLayout()  %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), selectedBy = sel_by) %>%
    visNetwork::visLegend(addEdges = ledges, position = "right") %>%
    return()
}

get_dif_net <- function(vals, input){
  mat1 <- vals$networks[[input$net1]]
  mat2 <- vals$networks[[input$net2]]

  diff <- get_diff_mat(mat1, mat2)
  diff <- getNZ(vals = vals, input = input, mat = diff, diff = TRUE)

  shiny::validate(
    shiny::need(dim(diff)[[1]] != 0, "No Difference Between Networks")
  )

  g <- igraph::graph_from_adjacency_matrix(diff, mode = "undirected", weighted = TRUE)
  clrs <- c("blue", "red", "green")
  edge_cols <- clrs[igraph::E(g)$weight]
  igraph::E(g)$color <- edge_cols
  igraph::E(g)$weight[igraph::E(g)$weight != 0] <- 1

  test.visn <- visNetwork::toVisNetworkData(g)

  sel_nodes <- dimnames(diff)[[1]]
  sel_by <- NULL
  if(!is.null(vals$map_nodes)){
    if(vals$mode == "gxe"){
      sel_by <- "Chromosome"
      #test.visn$nodes$Chromosome <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_group
      test.visn$nodes$Chromosome <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    else{
      sel_by <- "Group"
      #test.visn$nodes$Group <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_group
      test.visn$nodes$Group <- vals$map_nodes$node_group[match(sel_nodes, vals$map_nodes$node)]
    }
    #test.visn$nodes$color.background <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_color
    test.visn$nodes$color.background <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
    #igraph::V(g)$color <- vals$map_nodes[vals$map_nodes$node %in% sel_nodes, ]$node_color
    igraph::V(g)$color <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
  }

  test.visn$nodes$font.size <- 20

  ledges <- data.frame(color = c("blue", "green", "red"),
                       label = c("Sign Change", "Gained", "Lost"), arrows = c("unidrected", "undirected", "undirected"))

  visNetwork::visNetwork(test.visn$nodes, test.visn$edges, height = '1000px', width = '1000px') %>%
    visNetwork::visIgraphLayout()  %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), selectedBy = sel_by) %>%
    visNetwork::visLegend(addEdges = ledges, position = "right") %>%
    return()
}

##########################################################
##############DIFFERENCE TABLE SUBTAB#####################
##########################################################

# Function that return a table with the gained, lost, and sign change differences for a chosen network
# against all other networks

get_diff_table <- function(vals, input) {
  chosen_net <- vals$networks[[input$net1]]
  diff_df <- data.frame(Setting = character(), Gained = numeric(), Lost = numeric(), `Sign Change` = numeric())

  for (sett in setdiff(vals$sett_names, input$net1)) {
    res <- get_diff_mat(chosen_net, vals$networks[[sett]])
    gained <- sum(res == 3) / 2
    lost <- sum(res == 2) / 2
    sign_change <- sum(res == 1) / 2
    diff_df <- rbind(diff_df, data.frame("Setting" = sett, "Gained" = gained, "Lost" = lost, "Sign Change" = sign_change))
  }

  dt <- DT::datatable(data = diff_df, caption = input$net1, options = list(lengthChange = FALSE))

  return(dt)
}


##########################################################
##################NODES SETS SUBTAB#######################
##########################################################

get_diff_sets <- function(vals, input){
  mat1 <- getNZ(vals = vals, input = input, mat = vals$networks[[input$net1]])
  mat2 <- getNZ(vals = vals, input = input, mat = vals$networks[[input$net2]])

  mat1_nms <- dimnames(mat1)[[2]]
  mat2_nms <- dimnames(mat2)[[2]]

  if(input$sets_selin == "Union"){
    mats_nms <- union(mat1_nms, mat2_nms)
  }

  else if(input$sets_selin == "Intersection"){
    mats_nms <- intersect(mat1_nms, mat2_nms)
  }

  else if(input$sets_selin == "Complement"){
    mats_nms <- setdiff(mat1_nms, mat2_nms)
  }

  shiny::validate(
    shiny::need(length(mats_nms) != 0, "No Difference Between Networks")
  )

  if(shiny::isTruthy(vals$map_nodes)){
    #mats_nms_grps <- vals$map_nodes[vals$map_nodes$node %in% mats_nms, ]$node_group
    mats_nms_grps <- vals$map_nodes$node_group[match(mats_nms, vals$map_nodes$node)]
    df_tab <- data.frame(mats_nms, mats_nms_grps)
    if(vals$mode == "gxe"){
      colnames(df_tab) <- c("Markers", "Chromosome")
    }
    else{
      colnames(df_tab) <- c("Node", "Node Group")
    }
  }
  else{
    df_tab <- data.frame("Node" = mats_nms)
  }


  dt <- DT::datatable(df_tab, caption = input$sets_selin, options = list(
    pageLength = 15,
    lengthMenu = list(c(15, 25, 50, 75, 100, -1), c("15", "25", "50", "75", "100", "All"))
  ))
  return(dt)
}

##########################################################
##################VENN DIAGRAM SUBTAB#####################
##########################################################

get_venn_diag <- function(vals, input){
  settings <- vector("list")

  if(isTRUE(input$venn_opt)){
    for (i in input$venn_diag_sel){
      settings[[i]] <- get_nz_nodes(mat = vals$networks[[i]], vals = vals, input = input)
    }
  }
  else{
    for (i in input$venn_diag_sel){
      g <- igraph::graph_from_adjacency_matrix(adjmatrix = vals$networks[[i]], mode = "undirected", weighted = TRUE)
      edges <- igraph::get.edgelist(g)
      for (rw in 1:nrow(edges)){
        edges[rw, ] <- sort(edges[rw, ])
      }
      edges <- data.frame(edges)
      settings[[i]] <- do.call(paste, c(edges, sep="-"))
    }
  }

  p <- ggVennDiagram::ggVennDiagram(settings, color = "black", lwd = 0.8, lty = 1) +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::theme(legend.position = "none")
    ggplot2::theme_void()

  return(p)
}


##########################################################
###############CENTRALITY MEASURES TAB####################
##########################################################

create_central_meas <- function(vals, input){
  settings <- vector(mode = "list", length = length(vals$networks))
  for(i in 1:length(vals$networks)){
    settings[[i]] <- getNZ(vals = vals, input = input, mat = vals$networks[[i]])
    settings[[i]] <- get_subnet(vals = vals, mat = settings[[i]])
    settings[[i]]@x[abs(settings[[i]]@x) > 0] <- 1
  }

  settings <- settings[lapply(settings, function(y) length(y@x)) > 0]

  shiny::validate(shiny::need(length(settings) > 0, "No Connections"))

  graph_mats <- lapply(settings, igraph::graph_from_adjacency_matrix, mode = "undirected")
  graph_mats_connected <- lapply(graph_mats, del_iso_nodes)
  suppressWarnings(graph_dfs <- mapply(function(g, nm) data.frame(igraph::vertex_attr(g, "name"), igraph::degree(g), igraph::betweenness(g), igraph::closeness(g), nm),
                                       graph_mats_connected, vals$sett_names, SIMPLIFY = FALSE))

  graph_df <- do.call(rbind.data.frame, graph_dfs)

  if(vals$mode == "gxe"){
    sett_label <- "Environment"
  }
  else{
    sett_label <- "Setting"
  }

  colnames(graph_df) <- c("Name", "Degree", "Betweenness", "Closeness", "Setting")

  df_graphs_reshaped <- stats::reshape(data = graph_df,
                                       direction = "long",
                                       idvar = c("Name", "Setting"),
                                       varying = c("Degree", "Betweenness", "Closeness"),
                                       times = c("Degree", "Beweenness", "Closeness"),
                                       v.names = "Value",
                                       timevar = "Centrality")

  if(shiny::isTruthy(vals$map_nodes)){
    df_graphs_reshaped <- merge(df_graphs_reshaped, vals$map_nodes, by.x = "Name", by.y = "node")
    df_graphs_reshaped <- df_graphs_reshaped[order(factor(df_graphs_reshaped$Name, levels = vals$map_nodes$node)),]
    df_graphs_reshaped$Name <- factor(df_graphs_reshaped$Name, levels = vals$map_nodes$node)
    df_graphs_reshaped$node_group <- factor(df_graphs_reshaped$node_group, levels = unique(vals$map_nodes$node_group))

    if(input$cen_meas_col_switch){
      rects <- merge(graph_df, vals$map_nodes, by.x = "Name", by.y = "node")
      rects <- rects[, c(1, 6, 7)]
      rects <- rects[!duplicated(rects$Name), ]
      rects <- rects[order(factor(rects$Name, levels = vals$map_nodes$node)),]

      a <- tapply(seq_along(rects$node_group), rects$node_group, min)
      a <- a[!is.na(a)]
      a <- data.frame(from = a, node_group = names(a), row.names = NULL)

      b <- tapply(seq_along(rects$node_group), rects$node_group, max)
      b <- b[!is.na(b)]
      b <- data.frame(to = b, node_group = names(b), row.names = NULL)

      rects_com <- merge(a, b, by.x = "node_group", by.y = "node_group")
      rects_com <- merge(rects_com, rects[, 2:3], by.x = "node_group", by.y = "node_group")
      rects_com <- rects_com[!duplicated(rects_com$node_group), ]
      rects_com <- rects_com[order(factor(rects_com$node_group, levels = unique(vals$map_nodes$node_group))),]
      rects_com$node_group <- factor(rects_com$node_group, levels = unique(vals$map_nodes$node_group))

      rects_com$to <- as.numeric(rects_com$to) + 1
      rects_com$to <- rects$Name[rects_com$to]

      rects_com$from <- rects$Name[rects_com$from]

      rects_com[nrow(rects_com), 3] = rects$Name[nrow(rects)]
    }
  }

  if(input$cen_meas_leg_pos %in% c("bottom", "top")){
    div <- 3
    box <- "horizontal"
  }
  else{
    div <- 8
    box <- "vertical"
  }

  p <-  ggplot2::ggplot() +
    ggplot2::geom_jitter(data = df_graphs_reshaped, ggplot2::aes_(x = ~Name, y = ~Value, color = ~Setting), size = 3) +
    ggplot2::facet_grid(Centrality ~ ., scales = "free_y") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = sett_label) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1, size = 13),
                   legend.text = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   plot.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   panel.background = ggplot2::element_rect(fill = "#F5F2F2", color = "black"),
                   legend.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   legend.key = ggplot2::element_rect(fill = "#F5F2F2"),
                   legend.key.size = ggplot2::unit(3, "line"),
                   legend.title =  ggplot2::element_blank(),
                   legend.position = input$cen_meas_leg_pos,
                   legend.box = box)

  if(input$cen_meas_col_switch && shiny::isTruthy(vals$map_nodes)){
    p <- p +
      ggplot2::geom_rect(data = rects_com, ggplot2::aes_(xmin = ~from, xmax = ~to, ymin = -Inf, ymax = Inf, fill = ~node_group), alpha = 0.2) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = ceiling(length(rects_com$node_group)/div))) +
      ggplot2::scale_fill_manual(values = rects_com$node_color)

  }

  return(p)
}

##########################################################
###################METRICS SUBTAB#########################
##########################################################

# Function that gets the plot for the summary statistics plot
# This function takes in no arguments. When called the function returns a
# ggplot object
get_summ_stats_plot <- function(vals, input){
  dat <- data.frame(sett = factor(),
                    meas = factor(),
                    val = numeric())
  err <- FALSE
  for (i in vals$sett_names){
    mat <- getNZ(vals = vals, input = input, mat = vals$networks[[i]], to_plot = TRUE)
    mat <- get_subnet(vals = vals, mat = mat)

    g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
    g2 <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = NULL)
    mat <- del_iso_nodes(g)

    dat <- rbind(dat, data.frame("sett" = i, "meas" ="# of nodes", "val" = igraph::vcount(g)))
    dat <- rbind(dat, data.frame("sett" = i, "meas" = "# of edges", "val" = igraph::ecount(g)))
    dat <- rbind(dat, data.frame("sett" = i, "meas" = "Global Clustering Coefficient", "val" = igraph::transitivity(g)))
    dat <- rbind(dat, data.frame("sett" = i, "meas" = "Average Clustering Coefficient", "val" = igraph::transitivity(g, type = "average")))

    #When users input their own function
    if(input$meas_butt != "" && isFALSE(err)){
      for(str in unlist(strsplit(input$meas_butt, ";"))){
        str <- trimws(str, which = "both")
        func <- unlist(strsplit(str, ","))
        if(!exists(x = func[1], mode = "function", where = asNamespace('igraph'))){
          shiny::showNotification(paste0("Could not find function ", func[1], " in package igraph"), type = "warning")
          err <- TRUE
          next
        }

        tot_func <- paste0("igraph::", func[1], "(g")
        if(length(func) > 1){
          for(j in func[2:length(func)]){
            tot_func <- paste0(tot_func, ",", j)
          }
          tot_func <- paste0(tot_func, ")")
        }

        else{
          tot_func <- paste0(tot_func, ")")
        }
        tryCatch(expr = {
          to_eval <- parse(text = tot_func)
          dat <- rbind(dat, data.frame("sett" = i, "meas" = func[[1]], "val" = eval(to_eval)))
        },
        error = function(cond) {
          err <- TRUE
          shiny::showNotification(paste0("Error: ", cond), type = "error", duration = NULL)
        }
        )
        #to_eval <- parse(text = tot_func)
        #try(dat <- rbind(dat, data.frame("sett" = i, "meas" = func[[1]], "val" = eval(to_eval))), silent = TRUE)
      }
    }
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes_(x = ~sett, y = ~val, fill = ~sett)) +
    ggplot2::geom_bar(stat = 'identity', width = 0.95) +
    ggplot2::facet_wrap(~meas, ncol = 3, scale = "free_y") +
    ggplot2::labs(fill = NULL) +
    ggplot2::ylab("Value") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   plot.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   panel.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   legend.position = "none")

  return(p)
}

##########################################################
###################UNCERTAINTY TAB########################
##########################################################

# Helper function for getting the plots for bootstrap.
# This matrix takes in either an adjacency matrix or an partial correlation
# matrix from one bootstrap sample, it also takes a marker, and a boolean
# value (trait) that indicates whether it is a trait or not
# This function then gets all the names of the other markers that the chosen marker
# has a connection with in a particular bootstrap sample
helper_bootstrap_func <- function(mat, mkr){
  lst <- names(which(mat[mkr, ] != 0))
  lst <- lst[lst != mkr]
  return(lst)
}

# This function creates the bootstrap plots
# This function takes no argument and returns the resamples information on the chosen marker
# or trait chosen in the dropdownmenu
bootstrap_func <- function(vals, mkr, new_res){
  res_boots <- vector("list", length(new_res))
  for (i in 1:length(new_res)){
    res_boots[[i]] <- unlist(lapply(new_res[[i]], helper_bootstrap_func, mkr))
  }
  data <- utils::stack(stats::setNames(res_boots, seq_along(res_boots)))
  names(data) <- c("node", "environment")

  tab <- table(data$node, data$environment)
  for(i in 1:dim(tab)[2]){
    tab[, i] <- tab[, i]/length(new_res[[i]])
  }
  tab <- data.frame(tab)
  colnames(tab) <- c("node", "environment", "freq")
  tab <- merge(tab, vals$map_nodes, by.x = "node", by.y = "node")
  tab$environment <- factor(tab$environment, levels = as.character(1:length(vals$networks)), labels = vals$sett_names)
  tab <- tab[order(factor(tab$node, levels = vals$map_nodes$node)),]
  tab$node <- factor(tab$node, levels = vals$map_nodes$node)
  tab$node_group <- factor(tab$node_group, levels = unique(vals$map_nodes$node_group))
  row.names(vals$map_nodes) <- vals$map_nodes$node
  frame_colors <- unique(vals$map_nodes[unique(as.character(tab$node)), ]$node_color)
  # We create a dataframe that plots diamonds shapes on top of markers
  # that in the real data also has a connection with marker/trait. True positives.
  df_conn <- data.frame(node = factor(),
                        environment = factor(),
                        freq = as.numeric())

  for (i in vals$sett_names){
    mat <- vals$networks[[i]][, mkr]
    #POTENTIAL BUG
    mks_all <- vals$node_names[which(mat != 0)]
    mks_all <- mks_all[mks_all != mkr]
    tab_new <- tab[tab$environment == i, ]
    df_conn <- rbind(df_conn, tab_new[tab_new$node %in% mks_all, 1:3])
  }
  lgnd.pos <- ifelse(is.null(vals$map_nodes), "none", "right")

  if(vals$mode == "gxe"){
    text_tooltip <- "Chromosome: "
  }
  else{
    text_tooltip <- "Group: "
  }
  p <- suppressWarnings(
    ggplot2::ggplot() +
      ggplot2::geom_bar(data = tab, ggplot2::aes(x = node,
                                                 y = freq,
                                                 fill = node_group,
                                                 text = paste0("Name: ",  node, "\n", text_tooltip, node_group),
                                                 text2 = paste0("Percentage", freq * 100)),
                        stat = 'identity') +
      ggplot2::geom_point(data = df_conn, ggplot2::aes_(x = ~node, y = ~freq), shape = 23, fill = "red", color = "black", color = "red", size = 2) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
      ggplot2::facet_wrap(~environment, nrow = length(vals$networks)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1, size = 10),
                     legend.background = ggplot2::element_rect(fill = "#F5F2F2"),
                     legend.text = ggplot2::element_text(size = 15),
                     strip.text = ggplot2::element_text(face = "bold"),
                     axis.title.y = ggplot2::element_text(size = 15),
                     axis.title.x = ggplot2::element_text(size = 16, face = "bold")) +
      ggplot2::labs(fill = NULL) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::scale_fill_manual(values = frame_colors) +
      ggplot2::theme(legend.position = lgnd.pos))

  gp <- plotly::ggplotly(p, tooltip = c("text", "text2"))

  for(i in 1:length(gp$x$data)){
    text <- gp$x$data[[i]]$text
    text <- gsub("paste0(\"Percentage\", freq * 100): Percentage", "Certainty: ", text, fixed = TRUE)
    if(is.null(vals$mapping)){
      if(vals$mode == "gxe"){
        text <- gsub("Chromosome:(.*?)>", "", text)
      }
      else{
        text <- gsub("Group:(.*?)>", "", text)
      }

    }
    gp$x$data[[i]]$text <- text
  }

  gp <- plotly::layout(gp, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')

  return(gp)
}

##########################################################
###############NETWORK DISTANCES SUBTAB###################
##########################################################

# Function that takes two matrices as argument and gets the distance between the two matrices
# based on a distance metric. The function can calculate the distance between matrices based
# on the weighted matrices or adjacency matrices
mat_dist <- function(mat1, mat2, dist = "Euclidean", mat_type){
  if(mat_type == "Adjacency"){
    mat1@x[abs(mat1@x) > 0] <- 1
    mat2@x[abs(mat2@x) > 0] <- 1
  }

  diff_mat <- mat1 - mat2

  if(dist == "Euclidean"){
    diff_mat <- diff_mat^2
    all_sum <- sqrt(sum(diff_mat))

  }
  else if(dist == "Manhattan"){
    diff_mat <- abs(diff_mat)
    all_sum <- sum(diff_mat)
  }

  else if(dist == "Canberra"){
    diff_mat <- sum(abs(diff_mat))
    denom <- sum(abs(mat1) + abs(mat2))
    all_sum <- diff_mat/denom
  }

  # else if(dist == "Jaccard"){
  #   intersection <- sum(mat1 == mat2)
  #   denom <- mat1@dim[[1]]^2 - intersection
  #   all_sum <- 1 - intersection/denom
  # }
  return(all_sum)
}

apply_mat_dist_list <- function(vals, input, for_plot = FALSE){
  mat_list <- vals$networks
  names(mat_list) <- vals$sett_names
  check_dims <- sapply(vals$networks, function(x) dim(x)[[2]])
  if(length(unique(check_dims)) != 1){
    shiny::showNotification("WARNING: Dimension of Networks Are Different. Only Common Nodes Will Be Used.", type = "warning", duration = NULL)
    node_per_network <- lapply(vals$networks, function(x) dimnames(x)[[1]])
    intersection <- Reduce(intersect, node_per_network)
    for(i in 1:length(mat_list)){
      ind <- which(dimnames(mat_list[[i]])[[2]] %in% intersection)
      mat_list[[i]] <- mat_list[[i]][ind, ind]
    }
  }
  combis <- t(utils::combn(names(mat_list), 2))

  if(for_plot){
    combis_df <- data.frame(combis)
    colnames(combis_df) <- c("net1", "net2")
    for(d in input$dist_meas_plot){
      distances <- apply(combis, 1,
                         function(x) mat_dist(mat_list[[x[1]]], mat_list[[x[2]]], dist = d, mat_type = input$mat_type_plot))
      combis_df[[paste0(d)]] <- distances
    }

    combis_df <- combis_df[order(factor(combis_df$net1, levels = vals$sett_names)),]
    combis_df$net2 <- factor(combis_df$net2, levels = vals$sett_names)
    combis_df$net1 <- factor(combis_df$net1, levels = vals$sett_names)
    nc <- ncol(combis_df)
    new_combis_df <- combis_df[, c(2, 1, 3:nc)]
    colnames(new_combis_df) <- colnames(combis_df)
    combis_df <- rbind(combis_df, new_combis_df)
    combis_df <- stats::reshape(combis_df,
                                varying = colnames(combis_df[3:nc]),
                                v.names = "value",
                                timevar = "Distance Measure",
                                times = colnames(combis_df[3:nc]),
                                direction = "long")

    if(isTRUE(input$log_check)){
      combis_df$value <- log(combis_df$value)
    }

    combis_df$net2 <- factor(combis_df$net2)

    p <- ggplot2::ggplot(combis_df, ggplot2::aes_(x = ~net2, y = ~value, fill =  ~`Distance Measure`)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge") +
      ggplot2::facet_wrap(~net1, scales = "free_x") +
      ggplot2::xlab(NULL) +
      ggplot2::ylab("Value") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                     axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                     axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                     strip.text = ggplot2::element_text(size = 14, face = "bold")) +
      ggplot2::labs(fill = "Distance Measures")


    return(p)
  }

  else{
    distances <- apply(combis, 1,
                       function(x) mat_dist(mat_list[[x[1]]], mat_list[[x[2]]], dist = input$dist_meas_table, mat_type = input$mat_type_table))
    distances_mat <- matrix(nrow = length(mat_list), ncol = length(mat_list))
    distances_mat[lower.tri(distances_mat)] <- distances

    rownames(distances_mat) <- vals$sett_names
    colnames(distances_mat) <- vals$sett_names
    if(ncol(distances_mat) > 1){
      distances_mat <- distances_mat[-1, -ncol(distances_mat), drop = FALSE]
    }
  }

  return(distances_mat)
}

##########################################################
##################COMMUNITIES SUBTAB######################
##########################################################

comm_detection_plot <- function(vals, input, setting){
  dat <- data.frame(sett = factor(),
                    meas = factor(),
                    val = numeric())

  mat <- getNZ(vals = vals, input = input, mat = vals$networks[[setting]])
  mat <- get_subnet(vals = vals, mat = mat)

  shiny::validate(shiny::need(length(mat@x) > 0, "No Connections"))

  mat@x[abs(mat@x) > 0] <- 1
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = NULL)
  g <- del_iso_nodes(g)

  lay <- igraph::layout_with_fr(g)
  row.names(lay) <- names(igraph::V(g))

  if(vals$cluster_algs == "Fast Greedy"){
    try(c1 <- igraph::cluster_fast_greedy(g))
  }
  else if(vals$cluster_algs == "Edge Betweenness"){
    try(c1 <- igraph::cluster_edge_betweenness(g))
  }

  row_lay <- rownames(lay)
  row_coords <- rownames(vals$coords)
  for (i in 1:nrow(lay)){
    if(is.element(row_lay[i], row_coords)){
      lay[row_lay[i], ] <- vals$coords[row_lay[i], ]
    }
  }

  sel_nodes <- dimnames(mat)[[1]]
  if(!is.null(vals$map_nodes)){
    frame_colors <- vals$map_nodes$node_color[match(sel_nodes, vals$map_nodes$node)]
  }

  p <- plot(c1, g, layout = cbind(lay[, 1], -1 * lay[, 2]), edge.curved = vals$roundness)
  return(p)
}

##########################################################
##################MODULARITY SUBTAB#######################
##########################################################

modularity_plot <- function(vals, input){
  dat <- data.frame(sett = factor(),
                    meas = factor(),
                    val = numeric())
  for (i in vals$sett_names){
    mat <- getNZ(vals = vals, input = input, mat = vals$networks[[i]])
    mat <- get_subnet(vals = vals, mat = mat)
    mat@x[abs(mat@x) > 0] <- 1
    g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = NULL)
    g <- del_iso_nodes(g)

    try(dat <- rbind(dat, data.frame("sett" = i, "meas" ="Fast Greedy", "val" = igraph::modularity(igraph::cluster_fast_greedy(g)))))
    try(dat <- rbind(dat, data.frame("sett" = i, "meas" = "Leading Eigenvector", "val" = igraph::modularity(igraph::cluster_leading_eigen(g)))))
    if(input$meas_butt != ""){
      for(str in unlist(strsplit(input$meas_butt, ";"))){
        str <- trimws(str, which = "both")
        func <- unlist(strsplit(str, ","))

        if(!exists(func[1], mode = "function")){
          next
        }

        tot_func <- paste0(func[1], "(g")
        if(length(func) > 1){
          for(j in func[2:length(func)]){
            tot_func <- paste0(tot_func, ",", j)
          }
          tot_func <- paste0(tot_func, ")")
        }

        else{
          tot_func <- paste0(tot_func, ")")
        }
        tot_func <- paste0("modularity(", tot_func, ")")
        to_eval <- parse(text = tot_func)
        dat <- rbind(dat, data.frame("sett" = i, "meas" = func[1], "val" = eval(to_eval)))
      }
    }
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes_(x = ~sett, y = ~val, fill = ~sett)) +
    ggplot2::geom_bar(stat = 'identity', width = 0.95) +
    ggplot2::facet_wrap(~meas, ncol = 3, scale = "free_y") +
    ggplot2::ylab("Value") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   plot.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   panel.background = ggplot2::element_rect(fill = "#F5F2F2"),
                   legend.position = "none")

  return(p)
}
