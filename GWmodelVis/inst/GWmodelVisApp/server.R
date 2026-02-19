# server.R

# 服务器端代码目录 全局变量
code_dir <- getwd()
# 修改shiny工作目录
# setwd(code_dir)
# 临时路径
dir <- "temp"
if (!dir.exists(dir)) dir.create(dir)
temp_dir <- file.path(code_dir, "temp")  # 拼接 temp 路径
# 打印路径调试
cat("file path: ", code_dir, "\n")
cat("temp path: ", temp_dir, "\n")

# 设置最大上传文件大小限制（以字节为单位）
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# 全局变量，存储服务器对象
file_server <- NULL
# 启动文件服务器(for gwboxplot)
start_servr <- function() {
  # 停止已存在的服务器
  running_servers <- servr::daemon_list()
  if (length(running_servers) > 0) {
    lapply(running_servers, servr::daemon_stop)
    cat("Stopped all existing servers.\n")
  }

  # 动态分配端口
  port <- 8000
  while (port <= 9000) {
    tryCatch({
      server <- servr::httd(dir = temp_dir, port = port, daemon = TRUE)
      cat("Server started at port:", port, "\n")
      return(server)
    }, error = function(e) {
      cat("Port", port, "is in use. Trying next port...\n")
      port <- port + 1
    })
  }

  stop("Failed to start server: All ports are in use.")
}
# 停止文件服务器
stop_servr <- function(server) {
  if (!is.null(server) && server$server %in% names(servr::daemon_list())) {
    servr::daemon_stop(server$server)
    cat("R HTTP server stopped.\n")
  } else {
    cat("No running server found or invalid server object.\n")
  }
}

# gw_boxplot function
findmedian <- function(x, w) {
    xo <- sort(x)
    wo <- w[order(x)]

  # 找到累积和首次大于0.5的位置
    idx <- which(cumsum(wo) >= 0.5, arr.ind = TRUE)
    if (length(idx) == 0) {
        # 如果没有找到合适的索引，则返回最后一个元素
        idx <- length(xo)
    } else {
        idx <- idx[1]
    }
    xo[idx]
}
findQ1 <- function(x,w){
    xo <- sort(x)
    wo <- w[order(x)]

    # 找到累积和首次大于0.25的位置
    idx <- which(cumsum(wo) >= 0.25, arr.ind = TRUE)
    if (length(idx) == 0) {
        # 如果没有找到合适的索引，则返回最后一个元素
        idx <- length(xo)
    } else {
        idx <- idx[1]
    }
    xo[idx]
}
findQ3 <- function(x,w){
    xo <- sort(x)
    wo <- w[order(x)]

    # 找到累积和首次大于0.75的位置
    idx <- which(cumsum(wo) >= 0.75, arr.ind = TRUE)
    if (length(idx) == 0) {
        # 如果没有找到合适的索引，则返回最后一个元素
        idx <- length(xo)
    } else {
        idx <- idx[1]
    }
    xo[idx]
}
findMin <- function(Q1,Q3){
       IQR=Q3-Q1
       Min=Q1-1.5*IQR
       Min
}
findMax <- function(Q1,Q3){
       IQR=Q3-Q1
       Max=Q3+1.5*IQR
       Max
}
gw_boxplot <- function(bw, X, kernel, adaptive, dp.locat, p, theta, longlat, calibrationPoint, gwbp_bpcolor) {
  # 初始化默认值
  if (is.null(p)) p <- 2
  if (is.null(longlat)) longlat <- TRUE
  theta <- 0
  # 确保 dp.locat 和 calibrationPoint 是矩阵
  if (!is.matrix(dp.locat)) dp.locat <- as.matrix(dp.locat)
  if (!is.null(calibrationPoint) && !is.matrix(calibrationPoint)) calibrationPoint <- as.matrix(calibrationPoint)

  # 根据是否有 calibrationPoint 决定使用的循环点
  loop_points <- if (!is.null(calibrationPoint)) calibrationPoint else dp.locat

  # 检查 dp.locat 和 loop_points 是否有效
  if (nrow(dp.locat) == 0 || ncol(dp.locat) != 2) {
    stop("dp.locat must be a non-empty matrix with 2 columns (longitude, latitude).")
  }
  if (nrow(loop_points) == 0 || ncol(loop_points) != 2) {
    stop("loop_points must be a non-empty matrix with 2 columns (longitude, latitude).")
  }
  # 数据点数量
  point_count <- nrow(loop_points)

  # GWBoxplot Display---------------------------------------------------------------------------
  # 初始化结果存储
  result_data <- data.frame(
    FID = 1:point_count,
    Min = numeric(point_count),
    Q1 = numeric(point_count),
    Median = numeric(point_count),
    Q3 = numeric(point_count),
    Max = numeric(point_count),
    longitude = loop_points[, 1],
    latitude = loop_points[, 2]
  )

   # 计算 Boxplot 数据
  calculate_boxplot <- function(i) {
    # 计算距离和权重
    dist.vi <- GWmodel::gw.dist(dp.locat = dp.locat, rp.locat = loop_points, focus = i, p = p, theta = theta, longlat = longlat)
    W.i <- matrix(GWmodel::gw.weight(dist.vi, bw, kernel, adaptive), nrow = 1)
    Wi <- W.i / sum(W.i)  # 归一化权重

    # 计算统计值
    median <- findmedian(X, w = c(Wi))
    Q1 <- findQ1(X, w = c(Wi))
    Q3 <- findQ3(X, w = c(Wi))
    min_val <- findMin(Q1, Q3)
    max_val <- findMax(Q1, Q3)
    max_min_diff <- max_val - min_val

    return(list(
       min_val = min_val, Q1 = Q1, median = median, Q3 = Q3, max_val = max_val, max_min_diff = max_min_diff
    ))
  }

  # 逐点计算统计值
  boxplot_data <- lapply(1:point_count, calculate_boxplot)
  global_min <- min(sapply(boxplot_data, function(x) x$min_val))
  global_max <- max(sapply(boxplot_data, function(x) x$max_val))

  # 批量绘制 Boxplot
  generate_boxplot <- function(i, stats) {

    # 获取统计数据
    min_val <- stats$min_val
    Q1 <- stats$Q1
    median <- stats$median
    Q3 <- stats$Q3
    max_val <- stats$max_val
    
    # 统一全局高度，确保所有 Boxplot 处于相同数轴
    total_height <- 1.5 
    
    # 归一化函数，使所有 Boxplot 统一在 (global_min, global_max) 范围内缩放
    normalize <- function(val) {
      return ((val - global_min) / (global_max - global_min) * total_height)
    }

    # 计算归一化 y 坐标
    y_min <- normalize(min_val)
    y_Q1 <- normalize(Q1)
    y_median <- normalize(median)
    y_Q3 <- normalize(Q3)
    y_max <- normalize(max_val)

    # 设定坐标位置
    x_center <- 0
    box_width <- 0.4  # 箱体宽度
    x_range <- c(x_center - 0.5, x_center + 0.5)  # 固定x轴范围

    # 定义 Boxplot 的绘制数据
    # 箱体（矩形主体）
    box <- data.frame(
      x = c(x_center - box_width / 2, x_center + box_width / 2, 
            x_center + box_width / 2, x_center - box_width / 2),
      y = c(y_Q1, y_Q1, y_Q3, y_Q3),
      label = factor(i)
    )
    # 须线（whiskers）和极值线
    lines <- list(
      line_min = data.frame(x = c(x_center, x_center), y = c(y_min, y_Q1), label = factor(i)),
      line_minl = data.frame(x = c(x_center - box_width / 2, x_center + box_width / 2), y = c(y_min, y_min), label = factor(i)),
      line_max = data.frame(x = c(x_center, x_center), y = c(y_Q3, y_max), label = factor(i)),
      line_maxl = data.frame(x = c(x_center - box_width / 2, x_center + box_width / 2), y = c(y_max, y_max), label = factor(i)),
      line_median = data.frame(x = c(x_center - box_width / 2, x_center + box_width / 2), y = c(y_median, y_median), label = factor(i))
    )

    # 绘制 Boxplot
    p <- ggplot2::ggplot() +
      # 固定x轴显示范围确保宽度一致
      ggplot2::xlim(x_range[1], x_range[2]) +
      ggforce::geom_shape(data = box, ggplot2::aes(x, y, group = label), alpha = 0.7, linewidth = 0.5, fill = gwbp_bpcolor, colour = "black") +
      # geom_rect(data = box, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = gwbp_bpcolor), color = "black", alpha = 0.7) + 
      ggplot2::geom_line(data = lines$line_min, ggplot2::aes(x, y, group = label), linetype = 1, linewidth = 0.5) +
      ggplot2::geom_line(data = lines$line_minl, ggplot2::aes(x, y, group = label), linetype = 1, linewidth = 0.5) +
      ggplot2::geom_line(data = lines$line_maxl, ggplot2::aes(x, y, group = label), linetype = 1, linewidth = 0.5) +
      ggplot2::geom_line(data = lines$line_max, ggplot2::aes(x, y, group = label), linetype = 1, linewidth = 0.5) +
      ggplot2::geom_line(data = lines$line_median, ggplot2::aes(x, y, group = label), linetype = 1, linewidth = 1) +
      # geom_glowline(data = lines$line_median, ggplot2::aes(x, y, group = label),color = "cyan", size = 0.5, alpha = 0.8, shadowcolor = "cyan", shadowalpha = 0.3, shadowwidth = 0.5) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")  # 移除图例

    ggplot2::ggsave(
      filename = paste0(code_dir, "/temp/gw_boxplot_", i, ".png"), 
      plot = p, 
      width = 2,  # 固定宽度 + 边距
      height = total_height  + 2,  # 动态高度 + 边距
      units = "in", 
      dpi = 300)
  }

  # 批量绘制并保存结果
  for (i in 1:point_count) {
    generate_boxplot(i, boxplot_data[[i]])
    result_data[i, 2:6] <- unlist(boxplot_data[[i]][1:5])  # 保存结果
  }

  return(result_data)
}
gw_boxplot_outliers <- function(bw, X, kernel, adaptive, dp.locat, p = 2, theta = 0, longlat = TRUE) {
  if (!is.matrix(dp.locat)) dp.locat <- as.matrix(dp.locat)
  # 数据点数量
  dp.n <- nrow(dp.locat)

  # 初始化结果存储
  result <- data.frame(
    FID = numeric(dp.n),
    Min = numeric(dp.n),
    Q1 = numeric(dp.n),
    Median = numeric(dp.n),
    Q3 = numeric(dp.n),
    Max = numeric(dp.n),
    Outliers = numeric(dp.n),
    TorF_outlier = logical(dp.n),
    longitude = dp.locat[, 1],
    latitude = dp.locat[, 2]
  )

  # 核心函数：计算距离、权重和统计值
  calculate_outlier <- function(i) {
    # 计算距离
    dist.vi <- GWmodel::gw.dist(dp.locat = dp.locat, focus = i, p = p, theta = theta, longlat = longlat)
    # 计算权重
    W.i <- GWmodel::gw.weight(dist.vi, bw, kernel, adaptive)
    if (any(is.nan(W.i))) stop("The weight vector contains NaN values.")
    Wi <- W.i / sum(W.i)  # 归一化权重

    # 计算统计值
    median <- findmedian(X, w = Wi)
    Q1 <- findQ1(X, w = Wi)
    Q3 <- findQ3(X, w = Wi)
    min_val <- findMin(Q1, Q3)
    max_val <- findMax(Q1, Q3)
    outlier_flag <- (X[i] < min_val) || (X[i] > max_val)

    # 返回结果
    return(list(
      min = min_val,
      Q1 = Q1,
      median = median,
      Q3 = Q3,
      max = max_val,
      outlier = X[i],
      outlier_flag = outlier_flag
    ))
  }

  # 遍历每个点，计算结果
  outlier_results <- lapply(1:dp.n, calculate_outlier)

  # 填充结果到数据框
  result$FID <- 1:dp.n
  result$Median <- sapply(outlier_results, function(x) x$median)
  result$Q1 <- sapply(outlier_results, function(x) x$Q1)
  result$Q3 <- sapply(outlier_results, function(x) x$Q3)
  result$Min <- sapply(outlier_results, function(x) x$min)
  result$Max <- sapply(outlier_results, function(x) x$max)
  result$Outliers <- sapply(outlier_results, function(x) x$outlier)
  result$TorF_outlier <- sapply(outlier_results, function(x) x$outlier_flag)

  return(result)
}

# point_card function is used to generate the card content for each point on the map
point_card <- function (gwbp_Name, gwbp_price, gwbp_longitude, gwbp_latitude, gwbp_max, gwbp_Q3, gwbp_median, gwbp_Q1, gwbp_min) {
  card_content <- paste0("
                    <style>
                    div.leaflet-popup-content {width:auto !important;}
                        .flip-card {
                          background-color: transparent;
                          width: 300px;
                          height: 330px;
                          perspective: 1000px;
                        }

                        .flip-card-inner {
                          position: relative;
                          width: 100%;
                          height: 100%;
                          text-align: center;
                          transition: transform 0.6s;
                          transform-style: preserve-3d;
                          box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                        }

                        .flip-card:hover .flip-card-inner {
                          transform: rotateY(180deg);
                        }

                        .flip-card-front, .flip-card-back {
                          position: absolute;
                          width: 100%;
                          height: 100%;
                          backface-visibility: hidden;
                        }

                        .flip-card-front {
                          background-color: #CCE4EF;
                          color: black;
                          z-index: 2;
                        }

                        .flip-card-back {
                          background-color: #92B5CA;
                          color: white;
                          transform: rotateY(180deg);
                          z-index: 1;
                        }
                    </style>",
                    "<table style='width:100%; background-color: #CCE4EF;'>",
                    "<tr>",
                    "<th><b><h1 style='text-align: left;'>",gwbp_Name,"</h1></b></th>",
                    "</tr>",
                    "</table>",
                    "<div class='flip-card'>",
                      "<div class='flip-card-inner'>",
                        "<div class='flip-card-front'>",
                          "<h3>Variable parameter</h3>",
                          "<hr>",
                          "<table style='width:100%;'>",
                            "<tr>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 20px;'><h4>GWBoxplot_MAX:</h4></td>",
                              "<td><h4>",gwbp_max,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 20px;'><h4>GWBoxplot_Q3:<h4></td>",
                              "<td><h4>",gwbp_Q3,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 20px;'><h4>GWBoxplot_Median:<h4></td>",
                              "<td><h4>",gwbp_median,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 20px;'><h4>GWBoxplot_Q1:<h4></td>",
                              "<td><h4>",gwbp_Q1,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 20px;'><h4>GWBoxplot_MIN:<h4></td>",
                              "<td><h4>",gwbp_min,"</h4></td>",
                            "</tr>",
                          "</table>",
                          "</div>",
                        "<div class='flip-card-back'>",
                          "<h3>GW_Boxplot parameter</h3>",
                          "<hr>",
                          "<table style='width:80%;'>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Variable: </b>",gwbp_price,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Longitude: </b>",gwbp_longitude,"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Latitude: </b>",gwbp_latitude,"</h4></td>",
                            "</tr>",
                          "</table>",
                          "</div>",
                      "</div>",
                    "</div>"
  )
  return(card_content)
}
# small outliers popup function
small_outliers_popup <- function(x_field,Outliers) {
  content <- paste0('<body style="background-color: white;">
                      <div style="background-color: white; text-align:center;">
                      <h4>',x_field,'</h4>
                      <b>', Outliers, '</b>
                      </div>
                      </body>')
  return(content)
}
# 定义颜色渐变函数
color_gradients <- list(
  "green-red" = c(low = "green", high = "red"),
  "blue-yellow" = c(low = "blue", high = "yellow"),
  "purple-orange" = c(low = "purple", high = "orange"),
  "magenta-cyan" = c(low = "magenta", high = "cyan"),
  "pink-blue" = c(low = "pink", high = "blue"),
  "viridis" = c(low = "viridis", high = NA)  # 使用 viridis 色系
)
# 通用绘图函数
generate_plot <- function(data, sdf_col, bandwidth, output_path, color_palette, extended_min, extended_max, fixed_breaks) {
  # 判断几何类型
  geom_type <- unique(sf::st_geometry_type(data))
  is_polygon <- "POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type
  is_point <- "POINT" %in% geom_type || "MULTIPOINT" %in% geom_type
  
  # 根据用户选择的色阶动态设置颜色映射
  if (color_palette == "viridis") {
    color_mapping <- if (is_polygon) {
      ggplot2::scale_fill_viridis_c(
        limits = c(extended_min, extended_max),  # 扩展范围
        breaks = fixed_breaks,                  # 固定分割点
        oob = scales::squish                    # 超出范围的值压缩到边界颜色
      )
    } else if (is_point) {
      ggplot2::scale_color_viridis_c(
        limits = c(extended_min, extended_max),
        breaks = fixed_breaks,
        oob = scales::squish
      )
    } else {
      stop("Unsupported geometry type for plotting.")
    }
  } else {
    gradient <- color_gradients[[color_palette]]
    color_mapping <- if (is_polygon) {
      ggplot2::scale_fill_gradient(
        low = gradient["low"], 
        high = gradient["high"],
        limits = c(extended_min, extended_max),  # 扩展范围
        breaks = fixed_breaks,                  # 固定分割点
        oob = scales::squish                    # 超出范围的值压缩到边界颜色
      )
    } else if (is_point) {
      ggplot2::scale_color_gradient(
        low = gradient["low"], 
        high = gradient["high"],
        limits = c(extended_min, extended_max),
        breaks = fixed_breaks,
        oob = scales::squish
      )
    } else {
      stop("Unsupported geometry type for plotting.")
    }
  }

  # 绘制并保存图像
  p <- ggplot2::ggplot(data) +
    {if (is_polygon) {
      ggplot2::geom_sf(ggplot2::aes(fill = value))  # 使用 fill 来绘制多边形
    } else if (is_point) {
      ggplot2::geom_sf(ggplot2::aes(color = value))  # 使用 color 来绘制点数据
    }}  +
    color_mapping +
    ggplot2::labs(title = paste0(sdf_col, "  (Bandwidth: ", bandwidth, ")")) +
    ggspatial::annotation_north_arrow(
      location = "tl",  # 左上角
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering()
    ) +
    ggspatial::annotation_scale(
      location = "bl",  # 左下角
      width_hint = 0.3
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  # 保存图片
  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 5, height = 5, units = "in", dpi = 150
  )
}
# 通用Web Map生成音频（根据系数）
generate_audio <- function(value, filename,x_min = NULL, x_max = NULL) {

  # waveform_ratio = c(0.7, 0.2, 0.1)
  # adsr = c(attack = 0.05, decay = 0.1, sustain = 0.6, release = 0.15)
  # filter_cutoff = 0.4
  # loudness_factor = 2
  waveform_ratio = c(0.6, 0.3, 0.1)  # 降低方波占比，增强二次谐波
  adsr = c(attack = 0.05, decay = 0.1, sustain = 0.5, release = 0.2)  # 让音色更平滑
  filter_cutoff = 0.1  # **降低低通滤波器截止频率**
  loudness_factor = 0.2 # **降低音量归一化系数**
  sampling_rate = 44100
  duration_per_note = 0.5
  
  # 异常值处理
  if (is.na(value) || is.null(value)) value <- 0
  
  # --- 频率映射 ---
  if (is.null(x_min)) x_min <- min(value)
  if (is.null(x_max)) x_max <- max(value)
  
  a <- (1000 - 440) / (x_max - x_min)
  b <- 440 - a * x_min
  freq <- a * value + b  
  
  # --- 波形生成参数 ---
  note_samples <- round(duration_per_note * sampling_rate)
  t <- seq(0, duration_per_note, length.out = note_samples)

  tryCatch({
    # 基波（正弦波）
    base_wave <- sin(2 * pi * freq * t)

    # 谐波成分
    harmonic1 <- sin(2 * pi * 2 * freq * t) * waveform_ratio[2]
    harmonic2 <- sin(2 * pi * 3 * freq * t) * waveform_ratio[3]

    # 混合波形
    mixed_wave <- (waveform_ratio[1] * base_wave) + harmonic1 + harmonic2

    # --- ADSR包络 ---
    apply_envelope <- function(signal, adsr_params) {
      n <- length(signal)
      attack_len <- floor(adsr_params["attack"] * n)
      decay_len <- floor(adsr_params["decay"] * n)
      release_len <- floor(adsr_params["release"] * n)
      sustain_len <- n - attack_len - decay_len - release_len
      
      envelope <- c(
        seq(0, 1, length.out = attack_len),
        seq(1, adsr_params["sustain"], length.out = decay_len),
        rep(adsr_params["sustain"], sustain_len),
        seq(adsr_params["sustain"], 0, length.out = release_len)
      )
      length(envelope) <- n  # 长度对齐
      signal * envelope
    }
    
    enveloped_wave <- apply_envelope(mixed_wave, adsr)

    # --- 低通滤波 ---
    bf <- signal::butter(4, filter_cutoff, type = "low")
    filtered_wave <- signal::filtfilt(bf, enveloped_wave)

    # --- 规范化处理（降低音量）---
    normalized_wave <- filtered_wave / max(abs(filtered_wave)) * loudness_factor
    normalized_wave <- pmin(pmax(normalized_wave, -0.5), 0.5)  # **进一步限制振幅**

    # --- 生成Wave对象 ---
    audio_wave <- tuneR::Wave(
      left = as.integer(normalized_wave * 32767),
      right = as.integer(normalized_wave * 32767),
      samp.rate = sampling_rate,
      bit = 16
    )

    # 保存文件
    tuneR::writeWave(audio_wave, filename)
    return(filename)

  }, error = function(e) {
    warning("音频生成失败:", e$message)
    return(NULL)
  })
}
# 合成音频的函数（依赖 tuneR 包）
concatenate_audio <- function(audio_files, output_file) {
  if(length(audio_files) == 0) return(NULL)
  library(tuneR)
  combined <- tuneR::readWave(audio_files[1])
  if(length(audio_files) > 1) {
    for(file in audio_files[-1]){
      wav <- tuneR::readWave(file)
      # 简单拼接音频
      combined <- bind(combined, wav)
    }
  }
  tuneR::writeWave(combined, output_file)
}
# 检查字符串是否包含中文字符的函数
has_chinese <- function(x) {
  grepl("[\u4e00-\u9fff]", x)
}
musicIcon <- leaflet::makeIcon(
      iconUrl = "www/music_note.png",  # 确保在www目录放置音乐图标
      iconWidth = 24, 
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
  )

# Server
server <- function(input, output,session) {
  #--------------------------# 全局变量 #--------------------------#
  running <- reactiveVal(TRUE)
  point_cards_reactive <- reactiveVal(data.frame()) # 初始化反应式值来存储marker点数据
  addResourcePath("www", "www")  # 注册 www 目录

  #--------------------------# Home #--------------------------#
  observeEvent(input$btn_gwss, {
    updateTabItems(session, "tabs", "gwss")
  })
  observeEvent(input$btn_gw_boxplot, {
    updateTabItems(session, "tabs", "gw_boxplot")
  })
  observeEvent(input$btn_gwr_basic, {
    updateTabItems(session, "tabs", "gwr")
  })
  observeEvent(input$btn_gwr_multiscale, {
    updateTabItems(session, "tabs", "mgwr")
  })
  observeEvent(input$btn_gwpca, {
    updateTabItems(session, "tabs", "gwpca")
  })
  #--------------------------# GWSS #--------------------------#
  # 按钮跳转
  observeEvent(input$gwss_execute, {
    updateTabsetPanel(session, "gwss_tabs", selected = "Summary Information")
  })
  observeEvent(input$gwss_video_button, {
    updateTabsetPanel(session, "gwss_tabs", selected = "Video")
  })
  # shp读取函数
  gwss_shapefile_data <- reactive({
    req(input$gwss_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$gwss_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwss_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwss_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      temp_dir <- tempdir()
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch(
          {
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },
          error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # 更新自变量&图层选项
  observeEvent(gwss_shapefile_data(), {
    # 获取变量名并按首字母排序（不区分大小写）
    var_names <- names(gwss_shapefile_data())
    
    # 检查变量名是否为空
    if (length(var_names) == 0) {
      sorted_vars <- character(0)  # 空字符向量
    } else {
      # 不区分大小写排序
      sorted_vars <- var_names[order(tolower(var_names))]
    }
    
    # 确保默认选中项有效（至少有两个变量时才选中前两个）
    selected_vars <- if (length(sorted_vars) >= 2) {
      sorted_vars[1:2]
    } else {
      sorted_vars  # 不足两个时选中所有可用变量
    }
    updateSelectInput(session, "gwss_vars", choices = sorted_vars, selected = selected_vars)
  })
  # 输出自变量选择控件
  output$gwss_vars <- renderUI({
    selectInput("gwss_vars", "Choose variables", choices = NULL, multiple = TRUE, selected = NULL)
  })
  # 更新带宽滑块
  observe({
      req(input$gwss_shapefile)  # 确保文件已上传
      shp_data <- gwss_shapefile_data()  # 读取 shapefile 数据
      shp_data <- sf::st_make_valid(shp_data)      # 确保数据有效
      req(shp_data)

      # 计算最大距离（米），确保单位正确
      if (sf::st_is_longlat(shp_data)) {
          max_distance <- as.integer(max(sf::st_distance(shp_data)))  # 计算球面距离
      } else {
          coords <- sf::st_coordinates(shp_data)
          max_distance <- as.integer(max(dist(coords)))  # 计算欧式距离
      }

      # 读取是否为自适应带宽
      gwss_adaptive <- as.logical(input$gwss_adaptive)

      # 设置带宽的最大值和单位
      if (gwss_adaptive) {
          max_bw <- nrow(shp_data)  # 以点的个数为单位
          label <- "Bandwidth (Number of points)"
      } else {
          max_bw <- max_distance  # 以最大距离为单位
          label <- "Bandwidth (meter)"
      }

      # 更新滑块
      updateSliderInput(session, "gwss_bandwidth",
                        max = max_bw,
                        label = label)
  })
  # gwss结果对象
  gwss_bw_data <- reactiveValues(
    bw = NULL,          
  )
  # 地图可视化
  output$gwss_mapPlot <- leaflet::renderLeaflet({
    map <- leaflet::leaflet()%>%
          leaflet::clearMarkers() %>%
          leaflet::addProviderTiles("CartoDB.Positron")

    # 返回最终的map对象
    map
  })
  #--------------------------# GWSS Analysis #--------------------------#
  # 运行GWSS分析
  gwss_result <- eventReactive(input$gwss_execute, {
    # 保证参数存在
    req(input$gwss_shapefile)
    req(input$gwss_vars)
    req(input$gwss_kernel, input$gwss_adaptive)

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWSS Analysis...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwss_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 更新进度条10%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 10)

    # 获取实际的数据和坐标
    gwss_shp_data <- gwss_shapefile_data()
    # 检查数据是否为 sf 对象
    if (!inherits(gwss_shp_data, "sf")) {
      shiny::showNotification("Unsupported data types. Input is not an sf object.", type = "error", duration = NULL)
      return()
    }
    # 修复可能的无效几何数据
    gwss_shp_data <- sf::st_make_valid(gwss_shp_data)

    # 更新进度条20%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 20)

    # 初始化 dp.locat
    dp.locat <- NULL
    # 提取坐标点
    tryCatch({
      geometry_type <- unique(sf::st_geometry_type(gwss_shp_data))  # 获取所有几何类型
      if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
        centroids <- sf::st_centroid(sf::st_geometry(gwss_shp_data))  # 仅计算几何的质心
        coords <- sf::st_coordinates(centroids)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
        coords <- sf::st_coordinates(gwss_shp_data)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("LINESTRING" %in% geometry_type) {
        # 如果是线状几何，可以取中点（或其他逻辑）
        midpoints <- sf::st_line_sample(sf::st_geometry(gwss_shp_data), n = 1)
        coords <- sf::st_coordinates(midpoints)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else {
        stop(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")))
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })
    # 检查 dp.locat 是否为空
    if (is.null(dp.locat)) {
      shiny::showNotification("Coordinates could not be extracted from the shapefile.", type = "error", duration = NULL)
      return()
    }
    dp.locat <- na.omit(dp.locat)

    # 检查并生成 FID 列
    if (!"FID" %in% names(gwss_shp_data)) {
      dp.n <- nrow(dp.locat)
      gwss_shp_data$FID <- seq_len(dp.n)
    }

    # 更新进度条到 30%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 30)

    # 转换为 sp::SpatialPointsDataFrame
    spdf <- tryCatch({
      sp::SpatialPointsDataFrame(
        coords = dp.locat,
        data = as.data.frame(gwss_shp_data),  # 转换 sf 对象为 data.frame
        proj4string = sp::CRS("+proj=longlat +datum=WGS84")
      )
    }, error = function(e) {
      shiny::showNotification(paste("Error creating SpatialPointsDataFrame:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })

    # 更新进度条到 40%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 40)

    # 获取用户选定的变量
    gwss_vars <- as.character(input$gwss_vars)
    # 获取数据框并检查选定列的数据类型
    data_check <- spdf@data[gwss_vars]
    non_numeric_cols <- sapply(data_check, function(x) !is.numeric(x))

    if (any(non_numeric_cols)) {
      non_numeric_vars <- names(data_check)[non_numeric_cols]
      shiny::showNotification(
        paste("Error running GWSS: Non-numeric variables detected -", 
              paste(non_numeric_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return(NULL)
    }
    # 获取选项
    gwss_kernel <- as.character(input$gwss_kernel)
    gwss_adaptive <- as.logical(input$gwss_adaptive)
    # 选择带宽
    # bandwidth <- as.numeric(input$gwss_bandwidth) # 用户输入带宽
    gwss_bw_data$bw <- as.numeric(input$gwss_bandwidth)

    # 更新进度条到 50%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 50)

    # 执行 GWSS 模型
    model <- tryCatch({
      GWmodel::gwss(data = spdf, vars = gwss_vars, bw = gwss_bw_data$bw, kernel = gwss_kernel, adaptive = gwss_adaptive)
    }, error = function(e) {
      shiny::showNotification(paste("Error running GWSS:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })

    # 模型成功后检查FID列
    if (!is.null(model)) {
      # 获取空间数据框架
      sdf <- model$SDF
      # 检查是否存在FID列
      if (!"FID" %in% names(sdf)) {
        # 获取数据点数
        dp.n <- nrow(sdf@data)
        # sdf@data$FID <- seq_len(dp.n)
        sdf$FID <- seq_len(dp.n)
        # 更新模型结果
        model$SDF <- sdf
      }
    }

    # 函数执行完成后，关闭等待窗口
    # 更新进度条到 100%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_progress", value = 100)
    Sys.sleep(1)  # 等待一秒，确保用户看到完成的进度条
    shiny::removeModal()

    return(model)
  })
  # 更新图层选择控件
  observeEvent(gwss_result(), {
    req(gwss_result())

    # 获取 GWSS 结果
    gwss_result <- gwss_result()
    sdf <- gwss_result$SDF

    # 触发重新渲染 UI 控件
    output$gwss_map_layer <- renderUI({
        # 提取有效图层名称
        layer_names <- names(sdf)
        layer_names <- layer_names[!grepl("_LCV$", layer_names)]
        layer_names <- layer_names[!grepl("^Cov_", layer_names)]
        layer_names <- layer_names[!grepl("^Spearman", layer_names)]
        layer_names <- layer_names[!grepl("FID", layer_names)]

        # 确保至少有一个可用图层
        if (length(layer_names) == 0) {
          return(NULL)  # 没有可用图层时，不渲染 UI
        }

        # 创建下拉框
        selectInput(
            inputId = "gwss_map_layer",
            label = "Select Map Layer to Display",
            choices = layer_names,
            selected = layer_names[1]  # 默认选择第一个图层
        )
    })

    # 动态更新变量选择选项
    output$gwss_columns <- renderUI({
      # 筛选列名，排除不需要的列
      names <- names(sdf)
      names <- names[!grepl("_LCV$", names)]
      names <- names[!grepl("^Cov_", names)]
      names <- names[!grepl("^Spearman", names)]
      names <- names[!grepl("FID", names)]

      # 更新选择框的选项
      selectInput(
        inputId = "gwss_columns",
        label = "Choose Variables",
        choices = names,
        multiple = TRUE,
        selected = names[1]  # 默认不选中任何选项
      )
    })

    # **自动触发地图更新，确保默认图层直接显示**
    observe({
      req(input$gwss_map_layer)  # 确保 gwss_map_layer 不是 NULL
      shinyjs::delay(1000, shinyjs::click("gwss_map_layer_execute"))  
    })
  })
  #--------------------------# Web Map #-------------------------#
  # 更新地图上的图层
  observeEvent(input$gwss_map_layer_execute, {
    req(gwss_result())
    req(input$gwss_map_layer)
    req(gwss_shapefile_data())
    req(input$gwss_color_palette)

    # 每更新一次图层清除www/temp下的音频文件
    prefixes_to_remove <- c("^gwss_map_audio_",
                            "^gwss_map_audio_composite_",
                            "^gwss_map_waveform_",
                            "gwss_map_sound_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取底图map对象
    map <- leaflet::leafletProxy("gwss_mapPlot", session)
    if(!is.null(map)){
      # 获取result和sdf
      result <- gwss_result()
      if(!is.null(result$SDF)){sdf <- result$SDF}else{print("result sdf is null")} 
      # 获取sdf的坐标
      coords_sdf <- sp::coordinates(sdf)
      # 获取图层名称layer_to_plot
      layer_to_plot <- input$gwss_map_layer
      # 获取shp数据
      shp_data <- gwss_shapefile_data()

      library(leaflet.extras)
      library(RColorBrewer)

      # 输出图层选择控件
      if (inherits(shp_data, "sf")) {shp_data <- sf::st_make_valid(shp_data)}
      # 根据几何类型进行不同专题图显示
      if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
          # 根据用户选择动态设置颜色映射
          selected_palette <- input$gwss_color_palette
          if (selected_palette == "viridis") {
            color_palette <- leaflet::colorNumeric(palette = "viridis", domain = c(
              min(sdf[[layer_to_plot]], na.rm = TRUE),
              max(sdf[[layer_to_plot]], na.rm = TRUE)
            ))
          } else {
            gradient <- color_gradients[[selected_palette]]
            color_palette <- leaflet::colorNumeric(
              palette = c(gradient["low"], gradient["high"]),
              domain = c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                        max(sdf[[layer_to_plot]], na.rm = TRUE))
            )
          }
          map %>%
                  leaflet::clearGroup("Polygons") %>%
                  leaflet::clearGroup("Circles") %>%
                  leaflet::removeControl("Polygons") %>%
                  leaflet::removeControl("Circles") %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = shp_data,
                    fillColor = ~color_palette(sdf[[layer_to_plot]]),
                    color = "black",
                    fillOpacity = 0.4,
                    weight = 0.3,
                    opacity = 0.5,
                    # popup = paste("FID : ",sdf[["FID"]], "<br>",layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                    group = "Polygons",
                    layerId = sdf[["FID"]] 
                  )%>%
                  leaflet::addLegend(
                    position = "bottomright",
                    pal = color_palette,                                # 图例位置
                    values = sdf[[layer_to_plot]],
                    title = layer_to_plot,                              # 图例标题
                    labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                    opacity = 1,                                         # 图例的不透明度
                    layerId = "Polygons"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))

      } else if (sf::st_geometry_type(shp_data)[1] == "POINT" || sf::st_geometry_type(shp_data)[1] == "MULTIPOINT"){
          # 根据用户选择动态设置颜色映射
          domain <- c(min(sdf[[layer_to_plot]], na.rm = TRUE),
          max(sdf[[layer_to_plot]], na.rm = TRUE))
          selected_palette <- input$gwss_color_palette
          if (selected_palette == "viridis") {
            color_palette <- leaflet::colorNumeric(palette = "viridis", domain = domain)
          } else {
            colors <- color_gradients[[selected_palette]]
            color_palette <- leaflet::colorNumeric(palette = colors, domain = domain)
          }
          # 设置半径大小
          point_radius <- 50
          map %>%
                leaflet::clearGroup("Polygons") %>%
                leaflet::clearGroup("Circles") %>%
                leaflet::removeControl("Polygons") %>%
                leaflet::removeControl("Circles") %>%
                leaflet::clearGroup("Significant") %>%
                leaflet::removeControl("Significant") %>%
                # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
                leaflet::addCircles(
                          data = sdf,
                          weight = 1,
                          radius = point_radius,
                          color = ~color_palette(sdf[[layer_to_plot]]),
                          fillOpacity = 1,
                          lng = coords_sdf[, 1],
                          lat = coords_sdf[, 2],
                          # popup = paste("FID : ",sdf[["FID"]], "<br>",layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                          opacity = 1,
                          group = "Circles",
                          layerId = sdf[["FID"]])%>%
                leaflet::addLegend(
                  position = "bottomright",
                  pal = color_palette,                                # 图例位置
                  values = sdf[[layer_to_plot]],
                  title = layer_to_plot,                              # 图例标题
                  labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                  opacity = 1,                                         # 图例的不透明度
                  layerId = "Circles"
                )%>%
                # 动态计算地图边界
                leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                          lat1 = min(coords_sdf[, 2]),
                          lng2 = max(coords_sdf[, 1]),
                          lat2 = max(coords_sdf[, 2]))
      }
    }else{print("map is null")}

  })
  # 用 reactiveValues 存储用户点击的点和生成的短音频文件路径
  gwss_rv <- reactiveValues(
    clicked_points = list(),  # 用于存放点击的经纬度（仅连线模式时使用）
    audio_files = character() # 存放每个点击生成的短音频文件路径
  )
  # 点击地图产生音频
  observeEvent(input$gwss_mapPlot_click, {
    req(gwss_result())
    click <- input$gwss_mapPlot_click
    shp_data <- gwss_shapefile_data() %>% 
        sf::st_as_sf() %>% 
        sf::st_make_valid() %>% 
        sf::st_set_crs(4326)
    geom_type <- sf::st_geometry_type(shp_data) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    # 选择模式：点击模式直接生成短音频
    if(input$gwss_audio_mode == "click"){
      # 获取必要数据
      gwss_sdf <- gwss_result()$SDF
      layer_to_plot <- input$gwss_map_layer
      
      # 这里假设已将 gwss_sdf 转换为 sf 对象，或直接使用 spatial 包的 nearest feature 方法
      gwss_sdf_sf <- sf::st_as_sf(gwss_sdf)
      click_point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      selected_id <- sf::st_nearest_feature(click_point, gwss_sdf_sf)
      selected_row <- gwss_sdf[selected_id, ]
      
      # 生成 popup 内容
      popup_content <- paste("FID :", selected_row$FID, "<br>", 
                            layer_to_plot, ":", round(selected_row[[layer_to_plot]], 4))
      if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        # 找到包含点击点的 Polygon
        selected_feature <- shp_data[sf::st_contains(shp_data, click_point, sparse = FALSE), ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No polygon data found", type = "error")
          return(NULL)
        }

        leaflet::leafletProxy("gwss_mapPlot") %>%
          leaflet::clearPopups() %>%
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          leaflet::addPolygons(
            data = selected_feature,
            group = "highlighted_features",
            fillColor = "yellow",
            color = "#FF0000",
            fillOpacity = 0,
            weight = 2,
            highlightOptions = leaflet::highlightOptions(
              weight = 5,
              color = "#FF0000",
              bringToFront = TRUE
              )
            ) %>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
        # 找到最近的点
        selected_id <- sf::st_nearest_feature(click_point, shp_data)
        selected_feature <- shp_data[selected_id, ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No point data found", type = "error")
          return(NULL)
        }
        leaflet::leafletProxy("gwss_mapPlot") %>%
          leaflet::clearPopups() %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::addCircleMarkers(
            data = selected_feature,
            group = "highlighted_features",
            color = "red",
            fillOpacity = 0,
            radius = 8,
            stroke = TRUE,
            weight = 2
          ) %>%
          leaflet::addMarkers( 
                    lng = click$lng, lat = click$lat,
                    group = "highlighted_features",
                    popup = popup_content,
                    icon = musicIcon # 使用自定义图标
                )
      } else {
        shiny::showNotification("The current geometry type is not supported", type = "error")
        return(NULL)
      }
      
      
      # 生成短音频文件
      coeff <- ifelse(is.na(selected_row[[layer_to_plot]]), 0, selected_row[[layer_to_plot]])
      short_filename <- paste0("www/gwss_map_audio_", as.integer(Sys.time()), "_", sample(1:1000, 1), ".wav")
      generate_audio(value = coeff,
                    filename = short_filename,
                    x_min = min(gwss_sdf[[layer_to_plot]], na.rm = TRUE),
                    x_max = max(gwss_sdf[[layer_to_plot]], na.rm = TRUE))
      
      # 更新 audio 控件，播放短音频
      shinyjs::runjs(sprintf("document.getElementById('gwss_map_audio').src='%s'; document.getElementById('gwss_map_audio').play();", short_filename))
      
    } else if(input$gwss_audio_mode == "line") {
      # 连线模式：记录点击点，不直接生成音频
      gwss_rv$clicked_points <- append(gwss_rv$clicked_points, list(c(click$lng, click$lat)))
      # 在地图上添加标记以便确认顺序
      leaflet::leafletProxy("gwss_mapPlot") %>%
        leaflet::addMarkers(
                  lng = click$lng, 
                  lat = click$lat,
                  popup = paste("point", length(gwss_rv$clicked_points)),
                  group = "point",
                  icon = musicIcon 
                  )
    }
  })
  # 处理连线模式下的确认按钮：生成长音频
  observeEvent(input$gwss_confirm_audio, {
    req(length(gwss_rv$clicked_points) > 0)
    # 仅在连线模式时触发检查
    if (input$gwss_audio_mode == "line") {
      # 检查点击点数量是否足够
      if (length(gwss_rv$clicked_points) < 2) {
        shiny::showNotification("Choose at least two points", type = "error")
        return() # 不满足条件时提前退出
      }
    }

    gwss_buffer_length <- as.numeric(input$gwss_buffer_length)
    layer_to_plot <- input$gwss_map_layer

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWSS Audio...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwss_audio_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 1. 获取用户点击的坐标并绘制连线
    clicked_matrix <- do.call(rbind, gwss_rv$clicked_points)
    leaflet::leafletProxy("gwss_mapPlot") %>%
      leaflet::addPolylines(
                  lng = clicked_matrix[,1], 
                  lat = clicked_matrix[,2],
                  color = "red",
                  weight = gwss_buffer_length,
                  opacity = 0.8,
                  group = "point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除之前的高亮
    
    # 2. 创建 sf 线对象并确保坐标系统一
    clicked_line <- sf::st_linestring(clicked_matrix) %>%
      sf::st_sfc(crs = 4326) %>%
      sf::st_make_valid()
    line_buffer <- sf::st_buffer(clicked_line, dist = gwss_buffer_length)  # 可调整缓冲区大小
  
    # 3. 处理空间数据
    gwss_sdf <- gwss_result()$SDF
    shp_data <- gwss_shapefile_data() %>% 
      sf::st_as_sf() %>% 
      sf::st_make_valid() %>% 
      sf::st_set_crs(4326)

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwss_audio_progress", value = 10)

    # 4. 转换Spatial对象为sf
    gwss_sdf_sf <- sf::st_as_sf(gwss_sdf) 
    
    # 将gwss计算结果合并到原始shp属性（假设行顺序一致）
    if(nrow(shp_data) == nrow(gwss_sdf_sf)) {
      shp_data_with_gwss <- shp_data %>%
        dplyr::bind_cols(sf::st_drop_geometry(gwss_sdf_sf)) %>%  # 使用正确的属性数据
        sf::st_sf()
    } else {
      shiny::showNotification("The number of data rows is inconsistent", type = "error")
      shiny::removeModal()
      return()
    }

    geom_type <- sf::st_geometry_type(shp_data_with_gwss) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      # 精确相交检测
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwss, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      
      intersect_features <- intersects
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_gwss), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwss_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwss_mapPlot") %>%
        leaflet::addPolygons(
          data = intersects,
          group = "highlighted_features",
          color = "#FF0000",
          fillOpacity = 0,
          weight = 2,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#FF0000",
            bringToFront = TRUE
            )
          )
    }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
      # 点处理逻辑
      # intersect_idx  <- st_intersects(gwss_sdf_sf, line_buffer, sparse = FALSE)
      # intersect_features <- gwss_sdf_sf[which(intersect_idx, arr.ind = TRUE), ]
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwss, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      intersect_features <- intersects

      if(nrow(intersect_features) == 0) {
        shiny::showNotification("The line does not pass through any features", type = "warning")
        shiny::removeModal()
        return()
      }
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(gwss_sdf_sf), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwss_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwss_mapPlot") %>%
        leaflet::addCircleMarkers(
          data = intersect_features,
          group = "highlighted_features",
          color = "red",
          fillOpacity = 0,
          radius = 8,
          stroke = TRUE,
          weight = 2
        )

    } else {
      shiny::showNotification("The current geometry type is not supported", type = "error")
      shiny::removeModal()
      return()
    }
    
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwss_audio_progress", value = 20)

    # 6. 沿路径排序（确保交点顺序与点击点一致）
    if (nrow(intersect_features) > 0) {
      start_point <- sf::st_sfc(sf::st_point(clicked_matrix[1, ]), crs = 4326)
      intersect_features <- intersect_features %>%
        dplyr::mutate(
          dist_to_start = as.numeric(sf::st_distance(geometry, start_point))
        ) %>%
        dplyr::arrange(dist_to_start)
    }

    # 7. 生成音频
    gwss_rv$audio_files <- character()  # 重置音频存储

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwss_audio_progress", value = 30 )

    # 8. 遍历要素，生成音频
    for (i in seq_len(nrow(intersect_features))) {
      feature <- intersect_features[i, ]
      coeff <- ifelse(is.na(feature[[layer_to_plot]]), 0, feature[[layer_to_plot]])
      short_filename <- paste0("www/gwss_map_audio_", i, ".wav")

      generate_audio(
        value = coeff,
        filename = short_filename,
        x_min = min(gwss_sdf[[layer_to_plot]], na.rm = TRUE),
        x_max = max(gwss_sdf[[layer_to_plot]], na.rm = TRUE)
      )

      gwss_rv$audio_files <- c(gwss_rv$audio_files, short_filename)

      # 更新进度条
      shinyWidgets::updateProgressBar(session = session, id = "gwss_audio_progress", value = 30 + (i/nrow(intersect_features)) * 30)
    }
    
    # 9. 合成所有短音频文件为一个长音频
    composite_filename <- paste0("www/gwss_map_audio_composite.wav")
    concatenate_audio(gwss_rv$audio_files, composite_filename)
    # 确保 FFmpeg 使用合成后的音频
    sound_road <- composite_filename  # 这里修正

    # 提取系数数据
    ranges <- seq_len(nrow(intersect_features))  # 用于 X 轴
    coeff_values <- intersect_features[[layer_to_plot]]  # 提取回归系数
    ranges_length <- length(ranges)
    # 生成初始曲线图
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = coeff_values), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  
      ) +
      ggplot2::ggtitle("Coefficient Variation Over Features") +
      ggplot2::xlab("Feature Index") + 
      ggplot2::ylab("Coefficient Value")

    # 逐帧生成视频
    waveform_images <- c()
    for (i in seq_along(ranges)) {
      frame_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = ranges[i], color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Feature:", i, "  Coefficient:", round(coeff_values[i], 4)))

      frame_image <- file.path(temp_dir, paste0("gwss_map_waveform_", i, ".png"))
      ggplot2::ggsave(frame_image, frame_plot, width = 6, height = 4, dpi = 150)
      waveform_images <- c(waveform_images, frame_image)

      shinyWidgets::updateProgressBar(session, "gwss_video_progress", value = 60 + (i / ranges_length) * 30)
    }

    # 计算音频时长
    audio_info <- tuneR::readWave(composite_filename)
    total_frames <- length(waveform_images)
    total_audio_duration <- length(audio_info@left) / audio_info@samp.rate
    # 计算帧率，使得视频时长与音频一致
    frame_rate <- total_frames / total_audio_duration
    # 确保 framerate 合理（避免异常）
    if (frame_rate < 1) frame_rate <- 1
    if (frame_rate > 30) frame_rate <- 30  # 限制在 1-30 fps，防止帧率过大或过小
    # 生成视频
    waveform_video <- file.path(temp_dir, "gwss_map_waveform_video.mp4")
    av::av_encode_video(
      input = waveform_images,
      output = waveform_video,
      framerate = frame_rate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",
      audio = NULL,
      verbose = TRUE
    )

    # 合成音视频
    sound_video <- file.path("www", "gwss_map_sound_video.mp4")
    # sound_road <- gwss_rv$audio_files  # 获取之前生成的音频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", 
      "-i", shQuote(waveform_video),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " 合成波形图和音频完成，文件保存为: ", sound_video, "\n")

    # 在地图左下角显示视频
    leaflet::leafletProxy("gwss_mapPlot") %>%
      leaflet::removeControl( layerId = "map_video_control") %>%
      leaflet::addControl(
         HTML(sprintf(
          '<video id="gwss_video_player" width="300" autoplay controls>
            <source src="%s" type="video/mp4">
            Your browser does not support the video tag.
          </video>
          <audio id="gwss_audio" src="%s"></audio>
          <script>
            document.getElementById("gwss_video_player").onplay = function() {
              var audio = document.getElementById("gwss_audio");
              if (audio) {
                audio.play();
              }
            };
            document.getElementById("gwss_video_player").onpause = function() {
              var audio = document.getElementById("gwss_audio");
              if (audio) {
                audio.pause();
              }
            };
            document.getElementById("gwss_video_player").ontimeupdate = function() {
              var audio = document.getElementById("gwss_audio");
              if (audio) {
                audio.currentTime = this.currentTime;
              }
            };
          </script>', sound_video, composite_filename
        )),
        position = "bottomleft",
        layerId = "map_video_control" 
      )

    # 设置音频源
    # shinyjs::runjs(sprintf("document.getElementById('gwss_map_audio').src='%s';", composite_filename))

    # 重置记录，便于下次操作
    gwss_rv$clicked_points <- list()
    gwss_rv$audio_files <- character()

    shinyWidgets::updateProgressBar(session = session, id = "gwss_audio_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()
  })
  # 处理连线的图标
  observeEvent(input$gwss_confirm_audio_clear, {
    # 重置记录，便于下次操作
    gwss_rv$clicked_points <- list()
    gwss_rv$audio_files <- character()

    leaflet::leafletProxy("gwss_mapPlot") %>%
      leaflet::removeControl( layerId = "map_video_control") %>%
      leaflet::clearGroup("point") %>%
      leaflet::clearGroup("point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除高亮
  })
  #--------------------------# Table View #-------------------------#
  # 显示汇总表
  output$gwss_summaryTable <- DT::renderDataTable({
    result <- gwss_result()
    sdf <- as.data.frame(result$SDF)
    DT::datatable(
      sdf,
      extensions = 'Buttons',  # 关键要素1：声明使用扩展
      options = list(
        pageLength = 10,        # 默认每页显示10条
        lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
        searching = TRUE,       # 启用搜索框
        scrollX = TRUE,          # 启用水平滚动
        dom = 'Blfrtip',          # 控制界面元素布局
        buttons = list(       # 关键要素3：按钮嵌套配置
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'Export CSV'),
          list(extend = 'excel', text = 'Export Excel')
        )
      ),
      rownames = FALSE,         # 不显示行号
      class = "'display nowrap hover" 
    )
  })
  #--------------------------# Summary Information #-------------------------#
  # 显示模型诊断信息
  output$gwss_modelDiagnostics <- renderPrint({
    gwss_result()
  })
  #--------------------------# Multiple Visualizations #-------------------------#
  # 动态生成图片
  observe({
    req(input$gwss_columns)  # 确保用户已经选择了变量
    req(input$gwss_bandwidth)
    req(input$gwss_color_palette)  # 确保用户已经选择了色阶

    # 显示等待窗口
    showModal(modalDialog(
      title = "Please wait",
      "The GWSS result is drawing...",
      easyClose = FALSE
    ))

    bw <- round(as.numeric(gwss_bw_data$bw), 3)

    shp_data <- gwss_shapefile_data()
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}

    # 获取 GWSS 结果
    gwss_result <- gwss_result()
    sdf <- gwss_result$SDF

    # 设置高分辨率参数
    dpi <- 100  # 设置DPI值
    width_px <- 1500  # 设置宽度像素
    height_px <- 1500 # 设置高度像素

    # 遍历用户选择的列并动态生成图片
    selected_columns <- input$gwss_columns
    for (col in selected_columns) {
      local({
        column_name <- col  # 需要使用 local() 避免循环中的闭包问题
        # 动态创建 UI 输出控件
        output[[paste0("plot_", column_name)]] <- renderPlot({

          # 确保列存在于数据框中
          req(column_name %in% names(sdf))

          # 创建专题图
          target_data <- shp_data
          target_data$value <- sdf[[column_name]]  # 将数据加入到 sf 对象中，以便于绘制专题图

          # 判断数据类型，动态选择绘图样式
          geom_type <- unique(sf::st_geometry_type(target_data))
          # 根据选择的色阶动态设置颜色
          gradient <- color_gradients[[input$gwss_color_palette]]

          if ("POINT" %in% geom_type || "MULTIPOINT" %in% geom_type) {  # 点数据：使用 color 映射
              if (input$gwss_color_palette == "viridis") {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_viridis_c() +  # 使用 Viridis 色阶
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              } else {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_gradient(low = gradient["low"], high = gradient["high"]) +
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              }
          } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {  # 面数据：使用 fill 映射
            if (input$gwss_color_palette == "viridis") {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_viridis_c() +  # 使用 Viridis 色阶
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            } else {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_gradient(low = gradient["low"], high = gradient["high"]) +
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }
          } else {
            # 如果既不是点也不是面，抛出警告
            shiny::showNotification("Unsupported geometry type for plotting.", type = "error")
            shiny::removeModal()
            return()
          }        
        })
      })
    }

    # # 动态图片生成完成后关闭等待窗口
    shiny::removeModal()
  })
  # 动态显示图片
  output$gwss_Plot <- renderUI({
    req(input$gwss_columns)  # 确保用户已经选择了变量

    # 每行的图片数量
    images_per_row <- min(2, length(input$gwss_columns))  # 每行最多显示 2 张图片

    plot_outputs <- lapply(seq_along(input$gwss_columns), function(i) {
      column_name <- input$gwss_columns[i]

      # 使用 column 动态调整宽度，每行最多显示 images_per_row 张图片
      column(
        width = 12 / images_per_row,  # 动态设置列宽（12 栅格系统）
        plotOutput(outputId = paste0("plot_", column_name), height = "500px", width = "100%")
      )
    })

    # 将图片控件布局在 fluidRow 中
    do.call(fluidRow, plot_outputs)
  })
  #--------------------------# Video #-------------------------#
  # 更新范围图层选项
  gwss_range_layer <- eventReactive(input$gwss_vars, {
    req(input$gwss_vars)
    gwss_vars <- input$gwss_vars

    names <- gwss_vars

    # 初始化choices列表
    choices <- list()
    suffixes <- c("_LM", "_LSD", "_LVar", "_LSKe")  # 后缀列表

    # 动态添加回归系数选项
    for (var in names) {
      for (suffix in suffixes) {
        choice_name <- paste0(var, suffix)  # 生成选项
        choices[[choice_name]] <- choice_name
      }
    }

    # 设置默认选项（第一个变量加第一个后缀作为默认选项）
    default_choice <- paste0(names[1], suffixes[1])

    # 返回choices列表和默认选项
    list(choices = choices, default_choice = default_choice)
  }, ignoreNULL = FALSE)
  # 监听范围图层选项的变化并更新图层选项
  observeEvent(gwss_range_layer(), {
    layer_info <- gwss_range_layer()
    updateSelectInput(session, "gwss_range_layer", choices = layer_info$choices, selected = layer_info$default_choice)
  })
  # 步长
  observeEvent(input$gwss_range, {
    min_range <- as.integer(input$gwss_range[1])
    max_range <- as.integer(input$gwss_range[2])
    updateNumericInput(session, "gwss_step_length", max = max_range-min_range)
  })
  # 生成视频
  observeEvent(input$gwss_video_button, {
    # 保证参数存在
    req(input$gwss_shapefile)
    req(input$gwss_vars)
    req(input$gwss_kernel, input$gwss_adaptive)
    req(input$gwss_color_palette)

    # 带宽范围
    range_min <- input$gwss_range[1]
    range_max <- input$gwss_range[2]
    # 步长
    step_length <- input$gwss_step_length
    # 颜色盘
    color_palette <- input$gwss_color_palette

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWSS Video...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwss_video_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条5%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_video_progress", value = 5)
    cat("0% ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"\n")
    time1 <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))  # 第一个时间点

    # 在开始生成前，确保删除 temp_dir/www_dir 文件夹下以特定前缀开头的所有文件
    prefixes_to_remove <- c("^gwss_image_video_", 
                            "gwss_image_video.mp4",
                            "^gwss_sound_audio_",
                            "^gwss_waveform_",
                            "gwss_waveform_video.mp4",
                            "gwss_waveform_overview.png",
                            "^gwss_sound_video_",
                            "gwss_sound_video.mp4",
                            "gwss_final_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取实际的数据和坐标
    shp_data <- gwss_shapefile_data()
    spdf <- as(shp_data,"Spatial")
    # 获取选项
    kernel <- as.character(input$gwss_kernel)
    adaptive <- as.logical(input$gwss_adaptive)
    # 获取变量Vars
    vars <- as.list(input$gwss_vars)
    # 获取变量
    range_layer <- as.character(input$gwss_range_layer)
    # 确保数据是 sf 对象
    cp_shp_data <- NULL
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}
    # 确保数据中有有效的几何列
    if (is.null(sf::st_geometry(shp_data))){ 
      shiny::showNotification("shp_data does not contain geometry.", type = "error")
      shiny::removeModal()
      return()
    }
     # 计算模型结果
    target_shp_data <- shp_data
  
    # 频率数组
    intercept_vars <- c()
    # 根据步长生成序列
    ranges <- seq(
      from = range_min,  # 起始值
      to = range_max,    # 结束值
      by = step_length   # 步长
    )
    ranges_length <- length(ranges)
    # 更新进度条5%
    shinyWidgets::updateProgressBar(session = session, id = "gwss_video_progress", value = 5)

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始生成专题图图片...","\n")
    # 专题图视频
    # 几何类型计算
    geometry_type <- sf::st_geometry_type(target_shp_data)[1]
    is_polygon <- geometry_type == "MULTIPOLYGON" || geometry_type == "POLYGON"
    is_point <- geometry_type == "POINT" || geometry_type == "MULTIPOLYGON"
    # 确定全局范围：计算所有 range_layer 数据的最小值和最大值
    global_min <- Inf
    global_max <- -Inf
    range_now_fir <- 1
    for (range in ranges) {
      bandwidth <- range
      # GWSS 模型计算
      model <- GWmodel::gwss(data = spdf, vars = vars, kernel = kernel, adaptive = adaptive, bw = bandwidth)
      # 提取模型结果
      sdf <- model$SDF
      range_values <- sdf[[range_layer]]
      global_min <- min(global_min, min(range_values, na.rm = TRUE))
      global_max <- max(global_max, max(range_values, na.rm = TRUE))
      
      # 更新进度条
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwss_video_progress",
        value = as.integer(5 + (range_now_fir / ranges_length) * 15)
      )
      range_now_fir <- range_now_fir + 1
    }
    # 确保全局范围有效
    if (global_min == Inf || global_max == -Inf) {
      shiny::showNotification("No valid range values found for plotting.", type = "error")
      shiny::removeModal()
      return()
    }
    # 扩展比例
    padding_ratio <- 0.1  # 扩展范围的比例（10%）
    global_min <-  min(range_values)
    global_max <- max(range_values)
    print(paste0("global_min",global_min,"global_max",global_max))
    # 调整全局最小值和最大值
    range_span <- global_max - global_min
    extended_min <- global_min - range_span * padding_ratio
    extended_max <- global_max + range_span * padding_ratio
    print(paste0("extended_min",extended_min,"extended_max",extended_max))
    fixed_breaks <- seq(global_min, global_max, length.out = 7)  # 创建7份区间
    # 图片主循环
    range_now <- 1
    for (range in ranges) {
      bandwidth <- range
      # GWSS 模型计算
      model <- GWmodel::gwss(data = spdf, vars = vars, kernel = kernel, adaptive = adaptive, bw = bandwidth)
      
      # 提取模型结果
      sdf <- model$SDF
      result_Intercept <- as.list(sdf[[range_layer]])
      result_Intercept_vector <- unlist(result_Intercept)
      intercept_vars <- c(intercept_vars, var(result_Intercept_vector, na.rm = TRUE))

      # 绘图
      target_shp_data$value <- sdf[[range_layer]]  # 添加绘图列
      output_path <- file.path(temp_dir, paste0("gwss_image_video_", range, ".png"))
      generate_plot(target_shp_data, range_layer, bandwidth, output_path,color_palette,extended_min, extended_max,fixed_breaks)

      # 更新进度条
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwss_video_progress",
        value = as.integer(20 + (range_now / ranges_length) * 60)
      )
      range_now <- range_now + 1
    }
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 专题图图片生成完成","\n")

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始生成波形图和音频...","\n")
    # --- 音频参数配置 ---
    sampling_rate <- 44100  # 采样率
    duration_per_note <- 0.3  # 每个音符的持续时间（秒）
    note_samples <- duration_per_note * sampling_rate  # 每个音符样本数

    # --- 音色控制参数 ---
    waveform_ratio <- c(0.7, 0.2, 0.1)  # 基波/二次谐波/三次谐波振幅比例 (增大谐波比例（如 c(0.5,0.3,0.2)）会让音色更尖锐)
    adsr <- c(attack=0.05,   # 起音时间比例（0-1）  增大 attack 会产生渐强的起始效果
              decay=0.1,     # 衰减时间比例
              sustain=0.6,   # 延音电平（0-1）
              release=0.15)  # 释音时间比例   增加 release 会让音符结束时有更长的衰减尾音
    filter_cutoff <- 0.4          # 低通滤波器截止频率（比例，0-1对应0-Nyquist） 范围 0.1-0.8，值越小高频衰减越明显
    loudness_factor <- 2          # 响度增益因子

    # --- 频率映射（保持原逻辑）---
    intercept_vars <- na.omit(intercept_vars)
    x_min <- min(intercept_vars)
    x_max <- max(intercept_vars)
    a <- (1000 - 440) / (x_max - x_min)
    b <- 440 - a * x_min
    Intercept_values <- round(a * intercept_vars + b)

    # --- 定义ADSR包络函数 ---
    apply_envelope <- function(signal, adsr_params) {
      n <- length(signal)
      attack_len <- floor(adsr_params["attack"] * n)
      decay_len <- floor(adsr_params["decay"] * n)
      release_len <- floor(adsr_params["release"] * n)
      sustain_len <- n - attack_len - decay_len - release_len
      
      # 构造包络曲线
      envelope <- c(
        seq(0, 1, length.out = attack_len),                     # 起音
        seq(1, adsr_params["sustain"], length.out = decay_len), # 衰减
        rep(adsr_params["sustain"], sustain_len),               # 延音
        seq(adsr_params["sustain"], 0, length.out = release_len) # 释音
      )
      
      # 确保长度匹配
      length(envelope) <- n  # 自动截断多余部分
      return(signal * envelope)
    }
    
    # --- 生成复合波形 ---
    audio_signal <- do.call(c, lapply(Intercept_values, function(freq) {
      # 生成时间序列
      t <- seq(0, duration_per_note, length.out = note_samples)
      # 方波生成（核心波形）
      square_wave <- ifelse(sin(2 * pi * freq * t) > 0, 1, -1)
      # 谐波叠加
      harmonic1 <- 0.3 * sin(2 * pi * 2 * freq * t)   # 二次谐波
      harmonic2 <- 0.2 * sin(2 * pi * 3 * freq * t)   # 三次谐波
      # 混合波形
      mixed_wave <- waveform_ratio[1] * square_wave + 
                    waveform_ratio[2] * harmonic1 + 
                    waveform_ratio[3] * harmonic2
      # 应用包络
      apply_envelope(mixed_wave, adsr)
    }))

    # --- 后处理 ---
    # 低通滤波器（软化高频）
    bf <- signal::butter(4, filter_cutoff, type = "low")  # 4阶巴特沃斯低通
    audio_filtered <- signal::filtfilt(bf, audio_signal)

    # 规范化处理
    audio_normalized <- audio_filtered / max(abs(audio_filtered)) * loudness_factor
    audio_normalized[audio_normalized > 1] <- 1   # 硬限幅
    audio_normalized[audio_normalized < -1] <- -1

    # --- 生成Wave对象 ---
    audio_wave <- tuneR::Wave(
      left = as.integer(audio_normalized * 32767),
      right = as.integer(audio_normalized * 32767),
      samp.rate = sampling_rate,
      bit = 24
    )

    # 保存为WAV文件
    sound_road <- file.path(temp_dir,paste0("gwss_sound_audio_", range_min, "_", range_max, ".wav"))
    tuneR::writeWave(audio_wave, sound_road)
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 音频文件已保存为",sound_road,"\n")

    # 音频波形图
    # 生成总的点图并连成曲线图
    # y为该带宽下的总体系数方差
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = intercept_vars), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  # 添加离散点
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  # 平滑曲线
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  # 设定绘图区域背景为白色
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  # 设定整体背景为白色
      ) +
      ggplot2::ggtitle("Frequency Variation Over Ranges") +
      ggplot2::xlab("Ranges") + 
      ggplot2::ylab("Intercept Values")

    # 保存总波形图
    ggplot2::ggsave(file.path(temp_dir, "gwss_waveform_overview.png"), waveform_plot, width = 6, height = 4, dpi = 150)
    # 生成逐帧图片
    i = 1
    for (range in ranges) {
      single_waveform_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = range, color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Range:", range, "  Variance of variable coefficients:", as.character(round(intercept_vars[i],4))))

      waveform_image <- file.path(temp_dir, paste0("gwss_waveform_", as.character(range), ".png"))
      ggplot2::ggsave(waveform_image, single_waveform_plot, width = 6, height = 4, dpi = 150)
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwss_video_progress",
        value = as.integer(80 + (i / ranges_length) * 15)
      )
      i = i + 1
    }
    waveform_images <- file.path(temp_dir, paste0("gwss_waveform_", as.character(ranges), ".png"))
    waveform_video <- file.path(temp_dir, "gwss_waveform_video.mp4")
    av::av_encode_video(
      input = waveform_images,
      output = waveform_video,
      framerate = 10,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",  # 强制使用兼容像素格式
      audio = NULL,
      verbose = TRUE  # 显示详细日志
    )
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 波形图文件已保存为",waveform_video,"\n")

    # 确保总音频时长与图片生成的视频时长一致  
    # 计算音频总时长
    audio_duration <- length(audio_signal) / sampling_rate  # 音频总时长 (秒)
    # 确保视频帧率适配音频时长
    # 图片数量（即范围跨度）
    num_images <- length(waveform_images)
    # 动态计算帧率，使视频总时长与音频时长一致
    framerate <- num_images / audio_duration
    # 打印调试信息
    cat("音频时长:", audio_duration, "秒\n")
    cat("图片数量:", num_images, "\n")
    cat("视频帧率:", framerate, "帧/秒\n")

    # 专题图视频（无音频）
    image_files <- file.path(temp_dir, paste0("gwss_image_video_", ranges, ".png"))
    image_video <- file.path("www", "gwss_image_video.mp4")
    av::av_encode_video(
      input = image_files,
      output = image_video,
      framerate = framerate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",  # 强制使用兼容像素格式
      audio = NULL,
      verbose = TRUE  # 显示详细日志
    )
    # 波形图和音频
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始合成波形图和音频...","\n")
    video_file <- file.path(temp_dir, paste0("gwss_sound_video_", range_min, "_", range_max, ".mp4"))
    av::av_encode_video(
      input = waveform_images,
      output = video_file,
      framerate = framerate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",  # 强制使用兼容像素格式
      audio = NULL,
      verbose = TRUE  # 显示详细日志
    )
    # 使用FFmpeg合成波形图和音频
    sound_video <- file.path("www", "gwss_sound_video.mp4")
    # 合成音视频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", # 添加 -y 强制覆盖文件
      "-i", shQuote(video_file),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    # 调用系统命令运行 FFmpeg
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA  # 返回一个默认值
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    # 打印合成结果
    cat(paste0(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 合成波形图和音频完成，文件保存为: ", sound_video, "\n"))
    time2 <- as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S"))  # 第二个时间点
    time_diff <- difftime(time2, time1, units = "mins")  # 单位：分钟
    print(paste0("波形图和音频合成时间：", time_diff," mins"))

    # 最终视频
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始合成最终视频文件已保存为","\n")
    final_video <- file.path("www", "gwss_final_video.mp4")
    overlay_filter <- "[1:v]scale=200:150[ovrl];[0:v][ovrl]overlay=W-w-10:H-h-10"
    # "[1:v]scale=200:150[ovrl]"：将小视频缩放到200x150的大小，并将其输出到一个名为ovrl的临时虚拟视频。
    # "[0:v][ovrl]overlay=W-w-10:10"：将主视频和小视频叠加在一起。W-w-10表示小视频在主视频的右上角，距离右边和上边各10像素。
    # 左上角：overlay=10:10
    # 左下角：overlay=10:H-h-10
    # 右下角：overlay=W-w-10:H-h-10
    # 使用 FFmpeg 合并两个视频（水平分屏）
    ffmpeg_combine <- paste(
      "ffmpeg -y",
      "-i", shQuote(image_video),
      "-i", shQuote(sound_video),
      "-filter_complex", shQuote(overlay_filter),
      "-c:v libx264",
      shQuote(final_video)
    )
    # 执行命令并检查结果
    tryCatch({
      exit_code <- system(ffmpeg_combine, intern = FALSE, ignore.stderr = FALSE)
  
      # 防御性处理：确保 exit_code 是数值类型
      if (is.null(exit_code) || is.na(exit_code)) {
        shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
        shiny::removeModal()
        return()
      }
      
      if (exit_code != 0) {
        shiny::showNotification(paste0("FFmpeg exited with code ", exit_code))
        shiny::removeModal()
        return()
      }
      
      # 显式检查文件路径
      final_video_abs <- normalizePath(final_video, mustWork = FALSE)
      if (!file.exists(final_video_abs)) {
        shiny::showNotification(paste0("最终视频文件未生成: ", final_video_abs))
      } else {
        cat("最终视频路径验证成功:", final_video_abs, "\n")
      }
    }, error = function(e) {
      cat("!!! 最终视频合成失败:", e$message, "\n")
      shinyalert::shinyalert("致命错误", paste("视频合成失败:", e$message), type = "error")
      shiny::removeModal()  # 强制关闭进度窗口
      return()
    })

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 最终视频文件已保存为",final_video,"\n")
    time3 <- as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S"))  # 第三个时间点
    time_diff2 <- difftime(time3, time2, units = "mins")  # 单位：分钟
    print(paste0("最后视频合成时间：", time_diff2," mins"))

    # 函数执行完成后，关闭等待窗口
    shinyWidgets::updateProgressBar(session = session, id = "gwss_video_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()

    cat("检查最终视频文件是否存在:", final_video, "\n")
    cat("文件是否存在:", file.exists(final_video), "\n")
    cat("文件大小:", file.size(final_video), "字节\n")

    # 输出视频控件
    output$gwss_image_video <- renderUI({
      timestamp <- as.integer(Sys.time())
      tags$video(
          src = paste0("gwss_image_video.mp4?", timestamp),
          controls = TRUE,
          width = "300px"
      )
    })
    output$gwss_sound_video <- renderUI({
      timestamp <- as.integer(Sys.time())
      tags$video(
          src = paste0("gwss_sound_video.mp4?", timestamp),
          controls = TRUE,
          width = "300px"
      )
    })
    output$gwss_final_video <- renderUI({
      timestamp <- as.integer(Sys.time())
      tags$video(
          src = paste0("gwss_final_video.mp4?", timestamp),
          controls = TRUE,
          width = "600px"
      )
    })
  })


  #--------------------------# GW_Boxplot #--------------------------#
  # 启动文件服务器
  file_server <<- start_servr()
  # 回归点 shapefile 数据的临时文件路径缓存
  gwbp_cp_temp_files <- reactiveVal(NULL)
  # gwbp结果对象
  gwbp_data <- reactiveValues(
    results = NULL,           # 主分析结果
    outliers_true = NULL,     # 离群点
    outliers_false = NULL,     # 非离群点
    results_outliers = NULL    # 异常值计算
  )
  # shp读取函数
  gwbp_shapefile_data <- reactive({
    req(input$gwbp_shapefile)  # 确保用户已上传文件
    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    # 获取上传文件的路径和扩展名
    file_path <- input$gwbp_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwbp_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwbp_shapefile$name)

    # 定义临时目录
    temp_dir <- tempdir()

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        # 缓存解压文件路径
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch({
            sf::st_read(shp_file, options = "ENCODING=GBK", quiet = FALSE)
          }, error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # cp_shp读取函数
  gwbp_cp_shapefile_data <- reactive({
    req(input$gwbp_cp_shapefile)  # 确保用户已上传文件

   # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    # 获取上传文件的路径和扩展名
    file_path <- input$gwbp_cp_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwbp_cp_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwbp_cp_shapefile$name)

    # 定义临时目录
    temp_dir <- tempdir()

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)
      other_files <- list.files(temp_dir, pattern = paste0(file_name, ".*"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        # 缓存解压文件路径
        gwbp_cp_temp_files(other_files)
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch({
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # # 监听 Clear 按钮，清除回归点 shapefile 数据
  # observeEvent(input$gwbp_cp_shapefile_clear_button, {
  #   cat("Clearing regression points shapefile data...\n")
  #   # 删除缓存的临时文件
  #   temp_files <- gwbp_cp_temp_files()
  #   if (!is.null(temp_files)) {
  #     cat("Deleting cp temporary files:\n", paste(temp_files, collapse = "\n"), "\n")
  #     unlink(temp_files, recursive = TRUE)
  #   }
  #   gwbp_cp_temp_files(NULL)  # 清空缓存

  #   # 清空文件输入框
  #   # updateFileInput(session, "gwbp_cp_shapefile", value = NULL)
  #   js$resetFileInput(id = "gwbp_cp_shapefile")

  #   shiny::showNotification("Regression points Shapefile data cleared.", type = "message")
  # })
  # 更新变量选项
  observeEvent(gwbp_shapefile_data(), {
    # 获取变量名并按首字母排序（不区分大小写）
    var_names <- names(gwbp_shapefile_data())
    
    # 检查变量名是否为空
    if (length(var_names) == 0) {
      sorted_vars <- character(0)  # 空字符向量
    } else {
      # 不区分大小写排序
      sorted_vars <- var_names[order(tolower(var_names))]
    }

    updateSelectInput(session, "gwbp_X", choices = sorted_vars)
  })
  # 输出X变量选择控件
  output$gwbp_X <- renderUI({
    selectInput("gwbp_X", "Choose variable:", choices = NULL)
  })
  # 更新带宽滑块
  observe({
      req(input$gwbp_shapefile)  # 确保文件已上传
      shp_data <- gwbp_shapefile_data()  # 读取 shapefile 数据
      shp_data <- sf::st_make_valid(shp_data)      # 确保数据有效
      req(shp_data)

      # 计算最大距离（米），确保单位正确
      if (sf::st_is_longlat(shp_data)) {
          max_distance <- as.integer(max(sf::st_distance(shp_data)))  # 计算球面距离
      } else {
          coords <- sf::st_coordinates(shp_data)
          max_distance <- as.integer(max(dist(coords)))  # 计算欧式距离
      }

      # 读取是否为自适应带宽
      gwbp_adaptive <- as.logical(input$gwbp_adaptive)

      # 设置带宽的最大值和单位
      if (gwbp_adaptive) {
          max_bw <- nrow(shp_data)  # 以点的个数为单位
          label <- "Bandwidth (Number of points)"
      } else {
          max_bw <- max_distance  # 以最大距离为单位
          label <- "Bandwidth (meter)"
      }

      # 更新滑块
      updateSliderInput(session, "gwbp_bandwidth",
                        max = max_bw,
                        label = label)
  })
  # execute按钮执行gwboxplot分析
  observeEvent(input$gwbp_execute, {
    # 跳转分页
    updateTabsetPanel(session, "gwbp_tabs", selected = "Web Map")

    # 在开始生成前，确保删除 temp_dir 文件夹下以特定前缀开头的所有文件
    prefixes_to_remove <- c("^gw_boxplot_")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      }
    }

    # 判断必填项是否为空
    if(is.null(input$gwbp_shapefile)){
      shiny::showNotification("The SHP file selected to construct a geo-weighted box map cannot be empty", type = "error", duration = NULL)
      return()}
    
    current_map <- leaflet::leaflet()%>%
                      leaflet::addProviderTiles("CartoDB.Positron")

    # 获取shp数据的坐标
    req(gwbp_shapefile_data())

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GW Boxplot Analysis...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwbp_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条10%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 10)

    gwbp_shp_data <- gwbp_shapefile_data()
    # 检查数据是否为 sf 对象
    if (!inherits(gwbp_shp_data, "sf")) {
      shiny::showNotification("Unsupported data types. Input is not an sf object.", type = "error", duration = NULL)
      shiny::removeModal()
      return()
    }
    # 修复可能的无效几何数据
    gwbp_shp_data <- sf::st_make_valid(gwbp_shp_data)
    # 初始化 dp.locat
    dp.locat <- NULL

    # 更新进度条20%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 20)

    # 提取坐标点
    tryCatch({
      geometry_type <- unique(sf::st_geometry_type(gwbp_shp_data))  # 获取所有几何类型
      if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
        centroids <- sf::st_centroid(sf::st_geometry(gwbp_shp_data))  # 仅计算几何的质心
        coords <- sf::st_coordinates(centroids)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
        coords <- sf::st_coordinates(gwbp_shp_data)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("LINESTRING" %in% geometry_type) {
        # 如果是线状几何，可以取中点（或其他逻辑）
        midpoints <- sf::st_line_sample(sf::st_geometry(gwbp_shp_data), n = 1)
        coords <- sf::st_coordinates(midpoints)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else {
        shiny::showNotification(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")), type = "error")
        shiny::removeModal()
        return()
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })
    # 检查 dp.locat 是否为空
    if (is.null(dp.locat)) {
      shiny::showNotification("Coordinates could not be extracted from the shapefile.", type = "error", duration = NULL)
      return()
    }
    dp.locat <- na.omit(dp.locat)

    # 更新进度条30%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 30)

    # 获取 calibrationPoint 数据
    if (is.null(input$gwbp_cp_shapefile)) {
      cp_shp_data <- gwbp_shp_data
      calibrationPoint <- dp.locat
    } else {
      cp_shp_data <- gwbp_cp_shapefile_data()
      cp_shp_data <- sf::st_make_valid(cp_shp_data)
      # 提取坐标点
      tryCatch({
        geometry_type <- unique(sf::st_geometry_type(cp_shp_data))  # 获取所有几何类型
        if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
          centroids <- sf::st_centroid(sf::st_geometry(cp_shp_data))  # 仅计算几何的质心
          coords <- sf::st_coordinates(centroids)
          calibrationPoint <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
        } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
          coords <- sf::st_coordinates(cp_shp_data)
          calibrationPoint <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
        } else if ("LINESTRING" %in% geometry_type) {
          # 如果是线状几何，可以取中点（或其他逻辑）
          midpoints <- sf::st_line_sample(sf::st_geometry(cp_shp_data), n = 1)
          coords <- sf::st_coordinates(midpoints)
          calibrationPoint <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
        } else {
          shiny::showNotification(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")), type = "error")
          shiny::removeModal()
          return()
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
        shiny::removeModal()
        return(NULL)
      })
    }
    calibrationPoint <- na.omit(calibrationPoint)

    # 检查 dp.locat 和 calibrationPoint 是否有效
    if (nrow(dp.locat) == 0 || nrow(calibrationPoint) == 0) {
      shiny::showNotification("No valid points found in the shapefile or calibration data.", type = "error", duration = NULL)
      return()
    }

    # 获取选中的Kernel、adaptive、longlat、outliers选项
    gwbp_kernel <- as.character(input$gwbp_kernel)
    gwbp_adaptive <- as.logical(input$gwbp_adaptive)
    gwbp_longlat <- as.logical(input$gwbp_longlat)
    # gwbp_checkbox_outlier <- input$gwbp_outliers_checkbox
    gwbp_bw <- as.numeric(input$gwbp_bandwidth) # 获取滑块当前带宽值
    gwbp_bpcolor <- as.character(input$gwbp_bpcolor)

    # 获取 X 列的数据
    x_field <- as.character(input$gwbp_X)
    # 字段存在性检查
    if (!x_field %in% names(gwbp_shp_data)) {
      shiny::showNotification("The selected field does not exist in the shapefile.", type = "error", duration = NULL)
      return()
    }
    # 数值类型检查
    selected_col <- gwbp_shp_data[[x_field]]
    if (!is.numeric(selected_col)) {
      shiny::showNotification(
        paste("Error running GWBP: Selected variable", x_field, "is not numeric type"),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 数值转换与缺失值处理
    X <- na.omit(as.numeric(selected_col))

    # 更新进度条40%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 40)

    cat("开始GWboxplot分析\n")
    # 进行GWboxplot分析,生成对应boxplot图片在temp目录下
    results <- gw_boxplot(bw = gwbp_bw, X = X, kernel = gwbp_kernel, adaptive = gwbp_adaptive, dp.locat = dp.locat, p = 2, theta = 0,
                          longlat = gwbp_longlat, calibrationPoint = calibrationPoint, gwbp_bpcolor = gwbp_bpcolor)
    cat("结束GWboxplot分析\n")

    # 更新进度条60%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 60)

    # 获取outliers结果
    results_outliers <- gw_boxplot_outliers(bw = gwbp_bw, X = X, kernel = gwbp_kernel, adaptive = gwbp_adaptive, dp.locat = dp.locat, p = 2,
                                            theta = 0,longlat = gwbp_longlat)
    results_outliers <- as.data.frame(na.omit(results_outliers))
    outliers_true <- results_outliers[results_outliers$TorF_outlier == TRUE, ]
    outliers_false <- results_outliers[results_outliers$TorF_outlier == FALSE, ]

    # #------------------------# 在地图上标记markers,显示point card
    # 更新点数据
    results <- as.data.frame(results)
    # 检查并生成 FID 列
    if (!"FID" %in% names(gwbp_shp_data)) {
      dp.n <- nrow(dp.locat)
      gwbp_shp_data$FID <- seq_len(dp.n)
    }
    if (!"FID" %in% names(calibrationPoint)) {
      cp.n <- nrow(calibrationPoint)
      calibrationPoint$FID <- seq_len(cp.n)
    }
    # 检查字段是否存在
    if (is.null(input$gwbp_cp_shapefile)) {
      data <- NULL
      # 提取字段数据
      if ("Name" %in% names(gwbp_shp_data) && x_field %in% names(gwbp_shp_data)) {
        data <- gwbp_shp_data[, c("FID", "Name", x_field)]
      } else if (x_field %in% names(gwbp_shp_data)) {
        data <- gwbp_shp_data[, c("FID", x_field)]
      }
      # 合并数据
      if (!is.null(data)) {
        data <- as.data.frame(data)  # 确保为 data.frame
        results <- merge(results, data, by = "FID", all.x = TRUE)
      }
    } else {
      # 如果存在 cp_shapefile，则移除无关列
      results$Name <- NULL
      results$Price <- NA
    }
    # 关键修改部分：补充缺失的经纬度列
    if (!("longitude" %in% names(results)) || !("latitude" %in% names(results))) {
      # 确保 calibrationPoint 包含必要列
      required_cols <- c("FID", "longitude", "latitude")
      if (all(required_cols %in% names(calibrationPoint))) {
        # 根据 FID 合并校准点坐标
        results <- merge(
          results,
          calibrationPoint[, required_cols],
          by = "FID",
          all.x = TRUE
        )
      } else {
        shiny::showNotification("Data miss longitude and latitude columns", type = "error", duration = NULL)
        shiny::removeModal()
      }
    }

    gwbp_data$results <- results
    gwbp_data$results_outliers <- results_outliers
    gwbp_data$outliers_true <- outliers_true
    gwbp_data$outliers_false <- outliers_false

    # 更新进度条到 100%
    shinyWidgets::updateProgressBar(session = session, id = "gwbp_progress", value = 100)
    Sys.sleep(1)  # 等待一秒，确保用户看到完成的进度条
    shiny::removeModal()

    running(FALSE)
  })
  # 显示map地图，并更新shapefile
  output$gwbp_mapPlot <- leaflet::renderLeaflet({
      current_map <- leaflet::leaflet()%>%
                      leaflet::clearMarkers() %>%
                      leaflet::addProviderTiles("CartoDB.Positron")
      # 获取当前的 leaflet 地图对象
      if(!is.null(input$gwbp_shapefile)){
          shp_data <- gwbp_shapefile_data()
          print(paste0("shp_data class:",sf::st_geometry_type(shp_data)[1]))
          # 检查 gwbp_shp_data 是否为 sf 对象
          if (inherits(shp_data, "sf")) {
            if(sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
              current_map <- current_map %>%
                  leaflet::clearMarkers() %>%
                  leaflet::addPolygons(data = shp_data,
                              fillColor = "#C7DFF0",
                              stroke = TRUE,
                              weight = 0.3,
                              opacity = 0.5,
                              fillOpacity = 0.4  # 透明度
                              )
            }
          }
        }
      if(!is.null(input$gwbp_cp_shapefile)){
            cp_shp_data <- gwbp_cp_shapefile_data()
            print(paste0("cp_shp_data class:",sf::st_geometry_type(cp_shp_data)[1]))
            if (inherits(cp_shp_data, "sf")) {
              if(sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
                current_map <- current_map %>%
                    leaflet::clearMarkers() %>%
                    leaflet::addPolygons(data = cp_shp_data,
                                fillColor = "#C7DFF0",
                                stroke = TRUE,
                                weight = 0.3,
                                opacity = 0.5,
                                fillOpacity = 0.4,  # 透明度
                                group = "GWBP-Polygons"
                                )
            }
          }
        } else {
          current_map <- current_map %>%
                          leaflet::clearGroup("GWBP-Polygons")
        }

      
      req(gwbp_data$results)

      results <- gwbp_data$results 
      results_outliers <- gwbp_data$results_outliers
      outliers_true <- gwbp_data$outliers_true
      outliers_false <- gwbp_data$outliers_false
      x_field <- as.character(input$gwbp_X)
      print(paste0("gwbp_data results:",results))

      # 设置适合的缩放级别
      # 计算所有点的边界框
      # 检查是否存在经度和纬度列，并有效计算范围
      if (all(c("longitude", "latitude") %in% names(results))) {
          # 检查是否有非NA的值
          if (any(!is.na(results$longitude)) && any(!is.na(results$latitude))) {
              # 计算min和max，移除NA
              lon_min <- min(results$longitude, na.rm = TRUE)
              lat_min <- min(results$latitude, na.rm = TRUE)
              lon_max <- max(results$longitude, na.rm = TRUE)
              lat_max <- max(results$latitude, na.rm = TRUE)
              # 确保结果有效（非Inf）
              if (all(is.finite(c(lon_min, lat_min, lon_max, lat_max)))) {
                  bounds <- cbind(lon_min, lat_min, lon_max, lat_max)
              } else {
                  # 无效数值，使用shp_data的边界
                  shp_bbox <- sf::st_bbox(shp_data)
                  bounds <- cbind(shp_bbox["xmin"], shp_bbox["ymin"], shp_bbox["xmax"], shp_bbox["ymax"])
              }
          } else {
              # 全为NA，使用shp_data的边界
              shp_bbox <- sf::st_bbox(shp_data)
              bounds <- cbind(shp_bbox["xmin"], shp_bbox["ymin"], shp_bbox["xmax"], shp_bbox["ymax"])
          }
      } else {
          # 列不存在，使用shp_data的边界
          shp_bbox <- sf::st_bbox(shp_data)
          bounds <- cbind(shp_bbox["xmin"], shp_bbox["ymin"], shp_bbox["xmax"], shp_bbox["ymax"])
      }

      icon_url <- paste0("http://127.0.0.1:8000/gw_boxplot_", as.character(results$FID), ".png")

      # 添加标记点
      current_map <- current_map %>%
        leaflet::clearGroup("point_cards_markers") %>%
        leaflet::clearGroup("Outliers") %>%
        leaflet::clearGroup("Non-outliers") %>%
        leaflet::clearControls() %>%
        leaflet::addMarkers(
          data = results,
          lng = results$longitude,
          lat = results$latitude,
          clusterOptions = markerClusterOptions(), # 使用聚类标记
          popup = ~as.character(point_card(results$Name,results[[x_field]],results$longitude,results$latitude,
                                          results$Max,results$Q3,results$Median,results$Q1,results$Min)),
          icon = leaflet::makeIcon(
            iconUrl = icon_url,
            iconWidth = 80,
            iconHeight = 100,
            iconAnchorX = 10,
            iconAnchorY = 10),
          group = "point_cards_markers"
        )%>%
        leaflet::fitBounds(lng1 = bounds[1], lat1 = bounds[2],
                  lng2 = bounds[3], lat2 = bounds[4])

      # 处理 outliers 的点
      current_map <- current_map %>%
        leaflet::addCircles(
          data = outliers_true,
          lng = ~longitude,
          lat = ~latitude,
          popup = ~as.character(small_outliers_popup(x_field, outliers_true$Outliers)),
          color = "red",
          fillOpacity = 0.5,
          stroke = TRUE,
          group = "Outliers"
      ) %>%
      leaflet::addCircles(
        data = outliers_false,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~as.character(small_outliers_popup(x_field, outliers_false$Outliers)),
        color = "blue",
        fillOpacity = 0.5, # 半透明
        stroke = TRUE,
        group = "Non-outliers"
      )%>%
      # 添加组合图例
      leaflet::addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c(paste("Outliers (n =", nrow(outliers_true),")"),
                  paste("Non-outliers (n =", nrow(outliers_false),")")),
        title = "Outliers Display",
        opacity = 0.8,
        group = "outliers_Legend"
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Outliers", "Non-outliers"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      leaflet::fitBounds(lng1 = bounds[1], lat1 = bounds[2],
                lng2 = bounds[3], lat2 = bounds[4])
    

      current_map

  })
  # 响应用户点击marker产生point card
  lat_tolerance <- 0.0005  # 容差范围
  lng_tolerance <- 0.0005
  observeEvent(input$gwbp_mapPlot_marker_click, { # 当点击点时加载point card
      req(point_cards_reactive(), input$gwbp_mapPlot_marker_click)
      clicked_data <- point_cards_reactive()
      click_lat <- input$gwbp_mapPlot_marker_click$lat
      click_lng <- input$gwbp_mapPlot_marker_click$lng  # 从input$parksMap_marker_click获取点击的标记的信息，并将其存储在pin变量中。这个pin变量通常包含有关点击位置的坐标（如lat和lng）
      selectedPoint <- clicked_data[
        (clicked_data$latitude >= click_lat - lat_tolerance) &
        (clicked_data$latitude <= click_lat + lat_tolerance) &
        (clicked_data$longitude >= click_lng - lng_tolerance) &
        (clicked_data$longitude <= click_lng + lng_tolerance),
      ] # 点击的pin的经纬度lng/lat与数据points的经纬度相匹配
      if (!nrow(selectedPoint) == 0) {
        # 更新leaflet地图的弹出窗口
        leaflet::leafletProxy("gwbp_mapPlot") %>%
          leaflet::clearPopups() %>%
          addPopups(
                  lng = click_lng,
                  lat = click_lat, #指定了弹出窗口的位置
                  popup = as.character(point_card(selectedPoint$Name, selectedPoint$Price, selectedPoint$longitude, selectedPoint$latitude,
                  selectedPoint$Max, selectedPoint$Q3,selectedPoint$Median,selectedPoint$Q1,selectedPoint$Min)),
              )
      }
  })
  #--------------------------# Table View #-------------------------#
  # 显示汇总表
  output$gwbp_summaryTable <- DT::renderDataTable({
    result <- gwbp_data$results
    sdf <- as.data.frame(result)
    DT::datatable(
      sdf,
      extensions = 'Buttons',  # 关键要素1：声明使用扩展
      options = list(
        pageLength = 10,        # 默认每页显示10条
        lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
        searching = TRUE,       # 启用搜索框
        scrollX = TRUE,          # 启用水平滚动
        dom = 'Blfrtip',          # 控制界面元素布局
        buttons = list(       # 关键要素3：按钮嵌套配置
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'Export CSV'),
          list(extend = 'excel', text = 'Export Excel')
        )
      ),
      rownames = FALSE,         # 不显示行号
      class = "'display nowrap hover" 
    )
  })
  #--------------------------# GWR #--------------------------#
  # 按钮跳转
  observeEvent(input$gwr_execute, {
    updateTabsetPanel(session, "gwr_tabs", selected = "Scatter Plot")
  })
  observeEvent(input$gwr_video_button, {
    updateTabsetPanel(session, "gwr_tabs", selected = "Video")
  })
  observeEvent(input$gwr_predict_execute, {
    updateTabsetPanel(session, "gwr_tabs", selected = "Prediction")
  })
  # shp读取函数
  gwr_shapefile_data <- reactive({
    req(input$gwr_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$gwr_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwr_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwr_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      temp_dir <- tempdir()
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch(
          {
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },
          error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # cp_shp读取函数
  gwr_cp_shapefile_data <- reactive({
    req(input$gwr_cp_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$gwr_cp_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwr_cp_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwr_cp_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    temp_dir <- tempdir()

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch({
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # gwr结果对象
  gwr_bw_data <- reactiveValues(
    bw = NULL,          
  )
  # 带宽滑块显示隐藏
  observeEvent(input$gwr_best_bandwidth, {
    if (input$gwr_best_bandwidth) {
      shinyjs::hide("bandwidth_div")  # 隐藏带宽滑块
    } else {
      shinyjs::show("bandwidth_div")  # 显示带宽滑块
    }
  })
  # 显著性图例显示隐藏(面/点) 还要加是面的判断
  shinyjs::hide("Significant_line_div")
  shinyjs::hide("Significant_point_div")
  observeEvent(input$gwr_map_layer_execute, {
    # 获取shp数据
    shp_data <- gwr_shapefile_data()
    shp_data <- sf::st_make_valid(shp_data)
    # 初始化 cp_shp_data
    cp_shp_data <- NULL

    # 如果用户上传了regression shapefile，则获取数据
    if (!is.null(input$gwr_cp_shapefile)){
      cp_shp_data <- gwr_cp_shapefile_data()
      cp_shp_data <- sf::st_make_valid(cp_shp_data)
    }
    # 判断是否是面/点数据
    if ((sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON")||
        (!is.null(cp_shp_data) && (sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(cp_shp_data)[1] == "POLYGON"))) {

      # 面数据：隐藏 Significant_point_div，显示或隐藏 Significant_line_div
      shinyjs::hide("Significant_point_div")
      if (input$gwr_significance_show) {
        shinyjs::show("Significant_line_div")
      } else {
        shinyjs::hide("Significant_line_div")
      }

    } else if (sf::st_geometry_type(shp_data)[1] == "POINT") {

      # 点数据：隐藏 Significant_line_div，显示或隐藏 Significant_point_div
      shinyjs::hide("Significant_line_div")
      if (input$gwr_significance_show) {
        shinyjs::show("Significant_point_div")
      } else {
        shinyjs::hide("Significant_point_div")
      }

    } else {
      # 如果几何类型不在 MULTIPOLYGON 或 POINT 中，隐藏所有结果
      shinyjs::hide("Significant_line_div")
      shinyjs::hide("Significant_point_div")
    }
  })
  # 更新带宽滑块
  observe({
      req(input$gwr_shapefile)  # 确保文件已上传
      shp_data <- gwr_shapefile_data()  # 读取 shapefile 数据
      shp_data <- sf::st_make_valid(shp_data)      # 确保数据有效
      req(shp_data)

      # 计算最大距离（米），确保单位正确
      if (sf::st_is_longlat(shp_data)) {
          max_distance <- as.integer(max(sf::st_distance(shp_data)))  # 计算球面距离
      } else {
          coords <- sf::st_coordinates(shp_data)
          max_distance <- as.integer(max(dist(coords)))  # 计算欧式距离
      }

      # 读取是否为自适应带宽
      gwr_adaptive <- as.logical(input$gwr_adaptive)

      # 设置带宽的最大值和单位
      if (gwr_adaptive) {
          max_bw <- nrow(shp_data)  # 以点的个数为单位
          label <- "Bandwidth (Number of points)"
      } else {
          max_bw <- max_distance  # 以最大距离为单位
          label <- "Bandwidth (meter)"
      }

      # 设置滑块n和步长的最大值为数据的数量
      updateSliderInput(session, "gwr_bandwidth", max = max_bw,label = label)
      updateSliderInput(session, "gwr_range", max = max_bw,label = label)
  })
  # 更新因变量选项（独立模块）
  observeEvent(gwr_shapefile_data(), {
    req(gwr_shapefile_data())
    var_names <- names(gwr_shapefile_data()) 
    # 按首字母排序（不区分大小写）
    sorted_vars <- if (length(var_names) > 0) {
      var_names[order(tolower(var_names))]
    } else {
      character(0)
    }
    # 设置默认因变量（示例：选第一个变量）
    default_dependent <- if (length(sorted_vars) >= 1) sorted_vars[1] else character(0)
    
    # 更新因变量下拉框
    updateSelectInput(session,"gwr_dependentVar",choices = sorted_vars,selected = default_dependent)
  })
  # 更新自变量选项（监听因变量和数据变化）
  observeEvent(list(gwr_shapefile_data(), input$gwr_dependentVar), {
    req(gwr_shapefile_data())
    
    # 获取所有变量名并排序
    all_vars <- names(gwr_shapefile_data())
    sorted_all_vars <- all_vars[order(tolower(all_vars))]
    
    # 安全获取当前因变量（处理未选择的情况）
    dependent_var <- if (is.null(input$gwr_dependentVar)) {
      character(0)
    } else {
      input$gwr_dependentVar
    }
    
    # 排除因变量（若存在）
    independent_vars <- if (dependent_var %in% sorted_all_vars) {
      setdiff(sorted_all_vars, dependent_var)
    } else {
      sorted_all_vars  # 因变量无效时显示全部变量
    }
    
    # 设置默认自变量（选前两个有效变量）
    selected_independent <- if (length(independent_vars) >= 2) {
      independent_vars[1:2]
    } else if (length(independent_vars) == 1) {
      independent_vars[1]
    } else {
      character(0)
    }
    
    # 更新自变量下拉框
    updateSelectInput(session,"gwr_independentVars", choices = independent_vars, selected = selected_independent)
  })
  # 输出因变量选择控件
  output$gwr_dependentVar <- renderUI({
    selectInput("gwr_dependentVar", "Choose dependentVar", choices = NULL)
  })
  # 输出自变量选择控件
  output$gwr_independentVars <- renderUI({
    selectInput("gwr_independentVars", "Choose independentVars", choices = NULL, multiple = TRUE, selected = NULL)
  })
  #--------------------------# Scatter Plot #-------------------------#
  # 绘制散点图
  observeEvent(input$gwr_execute, {
    req(input$gwr_dependentVar, input$gwr_independentVars)
    dependent_var <- input$gwr_dependentVar
    independent_vars <- input$gwr_independentVars

    # 遍历每个自变量生成散点图
    lapply(independent_vars, function(independent_var) {
      plotname <- paste0("scatter_plot_", independent_var)

      output[[plotname]] <- renderPlot({
        shp_data <- gwr_shapefile_data()

        # 检查数据是否存在自变量和因变量
        req(independent_var %in% names(shp_data))
        req(dependent_var %in% names(shp_data))

        # 创建回归模型
        formula <- as.formula(paste(dependent_var, "~", independent_var))
        fit <- lm(formula, data = shp_data)

        # 提取回归模型的相关系数和显著性水平
        fit_summary <- summary(fit)
        r_squared <- fit_summary$r.squared
        p_value <- coef(fit_summary)[2, "Pr(>|t|)"]

        # 创建散点图并添加回归线、置信区间及注释
        ggplot2::ggplot(shp_data, ggplot2::aes_string(x = independent_var, y = dependent_var)) +
          ggplot2::geom_point(color = "blue", alpha = 0.6) +  # 散点
          ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red", fill = "orange", alpha = 0.2) +  # 回归线与置信区间
          ggplot2::labs(
            x = independent_var,
            y = dependent_var,
            title = paste(dependent_var, "vs", independent_var),
            subtitle = paste0(
              "R² = ", round(r_squared, 3),
              ", P-value = ", format.pval(p_value, digits = 3, eps = 0.001)
            )
          ) +
          ggplot2::theme_minimal()
      })
    })

    # 动态生成 UI 输出控件
    output$gwr_ScatterPlot <- renderUI({
      plot_outputs <- lapply(independent_vars, function(independent_var) {
        plotname <- paste0("scatter_plot_", independent_var)
        plotOutput(plotname, height = "400px")
      })
      do.call(tagList, plot_outputs)
    })
  })
  #--------------------------# GWR Analysis #-------------------------#
  # 运行GWR分析
  gwr_result <- eventReactive(input$gwr_execute, {
    # 保证参数存在
    req(input$gwr_shapefile)
    req(input$gwr_dependentVar, input$gwr_independentVars)
    req(input$gwr_kernel, input$gwr_adaptive)

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWR Analysis...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwr_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条20%
    shinyWidgets::updateProgressBar(session = session, id = "gwr_progress", value = 20)

    # 获取实际的数据和坐标
    gwr_shp_data <- gwr_shapefile_data()
    spdf <- as(gwr_shp_data, "Spatial")

    # 获取用户选定的变量
    gwr_dependentVar <- as.character(input$gwr_dependentVar)
    gwr_independentVars <- as.character(input$gwr_independentVars)

    # 1. 检查变量存在性
    all_vars <- c(gwr_dependentVar, gwr_independentVars)
    missing_vars <- all_vars[!all_vars %in% names(gwr_shp_data)]
    if (length(missing_vars) > 0) {
      shiny::showNotification(
        paste("Missing variables:", paste(missing_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 2. 检查数值类型
    check_numeric <- function(var_name) {
      var_data <- gwr_shp_data[[var_name]]
      if (!is.numeric(var_data)) {
        return(var_name)
      }
      return(NULL)
    }
    # 检查所有变量
    non_numeric_vars <- unlist(lapply(all_vars, check_numeric))
    if (length(non_numeric_vars) > 0) {
      shiny::showNotification(
        paste("Non-numeric variables:", paste(non_numeric_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }

    # 初始化 dp.locat
    dp.locat <- NULL
    # 提取坐标点
    tryCatch({
      geometry_type <- unique(sf::st_geometry_type(gwr_shp_data))  # 获取所有几何类型
      if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
        centroids <- sf::st_centroid(sf::st_geometry(gwr_shp_data))  # 仅计算几何的质心
        coords <- sf::st_coordinates(centroids)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
        coords <- sf::st_coordinates(gwr_shp_data)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("LINESTRING" %in% geometry_type) {
        # 如果是线状几何，可以取中点（或其他逻辑）
        midpoints <- sf::st_line_sample(sf::st_geometry(gwr_shp_data), n = 1)
        coords <- sf::st_coordinates(midpoints)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else {
        shiny::showNotification(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")), type = "error")
        shiny::removeModal()
        return()
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })
    # 检查 dp.locat 是否为空
    if (is.null(dp.locat)) {
      shiny::showNotification("Coordinates could not be extracted from the shapefile.", type = "error", duration = NULL)
      return()
    }
    dp.locat <- na.omit(dp.locat)
    # 检查并生成 FID 列
    if (!"FID" %in% names(gwr_shp_data)) {
      dp.n <- nrow(dp.locat)
      gwr_shp_data$FID <- seq_len(dp.n)
    }

    formula <- as.formula(paste(gwr_dependentVar, "~", paste(gwr_independentVars, collapse = "+")))

    # 更新进度条40%
    shinyWidgets::updateProgressBar(session = session, id = "gwr_progress", value = 40)

    # 获取选项
    gwr_kernel <- as.character(input$gwr_kernel)
    gwr_adaptive <- as.logical(input$gwr_adaptive)
    # 选择带宽
    if (input$gwr_best_bandwidth) {
      bandwidth <- GWmodel::bw.gwr(formula, data = spdf, kernel = gwr_kernel ,adaptive = gwr_adaptive) # 软件计算带宽
    } else {
      bandwidth <- as.numeric(input$gwr_bandwidth) # 用户输入带宽
    }
    gwr_bw_data$bw <- bandwidth

    # 更新进度条50%
    shinyWidgets::updateProgressBar(session = session, id = "gwr_progress", value = 50)

    gwr_cp_shp_data <- NULL
    cp_spdf <- NULL
    # 选择regression_points
    if(!is.null(input$gwr_cp_shapefile)){
      gwr_cp_shp_data <- gwr_cp_shapefile_data()
      cp_spdf <- as(gwr_cp_shp_data, "Spatial")
    }
    if(is.null(input$gwr_cp_shapefile)){
      # 执行GWR模型
      model <- GWmodel::gwr.basic(formula, data = spdf, kernel = gwr_kernel, adaptive = gwr_adaptive, bw = bandwidth)
    }else{
      # 执行GWR模型
      model <- GWmodel::gwr.basic(formula, data = spdf, regression.points = cp_spdf,  kernel = gwr_kernel, adaptive = gwr_adaptive, bw = bandwidth)
    }

    # 模型成功后检查FID列
    if (!is.null(model)) {
      # 获取空间数据框架
      sdf <- model$SDF
      # 检查是否存在FID列
      if (!"FID" %in% names(sdf)) {
        # 获取数据点数
        dp.n <- nrow(sdf@data)
        # sdf@data$FID <- seq_len(dp.n)
        sdf$FID <- seq_len(dp.n)
        # 更新模型结果
        model$SDF <- sdf
      }
    }

    # 更新进度条100%
    shinyWidgets::updateProgressBar(session = session, id = "gwr_progress", value = 100)
    Sys.sleep(1)  # 等待一秒，确保用户看到完成的进度条
    shiny::removeModal()

    return(model)  # 返回
  })
  # 更新图层选择控件
  observeEvent(gwr_result(), {
    req(gwr_result())

    result <- gwr_result()
    sdf <- result$SDF

    # 触发重新渲染 UI 控件
    output$gwr_map_layer <- renderUI({
        # 提取有效图层名称
        layer_names <- names(sdf)
        layer_names <- layer_names[!grepl("_SE$", layer_names)]
        layer_names <- layer_names[!grepl("y", layer_names)]
        layer_names <- layer_names[!grepl("yhat", layer_names)]
        layer_names <- layer_names[!grepl("_TV$", layer_names)]
        layer_names <- layer_names[!grepl("FID", layer_names)]

        # 创建下拉框
        selectInput(
            inputId = "gwr_map_layer",
            label = "Select Map Layer to Display",
            choices = layer_names,
            selected = layer_names[1]  # 默认选择第一个图层
        )
    })

    # 动态更新变量选择选项
    output$gwr_columns <- renderUI({
      # 筛选列名，排除不需要的列
      names <- names(sdf)
      names <- names[!grepl("_SE$", names)]
      names <- names[!grepl("y", names)]
      names <- names[!grepl("yhat", names)]
      names <- names[!grepl("_TV$", names)]
      names <- names[!grepl("FID", names)]

      # 更新选择框的选项
      selectInput(
        inputId = "gwr_columns",
        label = "Choose Variables",
        choices = names,
        multiple = TRUE,
        selected = names[1]  # 默认不选中任何选项
      )
    })
    
    # **自动触发地图更新，确保默认图层直接显示**
    observe({
      req(input$gwr_map_layer)  #
      shinyjs::delay(1000, shinyjs::click("gwr_map_layer_execute"))  
    })
  })
  #--------------------------# Table View #-------------------------#
  # 显示汇总表
  output$gwr_summaryTable <- DT::renderDataTable({
    result <- gwr_result()
    sdf <- as.data.frame(result$SDF)
    DT::datatable(
      sdf,
      extensions = 'Buttons',  # 关键要素1：声明使用扩展
      options = list(
        pageLength = 10,        # 默认每页显示10条
        lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
        searching = TRUE,       # 启用搜索框
        scrollX = TRUE,          # 启用水平滚动
        dom = 'Blfrtip',          # 控制界面元素布局
        buttons = list(       # 关键要素3：按钮嵌套配置
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'Export CSV'),
          list(extend = 'excel', text = 'Export Excel')
        )
      ),
      rownames = FALSE,         # 不显示行号
      class = "'display nowrap hover" 
    )
  })
  #--------------------------# Summary Information #-------------------------#
  # 显示模型诊断信息
  output$gwr_modelDiagnostics <- renderPrint({
    gwr_result()
  })
  #--------------------------# Web Map #-------------------------#
  # 地图可视化
  output$gwr_mapPlot <- leaflet::renderLeaflet({
    shinyjs::hide("Significant_line_div")
    shinyjs::hide("Significant_point_div")

    map <- leaflet::leaflet()%>%
          leaflet::clearMarkers() %>%
          leaflet::addProviderTiles("CartoDB.Positron")

    # 返回最终的map对象
    map
  })
  # 更新地图上的图层
  observeEvent(input$gwr_map_layer_execute, {
    req(gwr_result())
    req(input$gwr_map_layer)
    req(input$gwr_shapefile)
    req(input$gwr_independentVars)

    shinyjs::hide("Significant_point_div")
    shinyjs::hide("Significant_line_div")
    
    # 每更新一次图层清除音频文件
    prefixes_to_remove <- c("^gwr_map_audio_",
                            "^gwr_map_audio_composite_",
                            "^gwr_map_waveform_",
                            "gwr_map_sound_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取底图map对象
    map <- leaflet::leafletProxy("gwr_mapPlot", session)
    if(!is.null(map)){}
    else{print("map is null")}
    # 获取result和sdf
    result <- gwr_result()
    if(!is.null(result$SDF)){
      sdf <- result$SDF
    }else{print("result sdf is null")}

    # 获取sdf的坐标
    coords_sdf <- sp::coordinates(sdf)
    # 获取自变量independentVars
    independentvars <- as.character(input$gwr_independentVars)
    # 获取图层名称layer_to_plot
    layer_to_plot <- as.character(input$gwr_map_layer)
    # 获取shp数据
    shp_data <- gwr_shapefile_data()
    # 检查数据是否为 sf 对象
    if (!inherits(shp_data, "sf")) {
      shiny::showNotification("Unsupported data types. Input is not an sf object.", type = "error", duration = NULL)
      return()
    }
    # 修复可能的无效几何数据
    shp_data <- sf::st_make_valid(shp_data)

    library(leaflet.extras)
    library(RColorBrewer)

    cp_shp_data <- NULL
    # 修复可能的无效几何图形
    if (!is.null(input$gwr_cp_shapefile)){
        cp_shp_data <- gwr_cp_shapefile_data()
        if (!inherits(cp_shp_data, "sf")) {
          shiny::showNotification("Unsupported data types. Input is not an sf object.", type = "error", duration = NULL)
          return()
        }
        cp_shp_data <- sf::st_make_valid(cp_shp_data)
    }

    # 计算p值
    if(is.null(input$gwr_cp_shapefile)){
      result_enp <- result$GW.diagnostic$enp
      n<-nrow(sdf)
      rdf<-n-result_enp
      # 获取所有包含 "_TV" 的列名
      t_columns <- grep("_TV$", names(sdf), value = TRUE)
      for (col in t_columns){
        # 创建一个新的列名用于存储 p 值
        p_value_col <- gsub("_TV", "_p_value", col)
        sdf[[p_value_col]]<-2*pt(abs(sdf[[col]]), df=rdf, lower.tail = F)
      }
    } else( print(paste0("enp is null")))

    if (!(layer_to_plot %in% names(sdf))) {
      shiny::showNotification(paste("Error: Layer", layer_to_plot, "not found in dataset"), type = "error")
      return()
    }

    if (is.null(sdf[[layer_to_plot]]) || all(is.na(sdf[[layer_to_plot]]))) {
      shiny::showNotification(paste("Error: Layer", layer_to_plot, "contains only NA values"), type = "error")
      return()
    }
    # 根据几何类型进行不同专题图显示
    if ((!is.null(shp_data) && (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON")) ||
        (!is.null(cp_shp_data) && (sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(cp_shp_data)[1] == "POLYGON"))) {
          # 根据用户选择动态设置颜色映射
          selected_palette <- input$gwr_color_palette
          if (selected_palette == "viridis") {
            color_palette <- leaflet::colorNumeric(palette = "viridis", domain = c(
              min(sdf[[layer_to_plot]], na.rm = TRUE),
              max(sdf[[layer_to_plot]], na.rm = TRUE)
            ))
          } else {
            gradient <- color_gradients[[selected_palette]]
            color_palette <- leaflet::colorNumeric(
              palette = c(gradient["low"], gradient["high"]),
              domain = c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                        max(sdf[[layer_to_plot]], na.rm = TRUE))
            )
          }
          if (layer_to_plot == "Intercept" || layer_to_plot %in% independentvars) {
            shinyjs::hide("Significant_line_div")
            shinyjs::hide("Significant_point_div")
            # 添加多边形填充图&图例
            if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
              map %>%
                  leaflet::clearGroup("Polygons") %>%
                  leaflet::clearGroup("Circles") %>%
                  leaflet::removeControl("Polygons") %>%
                  leaflet::removeControl("Circles") %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = shp_data,
                    fillColor = ~color_palette(sdf[[layer_to_plot]]),
                    color = "black",
                    fillOpacity = 0.4,
                    weight = 0.3,
                    opacity = 0.5,
                    # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                    group = "Polygons",
                    layerId = sdf[["FID"]]
                  )%>%
                  leaflet::addLegend(
                    position = "bottomright",
                    pal = color_palette,                                # 图例位置
                    values = sdf[[layer_to_plot]],
                    title = layer_to_plot,                              # 图例标题
                    labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                    opacity = 1,                                         # 图例的不透明度
                    layerId = "Polygons"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))
            } else if (sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(cp_shp_data)[1] == "POLYGON"){
              map %>%
                  leaflet::clearGroup("Polygons") %>%
                  leaflet::clearGroup("Circles") %>%
                  leaflet::removeControl("Polygons") %>%
                  leaflet::removeControl("Circles") %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = cp_shp_data,
                    fillColor = ~color_palette(sdf[[layer_to_plot]]),
                    color = "black",
                    fillOpacity = 0.4,
                    weight = 0.3,
                    opacity = 0.5,
                    # popup = paste("FID : ",sdf[["FID"]], "<br>",layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                    group = "Polygons",
                    layerId = sdf[["FID"]]
                  )%>%
                  leaflet::addLegend(
                    position = "bottomright",
                    pal = color_palette,                                # 图例位置
                    values = sdf[[layer_to_plot]],
                    title = layer_to_plot,                              # 图例标题
                    labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                    opacity = 1,                                         # 图例的不透明度
                    layerId = "Polygons"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))
            }

            # Significance 图
            if(input$gwr_significance_show && is.null(input$gwr_cp_shapefile)){
              shinyjs::show("Significant_line_div")

              # 根据 p 值创建 dashArray 向量
              dash_array_vec <- function(p_value) {
                ifelse(p_value < 0.05, "10,1", "5, 10")
              }

              # 创建一个新的列名用于存储 p 值
              p_value_col <- paste0(layer_to_plot, "_p_value")
              # 添加多边形填充图&图例
              if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
                map %>%
                    leaflet::clearGroup("Significant") %>%
                    leaflet::removeControl("Significant") %>%
                    # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                    leaflet::addPolygons(
                      data = shp_data,
                      color = "black",
                      weight = 1,
                      dashArray = dash_array_vec(sdf[[p_value_col]]), # 粗线条为实线，细线条为虚线
                      fillOpacity = 0,
                      opacity = 1,
                      # popup = paste(layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                      group = "Significant"
                    )%>%
                    # 动态计算地图边界
                    leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                              lat1 = min(coords_sdf[, 2]),
                              lng2 = max(coords_sdf[, 1]),
                              lat2 = max(coords_sdf[, 2]))
              } else if (sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(cp_shp_data)[1] == "POLYGON"){
                map %>%
                    leaflet::clearGroup("Significant") %>%
                    leaflet::removeControl("Significant") %>%
                    # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                    leaflet::addPolygons(
                      data = cp_shp_data,
                      color = "black",
                      weight = 1,
                      dashArray = dash_array_vec, # 粗线条为实线，细线条为虚线
                      fillOpacity = 0,
                      opacity = 1,
                      group = "Significant"
                    )%>%
                    # 动态计算地图边界
                    leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                              lat1 = min(coords_sdf[, 2]),
                              lng2 = max(coords_sdf[, 1]),
                              lat2 = max(coords_sdf[, 2]))
              }
            }else{shinyjs::hide("Significant_line_div")}
          } else if (layer_to_plot == "residual" || layer_to_plot == "CV_Score" ||
                     layer_to_plot == "Stud_residual" || layer_to_plot == "Local_R2") {
            shinyjs::hide("Significant_point_div")
            shinyjs::hide("Significant_line_div")
            # 添加多边形填充图&图例
            if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
              map %>%
                  leaflet::clearGroup("Polygons") %>%
                  leaflet::clearGroup("Circles") %>%
                  leaflet::removeControl("Polygons") %>%
                  leaflet::removeControl("Circles") %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = shp_data,
                    fillColor = ~color_palette(sdf[[layer_to_plot]]),
                    color = "black",
                    fillOpacity = 0.4,
                    weight = 0.3,
                    opacity = 0.5,
                    # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot,  " : ", sdf[[layer_to_plot]]),
                    group = "Polygons"
                  )%>%
                  leaflet::addLegend(
                    position = "bottomright",
                    pal = color_palette,                                # 图例位置
                    values = sdf[[layer_to_plot]],
                    title = layer_to_plot,                              # 图例标题
                    labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                    opacity = 1,                                         # 图例的不透明度
                    layerId = "Polygons"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))
            } else if (sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(cp_shp_data)[1] == "POLYGON"){
              map %>%
                  leaflet::clearGroup("Polygons") %>%
                  leaflet::clearGroup("Circles") %>%
                  leaflet::removeControl("Polygons") %>%
                  leaflet::removeControl("Circles") %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = cp_shp_data,
                    fillColor = ~color_palette(sdf[[layer_to_plot]]),
                    color = "black",
                    fillOpacity = 0.4,
                    weight = 0.3,
                    opacity = 0.5,
                    # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot,  " : ", sdf[[layer_to_plot]]),
                    group = "Polygons"
                  )%>%
                  leaflet::addLegend(
                    position = "bottomright",
                    pal = color_palette,                                # 图例位置
                    values = sdf[[layer_to_plot]],
                    title = layer_to_plot,                              # 图例标题
                    labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                    opacity = 1,                                         # 图例的不透明度
                    layerId = "Polygons"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))
            }
          }
      } else if ((is.null(cp_shp_data) && (sf::st_geometry_type(shp_data)[1] == "POINT" || sf::st_geometry_type(shp_data)[1] == "MULTIPOINT") ) ||
                 (!is.null(cp_shp_data) && !is.null(input$gwr_cp_shapefile) && (sf::st_geometry_type(cp_shp_data)[1] == "POINT" || sf::st_geometry_type(cp_shp_data)[1] == "MULTIPOINT"))){
                  # 根据用户选择动态设置颜色映射
                  domain <- c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                  max(sdf[[layer_to_plot]], na.rm = TRUE))
                  selected_palette <- input$gwr_color_palette
                  if (selected_palette == "viridis") {
                    color_palette <- leaflet::colorNumeric(palette = "viridis", domain = domain)
                  } else {
                    colors <- color_gradients[[selected_palette]]
                    color_palette <- leaflet::colorNumeric(palette = colors, domain = domain)
                  }

                  if (layer_to_plot == "Intercept" || layer_to_plot %in% independentvars) {
                    shinyjs::hide("Significant_line_div")
                    shinyjs::hide("Significant_point_div")
                    # 设置半径大小
                    point_radius <- 50
                    # 添加气泡图&图例
                    map %>%
                        leaflet::clearGroup("Polygons") %>%
                        leaflet::clearGroup("Circles") %>%
                        leaflet::removeControl("Polygons") %>%
                        leaflet::removeControl("Circles") %>%
                        leaflet::clearGroup("Significant") %>%
                        leaflet::removeControl("Significant") %>%
                        # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
                        leaflet::addCircles(
                                  data = sdf,
                                  weight = 3,
                                  radius = point_radius,
                                  color = ~color_palette(sdf[[layer_to_plot]]),
                                  fillOpacity = 1,
                                  lng = coords_sdf[, 1],
                                  lat = coords_sdf[, 2],
                                  # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                                  opacity = 1,
                                  group = "Circles",
                                  layerId = sdf[["FID"]])%>%
                        leaflet::addLegend(
                          position = "bottomright",
                          pal = color_palette,                                # 图例位置
                          values = sdf[[layer_to_plot]],
                          title = layer_to_plot,                              # 图例标题
                          labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                          opacity = 1,                                         # 图例的不透明度
                          layerId = "Circles"
                        )%>%
                        # 动态计算地图边界
                        leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                                  lat1 = min(coords_sdf[, 2]),
                                  lng2 = max(coords_sdf[, 1]),
                                  lat2 = max(coords_sdf[, 2]))

                    # Significance 图
                    if(input$gwr_significance_show && is.null(input$gwr_cp_shapefile)){
                      shinyjs::show("Significant_point_div")
                      # 创建一个新的列名用于存储 p 值
                      p_value_col <- paste0(layer_to_plot, "_p_value")

                      # 假设sdf是一个SpatialPointsDataFrame，coords_sdf是其坐标矩阵
                      sdf_df <- as.data.frame(sdf@data)

                      # 将坐标添加到数据框中
                      sdf_df$lng <- coords_sdf[, 1]
                      sdf_df$lat <- coords_sdf[, 2]
                      # 过滤数据
                      p_values <- sdf_df %>% dplyr::pull(!!dplyr::sym(p_value_col))
                      sdf_significant <- sdf_df[p_values < 0.05 & !is.na(p_values), ]
                      sdf_not_significant <- sdf_df[p_values >= 0.05 & !is.na(p_values), ]

                      # 为两组数据创建不同的图标
                      icon_not_significant <-  leaflet::makeIcon(
                                                iconUrl = "https://s2.loli.net/2024/10/29/sih4RPljFwvVT5S.png",
                                                iconWidth = 15, iconHeight = 15
                                                # iconAnchorX = 0, iconAnchorY = 94 https://s2.loli.net/2024/10/29/sih4RPljFwvVT5S.png X.png
                                              )

                      # 添加气泡图&图例
                      map %>%
                          leaflet::clearGroup("Significant") %>%
                          leaflet::removeControl("Significant") %>%
                          setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
                          leaflet::addMarkers(data = sdf_not_significant,
                                            lng = ~lng, lat = ~lat,
                                            icon = icon_not_significant,
                                            group = "Significant")
                    }
                  } else if (layer_to_plot == "residual" || layer_to_plot == "CV_Score" ||
                            layer_to_plot == "Stud_residual" || layer_to_plot == "Local_R2") {
                    shinyjs::hide("Significant_point_div")
                    shinyjs::hide("Significant_line_div")
                    # 设置半径大小
                    point_radius <- 50
                    # 添加气泡图&图例
                    map %>%
                        leaflet::clearGroup("Polygons") %>%
                        leaflet::clearGroup("Circles") %>%
                        leaflet::removeControl("Polygons") %>%
                        leaflet::removeControl("Circles") %>%
                        leaflet::clearGroup("Significant") %>%
                        leaflet::removeControl("Significant") %>%
                        # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
                        leaflet::addCircles(
                                  data = sdf,
                                  weight = 3,
                                  radius = point_radius,
                                  color = ~color_palette(sdf[[layer_to_plot]]),
                                  fillOpacity = 1,
                                  lng = coords_sdf[, 1],
                                  lat = coords_sdf[, 2],
                                  # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                                  opacity = 1,
                                  group = "Circles",
                                  layerId = sdf[["FID"]])%>%
                        leaflet::addLegend(
                          position = "bottomright",
                          pal = color_palette,                                # 图例位置
                          values = sdf[[layer_to_plot]],
                          title = layer_to_plot,                              # 图例标题
                          labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                          opacity = 1,                                         # 图例的不透明度
                          layerId = "Circles"
                        )%>%
                        # 动态计算地图边界
                        leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                                  lat1 = min(coords_sdf[, 2]),
                                  lng2 = max(coords_sdf[, 1]),
                                  lat2 = max(coords_sdf[, 2]))
                  }
      }
  })
  # 清除显著性图层
  observeEvent(input$gwr_significance_show,{
    # 获取底图map对象
    map <- leaflet::leafletProxy("gwr_mapPlot", session)
    if(!is.null(map)){}
    else{print("map is null")}
    if(!input$gwr_significance_show){
      shinyjs::hide("Significant_line_div")
      shinyjs::hide("Significant_point_div")
      # 清除所有图层
      map %>%
          leaflet::clearGroup("Significant") %>%
          leaflet::removeControl("Significant")
    }
    map
  })
  # 用 reactiveValues 存储用户点击的点和生成的短音频文件路径
  gwr_rv <- reactiveValues(
    clicked_points = list(),  # 用于存放点击的经纬度（仅连线模式时使用）
    audio_files = character() # 存放每个点击生成的短音频文件路径
  )
  # 点击地图产生音频
  observeEvent(input$gwr_mapPlot_click, {
    req(gwr_result())
    click <- input$gwr_mapPlot_click

    # 选择模式：点击模式直接生成短音频
    if(input$gwr_audio_mode == "click"){
      # 获取必要数据
      gwr_sdf <- gwr_result()$SDF
      layer_to_plot <- input$gwr_map_layer

      # 这里假设已将 gwr_sdf 转换为 sf 对象，或直接使用 spatial 包的 nearest feature 方法
      gwr_sdf_sf <- sf::st_as_sf(gwr_sdf)
      click_point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      selected_id <- sf::st_nearest_feature(click_point, gwr_sdf_sf)
      selected_row <- gwr_sdf[selected_id, ]
      # 生成 popup 内容
      popup_content <- paste("FID :", selected_row$FID, "<br>", 
                            layer_to_plot, ":", round(selected_row[[layer_to_plot]], 4))
      if(is.null(input$gwr_cp_shapefile)){
        shp_data <- gwr_shapefile_data() %>% 
              sf::st_as_sf() %>% 
              sf::st_make_valid() %>% 
              sf::st_set_crs(4326)
      } else {
        shp_data <- gwr_cp_shapefile_data() %>% 
              sf::st_as_sf() %>% 
              sf::st_make_valid() %>% 
              sf::st_set_crs(4326)
      }
      geom_type <- sf::st_geometry_type(shp_data) %>% 
        as.character() %>% 
        unique() 
      print(paste0("geom_type",geom_type))
      if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        # 找到包含点击点的 Polygon
        selected_feature <- shp_data[sf::st_contains(shp_data, click_point, sparse = FALSE), ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No polygon data found", type = "error")
          return(NULL)
        }

        leaflet::leafletProxy("gwr_mapPlot") %>%
          leaflet::clearPopups() %>%
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          leaflet::addPolygons(
            data = selected_feature,
            group = "highlighted_features",
            fillColor = "yellow",
            color = "#FF0000",
            fillOpacity = 0,
            weight = 2,
            highlightOptions = leaflet::highlightOptions(
              weight = 5,
              color = "#FF0000",
              bringToFront = TRUE
              )
            )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
        # 找到最近的点
        selected_id <- sf::st_nearest_feature(click_point, shp_data)
        selected_feature <- shp_data[selected_id, ]
        if (length(selected_feature) == 0) {
          shiny::showNotification("No point data found", type = "error")
          return(NULL)
        }
        leaflet::leafletProxy("gwr_mapPlot") %>%
          leaflet::clearPopups() %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::addCircleMarkers(
            data = selected_feature,
            group = "highlighted_features",
            color = "red",
            fillOpacity = 0,
            radius = 8,
            stroke = TRUE,
            weight = 2
          )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      } else {
        shiny::showNotification("The current geometry type is not supported", type = "error")
        return(NULL)
      }
      
      # 生成短音频文件
      coeff <- ifelse(is.na(selected_row[[layer_to_plot]]), 0, selected_row[[layer_to_plot]])
      short_filename <- paste0("www/gwr_map_audio_", as.integer(Sys.time()), "_", sample(1:1000, 1), ".wav")
      generate_audio(value = coeff,
                    filename = short_filename,
                    x_min = min(gwr_sdf[[layer_to_plot]], na.rm = TRUE),
                    x_max = max(gwr_sdf[[layer_to_plot]], na.rm = TRUE))
      
      # 更新 audio 控件，播放短音频
      shinyjs::runjs(sprintf("document.getElementById('gwr_map_audio').src='%s'; document.getElementById('gwr_map_audio').play();", short_filename))
      
    } else if(input$gwr_audio_mode == "line") {
      # 连线模式：记录点击点，不直接生成音频
      gwr_rv$clicked_points <- append(gwr_rv$clicked_points, list(c(click$lng, click$lat)))
      # 在地图上添加标记以便确认顺序
      leaflet::leafletProxy("gwr_mapPlot") %>%
        leaflet::addMarkers(
                  lng = click$lng, 
                  lat = click$lat,
                  popup = paste("point", length(gwr_rv$clicked_points)),
                  group = "point",
                  icon = musicIcon
                  )
    }
  })
  # 处理连线模式下的确认按钮：生成长音频
  observeEvent(input$gwr_confirm_audio, {
    req(length(gwr_rv$clicked_points) > 0)
    # 仅在连线模式时触发检查
    if (input$gwr_audio_mode == "line") {
      # 检查点击点数量是否足够
      if (length(gwr_rv$clicked_points) < 2) {
        shiny::showNotification("Choose at least two points", type = "error")
        return() # 不满足条件时提前退出
      }
    }
    gwr_buffer_length <- as.numeric(input$gwr_buffer_length)
    layer_to_plot <- input$gwr_map_layer

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWR Audio...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwr_audio_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 1. 获取用户点击的坐标并绘制连线
    clicked_matrix <- do.call(rbind, gwr_rv$clicked_points)
    leaflet::leafletProxy("gwr_mapPlot") %>%
      leaflet::addPolylines(
                  lng = clicked_matrix[,1], 
                  lat = clicked_matrix[,2],
                  color = "red",
                  weight = gwr_buffer_length,
                  opacity = 0.8,
                  group = "point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除之前的高亮
    
    # 2. 创建 sf 线对象并确保坐标系统一
    clicked_line <- sf::st_linestring(clicked_matrix) %>%
      sf::st_sfc(crs = 4326) %>%
      sf::st_make_valid()
    line_buffer <- sf::st_buffer(clicked_line, dist = gwr_buffer_length)  # 可调整缓冲区大小
  
    # 3. 处理空间数据
    gwr_sdf <- gwr_result()$SDF
    if(is.null(input$gwr_cp_shapefile)){
      shp_data <- gwr_shapefile_data() %>% 
            sf::st_as_sf() %>% 
            sf::st_make_valid() %>% 
            sf::st_set_crs(4326)
    } else {
      shp_data <- gwr_cp_shapefile_data() %>% 
            sf::st_as_sf() %>% 
            sf::st_make_valid() %>% 
            sf::st_set_crs(4326)
    }

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_audio_progress", value = 10)

    # 4. 转换Spatial对象为sf
    gwr_sdf_sf <- sf::st_as_sf(gwr_sdf) 
    gwr_sdf_sf <- gwr_sdf_sf %>%
      dplyr::rename_with(~paste0("gwr_", .), .cols = -geometry)  

    # 将gwr计算结果合并到原始shp属性（假设行顺序一致）
    if(nrow(shp_data) == nrow(gwr_sdf_sf)) {
      shp_data_with_gwr <- shp_data %>%
        dplyr::bind_cols(sf::st_drop_geometry(gwr_sdf_sf)) %>%  # 使用正确的属性数据
        sf::st_sf()
    } else {
      shiny::showNotification("The number of data rows is inconsistent", type = "error")
      shiny::removeModal()
      return()
    }

    layer_name <- paste0("gwr_",layer_to_plot)

    geom_type <- sf::st_geometry_type(shp_data_with_gwr) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      # 精确相交检测
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwr, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      
      intersect_features <- intersects
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_gwr), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwr_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwr_mapPlot") %>%
        leaflet::addPolygons(
          data = intersects,
          group = "highlighted_features",
          color = "#FF0000",
          fillOpacity = 0,
          weight = 2,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#FF0000",
            bringToFront = TRUE
            )
          )
    }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwr, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      intersect_features <- intersects

      if(length(intersect_features) == 0) {
        shiny::showNotification("The line does not pass through any features", type = "warning")
        shiny::removeModal()
        return()
      }
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_gwr), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwr_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwr_mapPlot") %>%
        leaflet::addCircleMarkers(
          data = intersect_features,
          group = "highlighted_features",
          color = "red",
          fillOpacity = 0,
          radius = 8,
          stroke = TRUE,
          weight = 2
        )
    } else {
      shiny::showNotification("The current geometry type is not supported", type = "error")
      shiny::removeModal()
      return()
    }
    
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_audio_progress", value = 20)

    # 6. 沿路径排序（确保交点顺序与点击点一致）
    if (length(intersect_features) > 0) {
      start_point <- sf::st_sfc(sf::st_point(clicked_matrix[1, ]), crs = 4326)
      intersect_features <- intersect_features %>%
        dplyr::mutate(
          dist_to_start = as.numeric(sf::st_distance(geometry, start_point))
        ) %>%
        dplyr::arrange(dist_to_start)
    }

    # 7. 生成音频
    gwr_rv$audio_files <- character()  # 重置音频存储

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_audio_progress", value = 30 )

    # 8. 遍历要素，生成音频
    for (i in seq_len(nrow(intersect_features))) {
      feature <- intersect_features[i, ]
      coeff <- ifelse(is.na(feature[[layer_name]]), 0, feature[[layer_name]])
      short_filename <- paste0("www/gwr_map_audio_", i, ".wav")

      generate_audio(
        value = coeff,
        filename = short_filename,
        x_min = min(shp_data_with_gwr[[layer_name]], na.rm = TRUE),
        x_max = max(shp_data_with_gwr[[layer_name]], na.rm = TRUE)
      )

      gwr_rv$audio_files <- c(gwr_rv$audio_files, short_filename)

      # 更新进度条
      shinyWidgets::updateProgressBar(session = session, id = "gwr_audio_progress", value = 30 + (i/nrow(intersect_features)) * 30)
    }
    
    # 9. 合成所有短音频文件为一个长音频
    composite_filename <- paste0("www/gwr_map_audio_composite.wav")
    concatenate_audio(gwr_rv$audio_files, composite_filename)
    # 确保 FFmpeg 使用合成后的音频
    sound_road <- composite_filename  # 这里修正

    # 提取系数数据
    ranges <- seq_len(nrow(intersect_features))  # 用于 X 轴
    coeff_values <- intersect_features[[layer_name]]  # 提取回归系数
    ranges_length <- length(ranges)
    # 生成初始曲线图
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = coeff_values), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  
      ) +
      ggplot2::ggtitle("Coefficient Variation Over Features") +
      ggplot2::xlab("Feature Index") + 
      ggplot2::ylab("Coefficient Value")

    # 逐帧生成视频
    waveform_images <- c()
    for (i in seq_along(ranges)) {
      frame_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = ranges[i], color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Feature:", i, "  Coefficient:", round(coeff_values[i], 4)))

      frame_image <- file.path(temp_dir, paste0("gwr_map_waveform_", i, ".png"))
      ggplot2::ggsave(frame_image, frame_plot, width = 6, height = 4, dpi = 150)
      waveform_images <- c(waveform_images, frame_image)

      shinyWidgets::updateProgressBar(session, "gwr_video_progress", value = 60 + (i / ranges_length) * 30)
    }

    # 计算音频时长
    audio_info <- tuneR::readWave(composite_filename)
    total_frames <- length(waveform_images)
    total_audio_duration <- length(audio_info@left) / audio_info@samp.rate
    # 计算帧率，使得视频时长与音频一致
    frame_rate <- total_frames / total_audio_duration
    # 确保 framerate 合理（避免异常）
    if (frame_rate < 1) frame_rate <- 1
    if (frame_rate > 30) frame_rate <- 30  # 限制在 1-30 fps，防止帧率过大或过小
    # 生成视频
    waveform_video <- file.path(temp_dir, "gwr_map_waveform_video.mp4")
    av::av_encode_video(
      input = waveform_images,
      output = waveform_video,
      framerate = frame_rate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",
      audio = NULL,
      verbose = TRUE
    )

    # 合成音视频
    sound_video <- file.path("www", "gwr_map_sound_video.mp4")
    # sound_road <- gwr_rv$audio_files  # 获取之前生成的音频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", 
      "-i", shQuote(waveform_video),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " 合成波形图和音频完成，文件保存为: ", sound_video, "\n")

    # 在地图左下角显示视频
    leaflet::leafletProxy("gwr_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::addControl(
         HTML(sprintf(
          '<video id="gwr_video_player" width="300" autoplay controls>
            <source src="%s" type="video/mp4">
            Your browser does not support the video tag.
          </video>
          <audio id="gwr_audio" src="%s"></audio>
          <script>
            document.getElementById("gwr_video_player").onplay = function() {
              var audio = document.getElementById("gwr_audio");
              if (audio) {
                audio.play();
              }
            };
            document.getElementById("gwr_video_player").onpause = function() {
              var audio = document.getElementById("gwr_audio");
              if (audio) {
                audio.pause();
              }
            };
            document.getElementById("gwr_video_player").ontimeupdate = function() {
              var audio = document.getElementById("gwr_audio");
              if (audio) {
                audio.currentTime = this.currentTime;
              }
            };
          </script>', sound_video, composite_filename
        )),
        position = "bottomleft",
        layerId = "map_video_control" 
      )


    # 设置音频源
    # shinyjs::runjs(sprintf("document.getElementById('gwr_map_audio').src='%s';", composite_filename))

    # 重置记录，便于下次操作
    gwr_rv$clicked_points <- list()
    gwr_rv$audio_files <- character()

    shinyWidgets::updateProgressBar(session = session, id = "gwr_audio_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()
  })
  # 处理连线的图标
  observeEvent(input$gwr_confirm_audio_clear, {
    # 重置记录，便于下次操作
    gwr_rv$clicked_points <- list()
    gwr_rv$audio_files <- character()

    leaflet::leafletProxy("gwr_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::clearGroup("point") %>%
      leaflet::clearGroup("point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除高亮
  })
  #--------------------------# Video #-------------------------#
  # 更新范围图层选项
  gwr_range_layer <- eventReactive(input$gwr_independentVars, {
    req(input$gwr_independentVars)
    gwr_vars <- input$gwr_independentVars

    names <- c("Intercept", gwr_vars)

    # 初始化choices列表
    choices <- list()
    # 动态添加回归系数选项
    for (var in names) {
      choices[[var]] <- var
    }

    # 设置默认选项
    default_choice <- names[1]  # 假设第一个选项是默认选项

    # 返回choices列表和默认选项
    list(choices = choices, default_choice = default_choice)
  }, ignoreNULL = FALSE)
  # 监听范围图层选项的变化并更新图层选项
  observeEvent(gwr_range_layer(), {
    layer_info <- gwr_range_layer()
    updateSelectInput(session, "gwr_range_layer", choices = layer_info$choices, selected = layer_info$default_choice)
  })
  observeEvent(input$gwr_range, {
    min_range <- as.integer(input$gwr_range[1])
    max_range <- as.integer(input$gwr_range[2])
    updateNumericInput(session, "gwr_step_length", max = max_range-min_range)
  })
  # 生成视频
  observeEvent(input$gwr_video_button, {
    # 保证参数存在
    req(input$gwr_shapefile)
    req(input$gwr_dependentVar, input$gwr_independentVars)
    req(input$gwr_kernel, input$gwr_adaptive)
    req(input$gwr_color_palette)

    # 带宽范围
    range_min <- input$gwr_range[1]
    range_max <- input$gwr_range[2]
    # 步长
    step_length <- input$gwr_step_length
    # 颜色盘
    color_palette <- input$gwr_color_palette

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWR Video...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwr_video_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条5%
    shinyWidgets::updateProgressBar(session = session, id = "gwr_video_progress", value = 5)
    cat("0% ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"\n")
    time1 <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))  # 第一个时间点

    # 在开始生成前，确保删除 temp_dir/www_dir 文件夹下以特定前缀开头的所有文件
    prefixes_to_remove <- c("^gwr_image_video_", 
                            "gwr_image_video.mp4",
                            "^gwr_sound_audio_",
                            "^gwr_waveform_",
                            "gwr_waveform_video.mp4",
                            "gwr_waveform_overview.png",
                            "^gwr_sound_video_",
                            "gwr_sound_video.mp4",
                            "gwr_final_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取实际的数据和坐标
    gwr_shp_data <- gwr_shapefile_data()
    spdf <- as(gwr_shp_data,"Spatial")
    formula <- as.formula(paste(input$gwr_dependentVar, "~", paste(input$gwr_independentVars, collapse = "+")))
    # 获取选项
    kernel <- as.character(input$gwr_kernel)
    adaptive <- as.logical(input$gwr_adaptive)
    # 获取自变量independentVars
    independentvars <- as.character(input$gwr_independentVars)
    # 获取变量
    range_layer <- as.character(input$gwr_range_layer)
    # 确保数据是 sf 对象
    cp_shp_data <- NULL
    shp_data <- gwr_shapefile_data()
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}
    # 确保数据中有有效的几何列
    if (is.null(sf::st_geometry(shp_data))) 
    {
      shiny::showNotification("shp_data does not contain geometry.", type = "error")
      shiny::removeModal()
      return()
    }

    # 频率数组
    intercept_vars <- c()
    # 根据步长生成序列
    ranges <- seq(
      from = range_min,  # 起始值
      to = range_max,    # 结束值
      by = step_length   # 步长
    )
    ranges_length <- length(ranges)

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始生成专题图图片...","\n")
    # 专题图视频
    # 计算模型结果
    if (is.null(input$gwr_cp_shapefile)) {
      target_shp_data <- shp_data
      is_regression_point <- FALSE
    } else {
      cp_shp_data <- gwr_cp_shapefile_data()
      if (is.null(sf::st_geometry(cp_shp_data))) 
      {
        shiny::showNotification("cp_shp_data does not contain geometry.", type = "error")
        shiny::removeModal()
        return()
      }
      target_shp_data <- cp_shp_data
      is_regression_point <- TRUE
    }
    # 几何类型计算
    geometry_type <- sf::st_geometry_type(target_shp_data)[1]
    is_polygon <- geometry_type == "MULTIPOLYGON" || geometry_type == "POLYGON"
    is_point <- geometry_type == "POINT" || geometry_type == "MULTIPOLYGON"
    # 确定全局范围：计算所有 range_layer 数据的最小值和最大值
    global_min <- Inf
    global_max <- -Inf
    range_now_fir <- 1
    for (range in ranges) {
      bandwidth <- range
      #  GWR 模型计算
      model <- if (is_regression_point) {
        GWmodel::gwr.basic(formula = formula,data = spdf,regression.points = as(target_shp_data, "Spatial"),kernel = kernel,adaptive = adaptive,bw = bandwidth)
      } else {
        GWmodel::gwr.basic(formula = formula,data = spdf,kernel = kernel,adaptive = adaptive,bw = bandwidth)
      }
      # 提取模型结果
      sdf <- model$SDF
      range_values <- sdf[[range_layer]]
      global_min <- min(global_min, min(range_values, na.rm = TRUE))
      global_max <- max(global_max, max(range_values, na.rm = TRUE))
      
      # 更新进度条
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwr_video_progress",
        value = as.integer(5 + (range_now_fir / ranges_length) * 15)
      )
      range_now_fir <- range_now_fir + 1
    }
    # 确保全局范围有效
    if (global_min == Inf || global_max == -Inf) {
      shiny::showNotification("No valid range values found for plotting.", type = "error")
      shiny::removeModal()
      return()
    }
    # 扩展比例
    padding_ratio <- 0.1  # 扩展范围的比例（10%）
    global_min <-  min(range_values)
    global_max <- max(range_values)
    print(paste0("global_min",global_min,"global_max",global_max))
    # 调整全局最小值和最大值
    range_span <- global_max - global_min
    extended_min <- global_min - range_span * padding_ratio
    extended_max <- global_max + range_span * padding_ratio
    print(paste0("extended_min",extended_min,"extended_max",extended_max))
    fixed_breaks <- seq(global_min, global_max, length.out = 7)  # 创建7份区间
    # 图片主循环
    range_now <- 1
    for (range in ranges) {
      bandwidth <- range
      #  GWR 模型计算
      model <- if (is_regression_point) {
        GWmodel::gwr.basic(formula = formula,data = spdf,regression.points = as(target_shp_data, "Spatial"),kernel = kernel,adaptive = adaptive,bw = bandwidth)
      } else {
        GWmodel::gwr.basic(formula = formula,data = spdf,kernel = kernel,adaptive = adaptive,bw = bandwidth)
      }
      
      # 提取模型结果
      sdf <- model$SDF
      result_Intercept <- as.list(sdf[[range_layer]])
      result_Intercept_vector <- unlist(result_Intercept)
      intercept_vars <- c(intercept_vars, var(result_Intercept_vector, na.rm = TRUE))

      # 绘图
      target_shp_data$value <- sdf[[range_layer]]  # 添加绘图列
      output_path <- file.path(temp_dir, paste0("gwr_image_video_", range, ".png"))
      generate_plot(target_shp_data, range_layer, bandwidth, output_path,color_palette,extended_min, extended_max,fixed_breaks)

      # 更新进度条
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwr_video_progress",
        value = as.integer(20 + (range_now / ranges_length) * 60)
      )
      range_now <- range_now + 1
    }
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 专题图图片生成完成","\n")

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始生成波形图和音频...","\n")
    # --- 音频参数配置 ---
    sampling_rate <- 44100  # 采样率
    duration_per_note <- 0.3  # 每个音符的持续时间（秒）
    note_samples <- duration_per_note * sampling_rate  # 每个音符样本数

    # --- 音色控制参数 ---
    waveform_ratio <- c(0.7, 0.2, 0.1)  # 基波/二次谐波/三次谐波振幅比例 (增大谐波比例（如 c(0.5,0.3,0.2)）会让音色更尖锐)
    adsr <- c(attack=0.05,   # 起音时间比例（0-1）  增大 attack 会产生渐强的起始效果
              decay=0.1,     # 衰减时间比例
              sustain=0.6,   # 延音电平（0-1）
              release=0.15)  # 释音时间比例   增加 release 会让音符结束时有更长的衰减尾音
    filter_cutoff <- 0.4          # 低通滤波器截止频率（比例，0-1对应0-Nyquist） 范围 0.1-0.8，值越小高频衰减越明显
    loudness_factor <- 2          # 响度增益因子

    # --- 频率映射（保持原逻辑）---
    intercept_vars <- na.omit(intercept_vars)
    x_min <- min(intercept_vars)
    x_max <- max(intercept_vars)
    a <- (1000 - 440) / (x_max - x_min)
    b <- 440 - a * x_min
    Intercept_values <- round(a * intercept_vars + b)

    # --- 定义ADSR包络函数 ---
    apply_envelope <- function(signal, adsr_params) {
      n <- length(signal)
      attack_len <- floor(adsr_params["attack"] * n)
      decay_len <- floor(adsr_params["decay"] * n)
      release_len <- floor(adsr_params["release"] * n)
      sustain_len <- n - attack_len - decay_len - release_len
      # 构造包络曲线
      envelope <- c(
        seq(0, 1, length.out = attack_len),                     # 起音
        seq(1, adsr_params["sustain"], length.out = decay_len), # 衰减
        rep(adsr_params["sustain"], sustain_len),               # 延音
        seq(adsr_params["sustain"], 0, length.out = release_len) # 释音
      )
      # 确保长度匹配
      length(envelope) <- n  # 自动截断多余部分
      return(signal * envelope)
    }

    # --- 生成复合波形 ---
    audio_signal <- do.call(c, lapply(Intercept_values, function(freq) {
      # 生成时间序列
      t <- seq(0, duration_per_note, length.out = note_samples)
      # 方波生成（核心波形）
      square_wave <- ifelse(sin(2 * pi * freq * t) > 0, 1, -1)
      # 谐波叠加
      harmonic1 <- 0.3 * sin(2 * pi * 2 * freq * t)   # 二次谐波
      harmonic2 <- 0.2 * sin(2 * pi * 3 * freq * t)   # 三次谐波
      # 混合波形
      mixed_wave <- waveform_ratio[1] * square_wave + 
                    waveform_ratio[2] * harmonic1 + 
                    waveform_ratio[3] * harmonic2
      # 应用包络
      apply_envelope(mixed_wave, adsr)
    }))

    # --- 后处理 ---
    # 低通滤波器（软化高频）
    bf <- signal::butter(4, filter_cutoff, type = "low")  # 4阶巴特沃斯低通
    audio_filtered <- signal::filtfilt(bf, audio_signal)
    # 规范化处理
    audio_normalized <- audio_filtered / max(abs(audio_filtered)) * loudness_factor
    audio_normalized[audio_normalized > 1] <- 1   # 硬限幅
    audio_normalized[audio_normalized < -1] <- -1

    # --- 生成Wave对象 ---
    audio_wave <- tuneR::Wave(
      left = as.integer(audio_normalized * 32767),
      right = as.integer(audio_normalized * 32767),
      samp.rate = sampling_rate,
      bit = 24
    )
    # 保存为WAV文件
    sound_road <- file.path(temp_dir,paste0("gwr_sound_audio_", range_min, "_", range_max, ".wav"))
    tuneR::writeWave(audio_wave, sound_road)
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 音频文件已保存为",sound_road,"\n")


     # 音频波形图
    # 生成总的点图并连成曲线图
    # y为该带宽下的总体系数方差
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = intercept_vars), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  # 添加离散点
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  # 平滑曲线
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  # 设定绘图区域背景为白色
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  # 设定整体背景为白色
      ) +
      ggplot2::ggtitle("Frequency Variation Over Ranges") +
      ggplot2::xlab("Ranges") + ggplot2::ylab("Intercept Values")

    # 保存总波形图
    ggplot2::ggsave(file.path(temp_dir, "gwr_waveform_overview.png"), waveform_plot, width = 6, height = 4, dpi = 150)
    # 生成逐帧图片
    i = 1
    for (range in ranges) {
      single_waveform_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = range, color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Range:", range, "  Variance of variable coefficients:", as.character(round(intercept_vars[i],4))))

      waveform_image <- file.path(temp_dir, paste0("gwr_waveform_", as.character(range), ".png"))
      ggplot2::ggsave(waveform_image, single_waveform_plot, width = 6, height = 4, dpi = 150)
      shinyWidgets::updateProgressBar(
        session = session,
        id = "gwr_video_progress",
        value = as.integer(80 + (i / ranges_length) * 15)
      )
      i = i + 1
    }
    waveform_images <- file.path(temp_dir, paste0("gwr_waveform_", as.character(ranges), ".png"))
    waveform_video <- file.path(temp_dir, "gwr_waveform_video.mp4")
    av::av_encode_video(waveform_images, output = waveform_video, framerate = 10, codec = "libx264", vfilter = "scale=720:720")
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 波形图文件已保存为",waveform_video,"\n")

    # 确保总音频时长与图片生成的视频时长一致  
    # 计算音频总时长
    audio_duration <- length(audio_signal) / sampling_rate  # 音频总时长 (秒)
    # 确保视频帧率适配音频时长
    # 图片数量（即范围跨度）
    num_images <- length(waveform_images)
    # 动态计算帧率，使视频总时长与音频时长一致
    framerate <- num_images / audio_duration
    # 打印调试信息
    cat("音频时长:", audio_duration, "秒\n")
    cat("图片数量:", num_images, "\n")
    cat("视频帧率:", framerate, "帧/秒\n")

    # 专题图视频（无音频）
    image_files <- file.path(temp_dir, paste0("gwr_image_video_", ranges, ".png"))
    image_video <- file.path("www", "gwr_image_video.mp4")
    av::av_encode_video(image_files, output = image_video, framerate = framerate, codec = "libx264", vfilter = "scale=720:720")

    # 波形图和音频
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始合成波形图和音频...","\n")
    video_file <- file.path(temp_dir, paste0("gwr_sound_video_", range_min, "_", range_max, ".mp4"))
    av::av_encode_video(waveform_images,
                        output = video_file,
                        framerate = framerate,
                        codec = "libx264",
                        vfilter = "scale=720:720")
    # 使用FFmpeg合成波形图和音频
    sound_video <- file.path("www", "gwr_sound_video.mp4")
    # 合成音视频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", # 添加 -y 强制覆盖文件
      "-i", shQuote(video_file),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    # 调用系统命令运行 FFmpeg
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA  # 返回一个默认值
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    # 打印合成结果
    cat(paste0(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 合成波形图和音频完成，文件保存为: ", sound_video, "\n"))
    time2 <- as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S"))  # 第二个时间点
    time_diff <- difftime(time2, time1, units = "mins")  # 单位：分钟
    print(paste0("波形图和音频合成时间：", time_diff," mins"))

    # 最终视频
    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 开始合成最终视频文件已保存为","\n")
    final_video <- file.path("www", "gwr_final_video.mp4")
    overlay_filter <- "[1:v]scale=200:150[ovrl];[0:v][ovrl]overlay=W-w-10:H-h-10"
    # "[1:v]scale=200:150[ovrl]"：将小视频缩放到200x150的大小，并将其输出到一个名为ovrl的临时虚拟视频。
    # "[0:v][ovrl]overlay=W-w-10:10"：将主视频和小视频叠加在一起。W-w-10表示小视频在主视频的右上角，距离右边和上边各10像素。
    # 左上角：overlay=10:10
    # 左下角：overlay=10:H-h-10
    # 右下角：overlay=W-w-10:H-h-10
    # 使用 FFmpeg 合并两个视频（水平分屏）
    ffmpeg_combine <- paste(
      "ffmpeg -y",
      "-i", shQuote(image_video),
      "-i", shQuote(sound_video),
      "-filter_complex", shQuote(overlay_filter),
      "-c:v libx264",
      shQuote(final_video)
    )
    # system(ffmpeg_combine)
    # 调用系统命令运行 FFmpeg
    ffmpeg_result_conbine <- tryCatch({
      system(ffmpeg_combine, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA  # 返回一个默认值
    })
    if (!is.na(ffmpeg_result_conbine) && ffmpeg_result_conbine != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }

    cat(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," 最终视频文件已保存为",final_video,"\n")
    time3 <- as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S"))  # 第三个时间点
    time_diff2 <- difftime(time3, time2, units = "mins")  # 单位：分钟
    print(paste0("最后视频合成时间：", time_diff2," mins"))

    # 函数执行完成后，关闭等待窗口
    shinyWidgets::updateProgressBar(session = session, id = "gwr_video_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()

     # 输出视频控件
    output$gwr_image_video <- renderUI({
        tags$video(
            src = image_video, 
            controls = TRUE,
            width = "300px"
        )
    })
    output$gwr_sound_video <- renderUI({
        tags$video(
            src = sound_video, 
            controls = TRUE,
            width = "300px"
        )
    })
    output$gwr_final_video <- renderUI({
        tags$video(
            src = final_video, 
            controls = TRUE,
            width = "600px"
        )
    })
  })
  #--------------------------# Multiple Visualizations #-------------------------#
  observe({
    req(input$gwr_columns)  # 确保用户已经选择了变量
    req(input$gwr_bandwidth)
    req(input$gwr_color_palette)  # 确保用户已经选择了色阶

    # 显示等待窗口
    showModal(modalDialog(
      title = "Please wait",
      "The GWR result is drawing...",
      easyClose = FALSE
    ))

    bw <- round(as.numeric(gwr_bw_data$bw), 3)

    shp_data <- gwr_shapefile_data()
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}

    if (is.null(input$gwr_cp_shapefile)) {
      target_shp_data <- shp_data
      is_regression_point <- FALSE
    } else {
      cp_shp_data <- gwr_cp_shapefile_data()
      if (is.null(sf::st_geometry(cp_shp_data))) 
      {
        shiny::showNotification("cp_shp_data does not contain geometry.", type = "error")
        shiny::removeModal()
        return()
      }
      target_shp_data <- cp_shp_data
      is_regression_point <- TRUE
    }
    # 获取 gwr 结果
    gwr_result <- gwr_result()
    sdf <- gwr_result$SDF

    # 设置高分辨率参数
    dpi <- 100  # 设置DPI值
    width_px <- 1500  # 设置宽度像素
    height_px <- 1500 # 设置高度像素

    # 遍历用户选择的列并动态生成图片
    selected_columns <- input$gwr_columns
    for (col in selected_columns) {
      local({
        column_name <- col  # 需要使用 local() 避免循环中的闭包问题
        # 动态创建 UI 输出控件
        output[[paste0("gwr_plot_", column_name)]] <- renderPlot({

          # 确保列存在于数据框中
          req(column_name %in% names(sdf))

          # 创建专题图
          target_data <- target_shp_data
          target_data$value <- sdf[[column_name]]  # 将数据加入到 sf 对象中，以便于绘制专题图

          # 判断数据类型，动态选择绘图样式
          geom_type <- unique(sf::st_geometry_type(target_data))
          # 根据选择的色阶动态设置颜色
          gradient <- color_gradients[[input$gwr_color_palette]]

          if ("POINT" %in% geom_type || "MULTIPOINT" %in% geom_type) {  # 点数据：使用 color 映射
              if (input$gwr_color_palette == "viridis") {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_viridis_c() +  # 使用 Viridis 色阶
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              } else {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_gradient(low = gradient["low"], high = gradient["high"]) +
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              }
          } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {  # 面数据：使用 fill 映射
            if (input$gwr_color_palette == "viridis") {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_viridis_c() +  # 使用 Viridis 色阶
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            } else {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_gradient(low = gradient["low"], high = gradient["high"]) +
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }
          } else {
            shiny::showNotification("Unsupported geometry type for plotting.", type = "error")
            shiny::removeModal()
            return()
          }        
        })
      })
    }

    # # 动态图片生成完成后关闭等待窗口
    shiny::removeModal()
  })
  # 动态显示图片
  output$gwr_Plot <- renderUI({
    req(input$gwr_columns)  # 确保用户已经选择了变量

    # 每行的图片数量
    images_per_row <- min(2, length(input$gwr_columns))  # 每行最多显示 2 张图片

    plot_outputs <- lapply(seq_along(input$gwr_columns), function(i) {
      column_name <- input$gwr_columns[i]

      # 使用 column 动态调整宽度，每行最多显示 images_per_row 张图片
      column(
        width = 12 / images_per_row,  # 动态设置列宽（12 栅格系统）
        plotOutput(outputId = paste0("gwr_plot_", column_name), height = "500px", width = "100%")
      )
    })

    # 将图片控件布局在 fluidRow 中
    do.call(fluidRow, plot_outputs)
  })
  #--------------------------# Prediction #-------------------------#
  # 预测模块
  observeEvent(input$gwr_predict_execute, {
    req(gwr_result(), input$gwr_predict_datafile)

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWR Prediction...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwr_predict_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_predict_progress", value = 10)
    
    # 读取预测数据
    df <- read.csv(input$gwr_predict_datafile$datapath)
    # 确保预测数据包含坐标
    if (!all(c("longitude", "latitude") %in% colnames(df))) {
      shiny::showNotification(
        paste("Predict data must include longitude and latitude column。"),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 转换为 sf 对象
    sf_obj <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
    # 转换为 sp 对象
    pred_data <- as(sf_obj, "Spatial")
  
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_predict_progress", value = 30)

    # 获取实际的数据和坐标
    gwr_shp_data <- gwr_shapefile_data()
    # 修复可能的无效几何数据
    gwr_shp_data <- sf::st_make_valid(gwr_shp_data)
    spdf <- as(gwr_shp_data, "Spatial")
     # 获取用户选定的变量
    gwr_dependentVar <- as.character(input$gwr_dependentVar)
    gwr_independentVars <- as.character(input$gwr_independentVars)
    # 1. 检查变量存在性
    all_vars <- c(gwr_dependentVar, gwr_independentVars)
    missing_vars <- all_vars[!all_vars %in% names(gwr_shp_data)]
    if (length(missing_vars) > 0) {
      shiny::showNotification(
        paste("Missing variables:", paste(missing_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 2. 检查数值类型
    check_numeric <- function(var_name) {
      var_data <- gwr_shp_data[[var_name]]
      if (!is.numeric(var_data)) {
        return(var_name)
      }
      return(NULL)
    }
    # 检查所有变量
    non_numeric_vars <- unlist(lapply(all_vars, check_numeric))
    if (length(non_numeric_vars) > 0) {
      shiny::showNotification(
        paste("Non-numeric variables:", paste(non_numeric_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 获取用户选定的变量
    gwr_dependentVar <- as.character(input$gwr_dependentVar)
    gwr_independentVars <- as.character(input$gwr_independentVars)
    # # 验证因变量
    # validate(
    #   need(
    #     !has_chinese(gwr_dependentVar),
    #     "错误：因变量名包含中文字符！"
    #   )
    # )
    # # 分割自变量并验证
    # independent_list <- trimws(unlist(strsplit(gwr_independentVars, ",\\s*")))
    # if (length(independent_list) == 0) {
    #   validate(need(FALSE, "错误：请输入至少一个自变量！"))
    # }
    # invalid_vars <- sapply(independent_list, function(var) {
    #   has_chinese(var)
    # })
    # if (any(invalid_vars)) {
    #   invalid_names <- independent_list[invalid_vars]
    #   validate(
    #     need(
    #       FALSE,
    #       paste("错误：以下自变量包含中文字符：", paste(invalid_names, collapse = ", "))
    #     )
    #   )
    # }
    formula <- as.formula(paste(gwr_dependentVar, "~", paste(gwr_independentVars, collapse = "+")))
    bw <- gwr_bw_data$bw
    # 获取选项
    gwr_kernel <- as.character(input$gwr_kernel)
    gwr_adaptive <- as.logical(input$gwr_adaptive)

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_predict_progress", value = 60)
    
    prediction <- tryCatch({
      GWmodel::gwr.predict(formula = formula, data = spdf, predictdata = pred_data, bw = bw, kernel = gwr_kernel, adaptive = gwr_adaptive)
    }, error = function(e) {
      shiny::showNotification(
        paste("GWR predict error: ", e$message),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    })
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_predict_progress", value = 80)

    # 预测数据地图
    output$gwr_predict_map <- leaflet::renderLeaflet({
      req(prediction, gwr_shp_data)  # 确保预测结果和原始数据存在
      # 检查 prediction 结果
      if (!"SDF" %in% names(prediction)) {
        return(NULL)
      }
      pred_values <- prediction$SDF$prediction
      if (is.null(pred_values)) {
        pred_values <- rep("No Prediction", nrow(df))  # 预防性填充
      }
      
      # 获取原始数据的几何类型
      geom_type <- sf::st_geometry_type(gwr_shp_data, by_geometry = FALSE) %>% 
        as.character() %>%
        unique() %>%
        dplyr::first()

      # 创建基础地图
      map <- leaflet::leaflet()%>%
        leaflet::addProviderTiles("CartoDB.Positron")

      # 添加原始数据图层（蓝色）
      if (geom_type %in% c("POINT", "MULTIPOINT")) {
        map <- map %>% 
          leaflet::addCircleMarkers(
            data = gwr_shp_data,
            radius = 3,
            color = "blue",
            fillOpacity = 0.5,
            group = "Train Data",
            popup = ~paste("Train Data:", get(input$gwr_dependentVar))
          )
      } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        map <- map %>% 
          leaflet::addPolygons(
            data = gwr_shp_data,
            color = "blue",
            weight = 1,
            fillOpacity = 0.3,
            group = "Train Data",
            popup = ~paste("Train Data:", get(input$gwr_dependentVar))
          )
      }

      # 添加预测数据图层（红色）
      if (exists("pred_values") && !all(is.na(pred_values))) {
        map <- map %>%
          leaflet::addCircleMarkers(
            data = df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 5,
            color = "red",
            fillOpacity = 0.8,
            group = "Predict Data",
            popup = ~paste("Predict Data:", round(pred_values, 2))
          ) %>%
          leaflet::addLayersControl(
            overlayGroups = c("Train Data", "Predict Data"),
            options = layersControlOptions(collapsed = FALSE)
          )
      }

      # 添加图例
      map %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = c("blue", "red"),
          labels = c("Train Data", "Predict Data"),
          opacity = 0.8
        )
    })

    # 显示预测模型诊断信息
    output$gwr_predict_information <- renderPrint({
      prediction
    })

    # 显示预测结果表
    output$gwr_predict_table <- DT::renderDataTable({
      result <- prediction
      sdf <- as.data.frame(result$SDF)
      DT::datatable(
        sdf,
        extensions = 'Buttons',  # 关键要素1：声明使用扩展
        options = list(
          pageLength = 10,        # 默认每页显示10条
          lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
          searching = TRUE,       # 启用搜索框
          scrollX = TRUE,          # 启用水平滚动
          dom = 'Blfrtip',          # 控制界面元素布局
          buttons = list(       # 关键要素3：按钮嵌套配置
            list(extend = 'copy', text = 'Copy'),
            list(extend = 'csv', text = 'Export CSV'),
            list(extend = 'excel', text = 'Export Excel')
          )
        ),
        rownames = FALSE,         # 不显示行号
        class = "'display nowrap hover" 
      )
    })

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwr_predict_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()
  })

  #--------------------------# MGWR #--------------------------#
  # 显著性图例显示隐藏(面/点) 还要加是面的判断
  shinyjs::hide("mgwr_Significant_line_div")
  shinyjs::hide("mgwr_Significant_point_div")
  # 按钮跳转
  observeEvent(input$mgwr_execute, {
    updateTabsetPanel(session, "mgwr_tabs", selected = "Summary Information")
  })
  # shp读取函数
  mgwr_shapefile_data <- reactive({
    req(input$mgwr_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$mgwr_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$mgwr_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$mgwr_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      temp_dir <- tempdir()
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch(
          {
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },
          error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # 更新因变量选项（独立模块）
  observeEvent(mgwr_shapefile_data(), {
    req(mgwr_shapefile_data())
    var_names <- names(mgwr_shapefile_data()) 
    # 按首字母排序（不区分大小写）
    sorted_vars <- if (length(var_names) > 0) {
      var_names[order(tolower(var_names))]
    } else {
      character(0)
    }
    # 设置默认因变量（示例：选第一个变量）
    default_dependent <- if (length(sorted_vars) >= 1) sorted_vars[1] else character(0)
    
    # 更新因变量下拉框
    updateSelectInput(session,"mgwr_dependentVar",choices = sorted_vars,selected = default_dependent)
  })
  # 更新自变量选项（监听因变量和数据变化）
  observeEvent(list(mgwr_shapefile_data(), input$mgwr_dependentVar), {
    req(mgwr_shapefile_data())
    
    # 获取所有变量名并排序
    all_vars <- names(mgwr_shapefile_data())
    sorted_all_vars <- all_vars[order(tolower(all_vars))]
    
    # 安全获取当前因变量（处理未选择的情况）
    dependent_var <- if (is.null(input$mgwr_dependentVar)) {
      character(0)
    } else {
      input$mgwr_dependentVar
    }
    
    # 排除因变量（若存在）
    independent_vars <- if (dependent_var %in% sorted_all_vars) {
      setdiff(sorted_all_vars, dependent_var)
    } else {
      sorted_all_vars  # 因变量无效时显示全部变量
    }
    
    # 设置默认自变量（选前两个有效变量）
    selected_independent <- if (length(independent_vars) >= 2) {
      independent_vars[1:2]
    } else if (length(independent_vars) == 1) {
      independent_vars[1]
    } else {
      character(0)
    }
    
    # 更新自变量下拉框
    updateSelectInput(session,"mgwr_independentVars", choices = independent_vars, selected = selected_independent)
  })
  # 输出因变量选择控件
  output$mgwr_dependentVar <- renderUI({
    selectInput("mgwr_dependentVar", "Choose dependentVar", choices = NULL)
  })
  # 输出自变量选择控件
  output$mgwr_independentVars <- renderUI({
    selectInput("mgwr_independentVars", "Choose independentVars", choices = NULL, multiple = TRUE, selected = NULL)
  })
  # 带宽值
  bwValues <- reactiveValues(sliders = NULL)
  # 带宽响应函数
  observe({
      if (is.null(input$mgwr_shapefile)) { return(NULL) }  # 确保 shapefile 存在
      shp_data <- mgwr_shapefile_data()  # 读取 shapefile 数据
      req(shp_data)
      req(input$mgwr_independentVars)

      # 处理无效几何
      shp_data <- sf::st_make_valid(shp_data)

      # 计算最大距离
      if (sf::st_is_longlat(shp_data)) {
          max_distance <- as.integer(max(sf::st_distance(shp_data)))  # 计算球面距离
      } else {
          coords <- sf::st_coordinates(shp_data)
          max_distance <- as.integer(max(dist(coords)))  # 计算欧式距离
      }

      # 监听 adaptive 变化
      mgwr_adaptive <- as.logical(input$mgwr_adaptive)

      # 设定最大带宽
      max_bw <- ifelse(mgwr_adaptive, nrow(shp_data), max_distance)
      if (!is.finite(max_bw) || is.na(max_bw)) {
          max_bw <- 1000  # 兜底处理，防止 NaN 出现
      }

      # 变量列表：Intercept + 用户选择的变量
      Vars <- c("Intercept", input$mgwr_independentVars)

      # 生成滑块
      if(input$mgwr_adaptive)
      {
        bwValues$sliders <- lapply(Vars, function(var) {
          sliderInput(
              inputId = paste0("bw_", var),
              label = paste0("Bandwidth for ", var,"(Number of points)"),
              min = 0,
              max = max_bw,  # 这里响应 shapefile 和 adaptive
              value = 20
          )
        })
      }else{
        bwValues$sliders <- lapply(Vars, function(var) {
          sliderInput(
              inputId = paste0("bw_", var),
              label = paste0("Bandwidth for ", var,"(meter)"),
              min = 0,
              max = max_bw,  # 这里响应 shapefile 和 adaptive
              value = 20
          )
        })
      }
     
  })
  # 带宽输出控件
  output$mgwr_bandwidth <- renderUI({
    req(bwValues$sliders)
    if(!input$mgwr_best_bandwidth)
    {
      do.call(tagList, bwValues$sliders)
    }
  })
  #--------------------------# MGWR Analysis #-------------------------#
  # mgwr模型结果
  mgwr_result <- eventReactive(input$mgwr_execute, {
    req(input$mgwr_dependentVar, input$mgwr_independentVars)
    req(input$mgwr_shapefile)
    req(input$mgwr_kernel, input$mgwr_adaptive)

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing MGWR Analysis...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "mgwr_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))
    # 更新进度条20%
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_progress", value = 20)

    # 获取实际的数据和坐标
    mgwr_shp_data <- mgwr_shapefile_data()
    if (inherits(mgwr_shp_data, "sf")) {mgwr_shp_data <- sf::st_make_valid(mgwr_shp_data)}
    spdf <- as(mgwr_shp_data,"Spatial")

    # 更新进度条40%
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_progress", value = 40)

    # 获取用户选定的变量
    mgwr_dependentVar <- as.character(input$mgwr_dependentVar)
    mgwr_independentVars <- as.character(input$mgwr_independentVars)

    # 1. 检查变量存在性
    all_vars <- c(mgwr_dependentVar, mgwr_independentVars)
    missing_vars <- all_vars[!all_vars %in% names(mgwr_shp_data)]
    if (length(missing_vars) > 0) {
      shiny::showNotification(
        paste("Missing variables:", paste(missing_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
    # 2. 检查数值类型
    check_numeric <- function(var_name) {
      var_data <- mgwr_shp_data[[var_name]]
      if (!is.numeric(var_data)) {
        return(var_name)
      }
      return(NULL)
    }
    # 检查所有变量
    non_numeric_vars <- unlist(lapply(all_vars, check_numeric))
    if (length(non_numeric_vars) > 0) {
      shiny::showNotification(
        paste("Non-numeric variables:", paste(non_numeric_vars, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      shiny::removeModal()
      return()
    }
        # 初始化 dp.locat
    dp.locat <- NULL
    # 提取坐标点
    tryCatch({
      geometry_type <- unique(sf::st_geometry_type(mgwr_shp_data))  # 获取所有几何类型
      if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
        centroids <- sf::st_centroid(sf::st_geometry(mgwr_shp_data))  # 仅计算几何的质心
        coords <- sf::st_coordinates(centroids)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
        coords <- sf::st_coordinates(mgwr_shp_data)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("LINESTRING" %in% geometry_type) {
        # 如果是线状几何，可以取中点（或其他逻辑）
        midpoints <- sf::st_line_sample(sf::st_geometry(mgwr_shp_data), n = 1)
        coords <- sf::st_coordinates(midpoints)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else {
        shiny::showNotification(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")), type = "error")
        shiny::removeModal()
        return()
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })
    # 检查 dp.locat 是否为空
    if (is.null(dp.locat)) {
      shiny::showNotification("Coordinates could not be extracted from the shapefile.", type = "error", duration = NULL)
      return()
    }
    dp.locat <- na.omit(dp.locat)
    # 检查并生成 FID 列
    if (!"FID" %in% names(mgwr_shp_data)) {
      dp.n <- nrow(dp.locat)
      mgwr_shp_data$FID <- seq_len(dp.n)
    }

    # 获取选项
    mgwr_kernel <- as.character(input$mgwr_kernel)
    mgwr_adaptive <- as.logical(input$mgwr_adaptive)
    # 构造公式
    formula <- as.formula(paste(mgwr_dependentVar, "~", paste(mgwr_independentVars, collapse = "+")))

    # 更新进度条60%
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_progress", value = 60)

    # 执行MGWR模型
    if(input$mgwr_best_bandwidth){
      model <- GWmodel::gwr.multiscale(formula, data = spdf, kernel = mgwr_kernel, adaptive = mgwr_adaptive, bw.seled = FALSE)
      # 添加最优带宽到模型对象
      model$optimal_bws <- model$GW.arguments$bws
    }else{
      # 获取带宽值，确保它们是数值类型
      Vars <- c("Intercept", input$mgwr_independentVars)
      bw <- sapply(Vars, function(x) {
        as.numeric(input[[paste0("bw_", x)]])
      })
      model <- GWmodel::gwr.multiscale(formula, data = spdf, kernel = mgwr_kernel, adaptive = mgwr_adaptive, bws0 = bw, bw.seled = TRUE)
    }

    # 模型成功后检查FID列
    if (!is.null(model)) {
      # 获取空间数据框架
      sdf <- model$SDF
      # 检查是否存在FID列
      if (!"FID" %in% names(sdf)) {
        # 获取数据点数
        dp.n <- nrow(sdf@data)
        # sdf@data$FID <- seq_len(dp.n)
        sdf$FID <- seq_len(dp.n)
        # 更新模型结果
        model$SDF <- sdf
      }
    }

    # 更新进度条100%
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()

    return(model)  # 返回
  })
  # 更新图层选择控件
  observeEvent(mgwr_result(), {
    req(mgwr_result())

    result <- mgwr_result()
    sdf <- result$SDF

    # 触发重新渲染 UI 控件
    output$mgwr_map_layer <- renderUI({
        # 提取有效图层名称
        layer_names <- names(sdf)
        layer_names <- layer_names[!grepl("_SE$", layer_names)]
        layer_names <- layer_names[!grepl("y", layer_names)]
        layer_names <- layer_names[!grepl("yhat", layer_names)]
        layer_names <- layer_names[!grepl("_TV$", layer_names)]
        layer_names <- layer_names[!grepl("FID", layer_names)]


        # 创建下拉框
        selectInput(
            inputId = "mgwr_map_layer",
            label = "Select Map Layer to Display",
            choices = layer_names,
            selected = layer_names[1]  # 默认选择第一个图层
        )
    })

    # 动态更新变量选择选项
    output$mgwr_columns <- renderUI({
      # 筛选列名，排除不需要的列
      names <- names(sdf)
      names <- names[!grepl("_SE$", names)]
      names <- names[!grepl("y", names)]
      names <- names[!grepl("yhat", names)]
      names <- names[!grepl("_TV$", names)]
      names <- names[!grepl("FID", names)]

      # 更新选择框的选项
      selectInput(
        inputId = "mgwr_columns",
        label = "Choose Variables",
        choices = names,
        multiple = TRUE,
        selected = names[1]  # 默认不选中任何选项
      )
    })

    # **自动触发地图更新，确保默认图层直接显示**
    observe({
      req(input$mgwr_map_layer)  #
      shinyjs::delay(1000, shinyjs::click("mgwr_map_layer_execute"))  
    })
  })
  #--------------------------# Table View #-------------------------#
  # 输出模型结果
  output$mgwr_summaryTable <- DT::renderDataTable({
    result <- mgwr_result()
    sdf <- as.data.frame(result$SDF)
    DT::datatable(
      sdf,
      extensions = 'Buttons',  # 关键要素1：声明使用扩展
      options = list(
        pageLength = 10,        # 默认每页显示10条
        lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
        searching = TRUE,       # 启用搜索框
        scrollX = TRUE,          # 启用水平滚动
        dom = 'Blfrtip',          # 控制界面元素布局
        buttons = list(       # 关键要素3：按钮嵌套配置
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'Export CSV'),
          list(extend = 'excel', text = 'Export Excel')
        )
      ),
      rownames = FALSE,         # 不显示行号
      class = "'display nowrap hover" 
    )
  })
  #--------------------------# Summary Information #-------------------------#
  # 输出模型结果
  output$mgwr_modelDiagnostics <- renderPrint({
    mgwr_result()
  })
  #--------------------------# Web Map #-------------------------#
  # 地图可视化
  output$mgwr_mapPlot <- leaflet::renderLeaflet({
    map <- leaflet::leaflet() %>%
          leaflet::clearMarkers() %>%
          leaflet::addProviderTiles("CartoDB.Positron")

    # 返回最终的map对象
    map
  })
  # 更新地图上的图层
  observeEvent(input$mgwr_map_layer_execute, {
    req(mgwr_result())
    req(input$mgwr_map_layer)
    req(mgwr_shapefile_data())
    req(input$mgwr_independentVars)
    req(input$mgwr_color_palette)

    shinyjs::hide("mgwr_Significant_point_div")
    shinyjs::hide("mgwr_Significant_line_div")

    # 每更新一次图层清除音频文件
    prefixes_to_remove <- c("^mgwr_map_audio_",
                            "^mgwr_map_audio_composite_",
                            "^mgwr_map_waveform_",
                            "mgwr_map_sound_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取底图map对象
    map <- leaflet::leafletProxy("mgwr_mapPlot", session)
    if(!is.null(map)){}
    else{print("map is null")}
    # 获取result和sdf
    result <- mgwr_result()
    if(!is.null(result$SDF)){sdf <- result$SDF}else{print("result sdf is null")}

    # 获取sdf的坐标
    coords_sdf <- sp::coordinates(sdf)
    # 获取自变量independentVars
    independentvars <- as.character(input$mgwr_independentVars)
    # 获取图层名称layer_to_plot
    layer_to_plot <- input$mgwr_map_layer
    # 获取shp数据
    shp_data <- mgwr_shapefile_data()

    library(leaflet.extras)
    library(RColorBrewer)

    # 计算p值
    result_enp <- result$GW.diagnostic$enp
    n<-nrow(sdf)
    rdf<-n-result_enp
    # 获取所有包含 "_TV" 的列名
    t_columns <- grep("_TV$", names(sdf), value = TRUE)
    for (col in t_columns){
      # 创建一个新的列名用于存储 p 值
      p_value_col <- gsub("_TV", "_p_value", col)
      sdf[[p_value_col]]<-2*pt(abs(sdf[[col]]), df=rdf, lower.tail = F)
    }

    # 输出图层选择控件
    if (inherits(shp_data, "sf")) {shp_data <- sf::st_make_valid(shp_data)}
    # 根据几何类型进行不同专题图显示
    if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
        # 根据用户选择动态设置颜色映射
        selected_palette <- input$mgwr_color_palette
        if (selected_palette == "viridis") {
          color_palette <- leaflet::colorNumeric(palette = "viridis", domain = c(
            min(sdf[[layer_to_plot]], na.rm = TRUE),
            max(sdf[[layer_to_plot]], na.rm = TRUE)
          ))
        } else {
          gradient <- color_gradients[[selected_palette]]
          color_palette <- leaflet::colorNumeric(
            palette = c(gradient["low"], gradient["high"]),
            domain = c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                      max(sdf[[layer_to_plot]], na.rm = TRUE))
          )
        }
        if (layer_to_plot == "Intercept" || layer_to_plot %in% independentvars) {
          shinyjs::hide("mgwr_Significant_line_div")
          shinyjs::hide("mgwr_Significant_point_div")
          # 添加多边形填充图&图例
          if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
            map %>%
                leaflet::clearGroup("Polygons") %>%
                leaflet::clearGroup("Circles") %>%
                leaflet::removeControl("Polygons") %>%
                leaflet::removeControl("Circles") %>%
                leaflet::clearGroup("Significant") %>%
                leaflet::removeControl("Significant") %>%
                # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                leaflet::addPolygons(
                  data = shp_data,
                  fillColor = ~color_palette(sdf[[layer_to_plot]]),
                  color = "black",
                  fillOpacity = 0.4,
                  weight = 0.3,
                  opacity = 0.5,
                  # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                  group = "Polygons",
                  layerId = sdf[["FID"]]
                )%>%
                leaflet::addLegend(
                  position = "bottomright",
                  pal = color_palette,                                # 图例位置
                  values = sdf[[layer_to_plot]],
                  title = layer_to_plot,                              # 图例标题
                  labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                  opacity = 1,                                         # 图例的不透明度
                  layerId = "Polygons"
                )%>%
                # 动态计算地图边界
                leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                          lat1 = min(coords_sdf[, 2]),
                          lng2 = max(coords_sdf[, 1]),
                          lat2 = max(coords_sdf[, 2]))
          }

          # Significance 图
          if(input$mgwr_significance_show){
            shinyjs::show("mgwr_Significant_line_div")

            # 根据 p 值创建 dashArray 向量
            dash_array_vec <- function(p_value) {
              ifelse(p_value < 0.05, "10,1", "5, 10")
            }

            # 创建一个新的列名用于存储 p 值
            p_value_col <- paste0(layer_to_plot, "_p_value")
            # 添加多边形填充图&图例
            if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
              map %>%
                  leaflet::clearGroup("Significant") %>%
                  leaflet::removeControl("Significant") %>%
                  # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                  leaflet::addPolygons(
                    data = shp_data,
                    color = "black",
                    weight = 1,
                    dashArray = dash_array_vec(sdf[[p_value_col]]), # 粗线条为实线，细线条为虚线
                    fillOpacity = 0,
                    opacity = 1,
                    # popup = paste(layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                    group = "Significant"
                  )%>%
                  # 动态计算地图边界
                  leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                            lat1 = min(coords_sdf[, 2]),
                            lng2 = max(coords_sdf[, 1]),
                            lat2 = max(coords_sdf[, 2]))
            }
          }else{
              shinyjs::hide("mgwr_Significant_line_div")
          }
        } else if (layer_to_plot == "residual" || layer_to_plot == "CV_Score" ||
                    layer_to_plot == "Stud_residual" || layer_to_plot == "Local_R2") {
          shinyjs::hide("mgwr_Significant_point_div")
          shinyjs::hide("mgwr_Significant_line_div")
          # 添加多边形填充图&图例
          if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
            map %>%
                leaflet::clearGroup("Polygons") %>%
                leaflet::clearGroup("Circles") %>%
                leaflet::removeControl("Polygons") %>%
                leaflet::removeControl("Circles") %>%
                leaflet::clearGroup("Significant") %>%
                leaflet::removeControl("Significant") %>%
                # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                leaflet::addPolygons(
                  data = shp_data,
                  fillColor = ~color_palette(sdf[[layer_to_plot]]),
                  color = "black",
                  fillOpacity = 0.4,
                  weight = 0.3,
                  opacity = 0.5,
                  # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot,  " : ", sdf[[layer_to_plot]]),
                  group = "Polygons",
                  layerId = sdf[["FID"]]
                )%>%
                leaflet::addLegend(
                  position = "bottomright",
                  pal = color_palette,                                # 图例位置
                  values = sdf[[layer_to_plot]],
                  title = layer_to_plot,                              # 图例标题
                  labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                  opacity = 1,                                         # 图例的不透明度
                  layerId = "Polygons"
                )%>%
                # 动态计算地图边界
                leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                          lat1 = min(coords_sdf[, 2]),
                          lng2 = max(coords_sdf[, 1]),
                          lat2 = max(coords_sdf[, 2]))
          }
        }
    } else if (sf::st_geometry_type(shp_data)[1] == "POINT" || sf::st_geometry_type(shp_data)[1] == " MULTIPOINT"){
        # 根据用户选择动态设置颜色映射
        domain <- c(min(sdf[[layer_to_plot]], na.rm = TRUE),
        max(sdf[[layer_to_plot]], na.rm = TRUE))
        selected_palette <- input$mgwr_color_palette
        if (selected_palette == "viridis") {
          color_palette <- leaflet::colorNumeric(palette = "viridis", domain = domain)
        } else {
          colors <- color_gradients[[selected_palette]]
          color_palette <- leaflet::colorNumeric(palette = colors, domain = domain)
        }
        # 设置半径大小
        point_radius <- 50
        if (layer_to_plot == "Intercept" || layer_to_plot %in% independentvars) {
          shinyjs::hide("mgwr_Significant_line_div")
          shinyjs::hide("mgwr_Significant_point_div")
          # 添加气泡图&图例
          map %>%
              leaflet::clearGroup("Polygons") %>%
              leaflet::clearGroup("Circles") %>%
              leaflet::removeControl("Polygons") %>%
              leaflet::removeControl("Circles") %>%
              leaflet::clearGroup("Significant") %>%
              leaflet::removeControl("Significant") %>%
              # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
              leaflet::addCircles(
                        data = sdf,
                        weight = 3,
                        radius = point_radius,
                        color = ~color_palette(sdf[[layer_to_plot]]),
                        fillOpacity = 1,
                        lng = coords_sdf[, 1],
                        lat = coords_sdf[, 2],
                        # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                        opacity = 1,
                        group = "Circles",
                        layerId = sdf[["FID"]])%>%
              leaflet::addLegend(
                position = "bottomright",
                pal = color_palette,                                # 图例位置
                values = sdf[[layer_to_plot]],
                title = layer_to_plot,                              # 图例标题
                labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                opacity = 1,                                         # 图例的不透明度
                layerId = "Circles"
              )%>%
              # 动态计算地图边界
              leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                        lat1 = min(coords_sdf[, 2]),
                        lng2 = max(coords_sdf[, 1]),
                        lat2 = max(coords_sdf[, 2]))

          # Significance 图
          if(input$mgwr_significance_show){
            shinyjs::show("mgwr_Significant_point_div")
            # 创建一个新的列名用于存储 p 值
            p_value_col <- paste0(layer_to_plot, "_p_value")

            # 假设sdf是一个SpatialPointsDataFrame，coords_sdf是其坐标矩阵
            sdf_df <- as.data.frame(sdf@data)
            # 将坐标添加到数据框中
            sdf_df$lng <- coords_sdf[, 1]
            sdf_df$lat <- coords_sdf[, 2]
            # 过滤数据
            p_values <- sdf_df %>% dplyr::pull(!!dplyr::sym(p_value_col))
            sdf_significant <- sdf_df[p_values < 0.05 & !is.na(p_values), ]
            sdf_not_significant <- sdf_df[p_values >= 0.05 & !is.na(p_values), ]
            # 为两组数据创建不同的图标
            icon_not_significant <-  leaflet::makeIcon(
                                      iconUrl = "https://s2.loli.net/2024/10/29/sih4RPljFwvVT5S.png",
                                      iconWidth = 15, iconHeight = 15
                                      # iconAnchorX = 0, iconAnchorY = 94
                                    )

            # 添加气泡图&图例
            map %>%
                leaflet::clearGroup("Significant") %>%
                leaflet::removeControl("Significant") %>%
                # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
                leaflet::addMarkers(data = sdf_not_significant,
                                  lng = ~lng, lat = ~lat,
                                  icon = icon_not_significant,
                                  group = "Significant")%>%
                # 动态计算地图边界
                leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                          lat1 = min(coords_sdf[, 2]),
                          lng2 = max(coords_sdf[, 1]),
                          lat2 = max(coords_sdf[, 2]))
          }
        } else if (layer_to_plot == "residual" || layer_to_plot == "CV_Score" ||
                  layer_to_plot == "Stud_residual" || layer_to_plot == "Local_R2") {
          shinyjs::hide("mgwr_Significant_point_div")
          shinyjs::hide("mgwr_Significant_line_div")
          # 添加气泡图&图例
          map %>%
              leaflet::clearGroup("Polygons") %>%
              leaflet::clearGroup("Circles") %>%
              leaflet::removeControl("Polygons") %>%
              leaflet::removeControl("Circles") %>%
              leaflet::clearGroup("Significant") %>%
              leaflet::removeControl("Significant") %>%
              # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
              leaflet::addCircles(
                        data = sdf,
                        weight = 3,
                        radius = point_radius,
                        color = ~color_palette(sdf[[layer_to_plot]]),
                        fillOpacity = 1,
                        lng = coords_sdf[, 1],
                        lat = coords_sdf[, 2],
                        # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                        opacity = 1,
                        group = "Circles",
                        layerId = sdf[["FID"]])%>%
              leaflet::addLegend(
                position = "bottomright",
                pal = color_palette,                                # 图例位置
                values = sdf[[layer_to_plot]],
                title = layer_to_plot,                              # 图例标题
                labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                opacity = 1,                                         # 图例的不透明度
                layerId = "Circles"
              )%>%
              # 动态计算地图边界
              leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                        lat1 = min(coords_sdf[, 2]),
                        lng2 = max(coords_sdf[, 1]),
                        lat2 = max(coords_sdf[, 2]))
        }
    }

  })
  # 清除显著性图层
  observeEvent(input$mgwr_significance_show,{
    # 获取底图map对象
    map <- leaflet::leafletProxy("mgwr_mapPlot", session)
    if(!is.null(map)){}
    else{print("map is null")}
    if(!input$mgwr_significance_show){
      shinyjs::hide("mgwr_Significant_line_div")
      shinyjs::hide("mgwr_Significant_point_div")
      # 清除所有图层
      map %>%
          leaflet::clearGroup("Significant") %>%
          leaflet::removeControl("Significant")
    }
    map
  })
  # 用 reactiveValues 存储用户点击的点和生成的短音频文件路径
  mgwr_rv <- reactiveValues(
    clicked_points = list(),  # 用于存放点击的经纬度（仅连线模式时使用）
    audio_files = character() # 存放每个点击生成的短音频文件路径
  )
  # 点击地图产生音频
  observeEvent(input$mgwr_mapPlot_click, {
    req(mgwr_result())
    click <- input$mgwr_mapPlot_click
    shp_data <- mgwr_shapefile_data() %>% 
        sf::st_as_sf() %>% 
        sf::st_make_valid() %>% 
        sf::st_set_crs(4326)
    geom_type <- sf::st_geometry_type(shp_data) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    # 选择模式：点击模式直接生成短音频
    if(input$mgwr_audio_mode == "click"){
      # 获取必要数据
      mgwr_sdf <- mgwr_result()$SDF
      layer_to_plot <- input$mgwr_map_layer
      
      # 这里假设已将 mgwr_sdf 转换为 sf 对象，或直接使用 spatial 包的 nearest feature 方法
      mgwr_sdf_sf <- sf::st_as_sf(mgwr_sdf)
      click_point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      selected_id <- sf::st_nearest_feature(click_point, mgwr_sdf_sf)
      selected_row <- mgwr_sdf[selected_id, ]
      
      # 生成 popup 内容
      popup_content <- paste("FID :", selected_row$FID, "<br>", 
                            layer_to_plot, ":", round(selected_row[[layer_to_plot]], 4))
      if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        # 找到包含点击点的 Polygon
        selected_feature <- shp_data[sf::st_contains(shp_data, click_point, sparse = FALSE), ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No polygon data found", type = "error")
          return(NULL)
        }

        leaflet::leafletProxy("mgwr_mapPlot") %>%
          leaflet::clearPopups() %>%
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          leaflet::addPolygons(
            data = selected_feature,
            group = "highlighted_features",
            fillColor = "yellow",
            color = "#FF0000",
            fillOpacity = 0,
            weight = 2,
            highlightOptions = leaflet::highlightOptions(
              weight = 5,
              color = "#FF0000",
              bringToFront = TRUE
              )
            )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
        # 找到最近的点
        selected_id <- sf::st_nearest_feature(click_point, shp_data)
        selected_feature <- shp_data[selected_id, ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No point data found", type = "error")
          return(NULL)
        }
        leaflet::leafletProxy("mgwr_mapPlot") %>%
          leaflet::clearPopups() %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::addCircleMarkers(
            data = selected_feature,
            group = "highlighted_features",
            color = "red",
            fillOpacity = 0,
            radius = 8,
            stroke = TRUE,
            weight = 2
          )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      } else {
        shiny::showNotification("The current geometry type is not supported", type = "error")
        return(NULL)
      }
      
      
      # 生成短音频文件
      coeff <- ifelse(is.na(selected_row[[layer_to_plot]]), 0, selected_row[[layer_to_plot]])
      short_filename <- paste0("www/mgwr_map_audio_", as.integer(Sys.time()), "_", sample(1:1000, 1), ".wav")
      generate_audio(value = coeff,
                    filename = short_filename,
                    x_min = min(mgwr_sdf[[layer_to_plot]], na.rm = TRUE),
                    x_max = max(mgwr_sdf[[layer_to_plot]], na.rm = TRUE))
      
      # 更新 audio 控件，播放短音频
      shinyjs::runjs(sprintf("document.getElementById('mgwr_map_audio').src='%s'; document.getElementById('mgwr_map_audio').play();", short_filename))
      
    } else if(input$mgwr_audio_mode == "line") {
      # 连线模式：记录点击点，不直接生成音频
      mgwr_rv$clicked_points <- append(mgwr_rv$clicked_points, list(c(click$lng, click$lat)))
      # 在地图上添加标记以便确认顺序
      leaflet::leafletProxy("mgwr_mapPlot") %>%
        leaflet::addMarkers(
                  lng = click$lng, 
                  lat = click$lat,
                  popup = paste("point", length(mgwr_rv$clicked_points)),
                  group = "point",
                  icon = musicIcon
                  )
    }
  })
  # 处理连线模式下的确认按钮：生成长音频
  observeEvent(input$mgwr_confirm_audio, {
    req(length(mgwr_rv$clicked_points) > 0)
    # 仅在连线模式时触发检查
    if (input$mgwr_audio_mode == "line") {
      # 检查点击点数量是否足够
      if (length(mgwr_rv$clicked_points) < 2) {
        shiny::showNotification("Choose at least two points", type = "error")
        return() # 不满足条件时提前退出
      }
    }

    mgwr_buffer_length <- as.numeric(input$mgwr_buffer_length)
    layer_to_plot <- input$mgwr_map_layer

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing mgwr Audio...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "mgwr_audio_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 1. 获取用户点击的坐标并绘制连线
    clicked_matrix <- do.call(rbind, mgwr_rv$clicked_points)
    leaflet::leafletProxy("mgwr_mapPlot") %>%
      leaflet::addPolylines(
                  lng = clicked_matrix[,1], 
                  lat = clicked_matrix[,2],
                  color = "red",
                  weight = mgwr_buffer_length,
                  opacity = 0.8,
                  group = "point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除之前的高亮
    
    # 2. 创建 sf 线对象并确保坐标系统一
    clicked_line <- sf::st_linestring(clicked_matrix) %>%
      sf::st_sfc(crs = 4326) %>%
      sf::st_make_valid()
    line_buffer <- sf::st_buffer(clicked_line, dist = mgwr_buffer_length)  # 可调整缓冲区大小
  
    # 3. 处理空间数据
    mgwr_sdf <- mgwr_result()$SDF
    shp_data <- mgwr_shapefile_data() %>% 
      sf::st_as_sf() %>% 
      sf::st_make_valid() %>% 
      sf::st_set_crs(4326)

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_audio_progress", value = 10)

    # 4. 转换Spatial对象为sf
    mgwr_sdf_sf <- sf::st_as_sf(mgwr_sdf) 
    mgwr_sdf_sf <- mgwr_sdf_sf %>%
      dplyr::rename_with(~paste0("mgwr_", .), .cols = -geometry)  
    
    # 将mgwr计算结果合并到原始shp属性（假设行顺序一致）
    if(nrow(shp_data) == nrow(mgwr_sdf_sf)) {
      shp_data_with_mgwr <- shp_data %>%
        dplyr::bind_cols(sf::st_drop_geometry(mgwr_sdf_sf)) %>%  # 使用正确的属性数据
        sf::st_sf()
    } else {
      shiny::showNotification("The number of data rows is inconsistent", type = "error")
      shiny::removeModal()
      return()
    }

    layer_name <- paste0("mgwr_",layer_to_plot)

    geom_type <- sf::st_geometry_type(shp_data_with_mgwr) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      # 精确相交检测
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_mgwr, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      
      intersect_features <- intersects
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_mgwr), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = mgwr_buffer_length, add = TRUE)

      leaflet::leafletProxy("mgwr_mapPlot") %>%
        leaflet::addPolygons(
          data = intersects,
          group = "highlighted_features",
          color = "#FF0000",
          fillOpacity = 0,
          weight = 2,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#FF0000",
            bringToFront = TRUE
            )
          )
    }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
      # 点处理逻辑
      # intersect_idx  <- st_intersects(mgwr_sdf_sf, line_buffer, sparse = FALSE)
      # intersect_features <- mgwr_sdf_sf[which(intersect_idx, arr.ind = TRUE), ]
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_mgwr, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      intersect_features <- intersects

      if(nrow(intersect_features) == 0) {
        shiny::showNotification("The line does not pass through any features", type = "warning")
        shiny::removeModal()
        return()
      }
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_mgwr), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = mgwr_buffer_length, add = TRUE)

      leaflet::leafletProxy("mgwr_mapPlot") %>%
        leaflet::addCircleMarkers(
          data = intersect_features,
          group = "highlighted_features",
          color = "red",
          fillOpacity = 0,
          radius = 8,
          stroke = TRUE,
          weight = 2
        )

    } else {
      shiny::showNotification("The current geometry type is not supported", type = "error")
      shiny::removeModal()
      return()
    }
    
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_audio_progress", value = 20)

    # 6. 沿路径排序（确保交点顺序与点击点一致）
    if (nrow(intersect_features) > 0) {
      start_point <- sf::st_sfc(sf::st_point(clicked_matrix[1, ]), crs = 4326)
      intersect_features <- intersect_features %>%
        dplyr::mutate(
          dist_to_start = as.numeric(sf::st_distance(geometry, start_point))
        ) %>%
        dplyr::arrange(dist_to_start)
    }

    # 7. 生成音频
    mgwr_rv$audio_files <- character()  # 重置音频存储

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "mgwr_audio_progress", value = 30 )

    # 8. 遍历要素，生成音频
    for (i in seq_len(nrow(intersect_features))) {
      feature <- intersect_features[i, ]
      coeff <- ifelse(is.na(feature[[layer_name]]), 0, feature[[layer_name]])
      short_filename <- paste0("www/mgwr_map_audio_", i, ".wav")

      generate_audio(
        value = coeff,
        filename = short_filename,
        x_min = min(shp_data_with_mgwr[[layer_name]], na.rm = TRUE),
        x_max = max(shp_data_with_mgwr[[layer_name]], na.rm = TRUE)
      )

      mgwr_rv$audio_files <- c(mgwr_rv$audio_files, short_filename)

      # 更新进度条
      shinyWidgets::updateProgressBar(session = session, id = "mgwr_audio_progress", value = 30 + (i/nrow(intersect_features)) * 30)
    }
    
    # 9. 合成所有短音频文件为一个长音频
    composite_filename <- paste0("www/mgwr_map_audio_composite.wav")
    concatenate_audio(mgwr_rv$audio_files, composite_filename)
    # 确保 FFmpeg 使用合成后的音频
    sound_road <- composite_filename  # 这里修正

    # 提取系数数据
    ranges <- seq_len(nrow(intersect_features))  # 用于 X 轴
    coeff_values <- intersect_features[[layer_name]]  # 提取回归系数
    ranges_length <- length(ranges)
    # 生成初始曲线图
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = coeff_values), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  
      ) +
      ggplot2::ggtitle("Coefficient Variation Over Features") +
      ggplot2::xlab("Feature Index") + 
      ggplot2::ylab("Coefficient Value")

    # 逐帧生成视频
    waveform_images <- c()
    for (i in seq_along(ranges)) {
      frame_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = ranges[i], color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Feature:", i, "  Coefficient:", round(coeff_values[i], 4)))

      frame_image <- file.path(temp_dir, paste0("mgwr_map_waveform_", i, ".png"))
      ggplot2::ggsave(frame_image, frame_plot, width = 6, height = 4, dpi = 150)
      waveform_images <- c(waveform_images, frame_image)

      shinyWidgets::updateProgressBar(session, "mgwr_video_progress", value = 60 + (i / ranges_length) * 30)
    }

    # 计算音频时长
    audio_info <- tuneR::readWave(composite_filename)
    total_frames <- length(waveform_images)
    total_audio_duration <- length(audio_info@left) / audio_info@samp.rate
    # 计算帧率，使得视频时长与音频一致
    frame_rate <- total_frames / total_audio_duration
    # 确保 framerate 合理（避免异常）
    if (frame_rate < 1) frame_rate <- 1
    if (frame_rate > 30) frame_rate <- 30  # 限制在 1-30 fps，防止帧率过大或过小
    # 生成视频
    waveform_video <- file.path(temp_dir, "mgwr_map_waveform_video.mp4")
    av::av_encode_video(
      input = waveform_images,
      output = waveform_video,
      framerate = frame_rate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",
      audio = NULL,
      verbose = TRUE
    )

    # 合成音视频
    sound_video <- file.path("www", "mgwr_map_sound_video.mp4")
    # sound_road <- mgwr_rv$audio_files  # 获取之前生成的音频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", 
      "-i", shQuote(waveform_video),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " 合成波形图和音频完成，文件保存为: ", sound_video, "\n")

    # 在地图左下角显示视频
    leaflet::leafletProxy("mgwr_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::addControl(
        HTML(sprintf(
          '<video id="mgwr_video_player" width="300" autoplay controls>
            <source src="%s" type="video/mp4">
            Your browser does not support the video tag.
          </video>
          <audio id="mgwr_audio" src="%s"></audio>
          <script>
            document.getElementById("mgwr_video_player").onplay = function() {
              var audio = document.getElementById("mgwr_audio");
              if (audio) {
                audio.play();
              }
            };
            document.getElementById("mgwr_video_player").onpause = function() {
              var audio = document.getElementById("mgwr_audio");
              if (audio) {
                audio.pause();
              }
            };
            document.getElementById("mgwr_video_player").ontimeupdate = function() {
              var audio = document.getElementById("mgwr_audio");
              if (audio) {
                audio.currentTime = this.currentTime;
              }
            };
          </script>', sound_video, composite_filename
        )),
        position = "bottomleft",
        layerId ="map_video_control"
      )

    # 设置音频源
    # shinyjs::runjs(sprintf("document.getElementById('mgwr_map_audio').src='%s';", composite_filename))

    # 重置记录，便于下次操作
    mgwr_rv$clicked_points <- list()
    mgwr_rv$audio_files <- character()

    shinyWidgets::updateProgressBar(session = session, id = "mgwr_audio_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()
  })
  # 处理连线的图标
  observeEvent(input$mgwr_confirm_audio_clear, {
    # 重置记录，便于下次操作
    mgwr_rv$clicked_points <- list()
    mgwr_rv$audio_files <- character()

    leaflet::leafletProxy("mgwr_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::clearGroup("point") %>%
      leaflet::clearGroup("point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除高亮
  })
  #--------------------------# Multiple Visualizations #-------------------------#
  # # 动态生成图片
  # observe({
  #   req(input$mgwr_columns)  # 确保用户已经选择了变量
  #   req(input$mgwr_color_palette)  # 确保用户已经选择了色阶
  #   req(mgwr_result())
  #   req(input$mgwr_independentVars)

  #   shp_data <- mgwr_shapefile_data()
  #   if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}

  #   # 获取带宽对应名称
  #   Vars <- c("Intercept", input$mgwr_independentVars)

  #   # 获取 mgwr 结果
  #   mgwr_result <- mgwr_result()
  #   sdf <- mgwr_result$SDF
  #   print(sdf)

  #   # 获取带宽逻辑修改
  #   if(input$mgwr_best_bandwidth) {
  #     # 从模型结果中获取自动选择的带宽
  #     optimal_bws <- mgwr_result()$optimal_bws
  #     # 创建带宽命名向量
  #     bw_vector <- setNames(optimal_bws, Vars)
  #   }

  #   # 遍历用户选择的列并动态生成图片
  #   selected_columns <- input$mgwr_columns
  #   for (col in selected_columns) {
  #     local({
  #       column_name <- col  # 需要使用 local() 避免循环中的闭包问题
  #       # 动态创建 UI 输出控件
  #       output[[paste0("plot_", column_name)]] <- renderPlot({
  #         req(column_name %in% names(sdf))

  #         # 修改带宽获取逻辑
  #         if(input$mgwr_best_bandwidth) {
  #           # 从自动选择的带宽向量中获取对应值
  #           bw <- round(as.numeric(bw_vector[column_name]),4)
  #         } else {
  #           # 保持原有手动获取逻辑
  #           bw <- sapply(column_name, function(x) {
  #             as.numeric(input[[paste0("bw_", x)]])
  #           })
  #         } 

  #         # 创建专题图
  #         target_data <- shp_data
  #         target_data$value <- sdf[[column_name]]  # 将数据加入到 sf 对象中，以便于绘制专题图

  #         # 判断数据类型，动态选择绘图样式
  #         geom_type <- unique(sf::st_geometry_type(target_data))

  #         # 根据选择的色阶动态设置颜色
  #         gradient <- color_gradients[[input$mgwr_color_palette]]

  #         if ("POINT" %in% geom_type || "MULTIPOINT" %in% geom_type) {  # 点数据：使用 color 映射
  #             if (input$mgwr_color_palette == "viridis") {
  #               ggplot2::ggplot(target_data) +
  #                 ggplot2::geom_sf(aes(color = value)) +
  #                 ggplot2::scale_color_viridis_c() +  # 使用 Viridis 色阶
  #                 ggplot2::labs(
  #                   title = if (column_name %in% Vars) {
  #                     paste0(column_name, "  (Bandwidth: ", bw, ")")
  #                   } else {
  #                     column_name
  #                   }
  #                 ) +
  #                 ggplot2::theme(
  #                   panel.background = ggplot2::element_rect(fill = "white", color = NA),
  #                   panel.grid = ggplot2::element_blank(),
  #                   plot.background = ggplot2::element_rect(fill = "white", color = NA)
  #                 )
  #             } else {
  #               ggplot2::ggplot(target_data) +
  #                 ggplot2::geom_sf(aes(color = value)) +
  #                 ggplot2::scale_color_gradient(low = gradient["low"], high = gradient["high"]) +
  #                 ggplot2::labs(
  #                   title = if (column_name %in% Vars) {
  #                     paste0(column_name, "  (Bandwidth: ", bw, ")")
  #                   } else {
  #                     column_name
  #                   }
  #                 ) +
  #                 ggplot2::theme(
  #                   panel.background = ggplot2::element_rect(fill = "white", color = NA),
  #                   panel.grid = ggplot2::element_blank(),
  #                   plot.background = ggplot2::element_rect(fill = "white", color = NA)
  #                 )
  #             }
  #         } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {  # 面数据：使用 fill 映射
  #           if (input$mgwr_color_palette == "viridis") {
  #             ggplot2::ggplot(target_data) +
  #               ggplot2::geom_sf(aes(fill = value)) +
  #               ggplot2::scale_fill_viridis_c() +  # 使用 Viridis 色阶
  #               ggplot2::labs(
  #                   title = if (column_name %in% Vars) {
  #                     paste0(column_name, "  (Bandwidth: ", bw, ")")
  #                   } else {
  #                     column_name
  #                   }
  #                 ) +
  #               ggplot2::theme(
  #                 panel.background = ggplot2::element_rect(fill = "white", color = NA),
  #                 panel.grid = ggplot2::element_blank(),
  #                 plot.background = ggplot2::element_rect(fill = "white", color = NA)
  #               )
  #           } else {
  #             ggplot2::ggplot(target_data) +
  #               ggplot2::geom_sf(aes(fill = value)) +
  #               ggplot2::scale_fill_gradient(low = gradient["low"], high = gradient["high"]) +
  #               ggplot2::labs(
  #                   title = if (column_name %in% Vars) {
  #                     paste0(column_name, "  (Bandwidth: ", bw, ")")
  #                   } else {
  #                     column_name
  #                   }
  #                 ) +
  #               ggplot2::theme(
  #                 panel.background = ggplot2::element_rect(fill = "white", color = NA),
  #                 panel.grid = ggplot2::element_blank(),
  #                 plot.background = ggplot2::element_rect(fill = "white", color = NA)
  #               )
  #           }
  #         } else {
  #           # 如果既不是点也不是面，抛出警告
  #           stop("Unsupported geometry type for plotting.")
  #         }

  #       })
  #     })
  #   }
  # })
  # # 动态显示图片
  # output$mgwr_Plot <- renderUI({
  #   req(input$mgwr_columns)  # 确保用户已经选择了变量

  #   # 每行的图片数量
  #   images_per_row <- 3  # 每行最多显示 3 张图片
  #   plot_outputs <- lapply(seq_along(input$mgwr_columns), function(i) {
  #     column_name <- input$mgwr_columns[i]

  #     # 使用 column 动态调整宽度，每行最多显示 images_per_row 张图片
  #     column(
  #       width = 12 / images_per_row,  # 动态设置列宽（12 栅格系统）
  #       plotOutput(outputId = paste0("plot_", column_name), height = "400px", width = "100%")
  #     )
  #   })

  #   # 将图片控件布局在 fluidRow 中
  #   do.call(fluidRow, plot_outputs)
  # })
  observe({
    req(input$mgwr_color_palette)  # 确保用户已经选择了色阶
    req(mgwr_result())
    req(input$mgwr_independentVars)

    # 获取带宽对应名称
    Vars <- c("Intercept", input$mgwr_independentVars)
    # 获取 mgwr 结果
    mgwr_result <- mgwr_result()
    sdf <- mgwr_result$SDF
    print(sdf)

    shp_data <- mgwr_shapefile_data()
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}

    # 获取 mgwr 结果
    mgwr_result <- mgwr_result()
    sdf <- mgwr_result$SDF
    req(input$mgwr_columns)  # 确保用户已经选择了变量

    # 获取带宽逻辑修改
    if(input$mgwr_best_bandwidth) {
      # 从模型结果中获取自动选择的带宽
      optimal_bws <- mgwr_result()$optimal_bws
      # 创建带宽命名向量
      bw_vector <- setNames(optimal_bws, Vars)
    }

    # 设置高分辨率参数
    dpi <- 100  # 设置DPI值
    width_px <- 1500  # 设置宽度像素
    height_px <- 1500 # 设置高度像素

    # 遍历用户选择的列并动态生成图片
    selected_columns <- input$mgwr_columns
    for (col in selected_columns) {
      local({
        column_name <- col  # 需要使用 local() 避免循环中的闭包问题
        # 动态创建 UI 输出控件
        output[[paste0("plot_", column_name)]] <- renderPlot({

          # 确保列存在于数据框中
          req(column_name %in% names(sdf))
          # 修改带宽获取逻辑
          if(input$mgwr_best_bandwidth) {
            # 从自动选择的带宽向量中获取对应值
            bw <- round(as.numeric(bw_vector[column_name]),4)
          } else {
            # 保持原有手动获取逻辑
            bw <- sapply(column_name, function(x) {
              as.numeric(input[[paste0("bw_", x)]])
            })
          } 

          # 创建专题图
          target_data <- shp_data
          target_data$value <- sdf[[column_name]]  # 将数据加入到 sf 对象中，以便于绘制专题图

          # 判断数据类型，动态选择绘图样式
          geom_type <- unique(sf::st_geometry_type(target_data))
          # 根据选择的色阶动态设置颜色
          gradient <- color_gradients[[input$mgwr_color_palette]]

          if ("POINT" %in% geom_type || "MULTIPOINT" %in% geom_type) {  # 点数据：使用 color 映射
              if (input$mgwr_color_palette == "viridis") {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_viridis_c() +  # 使用 Viridis 色阶
                  ggplot2::labs(
                    title = if (column_name %in% Vars) {
                      paste0(column_name, "  (Bandwidth: ", bw, ")")
                    } else {
                      column_name
                    }
                  ) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              } else {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_gradient(low = gradient["low"], high = gradient["high"]) +
                  ggplot2::labs(
                    title = if (column_name %in% Vars) {
                      paste0(column_name, "  (Bandwidth: ", bw, ")")
                    } else {
                      column_name
                    }
                  ) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              }
          } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {  # 面数据：使用 fill 映射
            if (input$mgwr_color_palette == "viridis") {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_viridis_c() +  # 使用 Viridis 色阶
                ggplot2::labs(
                    title = if (column_name %in% Vars) {
                      paste0(column_name, "  (Bandwidth: ", bw, ")")
                    } else {
                      column_name
                    }
                  ) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            } else {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_gradient(low = gradient["low"], high = gradient["high"]) +
                ggplot2::labs(
                    title = if (column_name %in% Vars) {
                      paste0(column_name, "  (Bandwidth: ", bw, ")")
                    } else {
                      column_name
                    }
                  ) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }
          } else {
            # 如果既不是点也不是面，抛出警告
            shiny::showNotification("Unsupported geometry type for plotting.", type = "error")
            shiny::removeModal()
            return()
          }        
        })
      })
    }

  })
  # 动态显示图片
  output$mgwr_Plot <- renderUI({
    req(input$mgwr_columns)  # 确保用户已经选择了变量

    # 每行的图片数量
    images_per_row <- min(2, length(input$mgwr_columns))  # 每行最多显示 2 张图片

    plot_outputs <- lapply(seq_along(input$mgwr_columns), function(i) {
      column_name <- input$mgwr_columns[i]

      # 使用 column 动态调整宽度，每行最多显示 images_per_row 张图片
      column(
        width = 12 / images_per_row,  # 动态设置列宽（12 栅格系统）
        plotOutput(outputId = paste0("plot_", column_name), height = "500px", width = "100%")
      )
    })

    # 将图片控件布局在 fluidRow 中
    do.call(fluidRow, plot_outputs)
  })

  #--------------------------# GWPCA #-------------------------#
  # 按钮跳转
  observeEvent(input$gwpca_execute, {
    updateTabsetPanel(session, "gwpca_tabs", selected = "Summary Information")
  })
  # shp读取函数
  gwpca_shapefile_data <- reactive({
    req(input$gwpca_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$gwpca_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwpca_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwpca_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      temp_dir <- tempdir()
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch(
          {
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },
          error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # cp_shp读取函数
  gwpca_cp_shapefile_data <- reactive({
    req(input$gwpca_cp_shapefile)  # 确保用户已上传文件

    # 获取上传文件的路径和扩展名
    file_path <- input$gwpca_cp_shapefile$datapath
    file_ext <- tolower(tools::file_ext(input$gwpca_cp_shapefile$name))  # 获取文件扩展名，转为小写
    file_name <- tools::file_path_sans_ext(input$gwpca_cp_shapefile$name)

    # 设置 GDAL 配置选项
    Sys.setenv(SHAPE_RESTORE_SHX = "YES")

    temp_dir <- tempdir()

    if (file_ext == "zip") {
      # 如果是 ZIP 文件，解压缩并查找 .shp 文件
      unzip(file_path, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.shp$"), full.names = TRUE)
      dbf_files <- list.files(temp_dir, pattern = paste0("^", file_name, "\\.dbf$"), full.names = TRUE)

      if (length(shp_files) > 0 && length(dbf_files) > 0) {
        shp_file <- shp_files[1]
        dbf_file <- sub("\\.shp$", ".dbf", shp_file)
        shp_data <- tryCatch({
            sf::st_read(shp_file, options = "ENCODING=GBK")
          },error = function(e) {
            shiny::showNotification("Error reading Shapefile from ZIP. Please check the file content.", type = "error")
            return(NULL)
          }
        )
        # 处理 CRS（避免警告）
        if (!is.null(sf::st_crs(shp_data))) {
          shp_data <- sf::st_transform(shp_data, 4326)  # 如果已有 CRS，重新投影
        } else {
          sf::st_crs(shp_data) <- 4326  # 如果没有 CRS，先赋值再投影
          shp_data <- sf::st_transform(shp_data, 4326)
        }
        return(shp_data)
      } else {
        shiny::showNotification("No Shapefile found in the ZIP file.", type = "error")
        return(NULL)
      }
    } else {
      # 如果上传的文件既不是 .shp 也不是 .zip，显示错误通知
      shiny::showNotification("Unsupported file type. Please upload .zip file.", type = "error")
      return(NULL)
    }
  })
  # gwpca结果对象
  gwpca_bw_data <- reactiveValues(
    bw = NULL,          
  )
  # 更新自变量&图层选项
  observeEvent(gwpca_shapefile_data(), {
    # 获取变量名并按首字母排序（不区分大小写）
    var_names <- names(gwpca_shapefile_data())
    
    # 检查变量名是否为空
    if (length(var_names) == 0) {
      sorted_vars <- character(0)  # 空字符向量
    } else {
      # 不区分大小写排序
      sorted_vars <- var_names[order(tolower(var_names))]
    }

    # 确保默认选中项有效（至少有两个变量时才选中前两个）
    selected_vars <- if (length(sorted_vars) >= 2) {
      sorted_vars[1:2]
    } else {
      sorted_vars  # 不足两个时选中所有可用变量
    }

    updateSelectInput(session, "gwpca_vars", choices = sorted_vars, selected = selected_vars)
  })
  # 输出自变量选择控件
  output$gwpca_vars <- renderUI({
    selectInput("gwpca_vars", "Choose at least two variables", choices = NULL, multiple = TRUE, selected = NULL)
  })
  # 更新带宽滑块
  observe({
      req(input$gwpca_shapefile)  # 确保文件已上传
      shp_data <- gwpca_shapefile_data()  # 读取 shapefile 数据
      shp_data <- sf::st_make_valid(shp_data)      # 确保数据有效
      req(shp_data)

      # 计算最大距离（米），确保单位正确
      if (sf::st_is_longlat(shp_data)) {
          max_distance <- as.integer(max(sf::st_distance(shp_data)))  # 计算球面距离
      } else {
          coords <- sf::st_coordinates(shp_data)
          max_distance <- as.integer(max(dist(coords)))  # 计算欧式距离
      }

      # 读取是否为自适应带宽
      gwpca_adaptive <- as.logical(input$gwpca_adaptive)

      # 设置带宽的最大值和单位
      if (gwpca_adaptive) {
          max_bw <- nrow(shp_data)  # 以点的个数为单位
          label <- "Bandwidth (Number of points)"
      } else {
          max_bw <- max_distance  # 以最大距离为单位
          label <- "Bandwidth (meter)"
      }

      # 更新滑块
      updateSliderInput(session, "gwpca_bandwidth",
                        max = max_bw,
                        label = label)
  })
    # 带宽滑块显示隐藏
  observeEvent(input$gwpca_best_bandwidth, {
    if (input$gwpca_best_bandwidth) {
      shinyjs::hide("gwpca_bandwidth_div")  # 隐藏带宽滑块
    } else {
      shinyjs::show("gwpca_bandwidth_div")  # 显示带宽滑块
    }
  })
  #--------------------------# GWPCA Analysis #--------------------------#
  # 运行GWPCA分析
  gwpca_result <- eventReactive(input$gwpca_execute, {
    # 保证参数存在
    req(input$gwpca_shapefile)
    req(input$gwpca_vars)
    req(input$gwpca_kernel, input$gwpca_adaptive)
    req(input$gwpca_k)

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing GWPCA Analysis...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwpca_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 更新进度条10%
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_progress", value = 10)

    # 获取实际的数据和坐标
    gwpca_shp_data <- gwpca_shapefile_data()    
    # 获取用户选定的变量
    gwpca_vars <- as.character(input$gwpca_vars)
    # 获取选项
    gwpca_kernel <- as.character(input$gwpca_kernel)
    gwpca_adaptive <- as.logical(input$gwpca_adaptive)
    gwpca_k <- as.integer(input$gwpca_k)

    # 检查数据是否为 sf 对象
    if (!inherits(gwpca_shp_data, "sf")) {
      shiny::showNotification("Unsupported data types. Input is not an sf object.", type = "error", duration = NULL)
      return()
    }
    # 修复可能的无效几何数据
    gwpca_shp_data <- sf::st_make_valid(gwpca_shp_data)
    spdf <- as(gwpca_shp_data, "Spatial")

    # 更新进度条30%
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_progress", value = 30)

    # 初始化 dp.locat
    dp.locat <- NULL
    # 提取坐标点
    tryCatch({
      geometry_type <- unique(sf::st_geometry_type(gwpca_shp_data))  # 获取所有几何类型
      if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
        centroids <- sf::st_centroid(sf::st_geometry(gwpca_shp_data))  # 仅计算几何的质心
        coords <- sf::st_coordinates(centroids)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
        coords <- sf::st_coordinates(gwpca_shp_data)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else if ("LINESTRING" %in% geometry_type) {
        # 如果是线状几何，可以取中点（或其他逻辑）
        midpoints <- sf::st_line_sample(sf::st_geometry(gwpca_shp_data), n = 1)
        coords <- sf::st_coordinates(midpoints)
        dp.locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
      } else {
        shiny::showNotification(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")), type = "error")
        shiny::removeModal()
        return()
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })
    # 检查 dp.locat 是否为空
    if (is.null(dp.locat)) {
      shiny::showNotification("Coordinates could not be extracted from the shapefile.", type = "error", duration = NULL)
      return()
    }
    dp.locat <- na.omit(dp.locat)

    # 转换为 SpatialPointsDataFrame
    spdf <- tryCatch({
      sp::SpatialPointsDataFrame(
        coords = dp.locat,
        data = as.data.frame(gwpca_shp_data),  # 转换 sf 对象为 data.frame
        proj4string = sp::CRS("+proj=longlat +datum=WGS84")
      )
    }, error = function(e) {
      shiny::showNotification(paste("Error creating SpatialPointsDataFrame:", e$message), type = "error", duration = NULL)
      shiny::removeModal()
      return(NULL)
    })

    # 检查并生成 FID 列
    if (!"FID" %in% names(gwpca_shp_data)) {
      dp.n <- nrow(dp.locat)
      gwpca_shp_data$FID <- seq_len(dp.n)
    }

    # 选择带宽
    # if (input$gwpca_best_bandwidth) {
    #   bandwidth <- GWmodel::bw.gwpca(data = spdf, vars = gwpca_vars ,k = gwpca_k) # 软件计算带宽
    # } else {
    #   bandwidth <- as.numeric(input$gwpca_bandwidth) # 用户输入带宽
    # }
    bandwidth <- as.numeric(input$gwpca_bandwidth) # 用户输入带宽
    gwpca_bw_data$bw <- bandwidth

    # 更新进度条到 50%
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_progress", value = 50)

    # gwpca_cp_shp_data <- NULL
    # sp_cp_locat <- NULL
    # cp_locat <- NULL
    # # 选择regression_points
    # if(!is.null(input$gwpca_cp_shapefile)){
    #   gwpca_cp_shp_data <- gwpca_cp_shapefile_data()
    #   # 修复无效几何数据
    #   if (!is.null(gwpca_cp_shp_data)) {
    #     gwpca_cp_shp_data <- sf::st_make_valid(gwpca_cp_shp_data)
    #   } else {
    #     shiny::showNotification("Invalid shapefile data.", type = "error", duration = NULL)
    #     shiny::removeModal()
    #     return(NULL)
    #   }
      
    #   # 提取坐标点
    #   tryCatch({
    #     geometry_type <- unique(sf::st_geometry_type(gwpca_cp_shp_data))  # 获取所有几何类型
    #     if ("MULTIPOLYGON" %in% geometry_type || "POLYGON" %in% geometry_type) {
    #       centroids <- sf::st_centroid(sf::st_geometry(gwpca_cp_shp_data))  # 仅计算几何的质心
    #       coords <- sf::st_coordinates(centroids)
    #       cp_locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
    #     } else if ("POINT" %in% geometry_type || "MULTIPOINT" %in% geometry_type) {
    #       coords <- sf::st_coordinates(gwpca_cp_shp_data)
    #       cp_locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
    #     } else if ("LINESTRING" %in% geometry_type) {
    #       # 如果是线状几何，可以取中点（或其他逻辑）
    #       midpoints <- sf::st_line_sample(sf::st_geometry(gwpca_cp_shp_data), n = 1)
    #       coords <- sf::st_coordinates(midpoints)
    #       cp_locat <- data.frame(longitude = coords[, 1], latitude = coords[, 2])
    #     } else {
    #       stop(paste("Unsupported geometry type(s):", paste(geometry_type, collapse = ", ")))
    #     }
    #   }, error = function(e) {
    #     shiny::showNotification(paste("Error extracting coordinates:", e$message), type = "error", duration = NULL)
    #     shiny::removeModal()
    #     return(NULL)
    #   })
    # }
    # cp_locat <- na.omit(cp_locat)
    # # 检查 cp_locat 是否包含两列
    # if (ncol(cp_locat) != 2) {
    #   stop("cp_locat must contain exactly two columns (longitude and latitude).")
    # }
    # # 确保列名正确（经度和纬度）
    # colnames(cp_locat) <- c("longitude", "latitude")
    # # 处理 cp_locat 为 NULL 的情况
    # if (!is.null(cp_locat) && nrow(cp_locat) > 0) {
    #   sp_cp_locat <- SpatialPoints(cp_locat, proj4string = CRS("+proj=longlat +datum=WGS84"))
    # } else {
    #   shiny::showNotification("Coordinate extraction failed, cp_locat is NULL or empty.", type = "error", duration = NULL)
    #   shiny::removeModal()
    #   return(NULL)
    # }

    # 更新进度条到 60%
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_progress", value = 60)
    # 执行GWPCA模型
    model <- tryCatch({
              GWmodel::gwpca(data = spdf, vars = gwpca_vars, k = gwpca_k, bw = bandwidth, kernel = gwpca_kernel, adaptive = gwpca_adaptive)
            }, error = function(e) {
              shiny::showNotification(paste("Error running GWPCA:", e$message), type = "error", duration = NULL)
              shiny::removeModal()
              return(NULL)
            })

    # 模型成功后检查FID列
    if (!is.null(model)) {
      # 获取空间数据框架
      sdf <- model$SDF
      # 检查是否存在FID列
      if (!"FID" %in% names(sdf)) {
        # 获取数据点数
        dp.n <- nrow(sdf@data)
        # sdf@data$FID <- seq_len(dp.n)
        sdf$FID <- seq_len(dp.n)
        # 更新模型结果
        model$SDF <- sdf
      }
    }
    # if(is.null(input$gwpca_cp_shapefile)){
    #   # 执行GWPCA模型
    #   model <- tryCatch({
    #             GWmodel::gwpca(data = spdf, vars = gwpca_vars, k = gwpca_k, bw = bandwidth, kernel = gwpca_kernel, adaptive = gwpca_adaptive)
    #           }, error = function(e) {
    #             shiny::showNotification(paste("Error running GWPCA:", e$message), type = "error", duration = NULL)
    #             shiny::removeModal()
    #             return(NULL)
    #           })
    # }else{
    #   model <- tryCatch({
    #             GWmodel::gwpca(data = spdf, elocat = sp_cp_locat ,vars = gwpca_vars, k = gwpca_k, bw = bandwidth, kernel = gwpca_kernel, adaptive = gwpca_adaptive)
    #           }, error = function(e) {
    #             shiny::showNotification(paste("Error running GWPCA with elocat:", e$message), type = "error", duration = NULL)
    #             shiny::removeModal()
    #             return(NULL)
    #           })
    # }

    # 函数执行完成后，关闭等待窗口
    # 更新进度条到 100%
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_progress", value = 100)
    Sys.sleep(1)  # 等待一秒，确保用户看到完成的进度条
    shiny::removeModal()

    return(model)
  })
  # 更新图层选择控件
  observeEvent(gwpca_result(), {
    req(gwpca_result())

    # 获取 GWPCA 结果
    gwpca_result <- gwpca_result()
    sdf <- gwpca_result$SDF

    # 触发重新渲染 UI 控件
    output$gwpca_map_layer <- renderUI({
        # 提取图层名称
        layer_names <- names(sdf)
        layer_names <- layer_names[!grepl("FID", layer_names)]

        # 创建下拉框
        selectInput(
            inputId = "gwpca_map_layer",
            label = "Select Map Layer to Display",
            choices = layer_names,
            selected = layer_names[1]  # 默认选择第一个图层
        )
    })

    # 动态更新变量选择选项
    output$gwpca_columns <- renderUI({
      # 筛选列名
      names <- names(sdf)
      names <- names[!grepl("FID", names)]

      # 更新选择框的选项
      selectInput(
        inputId = "gwpca_columns",
        label = "Choose Variables",
        choices = names,
        multiple = TRUE,
        selected = names[1]  # 默认不选中任何选项
      )
    })

    # **自动触发地图更新，确保默认图层直接显示**
    observe({
      req(input$gwpca_map_layer)  #
      shinyjs::delay(1000, shinyjs::click("gwpca_map_layer_execute"))  
    })
  })
  #--------------------------# Table View #-------------------------#
  # 显示汇总表
  output$gwpca_summaryTable <- DT::renderDataTable({
    result <- gwpca_result()
    sdf <- as.data.frame(result$SDF)
    DT::datatable(
      sdf,
      extensions = 'Buttons',  # 关键要素1：声明使用扩展
      options = list(
        pageLength = 10,        # 默认每页显示10条
        lengthMenu = c(10, 15, 20, 25),  # 每页显示条目数选项
        searching = TRUE,       # 启用搜索框
        scrollX = TRUE,          # 启用水平滚动
        dom = 'Blfrtip',          # 控制界面元素布局
        buttons = list(       # 关键要素3：按钮嵌套配置
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'Export CSV'),
          list(extend = 'excel', text = 'Export Excel')
        )
      ),
      rownames = FALSE,         # 不显示行号
      class = "'display nowrap hover" 
    )
  })
  #--------------------------# Summary Information #-------------------------#
  # 显示模型诊断信息
  output$gwpca_modelDiagnostics <- renderPrint({
    gwpca_result()
  })
  #--------------------------# Web Map #-------------------------#
  # 地图可视化
  output$gwpca_mapPlot <- leaflet::renderLeaflet({
    map <- leaflet::leaflet() %>%
          leaflet::clearMarkers() %>%
          leaflet::addProviderTiles("CartoDB.Positron")

    # 返回最终的map对象
    map
  })
  # 更新地图上的图层
  observeEvent(input$gwpca_map_layer_execute, {
    req(gwpca_result())
    req(input$gwpca_map_layer)
    req(gwpca_shapefile_data())
    req(input$gwpca_color_palette)

    # 每更新一次图层清除音频文件
    prefixes_to_remove <- c("^gwpca_map_audio_",
                            "^gwpca_map_audio_composite_",
                            "^gwpca_map_waveform_",
                            "gwpca_map_sound_video.mp4")
    for (prefix in prefixes_to_remove) {
      files_to_remove <- list.files(temp_dir, pattern = prefix, full.names = TRUE)
      www_dir <- file.path(code_dir, "www")
      file_to_remove <- list.files(www_dir, pattern = prefix, full.names = TRUE)
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
      } else if (length(file_to_remove) > 0) {
        file.remove(file_to_remove)}
    }

    # 获取底图map对象
    map <- leaflet::leafletProxy("gwpca_mapPlot", session)
    if(!is.null(map)){}
    else{print("map is null")}
    # 获取result和sdf
    result <- gwpca_result()
    if(!is.null(result$SDF)){sdf <- result$SDF}else{print("result sdf is null")}

    # 获取sdf的坐标
    coords_sdf <- sp::coordinates(sdf)
    # 获取图层名称layer_to_plot
    layer_to_plot <- input$gwpca_map_layer
    # 获取shp数据
    shp_data <- gwpca_shapefile_data()

    library(leaflet.extras)
    library(RColorBrewer)

    # 输出图层选择控件
    if (inherits(shp_data, "sf")) {shp_data <- sf::st_make_valid(shp_data)}
    # 根据几何类型进行不同专题图显示
    if (sf::st_geometry_type(shp_data)[1] == "MULTIPOLYGON" || sf::st_geometry_type(shp_data)[1] == "POLYGON"){
      if(layer_to_plot == "win_var_PC1"){
        # 获取变量名称的唯一值
        unique_vars <- unique(sdf[[layer_to_plot]])
        
        # 动态设置颜色映射
        selected_palette <- input$gwpca_color_palette
        if (selected_palette == "viridis") {
          # 使用 viridis 调色板，颜色分配基于变量名称的索引
          color_mapping <- leaflet::colorFactor(palette = "viridis", domain = unique_vars)
        } else {
          # 使用自定义渐变调色板，动态生成颜色分配
          gradient <- color_gradients[[selected_palette]]
          
          # 为每个变量分配颜色，按渐变色从低到高
          gradient_palette <- colorRampPalette(c(gradient["low"], gradient["high"]))
          color_mapping <- leaflet::colorFactor(
            palette = gradient_palette(length(unique_vars)),  # 为每个唯一变量生成颜色
            domain = unique_vars
          )
        }

        map %>%
          leaflet::clearGroup("Polygons") %>%
          leaflet::clearGroup("Circles") %>%
          leaflet::removeControl("Polygons") %>%
          leaflet::removeControl("Circles") %>%
          leaflet::clearGroup("Significant") %>%
          leaflet::removeControl("Significant") %>%
          leaflet::addPolygons(
            data = shp_data,
            fillColor = ~color_mapping(sdf[[layer_to_plot]]),  # 根据变量名称映射颜色
            color = "black",
            fillOpacity = 0.7,  # 设置填充透明度
            weight = 0.3,
            opacity = 0.7,
            # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ", sdf[[layer_to_plot]]),  # 显示变量名称
            group = "Polygons",
            layerId = sdf[["FID"]]
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = color_mapping,  # 使用动态颜色映射
            values = unique_vars,  # 显示每个变量名称
            title = layer_to_plot,  # 设置图例标题为图层名称
            opacity = 1,
            layerId = "Polygons"
          ) %>%
          # 动态计算地图边界
          leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                    lat1 = min(coords_sdf[, 2]),
                    lng2 = max(coords_sdf[, 1]),
                    lat2 = max(coords_sdf[, 2]))
      } else{
        # 根据用户选择动态设置颜色映射
        selected_palette <- input$gwpca_color_palette
        if (selected_palette == "viridis") {
          color_palette <- leaflet::colorNumeric(palette = "viridis", domain = c(
            min(sdf[[layer_to_plot]], na.rm = TRUE),
            max(sdf[[layer_to_plot]], na.rm = TRUE)
          ))
        } else {
          gradient <- color_gradients[[selected_palette]]
          color_palette <- leaflet::colorNumeric(
            palette = c(gradient["low"], gradient["high"]),
            domain = c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                      max(sdf[[layer_to_plot]], na.rm = TRUE))
          )
        }
        map %>%
                leaflet::clearGroup("Polygons") %>%
                leaflet::clearGroup("Circles") %>%
                leaflet::removeControl("Polygons") %>%
                leaflet::removeControl("Circles") %>%
                leaflet::clearGroup("Significant") %>%
                leaflet::removeControl("Significant") %>%
                # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 9) %>%
                leaflet::addPolygons(
                  data = shp_data,
                  fillColor = ~color_palette(sdf[[layer_to_plot]]),
                  color = "black",
                  fillOpacity = 0.4,
                  weight = 0.3,
                  opacity = 0.5,
                  # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                  group = "Polygons",
                  layerId = sdf[["FID"]]
                )%>%
                leaflet::addLegend(
                  position = "bottomright",
                  pal = color_palette,                                # 图例位置
                  values = sdf[[layer_to_plot]],
                  title = layer_to_plot,                              # 图例标题
                  labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                  opacity = 1,                                         # 图例的不透明度
                  layerId = "Polygons"
                )%>%
                # 动态计算地图边界
                leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                          lat1 = min(coords_sdf[, 2]),
                          lng2 = max(coords_sdf[, 1]),
                          lat2 = max(coords_sdf[, 2]))
      }
    } else if (sf::st_geometry_type(shp_data)[1] == "POINT" || sf::st_geometry_type(shp_data)[1] == "MULTIPOINT"){
      if(layer_to_plot == "win_var_PC1"){
        # 获取变量名称的唯一值
        unique_vars <- unique(sdf[[layer_to_plot]])

        # 动态设置颜色映射
        selected_palette <- input$gwpca_color_palette
        if (selected_palette == "viridis") {
          # 使用 viridis 调色板
          color_palette <- leaflet::colorFactor(palette = "viridis", domain = unique_vars)
        } else {
          # 使用自定义渐变调色板
          gradient <- color_gradients[[selected_palette]]
          
          # 为每个变量分配颜色
          gradient_palette <- colorRampPalette(c(gradient["low"], gradient["high"]))
          color_palette <- leaflet::colorFactor(
            palette = gradient_palette(length(unique_vars)),  # 为每个唯一变量生成颜色
            domain = unique_vars
          )
        }

        # 绘制点图，使用变量颜色映射
        point_radius <- 50
        map %>%
          leaflet::clearGroup("Polygons") %>%
          leaflet::clearGroup("Circles") %>%
          leaflet::removeControl("Polygons") %>%
          leaflet::removeControl("Circles") %>%
          leaflet::clearGroup("Significant") %>%
          leaflet::removeControl("Significant") %>%
          leaflet::addCircles(
            data = sdf,
            weight = 3,
            radius = point_radius,
            color = ~color_palette(sdf[[layer_to_plot]]),  # 按变量分配颜色
            fillOpacity = 1,
            lng = coords_sdf[, 1],
            lat = coords_sdf[, 2],
            # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ", sdf[[layer_to_plot]]),
            opacity = 1,
            group = "Circles",
            layerId = sdf[["FID"]]
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = color_palette,                                # 图例
            values = unique_vars,
            title = layer_to_plot,                              # 图例标题
            labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
            opacity = 1,
            layerId = "Circles"
          ) %>%
          leaflet::fitBounds(
            lng1 = min(coords_sdf[, 1]),
            lat1 = min(coords_sdf[, 2]),
            lng2 = max(coords_sdf[, 1]),
            lat2 = max(coords_sdf[, 2])
          )
      } else {
        # 根据用户选择动态设置颜色映射
        domain <- c(min(sdf[[layer_to_plot]], na.rm = TRUE),
                    max(sdf[[layer_to_plot]], na.rm = TRUE))
        selected_palette <- input$gwpca_color_palette
        if (selected_palette == "viridis") {
          color_palette <- leaflet::colorNumeric(palette = "viridis", domain = domain)
        } else {
          colors <- color_gradients[[selected_palette]]
          color_palette <- leaflet::colorNumeric(palette = colors, domain = domain)
        }
        # 设置半径大小
        point_radius <- 50
        map %>%
              leaflet::clearGroup("Polygons") %>%
              leaflet::clearGroup("Circles") %>%
              leaflet::removeControl("Polygons") %>%
              leaflet::removeControl("Circles") %>%
              leaflet::clearGroup("Significant") %>%
              leaflet::removeControl("Significant") %>%
              # setView(lng = mean(coords_sdf[, 1]), lat = mean(coords_sdf[, 2]), zoom = 11) %>%
              leaflet::addCircles(
                        data = sdf,
                        weight = 3,
                        radius = point_radius,
                        color = ~color_palette(sdf[[layer_to_plot]]),
                        fillOpacity = 1,
                        lng = coords_sdf[, 1],
                        lat = coords_sdf[, 2],
                        # popup = paste("FID : ",sdf[["FID"]], "<br>", layer_to_plot, " : ",  sdf[[layer_to_plot]]),
                        opacity = 1,
                        group = "Circles",
                        layerId = sdf[["FID"]])%>%
              leaflet::addLegend(
                position = "bottomright",
                pal = color_palette,                                # 图例位置
                values = sdf[[layer_to_plot]],
                title = layer_to_plot,                              # 图例标题
                labFormat = labelFormat(prefix = "", suffix = ""),  # 可选：标签格式
                opacity = 1,                                         # 图例的不透明度
                layerId = "Circles"
              )%>%
              # 动态计算地图边界
              leaflet::fitBounds(lng1 = min(coords_sdf[, 1]),
                        lat1 = min(coords_sdf[, 2]),
                        lng2 = max(coords_sdf[, 1]),
                        lat2 = max(coords_sdf[, 2]))
      }
    }

  })
  # 更新gwpca_k最大值
  observeEvent(input$gwpca_vars, {
    max_length <- length(input$gwpca_vars)
    updateNumericInput(session, "gwpca_k", max = max_length)
  })
  # 用 reactiveValues 存储用户点击的点和生成的短音频文件路径
  gwpca_rv <- reactiveValues(
    clicked_points = list(),  # 用于存放点击的经纬度（仅连线模式时使用）
    audio_files = character() # 存放每个点击生成的短音频文件路径
  )
  # 点击地图产生音频
  observeEvent(input$gwpca_mapPlot_click, {
    req(gwpca_result())
    click <- input$gwpca_mapPlot_click
    shp_data <- gwpca_shapefile_data() %>% 
        sf::st_as_sf() %>% 
        sf::st_make_valid() %>% 
        sf::st_set_crs(4326)
    geom_type <- sf::st_geometry_type(shp_data) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    # 选择模式：点击模式直接生成短音频
    if(input$gwpca_audio_mode == "click"){
      # 获取必要数据
      gwpca_sdf <- gwpca_result()$SDF
      layer_to_plot <- input$gwpca_map_layer
      
      # 这里假设已将 gwpca_sdf 转换为 sf 对象，或直接使用 spatial 包的 nearest feature 方法
      gwpca_sdf_sf <- sf::st_as_sf(gwpca_sdf)
      click_point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      selected_id <- sf::st_nearest_feature(click_point, gwpca_sdf_sf)
      selected_row <- gwpca_sdf[selected_id, ]
      
      # 生成 popup 内容
      popup_content <- paste("FID :", selected_row$FID, "<br>", 
                            layer_to_plot, ":", round(selected_row[[layer_to_plot]], 4))
      if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        # 找到包含点击点的 Polygon
        selected_feature <- shp_data[sf::st_contains(shp_data, click_point, sparse = FALSE), ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No polygon data found", type = "error")
          return(NULL)
        }

        leaflet::leafletProxy("gwpca_mapPlot") %>%
          leaflet::clearPopups() %>%
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          leaflet::addPolygons(
            data = selected_feature,
            group = "highlighted_features",
            fillColor = "yellow",
            color = "#FF0000",
            fillOpacity = 0,
            weight = 2,
            highlightOptions = leaflet::highlightOptions(
              weight = 5,
              color = "#FF0000",
              bringToFront = TRUE
              )
            )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
        # 找到最近的点
        selected_id <- sf::st_nearest_feature(click_point, shp_data)
        selected_feature <- shp_data[selected_id, ]
        if (nrow(selected_feature) == 0) {
          shiny::showNotification("No point data found", type = "error")
          return(NULL)
        }
        leaflet::leafletProxy("gwpca_mapPlot") %>%
          leaflet::clearPopups() %>%
          leaflet::clearGroup("highlighted_features") %>%  # 清除之前的高亮
          # addPopups(lng = click$lng, lat = click$lat, popup = popup_content) %>%
          leaflet::addCircleMarkers(
            data = selected_feature,
            group = "highlighted_features",
            color = "red",
            fillOpacity = 0,
            radius = 8,
            stroke = TRUE,
            weight = 2
          )%>%
          leaflet::addMarkers(  
                  lng = click$lng, 
                  lat = click$lat, 
                  popup = popup_content,
                  group = "highlighted_features",
                  icon = musicIcon # 使用自定义图标
              )
      } else {
        shiny::showNotification("The current geometry type is not supported", type = "error")
        return(NULL)
      }
      
      
      # 生成短音频文件
      coeff <- ifelse(is.na(selected_row[[layer_to_plot]]), 0, selected_row[[layer_to_plot]])
      short_filename <- paste0("www/gwpca_map_audio_", as.integer(Sys.time()), "_", sample(1:1000, 1), ".wav")
      generate_audio(value = coeff,
                    filename = short_filename,
                    x_min = min(gwpca_sdf[[layer_to_plot]], na.rm = TRUE),
                    x_max = max(gwpca_sdf[[layer_to_plot]], na.rm = TRUE))
      
      # 更新 audio 控件，播放短音频
      shinyjs::runjs(sprintf("document.getElementById('gwpca_map_audio').src='%s'; document.getElementById('gwpca_map_audio').play();", short_filename))
      
    } else if(input$gwpca_audio_mode == "line") {
      # 连线模式：记录点击点，不直接生成音频
      gwpca_rv$clicked_points <- append(gwpca_rv$clicked_points, list(c(click$lng, click$lat)))
      # 在地图上添加标记以便确认顺序
      leaflet::leafletProxy("gwpca_mapPlot") %>%
        leaflet::addMarkers(
                  lng = click$lng, 
                  lat = click$lat,
                  popup = paste("point", length(gwpca_rv$clicked_points)),
                  group = "point",
                  icon = musicIcon
                  )
    }
  })
  # 处理连线模式下的确认按钮：生成长音频
  observeEvent(input$gwpca_confirm_audio, {
    req(length(gwpca_rv$clicked_points) > 0)
    # 仅在连线模式时触发检查
    if (input$gwpca_audio_mode == "line") {
      # 检查点击点数量是否足够
      if (length(gwpca_rv$clicked_points) < 2) {
        shiny::showNotification("Choose at least two points", type = "error")
        return() # 不满足条件时提前退出
      }
    }

    gwpca_buffer_length <- as.numeric(input$gwpca_buffer_length)
    layer_to_plot <- input$gwpca_map_layer

    # 显示自定义模态窗口
    showModal(modalDialog(
      title = "Processing gwpca Audio...",
      tags$div(
        style = "text-align: center;",
        shinyWidgets::progressBar(
          id = "gwpca_audio_progress",
          value = 0,
          display_pct = TRUE,
          status = "info",
          striped = TRUE
        )
      ),
      footer = tagList(
        modalButton("Cancel")  # 添加关闭按钮
      ),
      easyClose = FALSE
    ))

    # 1. 获取用户点击的坐标并绘制连线
    clicked_matrix <- do.call(rbind, gwpca_rv$clicked_points)
    leaflet::leafletProxy("gwpca_mapPlot") %>%
      leaflet::addPolylines(
                  lng = clicked_matrix[,1], 
                  lat = clicked_matrix[,2],
                  color = "red",
                  weight = gwpca_buffer_length,
                  opacity = 0.8,
                  group = "point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除之前的高亮
    
    # 2. 创建 sf 线对象并确保坐标系统一
    clicked_line <- sf::st_linestring(clicked_matrix) %>%
      sf::st_sfc(crs = 4326) %>%
      sf::st_make_valid()
    line_buffer <- sf::st_buffer(clicked_line, dist = gwpca_buffer_length)  # 可调整缓冲区大小
  
    # 3. 处理空间数据
    gwpca_sdf <- gwpca_result()$SDF
    shp_data <- gwpca_shapefile_data() %>% 
      sf::st_as_sf() %>% 
      sf::st_make_valid() %>% 
      sf::st_set_crs(4326)

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_audio_progress", value = 10)

    # 4. 转换Spatial对象为sf
    gwpca_sdf_sf <- sf::st_as_sf(gwpca_sdf) 
    
    # 将gwpca计算结果合并到原始shp属性（假设行顺序一致）
    if(nrow(shp_data) == nrow(gwpca_sdf_sf)) {
      shp_data_with_gwpca <- shp_data %>%
        dplyr::bind_cols(sf::st_drop_geometry(gwpca_sdf_sf)) %>%  # 使用正确的属性数据
        sf::st_sf()
    } else {
      shiny::showNotification("The number of data rows is inconsistent", type = "error")
      shiny::removeModal()
      return()
    }

    geom_type <- sf::st_geometry_type(shp_data_with_gwpca) %>% 
      as.character() %>% 
      unique() %>% 
      dplyr::first()

    if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      # 精确相交检测
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwpca, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      
      intersect_features <- intersects
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(shp_data_with_gwpca), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwpca_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwpca_mapPlot") %>%
        leaflet::addPolygons(
          data = intersects,
          group = "highlighted_features",
          color = "#FF0000",
          fillOpacity = 0,
          weight = 2,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#FF0000",
            bringToFront = TRUE
            )
          )
    }else if(geom_type %in% c("POINT", "MULTIPOINT")) {
      # 点处理逻辑
      # intersect_idx  <- st_intersects(gwpca_sdf_sf, line_buffer, sparse = FALSE)
      # intersect_features <- gwpca_sdf_sf[which(intersect_idx, arr.ind = TRUE), ]
      intersects <- tryCatch({
        sf::st_filter(shp_data_with_gwpca, line_buffer)
      }, error = function(e) {
        shiny::showNotification("Fail in intersects", type = "error")
        shiny::removeModal()
        return(NULL)
      })
      intersect_features <- intersects

      if(nrow(intersect_features) == 0) {
        shiny::showNotification("The line does not pass through any features", type = "warning")
        shiny::removeModal()
        return()
      }
      # **绘制调试图，检查是否有交集**
      plot(sf::st_geometry(gwpca_sdf_sf), col = "blue", border = "black", main = "空间要素 vs. 线")
      plot(sf::st_geometry(line_buffer), col = "red", lwd = gwpca_buffer_length, add = TRUE)

      leaflet::leafletProxy("gwpca_mapPlot") %>%
        leaflet::addCircleMarkers(
          data = intersect_features,
          group = "highlighted_features",
          color = "red",
          fillOpacity = 0,
          radius = 8,
          stroke = TRUE,
          weight = 2
        )

    } else {
      shiny::showNotification("The current geometry type is not supported", type = "error")
      shiny::removeModal()
      return()
    }
    
    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_audio_progress", value = 20)

    # 6. 沿路径排序（确保交点顺序与点击点一致）
    if (nrow(intersect_features) > 0) {
      start_point <- sf::st_sfc(sf::st_point(clicked_matrix[1, ]), crs = 4326)
      intersect_features <- intersect_features %>%
        dplyr::mutate(
          dist_to_start = as.numeric(sf::st_distance(geometry, start_point))
        ) %>%
        dplyr::arrange(dist_to_start)
    }

    # 7. 生成音频
    gwpca_rv$audio_files <- character()  # 重置音频存储

    # 更新进度条
    shinyWidgets::updateProgressBar(session = session, id = "gwpca_audio_progress", value = 30 )

    # 8. 遍历要素，生成音频
    for (i in seq_len(nrow(intersect_features))) {
      feature <- intersect_features[i, ]
      coeff <- ifelse(is.na(feature[[layer_to_plot]]), 0, feature[[layer_to_plot]])
      short_filename <- paste0("www/gwpca_map_audio_", i, ".wav")

      generate_audio(
        value = coeff,
        filename = short_filename,
        x_min = min(gwpca_sdf[[layer_to_plot]], na.rm = TRUE),
        x_max = max(gwpca_sdf[[layer_to_plot]], na.rm = TRUE)
      )

      gwpca_rv$audio_files <- c(gwpca_rv$audio_files, short_filename)

      # 更新进度条
      shinyWidgets::updateProgressBar(session = session, id = "gwpca_audio_progress", value = 30 + (i/nrow(intersect_features)) * 30)
    }
    
    # 9. 合成所有短音频文件为一个长音频
    composite_filename <- paste0("www/gwpca_map_audio_composite.wav")
    concatenate_audio(gwpca_rv$audio_files, composite_filename)
    # 确保 FFmpeg 使用合成后的音频
    sound_road <- composite_filename  # 这里修正

    # 提取系数数据
    ranges <- seq_len(nrow(intersect_features))  # 用于 X 轴
    coeff_values <- intersect_features[[layer_to_plot]]  # 提取回归系数
    ranges_length <- length(ranges)
    # 生成初始曲线图
    waveform_plot <- ggplot2::ggplot(data.frame(x = ranges, y = coeff_values), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "blue", size = 2) +  
      ggplot2::geom_smooth(method = "loess", color = "blue", span = 0.5, se = FALSE) +  
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),  
        plot.background = ggplot2::element_rect(fill = "white", color = NA)  
      ) +
      ggplot2::ggtitle("Coefficient Variation Over Features") +
      ggplot2::xlab("Feature Index") + 
      ggplot2::ylab("Coefficient Value")

    # 逐帧生成视频
    waveform_images <- c()
    for (i in seq_along(ranges)) {
      frame_plot <- waveform_plot +
        ggplot2::geom_vline(xintercept = ranges[i], color = "red", linetype = "dashed", size = 1) +
        ggplot2::ggtitle(paste("Feature:", i, "  Coefficient:", round(coeff_values[i], 4)))

      frame_image <- file.path(temp_dir, paste0("gwpca_map_waveform_", i, ".png"))
      ggplot2::ggsave(frame_image, frame_plot, width = 6, height = 4, dpi = 150)
      waveform_images <- c(waveform_images, frame_image)

      shinyWidgets::updateProgressBar(session, "gwpca_video_progress", value = 60 + (i / ranges_length) * 30)
    }

    # 计算音频时长
    audio_info <- tuneR::readWave(composite_filename)
    total_frames <- length(waveform_images)
    total_audio_duration <- length(audio_info@left) / audio_info@samp.rate
    # 计算帧率，使得视频时长与音频一致
    frame_rate <- total_frames / total_audio_duration
    # 确保 framerate 合理（避免异常）
    if (frame_rate < 1) frame_rate <- 1
    if (frame_rate > 30) frame_rate <- 30  # 限制在 1-30 fps，防止帧率过大或过小
    # 生成视频
    waveform_video <- file.path(temp_dir, "gwpca_map_waveform_video.mp4")
    av::av_encode_video(
      input = waveform_images,
      output = waveform_video,
      framerate = frame_rate,
      codec = "libx264",
      vfilter = "scale=720:720,format=yuv420p",
      audio = NULL,
      verbose = TRUE
    )

    # 合成音视频
    sound_video <- file.path("www", "gwpca_map_sound_video.mp4")
    # sound_road <- gwpca_rv$audio_files  # 获取之前生成的音频
    ffmpeg_command <- paste(
      "ffmpeg",
      "-y", 
      "-i", shQuote(waveform_video),
      "-i", shQuote(sound_road),
      "-ac 2 -c:v libx264 -c:a aac -b:a 192k -strict experimental",
      shQuote(sound_video)
    )
    ffmpeg_result <- tryCatch({
      system(ffmpeg_command, intern = FALSE)
    }, error = function(e) {
      cat("FFmpeg command failed:", e$message, "\n")
      NA
    })
    if (!is.na(ffmpeg_result) && ffmpeg_result != 0) {
      shiny::showNotification("The command execution of FFmpeg failed.Please check (1) whether ffmpeg is installed (2) whether the system path is configured.", type = "error")
      shiny::removeModal()
      return()
    }
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " 合成波形图和音频完成，文件保存为: ", sound_video, "\n")

    # 在地图左下角显示视频
    leaflet::leafletProxy("gwpca_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::addControl(
                HTML(sprintf(
          '<video id="gwpca_video_player" width="300" autoplay controls>
            <source src="%s" type="video/mp4">
            Your browser does not support the video tag.
          </video>
          <audio id="gwpca_audio" src="%s"></audio>
          <script>
            document.getElementById("gwpca_video_player").onplay = function() {
              var audio = document.getElementById("gwpca_audio");
              if (audio) {
                audio.play();
              }
            };
            document.getElementById("gwpca_video_player").onpause = function() {
              var audio = document.getElementById("gwpca_audio");
              if (audio) {
                audio.pause();
              }
            };
            document.getElementById("gwpca_video_player").ontimeupdate = function() {
              var audio = document.getElementById("gwpca_audio");
              if (audio) {
                audio.currentTime = this.currentTime;
              }
            };
          </script>', sound_video, composite_filename
        )),
        position = "bottomleft",
        layerId ="map_video_control"
      )

    # 设置音频源
    # shinyjs::runjs(sprintf("document.getElementById('gwpca_map_audio').src='%s';", composite_filename))

    # 重置记录，便于下次操作
    gwpca_rv$clicked_points <- list()
    gwpca_rv$audio_files <- character()

    shinyWidgets::updateProgressBar(session = session, id = "gwpca_audio_progress", value = 100)
    Sys.sleep(1)
    shiny::removeModal()
  })
  # 处理连线的图标
  observeEvent(input$gwpca_confirm_audio_clear, {
    # 重置记录，便于下次操作
    gwpca_rv$clicked_points <- list()
    gwpca_rv$audio_files <- character()

    leaflet::leafletProxy("gwpca_mapPlot") %>%
      leaflet::removeControl(layerId ="map_video_control") %>%
      leaflet::clearGroup("point") %>%
      leaflet::clearGroup("point_line") %>%
      leaflet::clearGroup("highlighted_features")  # 清除高亮
  })
  #--------------------------# Multiple Visualizations #-------------------------#
  # 动态生成图片
  observe({
    req(input$gwpca_columns)  # 确保用户已经选择了变量
    req(input$gwpca_bandwidth)
    req(input$gwpca_color_palette)  # 确保用户已经选择了色阶

    # 显示等待窗口
    showModal(modalDialog(
      title = "Please wait",
      "The GWPCA result is drawing...",
      easyClose = FALSE
    ))

    bw <- round(as.numeric(gwpca_bw_data$bw), 3)

    shp_data <- gwpca_shapefile_data()
    if (!inherits(shp_data, "sf")) {shp_data <- sf::st_as_sf(shp_data)}

    # 获取 gwpca 结果
    gwpca_result <- gwpca_result()
    sdf <- gwpca_result$SDF

    # 设置高分辨率参数
    dpi <- 100  # 设置DPI值
    width_px <- 1500  # 设置宽度像素
    height_px <- 1500 # 设置高度像素

    # 遍历用户选择的列并动态生成图片
    selected_columns <- input$gwpca_columns
    for (col in selected_columns) {
      local({
        column_name <- col  # 需要使用 local() 避免循环中的闭包问题
        # 动态创建 UI 输出控件
        output[[paste0("plot_", column_name)]] <- renderPlot({

          # 确保列存在于数据框中
          req(column_name %in% names(sdf))

          # 创建专题图
          target_data <- shp_data
          target_data$value <- sdf[[column_name]]  # 将数据加入到 sf 对象中，以便于绘制专题图

          # 判断数据类型，动态选择绘图样式
          geom_type <- unique(sf::st_geometry_type(target_data))
          # 根据选择的色阶动态设置颜色
          gradient <- color_gradients[[input$gwpca_color_palette]]

          if ("POINT" %in% geom_type || "MULTIPOINT" %in% geom_type) {  # 点数据：使用 color 映射
              if (input$gwpca_color_palette == "viridis") {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_viridis_c() +  # 使用 Viridis 色阶
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              } else {
                ggplot2::ggplot(target_data) +
                  ggplot2::geom_sf(ggplot2::aes(color = value)) +
                  ggplot2::scale_color_gradient(low = gradient["low"], high = gradient["high"]) +
                  ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                  ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                  ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.grid = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA)
                  )
              }
          } else if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {  # 面数据：使用 fill 映射
            if (input$gwpca_color_palette == "viridis") {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_viridis_c() +  # 使用 Viridis 色阶
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            } else {
              ggplot2::ggplot(target_data) +
                ggplot2::geom_sf(ggplot2::aes(fill = value)) +
                ggplot2::scale_fill_gradient(low = gradient["low"], high = gradient["high"]) +
                ggplot2::labs(title = paste0(column_name, "  (Bandwidth: ", bw, ")")) +
                ggspatial::annotation_north_arrow(
                    location = "tl",  # 左上角
                    which_north = "true",
                    pad_x = ggplot2::unit(0.3, "cm"),
                    pad_y = ggplot2::unit(0.3, "cm"),
                    style = ggspatial::north_arrow_fancy_orienteering()
                  ) +
                  ggspatial::annotation_scale(
                    location = "bl",  # 左下角
                    width_hint = 0.3
                  ) +
                ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "white", color = NA),
                  panel.grid = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }
          } else {
            # 如果既不是点也不是面，抛出警告
            shiny::showNotification("Unsupported geometry type for plotting.", type = "error")
            shiny::removeModal()
            return()
          }        
        })
      })
    }

    # # 动态图片生成完成后关闭等待窗口
    shiny::removeModal()
  })
  # 动态显示图片
  output$gwpca_Plot <- renderUI({
    req(input$gwpca_columns)  # 确保用户已经选择了变量

    # 每行的图片数量
    images_per_row <- min(2, length(input$gwpca_columns))  # 每行最多显示 2 张图片

    plot_outputs <- lapply(seq_along(input$gwpca_columns), function(i) {
      column_name <- input$gwpca_columns[i]

      # 使用 column 动态调整宽度，每行最多显示 images_per_row 张图片
      column(
        width = 12 / images_per_row,  # 动态设置列宽（12 栅格系统）
        plotOutput(outputId = paste0("plot_", column_name), height = "500px", width = "100%")
      )
    })

    # 将图片控件布局在 fluidRow 中
    do.call(fluidRow, plot_outputs)
  })
  #--------------------------# Glygh Plot #-------------------------#
  output$gwpca_glyph_plot <- renderPlot({
    gwpca_result <- gwpca_result()
    sdf    <- gwpca_result$SDF
    coords <- sp::coordinates(sdf)

    # 基本校验
    if (nrow(gwpca_result$loadings) != nrow(coords)) {
      showNotification("载荷与坐标行数不匹配", type = "error")
      return()
    }
    if (any(apply(gwpca_result$loadings, 1, max) == 0)) {
      showNotification("存在全零载荷行", type = "error")
      return()
    }

    if(isTRUE(input$gwpca_glyph_add)){
      # 取 k 个主成分
      k <- dim(gwpca_result$loadings)[3]
      for (pc in 1:k) {
        ld_mat <- gwpca_result$loadings[, , pc]
        GWmodel::gwpca.glyph.plot(
          ld            = ld_mat,
          loc           = coords,
          r1            = 50,
          add           = (pc != 1),        # 第一张图 recreate，后续叠加
          alpha         = 1,              # 可调整透明度
          sep.contrasts = as.logical(input$gwpca_glyph_sep) 
        )
      }
    }else{
      # 取第1个主成分的局部载荷矩阵（维度：n_locations × n_variables）
      local_loadings_pc1 <- gwpca_result$loadings[, , 1]
      GWmodel::gwpca.glyph.plot(
          ld = local_loadings_pc1,  
          loc = coords,
          sep.contrasts =  as.logical(input$gwpca_glyph_sep),
          r1 = 50,
          add = FALSE,
          alpha = 1
      )
    }
  })


  #--------------------------# 停止服务器 #--------------------------#
  # 在应用退出时停止服务器
  onStop(function() {
    stop_servr(file_server)
  })

}


# 运行Shiny应用
# shinyApp(ui = ui, server = server)