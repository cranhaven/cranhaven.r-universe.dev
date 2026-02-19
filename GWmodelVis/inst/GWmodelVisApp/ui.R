# ui.R

# 定义UI
ui <- shinydashboard::dashboardPage(
    # 设置主题
  skin = "blue",
  title = "GWmodel-Vis",  # 这里设置浏览器标签页标题
  # 界面标题
  shinydashboard::dashboardHeader(
    title = tags$div(
        style = "display: flex; align-items: center; justify-content: flex-start; height: 50px;",
        tags$img(src = "gwmodel.png", height = "35px", style = "margin-right: 10px;"),
        span("GWmodel-Vis", style = "font-size: 20px; font-weight: bold; color: white;")
    ),
    titleWidth = 230 # 设置标题宽度
  ),

  # 侧边栏
  shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Home", tabName = "home", icon = icon("home")),
        shinydashboard::menuSubItem("GWSS", tabName = "gwss", icon = icon("area-chart")),
        shinydashboard::menuSubItem("GW Boxplot", tabName = "gw_boxplot",icon = icon("th-large")),
        shinydashboard::menuItem("GWR", tabName = "gwr/mgwr",icon = icon("bar-chart") ,
                 shinydashboard::menuSubItem("GWR.basic", tabName = "gwr"),
                 shinydashboard::menuSubItem("GWR.multiscale", tabName = "mgwr")
        ),
        shinydashboard::menuSubItem("GWPCA", tabName = "gwpca",icon = icon("chart-gantt"))
      )
  ),

  # 主面板
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      text = "
        shinyjs.resetFileInput = function(params) {
          var id = params.id;
          var resetInput = $('#' + id);
          resetInput.val('');  // 清空文件选择值

          // 针对 Shiny 文件输入框的特殊处理，清空显示的文件名
          var parentDiv = resetInput.closest('.form-group');
          parentDiv.find('.form-control').val('');  // 清空文件名显示区域
        }
      ",
      functions = c("resetFileInput")  # 指定扩展函数
    ),
    tags$head(
      tags$style(HTML("
        /*软件整体背景色*/
        .custom-dashboard-body {
          height: 800px;/* 设置高度 */
          overflow-y: auto; /* 启用垂直滚动 */
          background-color: #f9f9f9;/* 设置背景颜色 */
        }

        /*显著性样式/图例*/
        .solid-line {
          border-bottom: 3px solid black;
          width: 20px;
          display: inline-block;
          margin-right: 5px;
        }
        .dashed-line {
          border-bottom: 3px dashed black;
          width: 20px;
          display: inline-block;
          margin-right: 5px;
        }

        /*参数板样式*/
        /* 加深Choose Shapefile ZIP file按钮背景色 */
        .btn-file {
          background-color:rgb(13, 91, 175) !important;
          color: white !important;
        }
        /* 确保下拉框不被遮挡 */
        .selectize-dropdown {
          z-index: 1050 !important;
        }
        /* 调整按钮样式 */
        .btn-primary {
          font-weight: bold;
          font-size: 14px;
          margin-top: 10px;
        }
        .parameter-section {
          border: 1px solid #ddd;
          padding: 10px;
          margin-bottom: 5px;
          background-color: #f9f9f9;
          border-radius: 5px;
        }
        .parameter-title {
          font-weight: bold;
          font-size: 16px;
          margin-bottom: 5px;
          color:rgb(49, 138, 234);
        }
      "))
      ),
    class = "custom-dashboard-body",  # 应用自定义的类名

    shinydashboard::tabItems(
      # Home
      shinydashboard::tabItem(tabName = "home",
        # 顶部部分
        fluidRow(
          column(
            width = 12,
            align = "center",
            div(
              style = "background-color: #3C8DBC; color: white; padding: 20px; border-radius: 10px;",
              HTML("<h1 style='font-weight: bold; font-size: 36px;'>Geographically Weighted Models</h1>"),
              HTML("<h3>Exploring Spatial Heterogeneity and Advanced Visualization</h3>") # 探索空间异质性与高级可视化
            )
          )
        ),
        br(),
        # 卡片式布局 - 研究内容
        fluidRow(
          column(
            width = 12,
            div(
              class = "card",
              style = "background-color: #F9F9F9; padding: 20px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              HTML("
                <h2 style='color: #3C8DBC;'>Research Overview</h2>
                <p style='font-size: 16px;'>
                  The core objective of this study is to design and evaluate a systematic visualization framework
                  for Geographically Weighted Modeling (GWM). This research emphasizes creating a multi-sensory
                  visualization solution to enhance the interpretability and usability of GWM results.
                </p>
                <ul style='font-size: 16px;'>
                  <li><b>Key Goals:</b> Identify key variables and spatial distributions, understand spatial heterogeneity,
                  and provide advanced tools for model comparison.</li>
                  <li><b>Innovations:</b> Multi-level analysis from basic data visualization to complex multi-model comparisons.</li>
                </ul>
              ")
              # 研究概述
              # 本研究的核心目标是设计并评估地理加权建模（GWM）的系统可视化框架。该研究着重于创建一种多感官可视化解决方案，以提高 GWM 结果的可解释性和可用性。
              # 关键目标：识别关键变量和空间分布，理解空间异质性，并为模型比较提供高级工具。
              # 创新点：从基本的数据可视化到复杂的多模型比较的多层次分析。
            )
          )
        ),
        br(),
        # 功能模块卡片
        fluidRow(
          column(
            width = 1, # 左侧空白
            offset = 0
          ),
          column(
            width = 2,
            align = "center",
            div(
              class = "card",
              style = "background-color: #FFFFFF; padding: 15px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              img(src = "gwss.jpg", height = "150px", width = "150px", style = "margin-bottom: 10px;"),
              HTML("<h4>GWSS</h4>"),
              actionButton("btn_gwss", "Go to GWSS", class = "btn btn-primary", style = "color: white; width: 100%;")
            )
          ),
          column(
            width = 2,
            align = "center",
            div(
              class = "card",
              style = "background-color: #FFFFFF; padding: 15px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              img(src = "gwbp.png", height = "150px", width = "150px", style = "margin-bottom: 10px;"),
              HTML("<h4>GW Boxplot</h4>"),
              actionButton("btn_gw_boxplot", "Go to GW Boxplot", class = "btn btn-primary", style = "color: white; width: 100%;")
            )
          ),
          column(
            width = 2,
            align = "center",
            div(
              class = "card",
              style = "background-color: #FFFFFF; padding: 15px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              img(src = "gwr.jpg", height = "150px", width = "150px", style = "margin-bottom: 10px;"),
              HTML("<h4>GWR</h4>"),
              actionButton("btn_gwr_basic", "Go to GWR", class = "btn btn-primary", style = "color: white; width: 100%;")
            )
          ),
          column(
            width = 2,
            align = "center",
            div(
              class = "card",
              style = "background-color: #FFFFFF; padding: 15px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              img(src = "mgwr.jpg", height = "150px", width = "150px", style = "margin-bottom: 10px;"),
              HTML("<h4>MGWR</h4>"),
              actionButton("btn_gwr_multiscale", "Go to MGWR", class = "btn btn-primary", style = "color: white; width: 100%;")
            )
          ),
          column(
            width = 2,
            align = "center",
            div(
              class = "card",
              style = "background-color: #FFFFFF; padding: 15px; border: 1px solid #DDD; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              img(src = "gwpca.jpg", height = "150px", width = "150px", style = "margin-bottom: 10px;"),
              HTML("<h4>GWPCA</h4>"),
              actionButton("btn_gwpca", "Go to GWPCA", class = "btn btn-primary", style = "color: white; width: 100%;")
            )
          ),
          column(
            width = 1 # 右侧空白
          )
        ),
        br(),
      ),
      # GWSS
      shinydashboard::tabItem(tabName = "gwss",
              sidebarPanel(
                    h2("GWSS Workflow",align = "center"),
                    div(class = "parameter-section",
                        div(class = "parameter-title", "1. Upload Data"),
                        fileInput("gwss_shapefile", "Choose Shapefile ZIP file", 
                                  accept = c('.zip'), multiple = FALSE, 
                                  buttonLabel = "Choose ZIP", placeholder = "No file selected")
                    ),
                    div(class = "parameter-section",
                        div(class = "parameter-title", "2. Select Parameters"),
                        uiOutput("gwss_vars"),
                        fluidRow(
                          column(6, selectizeInput("gwss_kernel", "Kernel Type", 
                                                  choices = c("gaussian", "exponential", "bisquare", "tricube", "boxcar"))),
                          column(6, selectizeInput("gwss_adaptive", "Adaptive", 
                                                  choices = c("TRUE","FALSE")))
                        ),
                        fluidRow(
                          column(6, selectizeInput("gwss_longlat", "Longlat", 
                                                  choices = c("TRUE", "FALSE"))),
                          column(6, selectizeInput("gwss_color_palette", "Color Palette",
                                                  choices = list(
                                                    "Green-Red" = "green-red",
                                                    "Blue-Yellow" = "blue-yellow",
                                                    "Purple-Orange" = "purple-orange",
                                                    "Magenta-Cyan" = "magenta-cyan",
                                                    "Pink-Blue" = "pink-blue",
                                                    "Viridis" = "viridis"
                                                  ), selected = "green-red"))
                        )
                    ),
                    div(class = "parameter-section",
                      div(class = "parameter-title", "3. Configure Analysis"),
                      tabsetPanel(id = "gwss_tabset",
                                  tabPanel("Model Parameters",
                                          sliderInput("gwss_bandwidth", "Bandwidth(Number of points)", 
                                                      min = 1, max = 100, value = 20),
                                          actionButton("gwss_execute", "Execute", 
                                                        width = "100%", 
                                                        class = "btn btn-primary", 
                                                        style = "color: white;")
                                  ),
                                  tabPanel("Video Settings",
                                          sliderInput("gwss_range", 
                                                      "Choose the bandwidth range for the Video", 
                                                      min = 2, max = 100, 
                                                      value = c(10, 100), step = 1),
                                          fluidRow(
                                            column(6, selectInput("gwss_range_layer", 
                                                                  "Variables for the Video", 
                                                                  choices = c("Waiting for data" = "Waiting for data"))),
                                            column(6, numericInput("gwss_step_length", 
                                                                    label = "Step length of the Video", 
                                                                    value = 2, min = 1, step = 1))
                                          ),
                                          actionButton("gwss_video_button", "Produce Video", 
                                                        width = "100%", 
                                                        class = "btn btn-primary", 
                                                        style = "color: white;")
                                  )
                      )
                  ),
                  width = 4,
              ),
              mainPanel(
                    tabsetPanel(id = "gwss_tabs",
                                type = "tabs",
                                tabPanel("Model Introduction",
                                          fluidPage(
                                            # 标题
                                            div(
                                              style = "text-align: center; margin-top: 20px;",
                                              h2("Geographically Weighted Summary Statistics (GWSS)", style = "color: #2C3E50; font-weight: bold;")
                                            ),

                                            # 使用 fluidRow 进行左右布局
                                            fluidRow(
                                              column(
                                                width = 12,  # Model Overview 模块占满整行
                                                div(
                                                  style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px; margin: 5px; font-size: 16px;",
                                                  # 使用 fluidRow 实现左文本 + 右图片
                                                  fluidRow(
                                                    column(
                                                      width = 9,  # 左侧文本
                                                      h3("Model Overview", style = "color: #34495E; font-weight: bold;"),
                                                      p("Geographically weighted summary statistics (GWSS) is a method used to calculate local statistics (such as mean, variance, etc.) of spatial data.
                                                        Unlike traditional global statistics, GWSS takes into account the effects of spatial location and calculates the local statistics for each location in a weighted manner, 
                                                        thus revealing the spatial heterogeneity of the data.")
                                                    ),
                                                    column(
                                                      width = 3,  # 右侧图片
                                                      div(
                                                        style = "text-align: center;",
                                                        img(src = "gwss.jpg", width = "100%", 
                                                            style = "max-width: 200px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);")
                                                      )
                                                    )
                                                  )
                                                )
                                              ),

                                              # Workflow 模块占满整个宽度
                                              column(
                                                width = 12, 
                                                div(
                                                  style = "padding: 15px; margin: 5px;",
                                                  h3("Workflow", style = "color: #34495E; font-weight: bold; text-align: left;"),
                                                  div(
                                                    style = "display: flex; flex-direction: column; align-items: flex-start; font-size: 18px;",
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("1. Upload Data:"), " Import spatial datasets for analysis.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("2. Select Parameters:"), " Choose variables and configure spatial weights.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("3. Configure Analysis:"), " Set bandwidth and compute localized statistics."))
                                                  )
                                                )
                                              )
                                            ),

                                            # 结果释义
                                            div(
                                              style = "background-color: #F8F9F9; padding: 10px; border-radius: 10px; margin: 5px; font-size: 18px;",
                                              h3("Result Interpretation", style = "color: #34495E; font-weight: bold;"),
                                              tags$ul(
                                                tags$li(strong("_LM:"), " Locally weighted Mean - Weighted mean in geographical space."),
                                                tags$li(strong("_LSD:"), " Locally weighted Standard Deviation - Dispersion of data within a spatial context."),
                                                tags$li(strong("_LVar:"), " Locally weighted Variance - Square of the locally weighted standard deviation."),
                                                tags$li(strong("_LSKe:"), " Locally weighted Skewness - Measures asymmetry in data distribution."),
                                                tags$li(strong("_LCV:"), " Locally weighted Coefficient of Variation - Relative dispersion measurement."),
                                                tags$li(strong("Cov_X.Y:"), " Covariance - Measures the strength of linear relationship between two variables."),
                                                tags$li(strong("Corr_X.Y:"), " Pearson Correlation - Evaluates the linear correlation between two spatial variables.")
                                              )
                                            )
                                          )
                                        ),
                                tabPanel("Summary Information", verbatimTextOutput("gwss_modelDiagnostics")),
                                tabPanel("Web Map",
                                          leaflet::leafletOutput("gwss_mapPlot", height = "700px"),
                                          absolutePanel(id = "gwss_map_panel",
                                                        fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                        draggable = TRUE,  # draggable = TRUE 允许拖动
                                                        top = 111, right = 30,  # 设置面板的位置
                                                        width = 300, height = 140,  # 面板的大小
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        wellPanel(
                                                          style = "background-color: rgba(255, 255, 255, 0.4);",
                                                          uiOutput("gwss_map_layer"),
                                                          actionButton("gwss_map_layer_execute", "Select",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                                                          br(),br(),
                                                          radioButtons("gwss_audio_mode", "Select the audio generation mode", 
                                                                            choices = c("Click to generate audio" = "click", "Connect to generate audio" = "line")),
                                                          # 当选择连线模式时显示确认按钮
                                                          conditionalPanel(
                                                            condition = "input.gwss_audio_mode == 'line'",
                                                            numericInput("gwss_buffer_length", label = "Buffer length", value = 1, min = 0.0000000000001, step = 1),
                                                            actionButton("gwss_confirm_audio", "Create a connected audio link", width = "100%", class = "btn btn-primary", style = "color: white; "),
                                                            actionButton("gwss_confirm_audio_clear", "Clear", width = "50%", class = "btn btn-primary", style = "color: white; "),
                                                          ),
                                                        ),
                                          ),
                                          tags$audio(id = "gwss_map_audio",type = "audio/wav", controls = TRUE, autoplay = TRUE, src = "")
                                        ),
                                tabPanel("Multiple Visualizations",
                                        uiOutput("gwss_Plot"),
                                        absolutePanel(id = "gwss_plot_panel",
                                                    fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                    draggable = TRUE,  # draggable = TRUE 允许拖动
                                                    top = 111, right = 30,  # 设置面板的位置
                                                    width = 300, height = 150,  # 面板的大小
                                                    style = "background-color: rgba(255, 255, 255, 0.4);",
                                                    wellPanel(
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      uiOutput("gwss_columns"),
                                                    ),
                                                  ),
                                        ),
                                tabPanel("Table View", br(),DT::dataTableOutput("gwss_summaryTable")),
                                tabPanel("Video", 
                                          fluidRow(
                                            column(width = 4,
                                                    h4("1.Watch the map change with your eyes", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwss_image_video"),
                                                    h4("2.Feel the audio change with your ears", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwss_sound_video")
                                            ),
                                            column(width = 8,
                                                    h4("3.Combine audio and video to feel the changes", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwss_final_video")
                                            )
                                          ),
                                         )
                    ),
                    width = 8,
              ),
      ),
      # GW_Boxplot
      shinydashboard::tabItem(tabName = "gw_boxplot",
              sidebarPanel(
                    h2("GWBP Workflow",align = "center"),
                    div(class = "parameter-section",
                        div(class = "parameter-title", "1. Upload Data"),
                        fileInput("gwbp_shapefile","Choose Shapefile ZIP file",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                        fileInput("gwbp_cp_shapefile","Choose Regression points Shapefile ZIP file(Optional)",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                        # fluidRow(
                        #   column(9,fileInput("gwbp_cp_shapefile","Choose Regression points Shapefile ZIP file(Optional)",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),),
                        #   column(3,actionButton("gwbp_cp_shapefile_clear_button", "Clear",width = "100%", class = "btn btn-primary" ,style = "color: white; height: 38px; margin-top: 40px;"),),
                        # ),
                    ),
                    div(class = "parameter-section",
                        div(class = "parameter-title", "2. Select Parameters and Analysis"),
                        fluidRow(
                          column(12,uiOutput("gwbp_X"),),
                        ),
                        fluidRow(
                          column(6,selectizeInput("gwbp_kernel", "Kernel Type",choices = c("gaussian", "exponential", "bisquare", "tricube", "boxcar"), multiple = FALSE), ),
                          column(6,selectizeInput("gwbp_adaptive", "Adaptive",choices = c("TRUE","FALSE"), multiple = FALSE), ),
                          
                        ),
                        fluidRow(
                          column(6,selectizeInput("gwbp_longlat", "Longlat",choices = c("TRUE", "FALSE"), multiple = FALSE), ),
                          column(6,selectizeInput("gwbp_bpcolor", "Boxplot color",choices = c("white","red", "blue","green","yellow"), multiple = FALSE), ),
                        ),
                        sliderInput("gwbp_bandwidth", "Bandwidth(Number of points)", min = 1, max = 100, value = 20), # 初始最大值设为100，稍后在server中更新
                        # 创建一个按钮，点击时调用函数
                        actionButton("gwbp_execute", "Execute",width = "100%", class = "btn btn-primary" ,style = "color: white; "), # background-color: #4F94CD; 
                    ),
                    width = 4,
              ),
              mainPanel(
                    tabsetPanel(id = "gwbp_tabs",
                                type = "tabs",
                                tabPanel("Model Introduction",
                                          fluidPage(
                                            # 标题
                                            div(
                                              style = "text-align: center; margin-top: 20px;",
                                              h2("Geographically Weighted Boxplot (GWBoxplot)", style = "color: #2C3E50; font-weight: bold;")
                                            ),

                                            fluidRow(
                                              column(
                                                width = 12,  
                                                div(
                                                  style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px; margin: 5px; font-size: 16px;",
                                                  fluidRow(
                                                    column(
                                                      width = 9,  # 左侧文本
                                                      h3("Model Overview", style = "color: #34495E; font-weight: bold;"),
                                                      p("Geographically Weighted Boxplot (GWBoxplot) is a visualization tool designed to explore 
                                                        the distribution characteristics of spatial data at different locations. 
                                                        Combining traditional boxplot methodology with geographically weighted principles, 
                                                        GWBoxplot generates location-specific weighted boxplots, allowing users to intuitively 
                                                        observe local data distribution, outliers, and spatial variability.")
                                                    ),
                                                    column(
                                                      width = 3,  # 右侧图片
                                                      div(
                                                        style = "text-align: center;",
                                                        img(src = "gwbp.png", width = "100%", 
                                                            style = "max-width: 200px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);")
                                                      )
                                                    )
                                                  )
                                                )
                                              ),

                                              # Workflow 占满整行
                                              column(
                                                width = 12, 
                                                div(
                                                  style = "padding: 15px; margin: 5px;",
                                                  h3("Workflow", style = "color: #34495E; font-weight: bold; text-align: left;"),
                                                  tags$ul(
                                                    style = "font-size: 18px; padding-left: 20px;",
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("1. Upload Data:"), " Import spatial datasets for analysis.")),
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("2. Select Parameters:"), " Choose variables and configure spatial weights.")),
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("3. Configure Analysis:"), " Set bandwidth and compute localized boxplots."))
                                                  )
                                                )
                                              )
                                            ),


                                            # 结果释义
                                            div(
                                              style = "background-color: #F8F9F9; padding: 5px; border-radius: 10px; margin: 5px; font-size: 18px;",
                                              h3("Result Interpretation", style = "color: #34495E; font-weight: bold;"),
                                              tags$ul(
                                                tags$li(strong("GWBoxplot_Max:"), " Maximum value after applying geographically weighted boxplot."),
                                                tags$li(strong("GWBoxplot_Q3:"), " Third quartile (75th percentile) of the geographically weighted data."),
                                                tags$li(strong("GWBoxplot_Median:"), " Median value after applying geographically weighted boxplot."),
                                                tags$li(strong("GWBoxplot_Q1:"), " First quartile (25th percentile) of the geographically weighted data."),
                                                tags$li(strong("GWBoxplot_Min:"), " Minimum value after applying geographically weighted boxplot."),
                                                tags$li(strong("Outliers:"), " Data points that are smaller than Q1 - 1.5 × IQR or larger than Q3 + 1.5 × IQR.")
                                              )
                                            )
                                          )
                                        ),
                                tabPanel("Web Map",leaflet::leafletOutput("gwbp_mapPlot", width = "100%", height = "750px")),
                                tabPanel("Table View", br(),
                                          DT::dataTableOutput("gwbp_summaryTable")
                                        ),
                    ),
                    width = 8,
              ),
      ),
      # GWR
      shinydashboard::tabItem(tabName = "gwr",
              shinyjs::useShinyjs(),
              sidebarPanel(
                      h2("GWR Workflow",align = "center"),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "1. Upload Data"),
                            fileInput("gwr_shapefile","Choose Shapefile ZIP file",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                            fileInput("gwr_cp_shapefile","Choose Regression points Shapefile ZIP file(Optional)",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                      ),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "2. Select Parameters"),
                            fluidRow(
                              column(6,uiOutput("gwr_dependentVar"),),
                              column(6,uiOutput("gwr_independentVars"),),
                            ),
                            fluidRow(
                              column(6,checkboxInput("gwr_best_bandwidth", "Optimal bandwidth", value = FALSE),),
                              column(6,checkboxInput("gwr_significance_show", "Significant level", value = TRUE),),
                            ),
                            fluidRow(
                              column(4,selectizeInput("gwr_kernel", "Kernel Type",choices = c("gaussian", "exponential", "bisquare", "tricube", "boxcar"), multiple = FALSE), ),
                              column(4,selectizeInput("gwr_adaptive", "Adaptive",choices = c("TRUE","FALSE"), multiple = FALSE), ),
                              column(4,selectizeInput("gwr_color_palette", "Color Palette",
                                      choices = list(
                                        "Green-Red" = "green-red",
                                        "Blue-Yellow" = "blue-yellow",
                                        "Purple-Orange" = "purple-orange",
                                        "Magenta-Cyan" = "magenta-cyan",
                                        "Pink-Blue" = "pink-blue",
                                        "Viridis" = "viridis"
                                      ),
                                      selected = "green-red"
                                    ),
                                  ),
                            ),
                      ),
                     div(class = "parameter-section",
                      div(class = "parameter-title", "3. Configure Analysis"),
                          tabsetPanel(id = "gwr_tabset",
                                      tabPanel("Model Parameters",
                                              div(id = "bandwidth_div", sliderInput("gwr_bandwidth", "Bandwidth(Number of points)", min = 1, max = 100, value = 20)),
                                              actionButton("gwr_execute", "Execute",width = "100%", class = "btn btn-primary" ,style = "color: white;  margin-bottom: 10px;")
                                      ),
                                      tabPanel("Video Settings",
                                              sliderInput("gwr_range", "Choose the bandwidth range for the Video", min = 2, max = 100, value = c(10, 100), step = 1),
                                              fluidRow(
                                                column(6,selectInput("gwr_range_layer", "Variables for the Video", choices = c("Waiting for data" = "Waiting for data"))),
                                                column(6,numericInput("gwr_step_length", label = "Step length of the Video",value = 2, min = 1, step = 1)),  # 修正min值和step参数
                                              ),
                                              actionButton("gwr_video_button", "Produce Video",width = "100%", class = "btn btn-primary" ,style = "color: white;")
                                      ),
                                      tabPanel("Prediction",
                                              fileInput("gwr_predict_datafile", "Choose CSV for Prediction", accept = c(".csv")),
                                              actionButton("gwr_predict_execute", "Produce Prediction",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                                      )
                          ),
                     ),  
                      width = 4,
                ),
              mainPanel(
                    tabsetPanel(
                                id = "gwr_tabs",
                                type = "tabs",
                                tabPanel("Introduction",
                                          fluidPage(
                                            # 标题
                                            div(
                                              style = "text-align: center; margin-top: 20px;",
                                              h2("Geographically Weighted Regression (GWR)", style = "color: #2C3E50; font-weight: bold;")
                                            ),

                                            fluidRow(
                                              # Model Overview 占据整个宽度
                                              column(
                                                width = 12,  
                                                div(
                                                  style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px; margin: 5px; font-size: 16px;",
                                                  fluidRow(
                                                    column(
                                                      width = 9,  # 左侧文本
                                                      h3("Model Overview", style = "color: #34495E; font-weight: bold;"),
                                                      p("Geographically Weighted Regression (GWR) is a spatial regression analysis technique used to explore the relationship 
                                                        between dependent and independent variables in spatial data. Unlike traditional linear regression, 
                                                        GWR allows regression coefficients to vary across geographic locations, effectively capturing spatial heterogeneity."),
                                                      p("It fits local regression models by assigning spatial weights to each observation based on proximity, 
                                                        providing insights into spatially varying relationships.")
                                                    ),
                                                    column(
                                                      width = 3,  # 右侧图片
                                                      div(
                                                        style = "text-align: center;",
                                                        img(src = "gwr.jpg", width = "100%", 
                                                            style = "max-width: 200px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);")
                                                      )
                                                    )
                                                  )
                                                )
                                              ),

                                              # Workflow 占满整行
                                              column(
                                                width = 12, 
                                                div(
                                                  style = "padding: 15px; margin: 5px;",
                                                  h3("Workflow", style = "color: #34495E; font-weight: bold; text-align: left;"),
                                                  tags$ul(
                                                    style = "font-size: 18px; padding-left: 20px;",
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("1. Upload Data:"), " Import spatial datasets for regression analysis.")),
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("2. Select Parameters:"), " Choose independent and dependent variables, set spatial weighting parameters.")),
                                                    div(style = "padding-bottom: 5px;",tags$li(strong("3. Configure Analysis:"), " Adjust bandwidth and compute spatially varying regression coefficients."))
                                                  )
                                                )
                                              )
                                            ),

                                            # 结果释义
                                            div(
                                              style = "background-color: #F8F9F9; padding: 5px; border-radius: 10px; margin: 5px; font-size: 18px;",
                                              h3("Result Interpretation", style = "color: #34495E; font-weight: bold;"),
                                              tags$ul(
                                                tags$li(strong("Intercept:"), " Represents the predicted value of the dependent variable when all independent variables are zero. 
                                                        In GWR, the intercept varies by location and serves as a local baseline."),
                                                tags$li(strong("Variable:"), " A coefficient representing an independent variable that explains variations in the dependent variable. 
                                                        In GWR, variable coefficients change spatially."),
                                                tags$li(strong("Y:"), " The dependent variable to be predicted or explained by the model."),
                                                tags$li(strong("Yhat:"), " The predicted value of the dependent variable calculated using the independent variables and their regression coefficients."),
                                                tags$li(strong("Residual:"), " The difference between the observed and predicted values (Y - Yhat). Lower residuals indicate a better model fit."),
                                                tags$li(strong("CV_Score:"), " A cross-validation score used to evaluate the predictive performance of the model. Lower scores indicate better predictive accuracy."),
                                                tags$li(strong("Stud_Residual:"), " Standardized residuals, used to identify outliers in the model. Large absolute values suggest possible anomalies."),
                                                tags$li(strong("Intercept_SE:"), " Standard error of the intercept, indicating its estimation precision (lower values mean higher reliability."),
                                                tags$li(strong("Variable_SE:"), " Standard error of a variable's coefficient, showing uncertainty in its effect size."),
                                                tags$li(strong("Intercept_TV:"), " T-value of the intercept, testing if it is statistically different from zero (|t| > 1.96 suggests significance."),
                                                tags$li(strong("Variable_TV:"), " T-value of a variable's coefficient, assessing its significance in explaining the dependent variable."),
                                                tags$li(strong("Local_R2:"), " Local R-squared (0~1), measuring how well the model fits the data at each location (closer to 1 indicates better local explanation.")
                                              )
                                            )
                                          )
                                        ),
                                tabPanel("Scatter Plot",uiOutput("gwr_ScatterPlot"),),
                                tabPanel("Summary Information", verbatimTextOutput("gwr_modelDiagnostics")),
                                tabPanel("Web Map",
                                        leaflet::leafletOutput("gwr_mapPlot", height = "700px"),
                                        absolutePanel(id = "gwr_map_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("gwr_map_layer"),
                                                        actionButton("gwr_map_layer_execute", "Select",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                                                        br(),br(),
                                                        radioButtons("gwr_audio_mode", "Select the audio generation mode", 
                                                                          choices = c("Click to generate audio" = "click", "Connect to generate audio" = "line")),
                                                        # 当选择连线模式时显示确认按钮
                                                        conditionalPanel(
                                                          condition = "input.gwr_audio_mode == 'line'",
                                                          numericInput("gwr_buffer_length", label = "Buffer length", value = 1, min = 0.0000000000001, step = 1),
                                                          actionButton("gwr_confirm_audio", "Create a connected audio link", width = "100%", class = "btn btn-primary", style = "color: white; "),
                                                          actionButton("gwr_confirm_audio_clear", "Clear", width = "50%", class = "btn btn-primary", style = "color: white; "),
                                                        ),
                                                        ),
                                                    ),
                                        div(id = "Significant_line_div",HTML("
                                            <div style='position:absolute; bottom:90px; right:120px; background:white; padding:10px; border:1px solid #ccc;'>
                                              <span><b> Significant level </b></span><br>
                                              <span class='solid-line'></span> Significance<br>
                                              <span class='dashed-line'></span> Non-significance
                                            </div>
                                          ")),
                                        div(id = "Significant_point_div",HTML("
                                          <div style='position:absolute; bottom:90px; right:120px; background:white; padding:10px; border:1px solid #ccc;'>
                                            <span><b> Significant level </b></span><br>
                                            <span class='fa-regular fa-circle'></span> Significance<br>
                                            <span class='fa-solid fa-xmark'></span> Non-significance
                                          </div>
                                        ")),
                                        tags$audio(id = "gwr_map_audio",type = "audio/wav", controls = TRUE, autoplay = TRUE, src = "")
                                        ),
                                tabPanel("Multiple Visualizations",
                                          uiOutput("gwr_Plot"),
                                          absolutePanel(id = "gwr_plot_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("gwr_columns"),
                                                      ),
                                                    ),
                                          ),
                                tabPanel("Table View", br(), DT::dataTableOutput("gwr_summaryTable")),
                                tabPanel("Video", 
                                          fluidRow(
                                            column(width = 4,
                                                    h4("1.Watch the map change with your eyes", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwr_image_video"),
                                                    h4("2.Feel the audio change with your ears", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwr_sound_video")
                                            ),
                                            column(width = 8,
                                                    h4("3.Combine audio and video to feel the changes", style = "color: #34495E; font-weight: bold;"),
                                                    uiOutput("gwr_final_video")
                                            )
                                          ),
                                        ),
                                tabPanel("Prediction",
                                          tabsetPanel(
                                            id = "gwr_predict_tabset",  # 可选ID
                                            type = "tabs",          # 选项卡样式
                                            tabPanel("Prediction Map",
                                              leaflet::leafletOutput("gwr_predict_map", height = "700px")
                                            ),
                                            tabPanel("Prediction Summary Information",
                                              verbatimTextOutput("gwr_predict_information")
                                            ),
                                            tabPanel("Prediction Table",
                                              DT::DTOutput("gwr_predict_table"),
                                            )
                                          )
                                        ),
                    ),
                    width = 8,
              ),
      ),
      # MGWR
      shinydashboard::tabItem(tabName = "mgwr",
              sidebarPanel(
                      h2("MGWR Workflow",align = "center"),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "1. Upload Data"),
                        fileInput("mgwr_shapefile","Choose Shapefile ZIP file",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                      ),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "2. Select Parameters and Analysis"),
                            uiOutput("mgwr_dependentVar"),
                            uiOutput("mgwr_independentVars"),
                            fluidRow(
                              column(4,selectizeInput("mgwr_kernel", "Kernel Type",choices = c("gaussian", "exponential", "bisquare", "tricube", "boxcar"), multiple = FALSE), ),
                              column(4,selectizeInput("mgwr_adaptive", "Adaptive",choices = c("TRUE","FALSE"), multiple = FALSE), ),
                              column(4,selectizeInput("mgwr_color_palette", "Color Palette",
                                choices = list(
                                  "Green-Red" = "green-red",
                                  "Blue-Yellow" = "blue-yellow",
                                  "Purple-Orange" = "purple-orange",
                                  "Magenta-Cyan" = "magenta-cyan",
                                  "Pink-Blue" = "pink-blue",
                                  "Viridis" = "viridis"
                                ),
                                selected = "green-red"
                            ),),
                            ),
                            fluidRow(
                              column(6,checkboxInput("mgwr_best_bandwidth", "Optimal bandwidth", value = FALSE),),
                              column(6,checkboxInput("mgwr_significance_show", "Significant level", value = TRUE),),
                            ),
                            uiOutput("mgwr_bandwidth"), # 动态生成带宽滑块
                            # 按钮，点击时调用函数
                            actionButton("mgwr_execute", "Execute",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                      ),
                      width = 4,
                ),
              mainPanel(
                    tabsetPanel(id = "mgwr_tabs",
                                type = "tabs",
                                tabPanel("Model Introduction",
                                          fluidPage(
                                            # 标题
                                            div(
                                              style = "text-align: center; margin-top: 20px;",
                                              h2("Multiscale Geographically Weighted Regression (MGWR)", style = "color: #2C3E50; font-weight: bold;")
                                            ),

                                            fluidRow(
                                              # Model Overview 模块占满整行
                                              column(
                                                width = 12,  
                                                div(
                                                  style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px; margin: 5px; font-size: 16px;",
                                                  fluidRow(
                                                    column(
                                                      width = 9,  # 左侧文本
                                                      h3("Model Overview", style = "color: #34495E; font-weight: bold;"),
                                                      p("Multiscale Geographically Weighted Regression (MGWR) is an extension of GWR that allows different regression coefficients to vary at different spatial scales. 
                                                        This method better captures the complexity of spatial data by considering the spatial heterogeneity and scale effects of different variables."),
                                                      p("Unlike traditional GWR, which assumes a single bandwidth for all variables, MGWR assigns unique bandwidths to each predictor, leading to more accurate and interpretable results.")
                                                    ),
                                                    column(
                                                      width = 3,  # 右侧图片
                                                      div(
                                                        style = "text-align: center;",
                                                        img(src = "mgwr.jpg", width = "100%", 
                                                            style = "max-width: 200px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);")
                                                      )
                                                    )
                                                  )
                                                )
                                              ),

                                              # Workflow 占满整行
                                              column(
                                                width = 12, 
                                                div(
                                                  style = "padding: 15px; margin: 5px;",
                                                  h3("Workflow", style = "color: #34495E; font-weight: bold; text-align: left;"),
                                                  tags$ul(
                                                    style = "font-size: 18px; padding-left: 20px;",
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("1. Upload Data:"), " Import spatial datasets for regression analysis.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("2. Select Parameters:"), " Choose independent and dependent variables, set spatial weighting parameters.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("3. Configure Analysis:"), " Adjust bandwidths for each predictor and compute spatially varying regression coefficients."))
                                                  )
                                                )
                                              )
                                            ),

                                            # 结果释义
                                            div(
                                              style = "background-color: #F8F9F9; padding: 5px; border-radius: 10px; margin: 5px; font-size: 18px;",
                                              h3("Result Interpretation", style = "color: #34495E; font-weight: bold;"),
                                              tags$ul(
                                                tags$li(strong("Intercept:"), " Represents the predicted value of the dependent variable when all independent variables are zero. 
                                                        In MGWR, the intercept varies across different locations, reflecting localized baseline values."),
                                                tags$li(strong("Variable:"), " A coefficient representing an independent variable that explains variations in the dependent variable. 
                                                        In MGWR, variable coefficients change spatially, with each predictor having its own optimal bandwidth."),
                                                tags$li(strong("Yhat:"), " The predicted value of the dependent variable computed based on independent variables and their regression coefficients."),
                                                tags$li(strong("Residual:"), " The difference between the observed and predicted values (Y - Yhat). Smaller residuals indicate a better model fit."),
                                                tags$li(strong("Intercept_SE:"), " Standard error of the intercept, indicating its estimation precision (lower values mean higher reliability."),
                                                tags$li(strong("Variable_SE:"), " Standard error of a variable's coefficient, showing uncertainty in its effect size."),
                                                tags$li(strong("Intercept_TV:"), " T-value of the intercept, testing if it is statistically different from zero (|t| > 1.96 suggests significance."),
                                                tags$li(strong("Variable_TV:"), " T-value of a variable's coefficient, assessing its significance in explaining the dependent variable.")
                                              )
                                            )
                                          )
                                        ),
                                tabPanel("Summary Information", verbatimTextOutput("mgwr_modelDiagnostics")),
                                tabPanel("Web Map",
                                        leaflet::leafletOutput("mgwr_mapPlot", height = "700px"),
                                        absolutePanel(id = "mgwr_map_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("mgwr_map_layer"),
                                                        actionButton("mgwr_map_layer_execute", "Select",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                                                        br(),br(),
                                                        radioButtons("mgwr_audio_mode", "Select the audio generation mode", 
                                                                          choices = c("Click to generate audio" = "click", "Connect to generate audio" = "line")),
                                                        # 当选择连线模式时显示确认按钮
                                                        conditionalPanel(
                                                          condition = "input.mgwr_audio_mode == 'line'",
                                                          numericInput("mgwr_buffer_length", label = "Buffer length", value = 1, min = 0.0000000000001, step = 1),
                                                          actionButton("mgwr_confirm_audio", "Create a connected audio link", width = "100%", class = "btn btn-primary", style = "color: white; "),
                                                          actionButton("mgwr_confirm_audio_clear", "Clear", width = "50%", class = "btn btn-primary", style = "color: white; "),
                                                        ),
                                                      ),
                                                    ),
                                        div(id = "mgwr_Significant_line_div",HTML("
                                          <div style='position:absolute; bottom:90px; right:120px; background:white; padding:10px; border:1px solid #ccc;'>
                                            <span><b> Significant level </b></span><br>
                                            <span class='solid-line'></span> Significance<br>
                                            <span class='dashed-line'></span> Non-significance
                                          </div>
                                        ")),
                                        div(id = "mgwr_Significant_point_div",HTML("
                                          <div style='position:absolute; bottom:90px; right:120px; background:white; padding:10px; border:1px solid #ccc;'>
                                            <span><b> Significant level </b></span><br>
                                            <span class='fa-regular fa-circle'></span> Significance<br>
                                            <span class='fa-solid fa-xmark'></span> Non-significance
                                          </div>
                                        ")),
                                        tags$audio(id = "mgwr_map_audio",type = "audio/wav", controls = TRUE, autoplay = TRUE, src = "")
                                        ),
                                tabPanel("Multiple Visualizations",
                                          uiOutput("mgwr_Plot"),
                                          absolutePanel(id = "mgwr_plot_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("mgwr_columns"),
                                                      ),
                                                    ),
                                          ),
                                tabPanel("Table View",br(),DT::dataTableOutput("mgwr_summaryTable")),

                    ),
                    width = 8,
              ),
      ),
      # GWPCA
      shinydashboard::tabItem(tabName = "gwpca",
              shinyjs::useShinyjs(),
              sidebarPanel(
                      h2("GWPCA Workflow",align = "center"),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "1. Upload Data"),
                        fileInput("gwpca_shapefile","Choose Shapefile ZIP file",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                        # fileInput("gwpca_cp_shapefile","Choose Evaluation locations Shapefile ZIP file(Optional)",accept = c('.zip'), multiple = FALSE,buttonLabel = "Choose ZIP",placeholder = "No file selected"),
                      ),
                      div(class = "parameter-section",
                        div(class = "parameter-title", "2. Select Parameters and Analysis"),
                            uiOutput("gwpca_vars"),
                            numericInput("gwpca_k", label = "Number of retained vars(k)",value = 2, min = 2, max = 2 ),
                            # checkboxInput("gwpca_best_bandwidth", "Optimal bandwidth", value = FALSE),
                            fluidRow(
                              column(4,selectizeInput("gwpca_kernel", "Kernel Type",choices = c("gaussian", "exponential", "bisquare", "tricube", "boxcar"), multiple = FALSE), ),
                              column(4,selectizeInput("gwpca_adaptive", "Adaptive",choices = c("TRUE","FALSE"), multiple = FALSE), ),
                              column(4,selectizeInput("gwpca_color_palette", "Color Palette",
                                      choices = list(
                                        "Green-Red" = "green-red",
                                        "Blue-Yellow" = "blue-yellow",
                                        "Purple-Orange" = "purple-orange",
                                        "Magenta-Cyan" = "magenta-cyan",
                                        "Pink-Blue" = "pink-blue",
                                        "Viridis" = "viridis"
                                      ),
                                      selected = "green-red"
                                    ),
                                  ),
                            ),
                            fluidRow(
                              column(6,checkboxInput("gwpca_glyph_add", "Add all components", FALSE),),
                              column(6,checkboxInput("gwpca_glyph_sep", "Separation contrast", FALSE),),
                            ),
                            div(id = "gwpca_bandwidth_div", sliderInput("gwpca_bandwidth", "Bandwidth(Number of points)", min = 1, max = 100, value = 20)),  # 将滑块放在div中 # 初始最大值设为100，稍后在server中更新
                            # 按钮，点击时调用函数
                            actionButton("gwpca_execute", "Execute",width = "100%", class = "btn btn-primary" ,style = "color: white;  margin-bottom: 10px;"), 
                      ),
                      width = 4,
                ),
              mainPanel(
                    tabsetPanel(id = "gwpca_tabs",
                                type = "tabs",
                                tabPanel("Model Introduction",
                                          fluidPage(
                                            # 标题
                                            div(
                                              style = "text-align: center; margin-top: 20px;",
                                              h2("Geographically Weighted Principal Components Analysis (GWPCA)", style = "color: #2C3E50; font-weight: bold;")
                                            ),

                                            fluidRow(
                                              column(
                                                width = 12,  
                                                div(
                                                  style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px; margin: 5px; font-size: 16px;",
                                                  fluidRow(
                                                    column(
                                                      width = 9,  # 左侧文本
                                                      h3("Model Overview", style = "color: #34495E; font-weight: bold;"),
                                                      p("Geographically Weighted Principal Components Analysis (GWPCA) is a spatially adaptive dimensionality reduction technique that combines 
                                                        the concepts of traditional Principal Components Analysis (PCA) with geographically weighted modeling."),
                                                      p("Unlike conventional PCA, which assumes a single global transformation, GWPCA computes localized principal components for each spatial location, 
                                                        capturing the spatial heterogeneity and regional variations in data structure.")
                                                    ),
                                                    column(
                                                      width = 3,  # 右侧图片
                                                      div(
                                                        style = "text-align: center;",
                                                        img(src = "gwpca.jpg", width = "100%", 
                                                            style = "max-width: 200px; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);")
                                                      )
                                                    )
                                                  )
                                                )
                                              ),

                                              # Workflow 占满整行
                                              column(
                                                width = 12, 
                                                div(
                                                  style = "padding: 10px; margin: 5px;",
                                                  h3("Workflow", style = "color: #34495E; font-weight: bold; text-align: left;"),
                                                  tags$ul(
                                                    style = "font-size: 18px; padding-left: 20px;",
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("1. Upload Data:"), " Import spatial datasets for principal components analysis.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("2. Select Parameters:"), " Define the number of principal components and set spatial weight parameters.")),
                                                    div(style = "padding-bottom: 5px;", tags$li(strong("3. Configure Analysis:"), " Compute localized principal components and visualize spatial variations."))
                                                  )
                                                )
                                              )
                                            ),


                                            # 结果释义
                                            div(
                                              style = "background-color: #F8F9F9; padding: 5px; border-radius: 10px; margin: 5px; font-size: 18px;",
                                              h3("Result Interpretation", style = "color: #34495E; font-weight: bold;"),
                                              tags$ul(
                                                tags$li(strong("Comp.1_PV:"), " Proportion of variance explained by the first principal component in a local area, indicating its importance in capturing data variability at that location."),
                                                tags$li(strong("Comp.2_PV:"), " Proportion of variance explained by the second principal component locally, showing its contribution to regional data patterns."),
                                                tags$li(strong("Local_CP:"), " Local principal components calculated for each spatial location. These localized components reflect 
                                                        regional structures and variations in data, highlighting spatial heterogeneity."),
                                                tags$li(strong("Win_Var_PC1:"), " The window variance of the first principal component. This metric evaluates the variance within 
                                                        a defined window, helping assess the stability and explanatory power of the first principal component across different spatial regions.")
                                              )
                                            )
                                          )
                                        ),
                                tabPanel("Summary Information", verbatimTextOutput("gwpca_modelDiagnostics")),
                                tabPanel("Web Map",
                                        leaflet::leafletOutput("gwpca_mapPlot", height = "700px"),
                                        absolutePanel(id = "gwpca_map_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("gwpca_map_layer"),
                                                        actionButton("gwpca_map_layer_execute", "Select",width = "100%", class = "btn btn-primary" ,style = "color: white; "),
                                                        br(),br(),
                                                        radioButtons("gwpca_audio_mode", "Select the audio generation mode", 
                                                                          choices = c("Click to generate audio" = "click", "Connect to generate audio" = "line")),
                                                        # 当选择连线模式时显示确认按钮
                                                        conditionalPanel(
                                                          condition = "input.gwpca_audio_mode == 'line'",
                                                          numericInput("gwpca_buffer_length", label = "Buffer length", value = 1, min = 0.0000000000001, step = 1),
                                                          actionButton("gwpca_confirm_audio", "Create a connected audio link", width = "100%", class = "btn btn-primary", style = "color: white; "),
                                                          actionButton("gwpca_confirm_audio_clear", "Clear", width = "50%", class = "btn btn-primary", style = "color: white; "),
                                                        ),
                                                      ),
                                                    ),
                                         tags$audio(id = "gwpca_map_audio",type = "audio/wav", controls = TRUE, autoplay = TRUE, src = "")
                                        ),
                                tabPanel("Multiple Visualizations",
                                          uiOutput("gwpca_Plot"),
                                          absolutePanel(id = "gwpca_plot_panel",
                                                      fixed = TRUE,  # fixed = TRUE 固定在页面上，不随浏览器窗口变化
                                                      draggable = TRUE,  # draggable = TRUE 允许拖动
                                                      top = 111, right = 30,  # 设置面板的位置
                                                      width = 300, height = 150,  # 面板的大小
                                                      style = "background-color: rgba(255, 255, 255, 0.4);",
                                                      wellPanel(
                                                        style = "background-color: rgba(255, 255, 255, 0.4);",
                                                        uiOutput("gwpca_columns"),
                                                      ),
                                                    ),
                                          ),
                                tabPanel("Glygh Plot",
                                          plotOutput(
                                          outputId = "gwpca_glyph_plot",
                                          height   = "700px"    # 建议使用具体像素，以避免百分比在 HTML/CSS 中表现不一
                                          ),
                                        ),     
                                tabPanel("Table View",br(),DT::dataTableOutput("gwpca_summaryTable")),
                    ),
                    width = 8,
                ),
      )


    )
  )
)