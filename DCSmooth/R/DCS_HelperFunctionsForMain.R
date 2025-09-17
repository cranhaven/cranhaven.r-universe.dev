################################################################################
#                                                                              #
#                DCSmooth Package: Additional Utility Functions                #
#                                                                              #
################################################################################

# Functions which are used as additional or auxiliary functions and do not
# belong to another category

# .plotly.3d
# plot.dcs.colors

#----------------------Function for 3d plots using plotly----------------------#

.plotly.3d = function(Y, X = 1, T = 1,
                     color = c("#444C5C", "#78A5A3", "#E1B16A", "#CE5A57"),
                     x_lab = "X", t_lab = "T", y_lab = "Value", 
                     showaxes = TRUE)
{
  if (class(Y)[1] == "dcs")
  {
    Y_data = Y$M
    X0 = Y$X
    T0 = Y$T
  } else {
    Y_data = Y
    
    if(length(X) == 1)
    {
      X0 = seq(from = 0, to = 1, length.out = dim(Y_data)[1])
    } else {
      X0 = X
    }
    if (length(T) == 1)
    {
      T0 = seq(from = 0, to = 1, length.out = dim(Y_data)[2])
    } else {
      T0 = T
    }
  }
  
  f1 = list(family = "Computer Modern", size = 14, color = "black")
  f2 = list(family = "Courier New", size = 14, color = "black")
  
  if (showaxes == TRUE)
  {
    # axx = list(titlefont = f1, tickfont = f2, title = t_lab,
    #           gridcolor = "lightgray", zerolinecolor = "black")
    # axy = list(titlefont = f1, tickfont = f2, title = x_lab,
    #           gridcolor = "lightgray", zerolinecolor = "black",
    #           autorange = "reversed")
    # axz = list(titlefont = f1, tickfont = f2, title = y_lab,
    #           backgroundcolor = "white",  gridcolor = "lightgray",
    #           showbackground = TRUE, zerolinecolor = "black",
    #           tickformat = ".1e")
    axx = list(title = t_lab, gridcolor = "lightgray", zerolinecolor = "black",
               automargin = TRUE)
    axy = list(title = x_lab, gridcolor = "lightgray", zerolinecolor = "black",
               autorange = "reversed", automargin = TRUE)
    axz = list(title = y_lab, backgroundcolor = "white",  gridcolor = "lightgray",
               showbackground = TRUE, zerolinecolor = "black",
               tickformat = ".1e", automargin = TRUE) #, tickmode = "linear", dtick = 4000, tick0 = 2000)
  } else {
    axx = list(title = "", showticklabels = FALSE, gridcolor = "lightgray",
               zerolinecolor = "black")
    axy = list(title = "", showticklabels = FALSE, gridcolor = "lightgray", 
               zerolinecolor = "black", autorange = "reversed")
    axz = list(title = "", showticklabels = FALSE, backgroundcolor = "white",
               gridcolor = "lightgray", showbackground = TRUE, 
               zerolinecolor = "black")
  }
  scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
             camera = list(eye = list(x = -1.8, y = 1.2, z = 1),
                           projection = list(type = "orthographic")),
             autosize = FALSE)
  # projection can be "projection" or "orthographic"
  
  fig = plotly::plot_ly(x = T0, y = X0, z = Y_data, showscale = FALSE,
              width = 600, height = 600)
  fig = plotly::add_surface(fig, colors = color)
  fig = plotly::layout(fig, scene = scene, margin = list(l = 100))
  
  return(fig)
}

plot.dcs.colors = function(Y, color, n_color = 100)
{
  Y_color = Y - min(Y)
  Y_color = Y_color/max(Y_color) * (n_color - 1) + 1
  color_fcn = grDevices::colorRampPalette(colors = color)
  color_grad = color_fcn(n_color)
  color_out = matrix(color_grad[Y_color])
  return(color_out)
}
