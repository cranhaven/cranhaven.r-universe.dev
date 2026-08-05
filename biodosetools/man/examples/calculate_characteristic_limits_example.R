
control_data <- c(aberr = 4,
                  cells = 1000)

calculate_characteristic_limits(
      y0 = control_data["aberr"],
      n0 = control_data["cells"],
      n1 = 20,
      alpha = 0.05,
      beta = 0.1,
      ymax = 100,
      type = "var"
)
