library(targets)

tar_source()

list(
  tarchives::tar_target_archive(
    data,
    package = "tarchives",
    pipeline = "example-model"
  ),
  tarchives::tar_target_archive(
    model,
    package = "tarchives",
    pipeline = "example-model"
  ),
  tar_target(
    plot,
    get_plot(
      data = data, 
      model = model
    )
  )
)
