## ---- include = FALSE, message=FALSE, results='hide'--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
library(threeBrain)

## ----setup--------------------------------------------------------------------
#  library(threeBrain)
#  subject_code <- "N27"
#  subject_path <- "~/Downloads/N27"
#  brain <- freesurfer_brain2(subject_path, subject_code)
#  print(brain)
#  #> Subject - N27
#  #> Transforms:
#  #>
#  #> - FreeSurfer TalXFM [from scanner to MNI305]:
#  #>        [,1]    [,2]    [,3]    [,4]
#  #> [1,] 0.9692 -0.0029 -0.0134 -0.1638
#  #> [2,] 0.0062  0.9685  0.0492 -2.0717
#  #> [3,] 0.0145  0.0276  0.9541  0.1361
#  #> [4,] 0.0000  0.0000  0.0000  1.0000
#  #>
#  #> - Torig [Voxel CRS to FreeSurfer origin, vox2ras-tkr]
#  #>      [,1] [,2] [,3] [,4]
#  #> [1,]   -1    0    0  128
#  #> [2,]    0    0    1 -128
#  #> [3,]    0   -1    0  128
#  #> [4,]    0    0    0    1
#  #>
#  #> - Norig [Voxel CRS to Scanner center, vox2ras]
#  #>      [,1] [,2] [,3]   [,4]
#  #> [1,]   -1    0    0  128.5
#  #> [2,]    0    0    1 -145.5
#  #> [3,]    0   -1    0  146.5
#  #> [4,]    0    0    0    1.0
#  #>
#  #> - Scanner center relative to FreeSurfer origin
#  #> [1]  -0.5  17.5 -18.5
#  #>
#  #> - FreeSurfer RAS to MNI305, vox2vox-MNI305
#  #>        [,1]    [,2]    [,3]      [,4]
#  #> [1,] 0.9692 -0.0029 -0.0134   0.12365
#  #> [2,] 0.0062  0.9685  0.0492 -18.10715
#  #> [3,] 0.0145  0.0276  0.9541  17.31120
#  #> [4,] 0.0000  0.0000  0.0000   1.00000
#  #> Surface information (total count 1)
#  #> Loading required namespace: rstudioapi
#  #>   pial [ std.141 ]
#  #> Volume information (total count 1)
#  #>   T1

## -----------------------------------------------------------------------------
#  brain$plot()

## -----------------------------------------------------------------------------
#  brain <- freesurfer_brain2(
#    subject_path, subject_code,
#    surface_types = c('pial', 'smoothwm'),
#    atlas_types = 'aseg')
#  
#  brain$plot(
#    controllers = list(
#      "Voxel Type" = "aseg",
#      "Voxel Label" = "4,5,6,7",
#      "Surface Type" = "smoothwm",
#      "Left Opacity" = 0.4,
#      "Overlay Coronal" = TRUE
#    ),
#    control_display = FALSE,
#    camera_pos = c(0, -500, 0)
#  )

