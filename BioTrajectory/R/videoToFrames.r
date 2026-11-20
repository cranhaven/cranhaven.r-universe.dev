#' Extract Frames from a Video and Save as Images
#'
#' This function is a wrapper that simplifies extracting frames from a video. It processes the video file and saves the frames as PNG images in the specified directory. By default, it extracts 15 frames per second.
#'
#' @param videoPath Path to the video file.
#' @param outputDir Path to the directory where the extracted images will be saved.
#' @param fps The number of frames shown per second in the video. By default, it extracts 15 frames per second.
#'
#' @return A list of file paths to the generated images.
#' 
#' @examples
#' \donttest{
#' # Not run:
#' videoPath <- system.file('extdata/video.mp4', package='BioTrajectory')
#' outputDir <- system.file('extdata/frames/', package='BioTrajectory')
#' images <- videoToFrames(videoPath, outputDir, fps=15)
#' print(images)  # Displays the paths to the generated images
#' }
#' @export

videoToFrames <- function(videoPath, outputDir, fps=15) {
  av_video_images(videoPath, destdir = outputDir, format = "png", fps=fps)
  listImages <- list.files(outputDir, pattern = "\\.(jpg|jpeg|png|tiff)$", full.names = TRUE)
  return(listImages)
}
