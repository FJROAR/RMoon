#' @title MoonPhasesImages
#'
#' @description This allows to manage 58 images taken during a complete Lunar cicle
#' taken between 11-03-2024 and 09-04-2024. For a 29.5 lunar cicle, these images shows,
#' the moon with an interval of 12 hours
#'
#'
#' @param phase Integer number between 1 - 58, two moon phases by day aprox
#' @param download boolean value F or T
#' @param path if download = T, it points where the file will be stored
#' @param size by default = 400, it can be changed by the user
#'
#'
#' @return If download = F, the function return the image itself but,
#' if download = T the function downloads the selected image in a path pointed by
#' the path parameter
#'
#' @examples
#'
#' Image <- MoonPhasesImages(7,
#'                           download = F,
#'                           path = "",
#'                           size = 400)
#'
#' @references
#' https://svs.gsfc.nasa.gov/gallery/moonphase
#'
#' @export

MoonPhasesImages <- function(phase,
                               download = F,
                               path = "",
                               size = 400){

  if (phase < 1 |
      phase > 58 |
      abs(phase - floor(phase)) > 0){

    print("Only a integer between 1 y 58 is allowed")
  } else {

    library(magick)

    if(download == F){

      imagesPath <- system.file(package = "RMoon")
      imagesPath <- file.path(imagesPath, "images")

      file = paste0("moon_", phase, ".jpg")

      MoonImage <- image_scale(image_read(paste0(imagesPath, "/", file)),
                               paste0("x", size))

      print(MoonImage)

      return(MoonImage)

    } else {

      imagesPath <- system.file(package = "RMoon")
      imagesPath <- file.path(imagesPath, "images")

      file = paste0("moon_", phase, ".jpg")

      MoonImage <- image_scale(image_read(paste0(imagesPath, "/", file)),
                               paste0("x", size))

      image_write(MoonImage,
                  paste0(path, paste0("moon_", phase, ".jpg")),
                  format = 'jpeg')

      print("Image downloaded")

    }

  }

}
