
convert_file_type <- function(path, folder, from_format, to_format) {

  img <- magick::image_read(here::here("docs", folder, path))
  new_path <- stringr::str_replace(path, paste0(from_format, "$"), to_format)
  magick::image_write(img,
                      format = to_format,
                      path = here::here("docs", folder, new_path)
  )
  rm(img)
  gc()
  cat(new_path, "\n")
}

convert_file <- function(path, input_size, output_size) {

  img <- magick::image_read(here::here("docs", input_size, path))
  img <- magick::image_resize(img, paste0(output_size, "x", output_size))
  magick::image_write(img,
    path = here::here("docs", output_size, path)
  )
  rm(img)
  gc()
  cat(path, "\n")
}



images <- list.files(here::here("docs", "5000"))
#purrr::walk(images, ~convert_file_type(.x, 5000, "png", "jpg"))
purrr::walk(images, ~convert_file(.x, 5000, 800))
