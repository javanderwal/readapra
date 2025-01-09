# Install required packages
install.packages("hexSticker", "rsvg")

# Load required packages
library(hexSticker)
library(rsvg)
library(svglite)

# Path to hex sticker image
imgurl <- "~/R/readapra/data-raw/readapra_hex_sticker_image.svg"

# Generate readapr hex sticker
sticker(
  subplot = imgurl,
  s_x = 1,
  s_y = .84,
  s_width = .42,
  s_height = 0.9,
  package = "readapra",
  p_x = 1,
  p_y = 1.57,
  p_color = "white",
  p_family = "sans",
  p_size = 7,
  h_size = 1.2,
  h_fill = "#012169",
  h_color = "#2B6EBD",
  filename = "inst/figures/readapra_hex_sticker.svg"
)
