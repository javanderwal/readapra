# Install required packages
install.packages("hexSticker", "rsvg")

# Path to hex sticker image
imgurl <- "~/R/readapra/data-raw/readapra_hex_sticker_image.svg"

# Generate readapr hex sticker
hexSticker::sticker(
  subplot = imgurl,
  s_x = 1,
  s_y = .84,
  s_width = .4,
  s_height = 0.9,
  package = "readapra",
  p_x = 1,
  p_y = 1.55,
  p_color = "white",
  p_family = "sans",
  p_size = 21,
  h_size = 1.2,
  h_fill = "#012169",
  h_color = "#2B6EBD",
  filename = "man/figures/readapra_hex_sticker.png"
)
