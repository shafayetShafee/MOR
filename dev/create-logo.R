library(hexSticker)
library(cowplot)
library(showtext)
library(magick)

font_add_google("Bebas Neue")

png <- ggdraw() + draw_image("dev/logo_cropped_no_bg.png", scale = 1.65)

logo_obj <- sticker(
  subplot  = png,
  package  = "MOR",
  s_x      = 1,
  s_y      = 0.85,
  s_height = 0.6,
  s_width  = 1.8,
  p_color  = "#C2185B",
  p_family = "Bebas Neue",
  p_size   = 60,
  p_y      = 1.5,
  h_fill   = '#f7e1ea',
  h_color  = "#C2185B",
  h_size   = 2,
  url      = "github.com/shafayetShafee/MOR",
  u_color  = "#AD1457",
  u_size   = 8,
  dpi = 800
)

logo_obj

#
# logo_obj <- sticker(
#   subplot  = png,
#   package  = "MOR",
#   s_x      = 1,
#   s_y      = 0.85,
#   s_height = 0.6,
#   s_width  = 1.8,
#   p_color  = "#C2185B",
#   p_family = "Bebas Neue",
#   p_size   = 120,
#   p_y      = 1.5,
#   h_fill   = '#f7e1ea',
#   h_color  = "#C2185B",
#   h_size   = 2,
#   url      = "github.com/shafayetShafee/MOR",
#   u_color  = "#AD1457",
#   u_size   = 16.5,
#   dpi = 800
# )
#
# logo_obj
#
# ggplot2::ggsave(
#   filename = "dev/mor.png",
#   plot     = logo_obj,
#   dpi      = 800,
#   units    = "in"
# )
#
# img <- image_read("dev/mor.png")
#
# img_resized <- image_resize(img, "240x278")
#
# image_write(img_resized, "dev/mor_small.png", density = 800)

