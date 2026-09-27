# =============================================================================
# surveyverse hex stickers
# Packages needed: hexSticker, cowplot, showtext, ggplot2
#
# install.packages(c("hexSticker", "cowplot", "showtext", "ggplot2"))
#
# Each sticker assumes you have a PNG icon file in your working directory.
# Suggested free icon sources:
#   - https://www.flaticon.com
#   - https://thenounproject.com
#   - https://icons8.com
# Export icons as PNG with a transparent background at ~512x512px.
# =============================================================================

# load the libraries
library(hexSticker)
library(cowplot)
library(sysfonts)
library(showtext)
library(ggplot2)


# load the font
font_add(
  family = "MonaNeon",
  regular = "~/Downloads/Static Fonts/Monaspace Neon/MonaspaceNeon-Semibold.otf",
  bold = "/Users/jacobdennen/Downloads/Static Fonts/Monaspace Neon/MonaspaceNeon-Bold.otf"
)

showtext_auto()

# Load the icon
icon_core <- ggdraw() + draw_image("man/figures/icon.png", scale = 1.35)

# create the sticker
sticker(
  subplot = icon_core,
  package = "surveycore",

  # ── Icon position & size ──────────────────────────────────────────────────
  s_x = 1.1, # horizontal center (1 = middle)
  s_y = 0.72, # vertical position (lower = further down)
  s_width = 0.65, # icon width  — increase to make icon larger
  s_height = 0.65, # icon height — keep same ratio as s_width

  # ── Package name ──────────────────────────────────────────────────────────
  p_color = "white",
  p_family = "MonaNeon",
  # p_fontface = "regular",
  p_size = 18, # font size — may need tuning based on font choice
  p_y = 1.43, # vertical position of text (higher = further up)

  # ── Hex background & border ───────────────────────────────────────────────
  h_fill = "#6666CC", # periwinkle fill
  h_color = "#4444AA", # darker periwinkle border

  # ── Optional URL at bottom ────────────────────────────────────────────────
  url = "", # set to "" to remove
  u_color = "white",
  u_size = 3.5,

  # ── Output file ───────────────────────────────────────────────────────────
  filename = "man/figures/logo.png",
  dpi = 300
)

# =============================================================================
# QUICK-TWEAK GUIDE
# =============================================================================
#
# FONT        Change font_add_google("Oxanium") to any Google Font name.
#             Preview fonts at: https://fonts.google.com
#             Popular choices for R packages:
#               "Fira Code", "Nunito", "Raleway", "Ubuntu Mono", "Cabin"
#
# COLORS      h_fill  = hex background color
#             h_color = hex border color (usually a darker shade of h_fill)
#             p_color = package name text color
#             Pick coordinated palettes at: https://coolors.co
#
# ICON SIZE   s_width / s_height control how large the icon appears.
#             s_x / s_y control where it sits (1/1 = dead center).
#             If the icon looks clipped, reduce s_width slightly.
#
# TEXT SIZE   p_size controls the package name font size.
#             Longer names like "surveywts" need a smaller p_size (~14-16).
#             Shorter names like "surveycore" can go bigger (~18-22).
#
# TEXT POSITION p_y = 1.43 puts the name near the bottom.
#               Increase toward 1.5 to push it closer to the bottom edge.
#
# PREVIEW     After running sticker(), open the PNG to preview.
#             The saved PNG dimensions match http://hexb.in/sticker.html
#             exactly (2 inches wide at 300 DPI = 600px).
# =============================================================================
