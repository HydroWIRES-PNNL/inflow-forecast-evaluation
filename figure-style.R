# %% ---------------------------------------------------------------------------
# Shared figure styling for the manuscript figures.
#
# Sourced by make-metric-figures.R (figures 3 and 4) and make-revision-figures.R
# (figures 5 and 6) so the two sets cannot drift apart again. Before this file
# existed the paper carried two unrelated palettes: figures 3 and 4 used the
# dashboard's five-colour set (#9FBB73, #F3B664, #c44900, #2C4E80, #4793AF) while
# figures 5 and 6 used a brown/teal set.
#
# The three forecast products get the categorical hues. `perfect` and
# `persistence` are benchmarks derived in the code rather than products, so they
# are drawn as neutral reference lines: greys keep them from competing with the
# products for attention, and the palette only has to validate over the three
# hues that actually carry identity.
#
# The product hues are validated as a set over all pairs -- worst colourblind
# separation dE 9.2, worst normal-vision dE 24.0 (OKLab x100). The previous
# figure 5 palette put B and GRH on adjacent teals that both fell below the
# chroma floor, so they read as near-greys and were nearly inseparable where the
# error bands overlapped.
#
# Identity is carried by dash pattern as well as by hue, so every figure stays
# readable in greyscale and under colourblindness.
# %% ---------------------------------------------------------------------------

product_levels <- c("A", "B", "GRH")
benchmark_levels <- c("perfect", "persistence")
source_levels <- c(product_levels, benchmark_levels)

source_colors <- c(
  A = "#2a78d6",
  B = "#eb6834",
  GRH = "#1baf7a",
  perfect = "grey45",
  persistence = "grey20"
)

source_linetypes <- c(
  A = "solid",
  B = "22",
  GRH = "42",
  perfect = "longdash",
  persistence = "dotdash"
)

source_shapes <- c(A = 16, B = 17, GRH = 15, perfect = 4, persistence = 3)

locations <- c("wilder", "bellows", "vernon")
location_labels <- c(
  wilder = "Wilder",
  bellows = "Bellows Falls",
  vernon = "Vernon"
)

label_locations <- function(x) {
  factor(
    as.character(x),
    levels = locations,
    labels = location_labels[locations]
  )
}

# Common theme for every manuscript figure: cream facet strips, no minor grid,
# legend on top.
theme_paper <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.9, "lines"),
      strip.background = ggplot2::element_rect(
        fill = "#EEE8D5",
        color = "grey50"
      )
    )
}

# Colour, linetype and fill scales sharing one legend title, so identity never
# rests on hue alone.
scale_source <- function(name = "Forecast", fill = FALSE, shape = FALSE) {
  out <- list(
    ggplot2::scale_color_manual(name, values = source_colors, drop = FALSE),
    ggplot2::scale_linetype_manual(name, values = source_linetypes, drop = FALSE)
  )
  if (fill) {
    out <- c(
      out,
      list(ggplot2::scale_fill_manual(name, values = source_colors, drop = FALSE))
    )
  }
  if (shape) {
    out <- c(
      out,
      list(ggplot2::scale_shape_manual(name, values = source_shapes, drop = FALSE))
    )
  }
  out
}
