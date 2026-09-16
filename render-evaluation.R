# %% ---------------------------------------------------------------------------
# Render the evaluation dashboards.
#
# Two reports come out of the one evaluation.Rmd, which differ only in the
# `flow_caps` parameter:
#
#   evaluation.html      all flow conditions
#   evaluation_cap.html  restricted to days that stayed entirely below the
#                        low-flow caps (Wilder and Bellows Falls 11,000 cfs,
#                        Vernon 14,000 cfs)
#
# Parameterizing one document rather than keeping a second copy of it is
# deliberate: the earlier standalone capped report had already drifted from the
# main one (a misspelled `persistance` benchmark, a missing location filter, and
# a metric-independent no-skill line).
#
# The dashboard must be rendered here rather than with Quarto. Its collapsible
# sections use the R Markdown `{.tabset}` heading attribute, which Quarto does
# not implement -- `quarto render evaluation.Rmd` produces a page whose tab
# groups are flattened into a long list of headings with no tab strip.
#
# Both outputs are self-contained (figures and CSS embedded), so either can be
# emailed or dropped on a share as a single file.
# %% ---------------------------------------------------------------------------

# Low-flow caps, in cfs, applied per location per day.
flow_caps <- list(wilder = 11000, bellows = 11000, vernon = 14000)

report_format <- rmarkdown::html_document(
  toc = TRUE,
  toc_float = TRUE,
  theme = "sandstone",
  css = "evaluation.css",
  self_contained = TRUE
)

render_report <- function(output_file, caps, subtitle) {
  rmarkdown::render(
    "evaluation.Rmd",
    output_format = report_format,
    output_file = output_file,
    params = list(flow_caps = caps, subtitle = subtitle),
    # A fresh session per report, so the two renders cannot leak objects into
    # each other and quietly produce a capped figure in the uncapped report.
    envir = new.env()
  )
}

render_report("evaluation.html", NULL, "All flow conditions")
render_report(
  "evaluation_cap.html",
  flow_caps,
  paste(
    "Low-flow days only: days on which observed inflow stayed below",
    "11,000 cfs at Wilder and Bellows Falls and below 14,000 cfs at Vernon"
  )
)
