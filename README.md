# Inflow Forecast Evaluation

A collection of scripts designed to evaluate real-time reservoir inflow forecast products for the Great River Hydro (GRH) facilities, as well as to visualize evaluation metrics in an HTML dashboard where users can dynamically switch their statistics based on seasonality or applied criteria. The dataset required to run the scripts is available at the following link: [http://zenodo.org/record/16921728](https://zenodo.org/records/16921728) (DOI: 10.5281/zenodo.16921728)

---

## Code Descriptions
   * [evaluation.Rmd](./evaluation.Rmd): read anonymized inflow forecasts and processed observations to calculate evaluation metrics and generate an HTML dashboard
   * [render-evaluation.R](./render-evaluation.R): render the dashboard, in both the all-conditions and low-flow variants
   * [evaluation.css](./evaluation.css): page width and layout for the dashboard
   * [verify-all.R](./verify-all.R): reproduce the quantitative results reported in the paper, including Table 1, the smoothing sensitivity analysis, and the cumulative forebay exceedance probabilities
   * [make-metric-figures.R](./make-metric-figures.R): regenerate the lead-time and by-month performance figures (Figures 3 and 4) from the published data
   * [make-revision-figures.R](./make-revision-figures.R): regenerate the cumulative forebay error and exceedance figures (Figures 5 and 6) from the published data
   * [figure-style.R](./figure-style.R): palette, linetypes, and theme shared by the dashboard and the two figure scripts, so the manuscript figures and the dashboard cannot drift apart
   * [exploratory-plots.R](./exploratory-plots.R): (optional) plot process observations compared to a series of inflow forecasts, outputting to ./plot/exploratory/
   * [paper/](./paper): the manuscript, as a git submodule

---
## Code Execution Steps
   1. Download anonymized and processed data from [Zenodo](https://zenodo.org/records/16921728) in [./processed-data](./processed-data/)
   2. Run `Rscript render-evaluation.R` to evaluate inflow forecasts and generate the dashboards
   3. Run `verify-all.R` to reproduce the numbers reported in the paper
   4. Run `make-metric-figures.R` and `make-revision-figures.R` to regenerate Figures 3--6

`render-evaluation.R` produces two self-contained reports from the one `evaluation.Rmd`,
which differ only in the `flow_caps` parameter:

| Output | Sample |
| --- | --- |
| `evaluation.html` | all flow conditions |
| `evaluation_cap.html` | only days on which observed inflow stayed below 11,000 cfs at Wilder and Bellows Falls and below 14,000 cfs at Vernon |

The cap is applied per location per day: if the observed flow at a location exceeds its
cap at any hour of a day, that whole day is dropped for that location. This keeps 85% of
forecast-hours at Wilder, 64% at Bellows Falls, and 67% at Vernon.

The dashboard must be rendered by **rmarkdown, not Quarto**. Its collapsible sections
use the R Markdown `{.tabset}` heading attribute, which Quarto does not implement:
`quarto render evaluation.Rmd` produces a page whose tab groups are flattened into a
long list of headings with no tab strip. `render-evaluation.R` calls
`rmarkdown::render()` and is the supported entry point.

Cloning with the manuscript included:

```bash
git clone --recurse-submodules git@github.com:HydroWIRES-PNNL/inflow-forecast-evaluation.git
```

See [SYNC.md](./SYNC.md) for how the paper submodule relates to Overleaf.

R dependencies: `tidyverse`, `hydroGOF`, `kableExtra`, `import`, `zoo`, `rmarkdown`.

---

## Forecast product labels

The two commercial forecast products are anonymized as **A** and **B** throughout this
repository, the published dataset, and the paper. The in-house forecast is labelled **GRH**.
Two benchmarks are derived in the code rather than supplied in the data: `perfect` (the
observations used in place of a forecast) and `persistence` (the last observed value carried
forward).

---

## Documentation
For more details about the evaluation framework, please refer to the following publication:
  - Bracken, C., Son, Y., Tidwell, V., and Voisin, N., A real-time reservoir inflow forecast evaluation framework. Submitted to the *Journal of the American Water Resources Association* (*in review*).

A preprint is available at <https://eartharxiv.org/repository/view/10530/>.

---

## Funding Acknowledgements
This work was supported by under ... <br>
The PNNL is a multi-program national laboratory operated by Battelle Memorial Institute for the U.S. Department of Energy (DOE) under Contract No. DE-AC05-76RL01830.

