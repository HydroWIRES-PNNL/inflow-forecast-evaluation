# Inflow Forecast Evaluation

A collection of scripts designed to evaluate real-time reservoir inflow forecast products for the Great River Hydro (GRH) facilities, as well as to visualize evaluation metrics in an HTML dashboard where users can dynamically switch their statistics based on seasonality or applied criteria. The dataset required to run the scripts is available at the following link: [http://zenodo.org/record/16921728](https://zenodo.org/records/16921728) (DOI: 10.5281/zenodo.16921728)

---

## Code Descriptions
   * [evaluation.Rmd](./evaluation.Rmd): read anonymized inflow forecasts and processed observations to calculate evaluation metrics and generate an HTML dashboard
   * [verify-all.R](./verify-all.R): reproduce the quantitative results reported in the paper, including Table 1, the smoothing sensitivity analysis, and the cumulative forebay exceedance probabilities
   * [make-metric-figures.R](./make-metric-figures.R): regenerate the lead-time and by-month performance figures (Figures 3 and 4) from the published data
   * [make-revision-figures.R](./make-revision-figures.R): regenerate the cumulative forebay error and exceedance figures (Figures 5 and 6) from the published data
   * [figure-style.R](./figure-style.R): palette, linetypes, and theme shared by the two figure scripts, so the manuscript figures cannot drift apart
   * [exploratory-plots.R](./exploratory-plots.R): (optional) plot process observations compared to a series of inflow forecasts, outputting to ./plot/exploratory/
   * [paper/](./paper): the manuscript, as a git submodule

---
## Code Execution Steps
   1. Download anonymized and processed data from [Zenodo](https://zenodo.org/records/16921728) in [./processed-data](./processed-data/)
   2. Run `evaluation.Rmd` to evaluate inflow forecasts and generate visualizations
   3. Run `verify-all.R` to reproduce the numbers reported in the paper
   4. Run `make-metric-figures.R` and `make-revision-figures.R` to regenerate Figures 3--6

Cloning with the manuscript included:

```bash
git clone --recurse-submodules git@github.com:HydroWIRES-PNNL/inflow-forecast-evaluation.git
```

See [SYNC.md](./SYNC.md) for how the paper submodule relates to Overleaf.

R dependencies: `tidyverse`, `hydroGOF`, `kableExtra`, `import`, `zoo`.

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

