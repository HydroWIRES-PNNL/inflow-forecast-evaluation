# Inflow Forecast Evaluation

A collection of scripts designed to evaluate real-time reservoir inflow forecast products for the Great River Hydro (GRH) facilities, as well as to visualize evaluation metrics in an HTML dashboard where users can dynamically switch their statistics based on seasonality or applied criteria. The dataset required to run the scripts is available at the following link: http://zenodo.org/record/X (DOI: X)

---

## Code Descriptions
   * [evaluation.Rmd](./evaluation.Rmd): read anonymized inflow forecasts and processed observations to calculate evaluation metrics and generate an HTML dashboard
   * [exploratory-plots.R](./exploratory-plots.R): (optional) plot process observations compared to a series of inflow forecasts, outputting to ./plot/exploratory/
   
---
## Code Execution Steps
   1. Download anonymized and processed data from [Zenodo](http://zenodo.org/record/X) in [./processed-data](./processed-data/)
   2. Run `evaluation.Rmd` to evaluate inflow forecasts and generate visualizations
   
---

## Documentation
For more details about the evaluation framework, please refer to the following publication:
  - Bracken, C., Son, Y., Tidwell, V., and Voisin, N., A real-time reserervoir inflow forecast evaluation framework. *in prep*

---

## Funding Acknowledgements
This work was supported by under ... <br>
The PNNL is a multi-program national laboratory operated by Battelle Memorial Institute for the U.S. Department of Energy (DOE) under Contract No. DE-AC05-76RL01830.
