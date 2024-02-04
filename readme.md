# Replication Materials for "The Conditional Association between Populism, Ideological Extremity, and Affective Polarization"

This repository hosts the replication materials for the paper titled ["The Conditional Association between Populism, Ideological Extremity, and Affective Polarization"](https://academic.oup.com/ijpor/article-abstract/35/2/edad014/7187151), which has been accepted by the International Journal of Public Opinion Research.

## Repository Structure

### Manuscript
- The `manuscript` folder contains the author’s version of the manuscript and the appendix.

### Data
- The `data` folder includes the ANES dataset, variable list, and codebook.

### Figures
- The `figures` folder contains figures of both the manuscript and the appendix.
  - Figures from the manuscript have the prefix `M_`.
  - Figures from the appendix have the prefix `APPENDIX_`.

### Scripts
- The `scripts` folder consists of the necessary scripts to replicate figures and tables from both the manuscript and the appendix.
  - There are four sub-folders corresponding to various analyses conducted for the paper.
  - Each sub-folder contains a `mplus_out` folder storing Mplus files with results and corresponding syntax.
    - These files include the results obtained from Mplus (.out files) and allow table and figure reproduction without a Mplus license.
  - Script prefixes:
    - `F_`: Writes out Mplus syntax and fits the model using Mplus (requires Mplus license).
    - `R_`: Reads the .out Mplus files containing the results.
    - `FR_`: Writes syntax and reads .out Mplus files containing results; typically used for models fitted without Mplus (e.g., OLS models in the appendix).
