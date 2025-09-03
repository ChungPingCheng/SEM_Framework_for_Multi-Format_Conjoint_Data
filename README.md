# Integrated SEM Framework for Conjoint Analysis

This repository accompanies the manuscript:  

> **Cheng, C.-P. (2025). Modeling Diverse Response Formats in Conjoint Analysis: An Integrated SEM Framework from Ratings to Choices.**

The project provides **all simulation and empirical analysis code** (R and Mplus) as well as the associated data files. This ensures full reproducibility of results reported in the paper, including Tables 4–5.

---

## Contents

- **`/01_simulation/`**  
  Scripts and data for the simulation study, covering six response formats:
  - Ratings  
  - Binary yes/no choices  
  - Paired comparisons  
  - Rankings  
  - Categorical choices  
  - Mixed-format responses  

- **`/02_chocolate/`**  
  Scripts and data for the empirical study on chocolate preferences (87 respondents × 8 profiles, transformed into pairwise comparisons).  

---

## Reproducing the Analyses

### Simulation Study

1. Run `01_simulation/data/simulate_conjoint_data.R` to generate synthetic datasets.  
2. Use either:
   - `01_simulation/MPLUS/Mx_xxx.inp` (Mplus)  
   - `01_simulation/lavaan/Mx_xxx.R` (R/lavaan)  
   to estimate the corresponding models for each response format.

### Empirical Illustration

1. Run `02_chocolate/data/EmpiricalDataManagement.R` to prepare the chocolate dataset and derive pairwise comparisons.  
2. Use either:
   - `02_chocolate/MPLUS/chocolate.inp` (Mplus)  
   - `02_chocolate/lavaan/chocolate.R` (R/lavaan)  
   to estimate the final SEM model.

---


## Requirements

- **R (≥ 4.2)** with packages: `lavaan`, `conjoint`, `tidyverse`  
- **Mplus (≥ 8.3)** (optional; syntax provided)  

---

## Citation

If you use this code or data, please cite:

> Cheng, C.-P. (2025). *Modeling Diverse Response Formats in Conjoint Analysis: An Integrated SEM Framework from Ratings to Choices*. [Manuscript under review].

---

## Contact

For questions about this repository:  
**Chung-Ping Cheng**  
Department of Psychology, National Cheng Kung University  
📧 cpcheng.psy@gmail.com  
