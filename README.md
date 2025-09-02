# Integrated SEM Framework for Conjoint Analysis

This repository accompanies the manuscript:  

> **Cheng, C.-P. (2025). Modeling Diverse Response Formats in Conjoint Analysis: An Integrated SEM Framework from Ratings to Choices.**

The project provides **all simulation and empirical analysis code** (R and Mplus) as well as the associated data files. This ensures full reproducibility of results reported in the paper, including Tables 1–5.

---

## Contents

- **`/simulation/`**  
  R and Mplus scripts for the simulation study, covering six conditions:  
  - Ratings  
  - Binary yes/no choices  
  - Paired comparisons  
  - Rankings  
  - Categorical choices  
  - Mixed-format responses  

- **`/empirical/`**  
  R scripts and data for the chocolate preference dataset (87 respondents × 16 profiles, transformed into rankings and pairwise comparisons).  

- **`/data/`**  
  - Simulation design matrices and generated datasets  
  - Chocolate dataset (subset of the *conjoint* R package, Bak & Bartlomowicz, 2012)  

---

## Reproducing the Analyses

### Simulation Study
1. Run `simulation/generate_data.R` to create synthetic datasets.  
2. Use `simulation/analyze_lavaan.R` or `simulation/analyze_mplus.inp` to estimate models.  
3. Compare outputs to Table 4 in the manuscript.

### Empirical Illustration
1. Run `empirical/chocolate_prepare.R` to convert rankings into pairwise comparisons.  
2. Run `empirical/chocolate_sem.R` for SEM estimation in `lavaan`.  
3. Results correspond to Table 5 in the manuscript.

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
