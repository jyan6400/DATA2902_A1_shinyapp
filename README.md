# DATA2902 Student Survey Explorer

An interactive **Shiny** app for exploring results from the **DATA2x02 Student Survey (2025)**.

The app provides cleaned summaries, hypothesis tests, and clear visualisations for:

- **Categorical × Categorical** relationships (e.g., _Grade Aim_ vs _Sleep Group_).
- **Numeric × Categorical** comparisons (e.g., _Study Hours/week_ by _Pays Rent_).

Each tab reads top-to-bottom like an R Markdown document (no nested scroll panes), with p-value **badges** and plain-English interpretations.

---

## 🔍 Features

### Two Categorical
- Contingency table (counts).
- Automatic choice of association test:
  - **Chi-squared test** when valid (with **Cramér’s V** effect size).
  - **Fisher’s exact test** when expected counts are small.
  - **Monte-Carlo Fisher** fallback if exact enumeration fails.
- Stacked **proportion bar chart** (clean, no % labels clutter).

### Numeric vs Categorical (two groups)
- Group **descriptives**: _n, mean, sd, median, IQR_.
- **Assumption checks**:
  - **Shapiro–Wilk** per group.
  - **F-test** for equal variances (context only).
- **Welch’s t-test** (primary) + **Wilcoxon rank-sum** (secondary).
- **Boxplot with jittered points** for clarity.

### Data Dictionary
- Generated table describing variables, cleaning rules, and ranges.

### UX niceties
- Same-variable guard (can’t select the same categorical twice).
- Download buttons for plots (**PNG**).
- **bslib theme** with single-page flow per tab.
- P-value **badges** (green < .05, red otherwise, amber if NA).

---

## 🧰 Requirements

- **R** (≥ 4.2 recommended)
- Required packages:

```r
install.packages(c(
  "shiny","bslib","dplyr","tidyr","readxl","readr",
  "stringr","janitor","ggplot2","forcats","scales","purrr"
))
```

## 📂 Project Structure

- `app.R` – main Shiny application file.  
- `data/` – contains the raw Excel survey responses.  
- `README.md` – project documentation.  

---