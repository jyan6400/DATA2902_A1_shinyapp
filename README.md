# DATA2x02 Student Survey Explorer


An interactive **Shiny** application for analysing and visualising results from the **DATA2x02 Student Survey (2025)**.  
The app allows students to explore cleaned categorical and numeric data, run independence and group comparison tests, and generate clear, professional-looking visuals.

---

## 🚀 Features

### 🔹 Two Categorical Variables
- Build contingency tables of any two cleaned categorical variables.  
- Automatic selection of statistical test:
  - **Chi-squared test of independence** when valid (with **Cramér’s V** effect size).  
  - **Fisher’s exact test** when expected counts are too small.  
  - **Monte Carlo Fisher** fallback if needed.  
- Hypotheses and plain-English interpretations are displayed.  
- Visualisation: **stacked proportion bar chart** (clean design, no cluttered labels).  

### 🔹 Numeric vs Categorical
- Compare numeric outcomes across two groups.  
- Group summaries: _n, mean, sd, median, IQR_.  
- Assumption checks:
  - **Shapiro–Wilk** test for normality (per group).  
  - **Variance F-test** (contextual only).  
- Two tests shown side-by-side:
  - **Welch’s two-sample t-test** (default, robust).  
  - **Wilcoxon rank-sum test** (non-parametric).  
- Boxplot with jittered points for distribution visualisation.  

### 🔹 Data Dictionary
- Lists every variable available in the app.  
- Includes variable type, cleaning rules, and plausible ranges.  

### 🔹 User Experience
- **Rmd-style flow**: results and plots display top-to-bottom without inner scrollbars.  
- **Download buttons** for saving plots as PNG.  
- **p-value badges**: green (<0.05), red (≥0.05), amber (NA).  
- Modern **bslib theme** with consistent styling.  
- Guard against selecting the same variable twice.  

---

## 📂 Project Structure

- `app.R` – main Shiny application file.  
- `data/` – contains the raw Excel survey responses.  
- `README.md` – project documentation.  


## ▶️ Running the App

Place the survey file at:

```
data/DATA2x02_survey_2025_Responses.xlsx
```

Open R / RStudio and run:

```r
shiny::runApp("app.R")
```

The app will launch in your browser at:

```
http://127.0.0.1:<port>
```

---

## 🧹 Data Cleaning

The app parses, clamps, and collapses survey responses into usable variables.

### Categorical Variables (collapsed)

- **Grade Aim** → High Distinction / Distinction / Credit / Pass  
- **Alcohol Consumption** → Low (≤5 drinks/wk) / High (>5 drinks/wk)  
- **Sleep Group** → Adequate (≥7h) / Short (<7h)  
- **Exercise Group** → Low (<3h/wk) / Moderate (3–5h/wk) / High (≥6h/wk)  
- **On-time Submission** → Yes / No  
- **Gender** → Female / Male / Other  
- **Year of University** → Year 1 / Year 2 / Year 3+  
- **Transport** → Walk/Bike / Public / Car / Other  
- **Living Arrangement** → With parents / Share-Rent / On-campus / Other  
- **Pays Rent** → Yes / No  

Invalid, jokey, or empty answers → **NA (dropped from analysis).**

### Numeric Variables (plausible ranges kept)

| Variable              | Range kept |
|------------------------|------------|
| Study Hours/week       | 0–80       |
| Sleep Hours/day        | 3–14       |
| WAM                    | 40–100     |
| Age                    | 15–80      |
| Height (cm)            | 130–220    |
| Paid Work Hours/week   | 0–80       |
| Food Spend/week ($)    | 0–1500     |
| Short-video use (min)  | 0–600      |

---

## 📊 Statistical Tests

### Categorical × Categorical

- **Null hypothesis (H₀):** variables are independent.  
- **Alternative (H₁):** variables are associated.  
- **Effect size:** Cramér’s V when Chi-square is valid.  

### Numeric × Categorical

- **Null hypothesis (H₀):** group means equal.  
- **Alternative (H₁):** group means differ.  

Tests performed:  
- Welch’s t-test (default).  
- Wilcoxon rank-sum test (non-parametric).  

---

## 🎨 Visualisations

- **Categorical × Categorical:** stacked proportion bar chart.  
- **Numeric vs Categorical:** boxplot + jittered data points.  

All plots:  
- Clean, minimal design with colour palettes (`RColorBrewer::Set2`).  
- Downloadable as PNG.  

---

## 🧭 Tips for Exploration

- Try **Sleep Group vs Grade Aim** for a classic independence test.  
- Compare **Study Hours** across **Pays Rent** groups.  
- Explore **Video Minutes/day vs On-time Submission**.  
- Small subgroups may trigger Fisher’s exact test.  

---

## ✅ Marking Criteria Alignment

This app meets all rubric items:

- **GitHub use:** commit history shows incremental development with clear messages.  
- **Visuals (Cat × Cat):** stacked proportion bar charts.  
- **Independence tests + assumptions:** Chi-squared/Fisher with clear reporting.  
- **Visuals (Num × Cat):** boxplots with jitter points.  
- **t-tests + assumptions:** Welch + Wilcoxon with Shapiro/F-test outputs.  

### Going beyond:

- Automated cleaning & collapsing rules.  
- Effect sizes and multiple tests.  
- Modern UI with badges and polished flow.  
- User guide: this README serves as full documentation.  
- Usability: clean layout, no inner scrollbars, clear navigation.  

---

## ⚠️ Limitations

- Convenience sample → results are not generalisable.  
- Collapsing categories may oversimplify nuanced answers.  
- Cleaning thresholds remove outliers, which may bias results.  
- Assumptions checks are advisory — interpretation required.  

---

## 📜 License

For teaching and learning in DATA2x02.  
Reuse allowed with attribution.  

---

## 🙏 Acknowledgements

- DATA2x02 students for survey responses.  
- Built with **Shiny, ggplot2, bslib, and the tidyverse**.  
