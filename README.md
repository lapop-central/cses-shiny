---
# 🌎 CSES Data Playground
---
**Date:** November 10th, 2025  
**Status:** On-going Development  
**OS:** Windows  

<!-- badges: start -->
[![R-CMD-check](https://github.com/lapop-central/lapop-viz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lapop-central/cses-shiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

---

## 📘 Overview
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>
The **CSES Data Playground** is an interactive **R Shiny** application designed to visualize survey responses from the **Comparative Study of Electoral Systems (CSES)** dataset.  
It provides a standardized interface for exploring political attitudes and behaviors across **countries**, **election years**, and **CSES modules**.

The app allows users to:

- 🎯 Select variables and modules from preprocessed CSES datasets  
- 📊 Visualize outcomes as  
  - Histograms  
  - Time Series  
  - Cross-Country Comparisons  
  - Demographic Breakdowns  
- 🧮 Apply recode sliders and survey weights interactively  
- 💾 Download publication-ready plots and tables  

---

## 📈 Data Playground Workflow
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>
1. **Select** an outcome variable (e.g., *IMD3010 – Satisfaction with Democracy*)  
2. **Choose** one or more CSES Modules (e.g., *Module 4* and *Module 5*)  
3. **Select** countries and election years of interest  
4. **Adjust** the recode slider (e.g., show only "Satisfied" and “Very satisfied” = 4–5)  
5. **Visualize** across  
   - Histogram – response distribution  
   - Time Series – temporal evolution  
   - Cross Country – comparative levels  
   - Breakdown – gender, education, income, etc.  
6. **Download** plots (.svg) or tables (.csv) for documentation or publications  

---

## 🗳️ CSES Modules
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>

<table style="width:100%; border-collapse:collapse; border-spacing:0;">
  <thead>
    <tr style="border-bottom:2px solid #C4722A;">
      <th style="width:2%; text-align:center; padding:8px;">Module</th>
      <th style="width:10%; text-align:center; padding:8px;">Years</th>
      <th style="width:33%; text-align:left; padding:8px;">Theme</th>
      <th style="width:55%; text-align:left; padding:8px;">Description</th>
    </tr>
  </thead>
  <tbody>
    <tr style="border-bottom:1px solid #ddd;">
      <td style="text-align:center; padding:8px;">1</td>
      <td style="text-align:center; padding:8px;">1996–2001</td>
      <td style="padding:8px;">System Performance</td>
      <td style="padding:8px;">Electoral institutions, political cleavages, and democratic evaluations.<br><small>(39 studies / 33 countries – e.g., Australia 1996, Germany 1998)</small></td>
    </tr>
    <tr style="border-bottom:1px solid #ddd;">
      <td style="text-align:center; padding:8px;">2</td>
      <td style="text-align:center; padding:8px;">2001–2006</td>
      <td style="padding:8px;">Accountability & Representation</td>
      <td style="padding:8px;">Elections as mechanisms of accountability versus citizen representation.<br><small>(41 studies / 38 countries – e.g., Brazil 2002, Japan 2004)</small></td>
    </tr>
    <tr style="border-bottom:1px solid #ddd;">
      <td style="text-align:center; padding:8px;">3</td>
      <td style="text-align:center; padding:8px;">2006–2011</td>
      <td style="padding:8px;">Meaningfulness of Electoral Choices</td>
      <td style="padding:8px;">Voter decision-making and contingency in electoral options.<br><small>(50 studies / 41 countries – e.g., Canada 2008, Chile 2009)</small></td>
    </tr>
    <tr style="border-bottom:1px solid #ddd;">
      <td style="text-align:center; padding:8px;">4</td>
      <td style="text-align:center; padding:8px;">2011–2016</td>
      <td style="padding:8px;">Distributional Politics & Social Protection</td>
      <td style="padding:8px;">Public policy preferences and institutional mediation.<br><small>(45 studies / 39 countries – e.g., Greece 2012, Turkey 2015)</small></td>
    </tr>
    <tr>
      <td style="text-align:center; padding:8px;">5</td>
      <td style="text-align:center; padding:8px;">2016–2021</td>
      <td style="padding:8px;">Attitudes Toward Elites & Out-Groups</td>
      <td style="padding:8px;">Anti-establishment voting, polarization, and social divides.<br><small>(56 studies / 45 countries – e.g., Brazil 2018, USA 2020)</small></td>
    </tr>
  </tbody>
</table>

---

## 🧠 App Logic
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>

### UI (User Interface)

Built with `fluidPage()` and `sidebarLayout()`.

**Sidebar includes**
- Variable, module, country, and year selectors  
- Weight-type radio buttons  
- Recode slider and mean-value toggle  
- Secondary variable and demographic breakdown selectors  

**Main Panel hosts**
- Dynamic captions and question wording  
- Tabs for each visualization type  
- Download buttons for figures and tables  
- Interactive *N-size* summary card  

---

### 🧩 Server Logic
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>

Built with R, Shiny, and LAPOP's custom visualization toolkit. Implements a **fully reactive architecture** with auto-updates and validation.

- 🔁 Auto-updates countries and years when modules change  
- 🧮 Dynamically adjusts recode ranges (e.g., 1–2 for 4-pt, 4–5 for 5-pt scales)  
- ⚙️ Auto-refresh to prevent empty plots  
- 🧾 Generates `.svg` plots and `.csv` tables using LAPOP standardized functions  

```r
lapop_hist()   # Histogram
lapop_ts()     # Time Series
lapop_cc()     # Cross-Country Comparison
lapop_mover()  # Breakdown Plot
lapop_save()   # Export figures and tables
```

---

## 📂 File Structure
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>

CSES_Data_Playground/

├── app.R # Main Shiny application

├── cses_shiny_data.rda # Preprocessed CSES data

├── cses_variable_labels.csv # Variable metadata (names, wording, responses)

├── cses_labs.rds # Main variable labels for dropdown menus

├── cses_labs_sec.rds # Secondary variable labels for breakdown plots

├── www/ # Fonts, icons, and theme assets

├── shiny_preprocessing.R # Data preparation script (prior to app launch)

└── README.md # You are here!

---

## 🤝 Acknowledgments
<hr style="border: 0; height: 2px; background: #ddd; margin-top: 4px; margin-bottom: 16px;"></hr>

Developed by the Center for Global Democracy (CGD) at Vanderbilt University.

Inspired by the **LAPOP Data Playground**, adapted for **CSES data** to support comparative political research. Special thanks to the LAPOP Data Team for design, and technical support.

For questions or contributions:
📧 robert.vidigal@vanderbilt.edu

How to cite CSES IMD dataset:

##### "The Comparative Study of Electoral Systems (www.cses.org). CSES INTEGRATED MODULE DATASET (IMD) [dataset and documentation]. February 27, 2024 version. doi:10.7804/cses.imd.2024-02-27"
