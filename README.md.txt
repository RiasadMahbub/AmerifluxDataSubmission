# LFG AmeriFlux Data Submission — Fourth Round

This repository contains the post-processing code and documentation for the fourth round of eddy covariance flux data submission to the AmeriFlux network from two rice paddy sites in California: **US-HRC (Way 3)** and **US-HRA (Way 4)**, covering years 2018–2024.

---

## Authors and Contributors

| Contributor | Role |
|---|---|
| Maria and Kevin | Master file compilation: merging Biomet and soil data, running EddyPro in Advanced mode |
| Colby | Provided code for master file compilation |
| Bea | Provided LAI, canopy height, and water table depth data |
| Marret | Corrected LAI and canopy height data |
| Riasad Mahbub | Developed the data post-processing code and analysis in this repository |

**Code repository:** [PostProcessingECdataLandscapeFlux](https://github.com/RiasadMahbub/PostProcessingECdataLandscapeFlux/tree/main/AmerifluDataSubmission_LandscapeFlux)

---

## Sites
* **US-HRC** — Way 3 (rice paddy, California)
* **US-HRA** — Way 4 (rice paddy, California)

---

## Repository Structure

```
C:\Users\rbmahbub\Documents\GitHub\Fluxdata\
│
├── R Scripts\
│   ├── Running Scripts\          ← Active scripts used for current submission
│   │   ├── WTDUnileverLEdataFixing.R
│   │   ├── LAICanopyHeight.R
│   │   ├── ChangeColumnLSWRDataThresholdFinalScript.R
│   │   ├── ChecktheColumnNames.R
│   │   └── CheckAfterAnalysis.R
│   │
│   └── Archived\                 ← Previous versions, no longer in active use
│
└── Figure\                       ← All output QC figures
    ├── ShortwaveRadiation\
    ├── WindSpeed\
    ├── Way3Columns\
    ├── Way4Columns\
    └── ...
```

---

## Script Descriptions

| Script | Role |
|---|---|
| `WTDUnileverLEdataFixing.R` | Step 1: Merge all raw data sources; fix WTD, LE, USTAR outliers, air temperature, CH4 units |
| `LAICanopyHeight.R` | Step 1b: Process and gap-fill LAI and canopy height from field measurements |
| `ChecktheColumnNames.R` | Utility: Verify required columns are present across all year files |
| `ChangeColumnLSWRDataThresholdFinalScript.R` | Step 2: Column renaming to AmeriFlux convention, timestamp alignment, radiation QC, physical range and percentile filtering, output |
| `CheckAfterAnalysis.R` | Step 3: Post-processing validation — radiation alignment checks, USTAR/WS plots, CH4 unit verification, WTD range checks |

---

## Data Flow and Processing Pipeline

Raw data enters the pipeline from several independent sources and is progressively merged and quality-controlled before AmeriFlux submission.

![Data processing pipeline](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/Processflowimage.png)

The master file compilation (`WTDUnileverLEdataFixing.R`) merges flux, soil, Biomet, LAI, canopy height, and WTD data into a single 30-minute resolution file per site per year (Masterfile v2), which then passes through the QA/QC and variable-renaming pipeline (`ChangeColumnLSWRDataThresholdFinalScript.R`).

---

## Data Processing Steps

### 1. LAI and Canopy Height (`LAICanopyHeight.R`)

Field LAI and canopy height measurements are collected approximately every 8 days from East and Central plots at both Way 3 and Way 4. Duplicate dates from W3 East and W3 West samples are averaged. The resulting sparse time series (2018–2024) is gap-filled to 30-minute resolution using linear interpolation (`na.approx` from the R `zoo` package).

**Source files (examples):**
* `2018-2020LAI_Canopy.xlsx`
* `LAIMasterFileScatteringCorrections2021.xlsx`, `RiceFieldDataCanopyLAI2021.xlsx`
* `LAI_MasterFileScatteringCorrections2022.xlsx`, `RiceFieldDataCanopyLAI2022.xlsx`
* `RiceFieldDataCanopyLAI2023.xlsx`
* `RiceFieldDataCanopyLAI_2024_Last.xlsx`

### 2. Water Table Depth (`WTDUnileverLEdataFixing.R`)

WTD data from the Unilever Tower Campbell station was provided by Bea for 2020–2023. Units were converted from centimeters to meters. Data collected at 5- or 15-minute intervals was aggregated to 30-minute intervals by rounding timestamps and averaging.

**Source files:**
* `MasterFile_Way3_SoilProf_VWC.xlsx`, `MasterFile_Way4SoilProfile.xlsx`
* `2021_MasterFile_Way3_SoilProf_All.xlsx`, `2021_MasterFile_Way4SoilProfile.xlsx`
* `2022_Way4_SoilProfile_MasterFile.xlsx`, `2022Way3SoilProfileMasterFile.xlsx`
* `2023Way3SoilProfileMasterFile copy.xlsx`, `2023Way4_SoilProfile_MasterFile copy.xlsx`

### 3. Master File Compilation and Data Fixes (`WTDUnileverLEdataFixing.R`)

After merging all sources, the following corrections are applied before column renaming:

**Rainfall data** — The `P_RAIN_Tot` column in Way 3 data is replaced with corresponding values from Way 4, which has the reliable tipping-bucket rain gauge.

**Air pressure** — For years 2018–2021, air pressure is converted from pascals (Pa) to kilopascals (kPa).

**Latent heat (LE) fix — 2021** — Summer 2021 LE data for Way 3 contained errors from the original EddyPro run. The affected values were replaced with corrected outputs from a re-processed EddyPro file (`eddypro_LE_Fix_2023_12_06_full_output_2023-12-06T144759_adv.csv`).

**CH4 unit conversion** — Methane flux (`ch4_flux`), mole fraction (`ch4_mole_fraction`), and mixing ratio (`ch4_mixing_ratio`) are converted from µmol to nmol by multiplying by 1000.

**USTAR outlier removal** — A linear regression is fit between wind speed (WS) and friction velocity (USTAR). Observations whose residuals exceed a standard deviation threshold are flagged and replaced with NaN:
* Threshold of 4× SD for years 2018 and 2021
* Threshold of 3.75× SD for years 2023 and 2024 (tightened after the prior round left too many anomalous points)
* Applied to Way 3 (years 2018, 2023, 2024) and Way 4 (years 2018, 2021, 2023, 2024)

The plot below (Way 3, 2021) shows the typical sonic anemometer vs. Biomet wind speed relationship used to diagnose and validate USTAR quality. R² = 0.90 and slope = 1.09 indicate good agreement with a slight Biomet positive bias:

![Wind Speed Comparison — Way 3, 2021](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/Way3_WindSpeed_2021.png)

**Temperature outlier removal** — A three-step process is applied to air temperature. First, values below 200 K are set to NA. Second, temperature is converted from Kelvin to Celsius. Third, a regression of sonic temperature against air temperature identifies outliers whose residuals exceed a year-specific SD multiple (1.5× to 5×, varying by year and site). Applied to Way 3 (2022, 2023, 2024) and Way 4 (2022, 2023, 2024).

### 4. Column Naming and Variable Alignment (`ChangeColumnLSWRDataThresholdFinalScript.R`)

Column names are standardized to the [AmeriFlux BASE variable naming convention](https://ameriflux.lbl.gov/data/aboutdata/data-variables/). The positional qualifier suffix `_H_V_R` identifies the horizontal position, vertical position, and replicate number of each sensor.

![AmeriFlux positional qualifier convention](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/positionqualifiers.png)

**Key internal-to-AmeriFlux name mappings:**

| Internal Name | AmeriFlux Name | Description |
|---|---|---|
| `co2_flux` | `FC_1_1_1` | CO₂ flux |
| `ch4_flux` | `FCH4` | CH₄ flux |
| `h2o_flux` | `FH2O` | H₂O flux |
| `wind_speed` | `WS` | Wind speed |
| `air_temperature` | `TA_1_1_1` | Air temperature |
| `u_` / `ustar` | `USTAR` | Friction velocity |
| `Lvl_m_Avg` | `WTD_1_1_1` | Water table depth |
| `PAR_IN_Avg` | `PPFD_IN` | Incoming photosynthetically active radiation |
| `SW_IN_Avg` | `SW_IN` | Incoming shortwave radiation |
| `LW_IN_T_Corr_Avg` | `LW_IN` | Incoming longwave radiation |
| `SWC_2_1_1_Avg` | `SWC_1_1_1` | Soil water content |
| `wt_corr_AVG_cm_fixedBias` | `WTD_1_2_1` | Unilever station WTD (corrected) |

AmeriFlux timestamps `TIMESTAMP_START` and `TIMESTAMP_END` are generated in `YYYYMMDDHHMM` format.

### 5. Timestamp Alignment — PAR/PPFD Shift Detection

A key QC step checks whether the PPFD_IN (PAR) sensor is temporally aligned with the theoretical solar curve. The method computes maximum diurnal composites of SW_IN, SW_IN_POT (clear-sky potential), and PPFD_IN (converted to W m⁻² using a 2.02 µmol J⁻¹ factor) within 15-day periods.

When PPFD_IN peaks earlier or later than SW_IN and SW_IN_POT, a timestamp offset is applied. The before/after plots for Way 4, 2019 illustrate this: in the uncorrected data (before), PPFD_IN in periods 22–25 leads SW_IN by several hours; after correction, the curves are aligned.

**Before correction:**

![Radiation plot before timestamp fix — Way 4, 2019](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/Way4_2019_RadiationPlotbefore.jpeg)

**After correction:**

![Radiation plot after timestamp fix — Way 4, 2019](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/Way4_2019_RadiationPlot.jpeg)

**All timestamp offsets applied** (positive = shift later, negative = shift earlier):

| Dataset | DOY Range | Offset |
|---|---|---|
| Way 3, 2022 | 1–105 | −30 min |
| Way 3, 2023 | 1–60 | −120 min |
| Way 3, 2023 | 61–105 | −60 min |
| Way 4, 2018 | 165–195 | −30 min |
| Way 4, 2019 | 1–15 | +210 min |
| Way 4, 2019 | 15–30 | +300 min |
| Way 4, 2019 | 315–365 | −120 min |
| Way 4, 2020 | 1–15 | −30 min |
| Way 4, 2023 | 1–60 | −30 min |
| Way 4, 2023 | 275–290 | +30 min |
| Way 4, 2023 | 320–365 | −30 min |
| Way 4, 2024 | 1–60, 105–230 | −30 min |
| Way 4, 2024 | 60–75 | +30 min |

PPFD_IN values exceeding SW_IN_POT are additionally set to NA as physically impossible.

### 6. Physical Range Filtering (`ChangeColumnLSWRDataThresholdFinalScript.R`)

Values outside physically plausible ranges are replaced with NA. Ranges are applied uniformly across all years and sites:

| Variable | Min | Max | Unit |
|---|---|---|---|
| `FC_1_1_1` | −100 | 100 | µmol CO₂ m⁻² s⁻¹ |
| `FCH4` | −500 | 4000 | nmol CH₄ m⁻² s⁻¹ |
| `FH2O` | −10 | 20 | mmol H₂O m⁻² s⁻¹ |
| `H` | −450 | 900 | W m⁻² |
| `LE` | −450 | 900 | W m⁻² |
| `SH` | −165 | 165 | W m⁻² |
| `SLE` | −150 | 150 | W m⁻² |
| `TA_1_1_1` | −50 | 50 | °C |
| `TS_1_1_1` | −40 | 65 | °C |
| `PA` | 60 | 105 | kPa |
| `VPD` | 0 | 80 | hPa |
| `LW_IN` | 50 | 600 | W m⁻² |
| `LW_OUT` | 100 | 750 | W m⁻² |
| `PPFD_IN` | 0 | 2400 | µmol m⁻² s⁻¹ |
| `SWC_1_1_1` | 0 | 100 | % |
| `WTD_1_1_1` | −0.5 | 5 | m |
| `TAU` | −10 | 2 | kg m⁻¹ s⁻² |
| `CO2_1_1_1` | 150 | 1200 | µmol mol⁻¹ |
| `CH4_1_1_1` | 0 | 15000 | nmol mol⁻¹ |

The plot below shows the physical range check for FC in 2024. Orange circles are values outside ±100 µmol m⁻² s⁻¹; red circles are outside the ±5% tolerance band (±110 µmol m⁻² s⁻¹):

![Physical range of FC — 2024](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/physicalrange.png)

### 7. Percentile-Based Filtering

A statistical outlier filter (1.5th–98.5th percentile) is applied to year-specific variables after the physical range filter. Variables filtered include turbulence statistics (CO2_SIGMA, U_SIGMA, V_SIGMA, W_SIGMA), fluxes (FC_1_1_1, FCH4, FH2O, LE, H), and auxiliary variables (ZL, MO_LENGTH, TAU, SLE, SCH4). Outliers are replaced with NaN.

### 8. Wind Direction Filtering

Data collected when wind direction is outside the 95°–265° sector are excluded for all flux and turbulence variables (FCH4, FC_1_1_1, FH2O, H, LE, USTAR, FETCH_70, FETCH_90, FETCH_MAX, and related QC flags).

---

## AmeriFlux Submission Protocols

All output files conform to AmeriFlux BASE submission requirements:

* **Timestamp format:** `YYYYMMDDHHMM` (12-character, no separators)
* **Filename format:** `<SITE_ID>_HH_<TS-START>_<TS-END>.csv`
  * Way 3 → `US-HRC_HH_<start>_<end>.csv`
  * Way 4 → `US-HRA_HH_<start>_<end>.csv`
* **Missing data sentinel:** −9999
* **Variable naming:** AmeriFlux positional qualifier convention (`_H_V_R` suffix)
* **Temporal resolution:** Half-hourly (HH)

---

## Data Coverage

Variable coverage (% of expected half-hourly records) for US-HRA (Way 4) and US-HRC (Way 3) after post-processing:

**US-HRA (Way 4):**

![Data coverage — US-HRA](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/US-HRA-variable_coverage-by_year.png)

**US-HRC (Way 3):**

![Data coverage — US-HRC](https://raw.githubusercontent.com/RiasadMahbub/AmerifluxDataSubmission/main/Figure/US-HRC-variable_coverage-by_year.png)

**Notable patterns:**
* CH4 measurements begin in 2018 for both sites; Way 3 CH4 is absent in 2019 (columns dropped)
* WTD_1_1_1 coverage drops sharply in 2024 for US-HRA; data were not available from the Unilever station
* LE was dropped for US-HRC 2020 due to a known instrument error
* SW_IN and LW_IN show notably high coverage (>80%) in 2020 for US-HRC
* WS coverage at US-HRC improved to 98% in 2023

---

## Known Issues and Limitations

* The WS–USTAR regression slope deviated >20% from expected for Way 3 and Way 4 in 2018, 2023, and 2024.
* FH2O and TAU for 2021 failed the physical range check.
* FH2O values for 2019 and 2021 exceed expected physical limits and were flagged.
* CH4 columns are absent for Way 3, 2019 (dropped).
* LE, SLE, and LE_SSITC_TEST were removed for Way 3, 2020 due to a known instrument error.
* WTD_1_2_1 (Unilever station corrected WTD) is entirely missing for several years and is excluded from those submissions.

---

## File Locations

| Type | Path |
|---|---|
| Active R scripts | `C:\Users\rbmahbub\Documents\GitHub\Fluxdata\R Scripts\Running Scripts\` |
| Archived R scripts | `C:\Users\rbmahbub\Documents\GitHub\Fluxdata\R Scripts\Archived\` |
| QC figures | `C:\Users\rbmahbub\Documents\GitHub\Fluxdata\Figure\` |
| AmeriFlux submission (Way 3) | `Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\AmerifluxSubmission\Way3` |
| AmeriFlux submission (Way 4) | `Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\AmerifluxSubmission\Way4` |
| Lab research data (Way 3) | `Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\ForLabResearachPurposeMoreColumns\Way3` |
| Lab research data (Way 4) | `Box\Field_Data\AmeriFlux_Submission_Figures\OutputLocalProcessedData_AFguidedSubmitted\ForLabResearachPurposeMoreColumns\Way4` |