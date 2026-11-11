# Evidence Across Africa: The HIV Pulse

An interactive **R Shiny dashboard** exploring HIV estimates, treatment coverage and historical patterns across African countries from **1990 to 2025**.

Developed by **Muhammad Ibrahim Bello**, using estimates from **UNAIDS AIDSinfo**.

## Dashboard features

The dashboard organises its indicators and visualisations into six tabs:

- **Africa overview:** Headline indicators, an interactive country map, country rankings and historical trends.
- **Country profile:** Country indicators and trends, with source uncertainty bands where available.
- **Country comparisons:** Comparisons of HIV burden, prevalence, treatment coverage and changes over time.
- **Progress since 2010:** Changes relative to a 2010 baseline.
- **Subregional patterns:** Comparisons across UN M49 geographical subregions.
- **Data explorer:** Searchable tables, indicator guidance and filtered CSV downloads.

Filters allow users to explore reporting years, subregions, countries and indicators.

## Main indicators

| Indicator | Population | Unit |
|---|---|---|
| People living with HIV | All ages, both sexes | Estimated number |
| HIV prevalence | Adults aged 15–49, both sexes | Percentage |
| New HIV infections | All ages, both sexes | Estimated annual number |
| AIDS-related deaths | All ages, both sexes | Estimated annual number |
| ART coverage | People living with HIV, all ages, both sexes | Percentage receiving antiretroviral therapy |

## Repository files

| File | Purpose |
|---|---|
| `app.R` | Dashboard application |
| `Africa_HIV_Data_1990_2025.xlsx` | Dataset, indicator dictionary and source notes |
| `README.md` | Project documentation |
| `.gitignore` | Excludes common local R files from version control |

Keep `app.R` and the Excel workbook in the same folder.

## Run locally

### 1. Download the project

On this repository page, select **Code → Download ZIP**, then extract the ZIP.

In RStudio, select **File → New Project → Existing Directory** and choose the extracted folder.

### 2. Install the required packages

Run this once in the RStudio Console:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "readxl", "scales",
  "leaflet", "plotly", "sf", "rnaturalearth",
  "rnaturalearthdata", "dplyr", "DT"
))
```

### 3. Launch the dashboard

Run:

```r
shiny::runApp(".")
```

Alternatively, open `app.R` and click **Run App** in RStudio.

## Data source

**Provider:** Joint United Nations Programme on HIV/AIDS (UNAIDS).

**Release:** AIDSinfo 2026 Estimates Data package.

**Observation years:** 1990–2025.

**Downloaded:** 10 November 2026.

[Original dataset and user guidance](https://aidsinfo.unaids.org/dataset)

The workbook includes five sheets: `Shiny_Data`, `Core_Long`, `Country_Coverage`, `Data_Dictionary` and `Readme`.

## Coverage and interpretation

- The workbook contains 1,944 country-year rows covering 54 African UN member states over 36 years. Availability varies by indicator and year.
- Four countries have no numeric values for these five indicators in this extract: Côte d’Ivoire, Equatorial Guinea, Mauritius and Seychelles.
- Blank values mean unavailable data, not zero. Missing values were not imputed or interpolated.
- Estimates retain their original unrounded values and may contain decimals.
- Percentages use a 0–100 scale.
- Count totals include available countries only and may not represent complete Africa totals.
- Combined ART coverage is weighted by people living with HIV, using countries with both inputs available.
- Adult HIV prevalence remains country-specific because matching adult population denominators are not included.
- Country uncertainty bounds are shown where available. They are not summed to produce regional intervals.
- Summing annual estimates of people living with HIV across years does not give a count of unique people.
- Changes in country coverage can affect comparisons over time.

## Source rights and reuse

The underlying UNAIDS data remain subject to the [UNAIDS terms of use](https://www.unaids.org/en/terms-of-use) and any source-specific conditions.

Please acknowledge UNAIDS when using the estimates. This repository does not grant additional rights over those data.

This is an independent dashboard project. It is not an official UNAIDS publication or an endorsement by UNAIDS.

## Author

**Muhammad Ibrahim Bello**

[GitHub profile](https://github.com/Keenmuhammad)
