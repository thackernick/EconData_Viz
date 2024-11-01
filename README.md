# **Macro Graphing Guide with R**

This repository is a comprehensive guide to graphing the broader contours of the U.S. economy. It covers critical areas such as inflation, the labor market, economic activity, and financial conditions, providing clear visuals to illustrate major economic trends using R. You’ll find all the essential R code, Excel datasets, and generated graphs in organized branches. Additionally, this guide walks you through how to use the FRED API key to fetch data.


**The repository is organized into the following branches:**

- r-code-templates

  - Contains R scripts that generate graphs.
Each script is annotated to guide you through customization.
- data

  - Stores Excel files used for some of the graphs.
  - Data is structured and cleaned to be ready for plotting.

- graphs

  - PNG files of all the generated graphs.
  - Use these as references or directly in the slide deck.


# **Getting Started**


**Ensure the following are installed on your machine:**

- R (latest version)
- RStudio (recommended for development)
- Required R libraries (most are in the templates; install with install.packages() if needed):
requirements.R


# **Using the FRED API Key**

The Federal Reserve Economic Data (FRED) API allows you to access financial and economic data directly in R. 

To use it in the R templates:

- Obtain your FRED API key:

    - Visit the FRED API page and create an account.
    - Navigate to My Account > API Keys and generate a key.

Setting the FRED API key in R:

In the R script, locate the following line:

fredr_set_key("YOUR_FRED_API_KEY")
Replace "YOUR_FRED_API_KEY" with the key you generated.

Verify the API connection:

After setting the key, test the connection by fetching a small dataset:

fred_data <- fredr(series_id = 'SAHMCURRENT', observation_start = as.Date("2020-01-01"))
print(head(fred_data))
