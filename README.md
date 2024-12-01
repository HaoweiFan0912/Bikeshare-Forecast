# Bikeshare Ridership Forecast

## Overview

This study uses Bayesian Poisson regression to predict the number of usage of bike-sharing stations at 27 locations on University of Toronto St. George campus for specific years, months, and four-hour intervals of the day.


## File Structure
The repository is organized as follows:

- **`data`**: Contains all datasets used in this project.  
  - **`data/00-simulated_data`**: Stores the simulated datasets.  
  - **`data/01-raw_data`**: Includes the raw data obtained from [Bike Share Toronto Ridership Data](https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/).  
  - **`data/02-analysis_data`**: Contains the cleaned datasets constructed for analysis.  

- **`model`**: Contains the fitted models.  

- **`other`**: Includes files unrelated to the project's core academic content.  
  - **`datasheet`**: Provides details about the raw data used in this study, including a Quarto document, reference bibliography file, and a PDF version of the datasheet.  
  - **`llm_usage`**: Contains five files detailing how ChatGPT-4 assisted with data cleaning, testing, exploratory data analysis, model selection, and presentation.  
  - **`sketches`**: Includes all sketches of figures and tables.  

- **`paper`**: Contains all files used to generate the paper, including the Quarto document, reference bibliography file, and the paper in PDF format.  

- **`scripts`**: Contains the R scripts used for simulating data, downloading data, cleaning data, testing data, fitting models, validating models, and conducting exploratory data analysis.  


## Statement on LLM usage
This project utilized OpenAI's ChatGPT-4 for assistance in various aspects of code writing and query resolution. Detailed conversations can be found in the directory: `other/llm_usage/`. Additionally, code comments clearly indicate sections where ChatGPT-4 provided support. The following subdirectories document specific contributions:

- `other/llm_usage/01-Data_Cleaning`: Explains how ChatGPT-4 assisted in the data cleaning process.  
- `other/llm_usage/02-Data_Test`: Describes ChatGPT-4's role in testing simulated and cleaned data.  
- `other/llm_usage/03-EDA`: Details ChatGPT-4's support in conducting exploratory data analysis.  
- `other/llm_usage/04-Model_Selection`: Highlights ChatGPT-4's contributions to model selection and knowledge dissemination about the models.  
- `other/llm_usage/05-Model_Presentation`: Showcases how ChatGPT-4 aided in presenting models using LaTeX.  


