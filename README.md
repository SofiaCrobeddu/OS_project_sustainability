# OS_project_sustainability

This individual project was carried out for the course of International organizations in producing Official Statistics for the 6 free credits of the Master's degree in Data science at Sapienza University of Rome. It was made at the end of the second semester of the academic year 2023-2024 (June 2024).

Personal information:
| NAME and SURNAME | MATRICOLA | EMAIL |
| --- | --- | --- |
| Sofia Noemi Crobeddu | 2130389 | crobeddu.2130389@studenti.uniroma1.it | 

## PURPOSE

The aim of this project is to examines the sustainability of Colombia’s agriculture for the environmental dimension, focusing also on an analysis of water stress in the second part. In the first part this environmental dimension is measured through four proxy sub-indicators, and the trend and status assessment. In the second part of the prject, instead, I try to make a prediction of the Water Stress Level indicator , doing also a preliminary reflexion on avocado production, in 2030 (the year of the SDGs Agenda).

## REPOSITORIES

The macro-repositories are:
- **data**: it contains the csv files with the original datasets. The files and repositories with data inside, are the following ones:
  - `colombia_emissions_total_CO2.csv`: contains Colombia's total emissions of CO2. These data were needed to calculate the Greenhouse Gas Emission, since they represent the numerator of this proxy sub-indicator. From FAOstat.
  - `colombia_fertilizer_use_intensity.csv`: contains Colombia's fertilizer use intensity. From FAOstat.
  - `colombia_pesticides_use_intensity.csv`: contains Colombia's pesticides use intensity. From FAOstat.
  - `colombia_value_prod_agriculture.csv`: contains Colombia's value of agricultural production. These data were needed to calculate the Greenhouse Gas Emission, since they represent the denominator of this proxy sub-indicator. From FAOstat.
  - `colombia_water_stress.csv`: contains Colombia's water stress. From FAOstat.
  - There are also two other internal repositories:
     - **water stress - World Bank**: it contains the dataset `water stress and variables - WB.csv` regarding data for water stress level for Colombia, from World Bank databases. Comapred to the dataset from FAOstat, this one contains more specific information such as "Annual freshwater withdrawals, agriculture (% of total freshwater withdrawal)", "Annual freshwater withdrawals, domestic (% of total freshwater withdrawal)", "Annual freshwater withdrawals, industry (% of total freshwater withdrawal)", "Annual freshwater withdrawals, total (billion cubic meters)", "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources", "Renewable internal freshwater resources, total (billion cubic meters)", "Average precipitation in depth (mm per year)", "Population, total", "GDP per capita (constant 2015 US$)", "Population density (people per sq. km of land area)".
     - **ML_part**: it contains the dataset `data_cleaned.csv` that comes from the previous one mentioned before (`water stress and variables - WB.csv`), after renaiming columns, converting in number the observations and reindexing.
- **script**: it contains the 
