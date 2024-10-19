# OS_project_sustainability

This individual project was carried out for the course of International organizations in producing Official Statistics for the 6 free credits of the Master's degree in Data science at Sapienza University of Rome. It was made at the end of the second semester of the academic year 2023-2024 (June 2024).

Personal information:
| NAME and SURNAME | MATRICOLA | EMAIL |
| --- | --- | --- |
| Sofia Noemi Crobeddu | 2130389 | crobeddu.2130389@studenti.uniroma1.it | 

## PURPOSE

The aim of this project is to examines the sustainability of Colombia’s agriculture for the environmental dimension, focusing also on an analysis of water stress in the second part. In the first part this environmental dimension is measured through four proxy sub-indicators, and the trend and status assessment. In the second part of the prject, instead, I try to make a prediction of the Water Stress Level indicator in 2030, the year of the SDGs Agenda.

## REPOSITORIES

The repositories are two:
- **data**: it contains the csv files with the original datasets. The files inside are the following ones:
  - `condition.csv`: contains the health conditions. Each condition has an id, a name and an url link. It is also connected to an other csv files through the column source_id.
  - `drug.csv`: contains the healthcare drug's information. Each drug has an id, a name, a specific url link and a link to the drugbank.
  - `interaction.csv`: contains the interactions between drugs. Each interaction has an id and it is connected to an others csv files through the columns source_drug_id and target_drug_id.
  - `manufacturer.csv`: contains the manufacturer information. Each manufacturer has an id and a name.
  - `price.csv`: contains the medical product's price. Each price has an id, the value indicated by the column "price", a specific type and an url link. It is also connected to the others csv files through the columns product_id and store_id.
  - `product.csv`: contains the medical product's information. Each product has an id, a name, a type, an url link, the number of reviews. It is also connected to the others csv files through the columns source_id, drug_id and manufacturer_id.
  - `source.csv`: contains the source of information. Each source has an id a name (such as Wikidata, Drugbank, etc...) and the correspondent url. This dataset was not actually used and it is just put here for completeness.
  - `store.csv`: contains the store information. Each store has an id and a name.
  - `treatement.csv`: contains the treatements' information. Each treatment has an id, and it is connected to the others csv files through the columns source_id, drug_id and condition_id.

- **script**: it contains the sql files with the instructions performing the task. The files inside are the following ones:

`tables.sql`
> The SQL code of the initial tables;
