# PalaeoColonialismUA
Code and data for a paper "From colonial legacy to global integration: How war transformed Ukrainian vertebrate palaeontology"

Files and variables
File: crimea_4.csv:
Description: bibliographic data and dataset for publications on Crimean vertebrate fossils post 2014

Variables
1: ID of the publication
Responsible: Person  responsible for this part of the dataset
Ref_pubyr: Year of the publication
Class: Classes of vertebrata mentioned in the paper
Order: Orders of vertebrata mentioned in the paper
Data_enterer: Person who entered the data
Type_locality: Type locality of specimens
Type_country: Country, in which type locality is located
Material deposition: Institution where material from the paper is deposited
Affiliations: Affiliations of the authors´of the paper
Countries: Countries of those affiliations
Language: Language in which paper was published
DOI: Doi of the paper
Note: miscellaneous notes
Type: Accession number of the type specimen/ s
File: df2.csv
Description: general bibliographic data and dataset for publications on the rest of Ukrainian vertebrate fossils

Variables
1: ID of the publication
Responsible: Person  responsible for this part of the dataset
Ref_pubyr: Year of the publication
Class: Classes of vertebrata mentioned in the paper
Order: Orders of vertebrata mentioned in the paper
Data_enterer: Person who entered the data
Type_locality: Type locality of specimens
Type_country: Country, in which type locality is located
Affiliations: Affiliations of the authors´of the paper
Countries: Countries of those affiliations
Language: Language in which paper is written
File: ua_14.csv
Description: bibliographic data and dataset for publications for the rest of Ukrainian vertebrate fossils post 2014

Variables
1: ID of the publication
Responsible: Person  responsible for this part of the dataset
Ref_pubyr: Year of the publication
Class: Classes of vertebrata mentioned in the paper
Order: Orders of vertebrata mentioned in the paper
Data_enterer: Person who entered the data
Type_locality: Type locality of specimens
Type_country: Country, in which type locality is located
Material deposition: Institution where material from the paper is deposited
Affiliations: Affiliations of the authors´of the paper
Countries: Countries of those affiliations
Language: Language in which paper was published
DOI: Doi of the paper
Note: miscellaneous notes
Type: Accession number of the type specimen/ s
File: main_bibliographic_database.xlsx
**Description: **raw bibliographic data (in Cyrillic) and dataset for publications on the rest of Ukrainian vertebrate fossils

Variables
1: Publication ID

Responsible: Person  responsible for this part of the dataset
Ref_author: Author/s of the paper
Ref_pubyr: Year of the publication
Primary_reference: First author
Class: Classes of vertebrata mentioned in the paper
Order: Orders of vertebrata mentioned in the paper
Data_enterer: Person who entered the data
Type_locality: Type locality of specimen
Type_country: Country of that locality
Country_reposited: Country in which specimen is deposited
Institution_reposited: Institution where its deposited
Type specimen(s): Accession number of the specimen/s
Affiliations_of_authors (separated by ;): Affiliation of the authors of the publication
Countries_of_authors (separated by ;): Countries of affiliation of the authors of the publication
Language: Language in which article was published
Reference: Full reference
Biographic info source: additional biographic info on the authors (years of life)
File: single_script1.R
Description:  R code, described in (Code/Software)

File: edges.xlsx
Description: 

Source: Country 1 in the network connection
Target: Country 1 in the network connection
Type: Type of connection - undirected
Id: Id of the edge
Label: Label of the edge
Weight:  relative wright of the edge, based on the number of connections, between the source and target
File: edges_dynamic_gephi.csv
Description: 

Variables
Source: Country 1 in the network connection
Target: Country 1 in the network connection
Type: Type of connection - undirected
Id: Id of the edge
Label: Label of the edge
timeset: time set (decade) of the publication
Weight: relative wright of the edge, based on the number of connections, between the source and target
File: nodes.xlsx
Description: Nodes for the general cooperation network 

Variables
id: Id of the node
Label: Node name (country)
File: nodes_dynamic.csv
Description: Nodes for the dynamic (decade-by-decade) cooperation network 

Variables
id: Id of the node
Label: Node name (country)
Code/software
single_script1.R
"From colonial legacy to global integration: How war transformed Ukrainian vertebrate palaeontology"

This code performs data processing, visualization, and network analysis on publication and specimen datasets to study trends in vertebrate paleontology in Ukraine.

1. Packages
The script uses the following R packages for data manipulation, visualization, and network analysis:

Data manipulation: dplyr, tidyr, purrr, readr, stringr, tibble, forcats
Visualization: ggplot2, patchwork, ggalluvial, gridExtra, circlize, RColorBrewer
Network analysis: igraph, ggraph
Time series and breakpoints: strucchange
Diversity metrics: codyn
Other tidy tools: tidyverse
2. Data Input
The script reads multiple CSV datasets:

df2.csv – Main dataset with publications and specimens
crimea_4.csv – Crimea-specific post-2014 data
ua_14.csv – Rest of Ukraine post-2014 data
Optional network data for chord diagrams (nodes/edges) are commented out but can be included for advanced network analysis.

3. Data Processing
Converts publication year (Ref_pubyr) to numeric and filters invalid values.
Combines datasets from Crimea and Ukraine into a single dataset.
Creates additional columns such as decade for temporal aggregation.
Cleans and trims country and taxonomic data for further analysis.
4. Publication Analysis
4.1 Publications per Year
Counts publications by year.
Fills missing years with zeros to generate complete time series.
4.2 Breakpoint Analysis
Uses strucchange::breakpoints() to identify changes in publication trends.
Generates fitted lines for 1- and 2-breakpoint models.
Produces ggplot visualizations highlighting breakpoints.
4.3 Language Trends
Counts publications per year by language.
Focuses on top 5 languages.
Generates stacked barplots over time.
Performs separate breakpoint analysis for Ukrainian-language publications.
5. Country vs. Taxa Analysis
Unnests semicolon-separated Countries, Class, and Order columns.
Creates alluvial (Sankey) plots showing connections between countries and taxa (Class and Order).
Filters to include only taxa with more than 20 occurrences and countries with more than 20 publications.
Produces side-by-side alluvial plots for Class and Order.
6. Specimen Distribution (Post-2014)
Prepares pie charts for:
Material deposition (Mainland vs. Crimea)
Country of deposition (Mainland vs. Crimea)
Groups smaller categories into “Others” for clarity.
Uses gridExtra::grid.arrange() to display multiple pie charts in a single row.
7. Network and Chord Diagram Analysis
7.1 Data Preparation
Splits and cleans country lists.
Builds edge lists for collaborations between countries (nodes = countries, edges = co-publications).
7.2 Network Construction
Uses igraph and ggraph to:
Compute degree, betweenness, and eigenvector centrality.
Identify Louvain communities.
Visualize networks with node size proportional to degree.
7.3 Chord Diagrams
Builds adjacency matrices for top collaborating countries.
Uses circlize::chordDiagram() to visualize collaboration intensity.
Produces side-by-side chord diagrams for Crimea and Mainland datasets.
8. Output
Publication plots: Time series, breakpoints, language trends.
Alluvial plots: Country–Class and Country–Order connections.
Pie charts: Material and country of deposition distributions.
Networks: Country collaboration networks with centrality metrics.
Chord diagrams: Top-country collaboration visualizations.
The script also stores results for further inspection:

network_results – Network plots, graphs, and centrality tables
network_plot – Individual network plot
centrality_table – Centrality metrics for all nodes
