#############################################################################
#Script for the analysis in the paper "From colonial legacy to global integration: How war transformed Ukrainian vertebrate palaeontology"

#load packages
library("dplyr")
library("tidyr")
library("igraph")
library("ggraph")
library("ggplot2")
library("patchwork")
library("ggalluvial")
library("gridExtra")
library("purrr")
library("readr")
library("stringr")
library("tibble")
library("tidyverse")
library(codyn)
library(forcats)   # For fct_lump
library(circlize)
require(RColorBrewer)
library(tidyverse)
##############################################
# Load dataset
#############################################

#general dataset
df <- read_csv("df2.csv")

#data for Crimea post 2014
crimea <- read_csv("crimea_4.csv")

#data for the rest of Ukraine post 2014
ua<- read_csv("ua_14.csv")

#nodes and edges for the Chord diagrams/ networks for Crimea post 2014
#nodes <- read.csv("nodes1_crimea.csv", stringsAsFactors = FALSE)
#edges <- read.csv("edges_crimea.csv", stringsAsFactors = FALSE)

#nodes and edges for the Chord diagrams/ networks for the rest of Ukraine post 2014
#nodes <- read.csv("nodes_ua14.csv", stringsAsFactors = FALSE)
#edges <- read.csv("edges_ua14.csv", stringsAsFactors = FALSE)

#Process data
crimea <- crimea %>%
  mutate(Ref_pubyr = suppressWarnings(as.integer(Ref_pubyr)))
crimea=crimea[1:259,]
combined <- bind_rows(
  crimea,
  df %>% select(any_of(names(crimea)))
)

df=combined



####################################################
#publications by year

# --- Step 0: Create decade column
df2=df
df2 <- df2 %>%
  mutate(decade = floor(Ref_pubyr / 10) * 10)




#pubs per year

table(df2$Ref_pubyr)


library(dplyr)
library(ggplot2)
library(strucchange)
library(tidyr)

df2 <- df2 %>%
  mutate(year = as.numeric(Ref_pubyr)) %>%
  filter(!is.na(year))

# Count publications per year
pubs_per_year <- df2 %>%
  group_by(year) %>%
  summarise(n_pubs = n(), .groups = "drop") %>%
  arrange(year)


####################################
#breakpoint analysis of the publications per year
#====================================
# Breakpoint analysis of the publications per year



# --- Step 1: Filter out 2025 ---
df_filtered <- df %>%
  filter(Ref_pubyr != 2025)

# --- Step 2: Count publications per year ---
pubs_per_year <- table(df_filtered$Ref_pubyr)

# --- Step 3: Convert to data frame ---
pubs_df <- as.data.frame(pubs_per_year)
colnames(pubs_df) <- c("Ref_pubyr", "pubs")
pubs_df$Ref_pubyr <- as.numeric(as.character(pubs_df$Ref_pubyr))
pubs_df$pubs <- as.numeric(pubs_df$pubs)

# --- Step 4: Fill missing years with 0 publications ---
pubs_full <- pubs_df %>%
  complete(Ref_pubyr = seq(min(pubs_df$Ref_pubyr), max(pubs_df$Ref_pubyr), by = 1),
           fill = list(pubs = 0)) %>%
  arrange(Ref_pubyr)

# --- Step 5: Create time series ---
pubs_ts <- ts(pubs_full$pubs, start = min(pubs_full$Ref_pubyr))

# --- Step 6: Breakpoint analysis ---
break_model <- breakpoints(pubs_ts ~ 1)

# Extract fitted lines
pubs_full$fitted_1 <- fitted(break_model, breaks = 1)
pubs_full$fitted_2 <- fitted(break_model, breaks = 2)

# Extract breakpoint years
bp_years <- break_model$breakpoints
bp_years <- bp_years + min(pubs_full$Ref_pubyr) - 1
bp_years <- bp_years[!is.na(bp_years)]   # remove NAs if fewer breakpoints

# --- Step 7: Plot with ggplot2 ---
ggplot(pubs_full, aes(x = Ref_pubyr, y = pubs)) +
  # raw data
  geom_line(color = "grey40", linewidth = 0.5) +
  geom_point(color = "grey40", size = 1) +

  # fitted lines (pastel colours)
  geom_line(aes(y = fitted_1), color = "#F1948A", linewidth = 1.2, linetype = "solid") +   # pastel pink
  geom_line(aes(y = fitted_2), color = "#85C1E9", linewidth = 1.2, linetype = "dashed") +  # pastel blue

  # breakpoints
  geom_vline(xintercept = bp_years, color = "#27AE60", linetype = "dotted", linewidth = 1) +
  annotate("text",
           x = bp_years,
           y = max(pubs_full$pubs) * 1.05,   # slightly above max
           label = bp_years,
           color = "#27AE60", fontface = "bold", size = 3.5) +

  labs(
    title = "Publications per Year (2025 Excluded) with Breakpoints",
    x = "Year",
    y = "Publications"
  ) +
  expand_limits(y = max(pubs_full$pubs) * 1.1) +   # leave space for labels
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# Breakpoint analysis of the publications in Ukrainian per year


# ---- Ukrainian subset ----
ukr_yearly <- df2 %>%
  filter(Language == "Ukrainian") %>%
  group_by(Ref_pubyr) %>%
  summarise(pubs = n(), .groups = "drop") %>%
  arrange(Ref_pubyr)

ukr_yearly_full <- ukr_yearly %>%
  complete(
    Ref_pubyr = seq(min(df2$Ref_pubyr), max(df2$Ref_pubyr), by = 1),
    fill = list(pubs = 0)
  ) %>%
  arrange(Ref_pubyr)

# ---- Trim leading zeros for Ukrainian ----
first_nonzero_ukr <- ukr_yearly_full %>%
  filter(pubs > 0) %>%
  summarise(min_year = min(Ref_pubyr)) %>%
  pull(min_year)

ukr_yearly_trimmed <- ukr_yearly_full %>%
  filter(Ref_pubyr >= first_nonzero_ukr)

# Time series from trimmed range
ukr_ts <- ts(ukr_yearly_trimmed$pubs, start = min(ukr_yearly_trimmed$Ref_pubyr))

# Re-run breakpoints
break_model_ukr <- breakpoints(ukr_ts ~ 1)

plot(ukr_ts, main = "Ukrainian Language Publications Over Time (Trimmed Leading Zeros)")
lines(fitted(break_model_ukr, breaks = 1), col = "blue")
lines(fitted(break_model_ukr, breaks = 2), col = "red")
abline(v = breakpoints(break_model_ukr)$breakpoints + min(ukr_yearly_trimmed$Ref_pubyr) - 1,
       col = "darkgreen", lty = 2)


#language by year

# Step 1: Remove NA Languages
df_filtered <- df %>%
  filter(!is.na(Language))

# Step 2: Identify top 5 most used languages
top_langs <- df_filtered %>%
  count(Language, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(Language)

# Step 3: Keep only top 5 languages
df_top <- df_filtered %>%
  filter(Language %in% top_langs)

# Step 4: Summarize counts per year per language
lang_counts <- df_top %>%
  group_by(Ref_pubyr, Language) %>%
  summarise(pub_count = n(), .groups = "drop")

# Step 5: Fill missing language-year combinations with 0
all_years <- seq(min(df_top$Ref_pubyr), max(df_top$Ref_pubyr))
lang_counts_full <- expand.grid(Ref_pubyr = all_years, Language = top_langs) %>%
  left_join(lang_counts, by = c("Ref_pubyr", "Language")) %>%
  mutate(pub_count = ifelse(is.na(pub_count), 0, pub_count))

# Step 6: Define x-axis breaks every 10 years
x_breaks <- seq(min(all_years), max(all_years), by = 10)

# Step 7: Plot stacked barplot
ggplot(lang_counts_full, aes(x = Ref_pubyr, y = pub_count, fill = Language)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Year",
    y = "Number of Publications",
    title = "Top 5 Languages per Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################
#Country vs taxa studied
#taxa from countries

library(ggalluvial)
# Step 1: Unnest Countries
df_countries <- df %>%
  mutate(Countries = str_split(Countries, ";")) %>%
  unnest(Countries)

# Step 2a: Unnest Class separately (keep rows with non-NA Class)
df_class <- df_countries %>%
  filter(!is.na(Class)) %>%
  mutate(Class = str_split(Class, ";")) %>%
  unnest(Class) %>%
  select(Countries, taxa = Class)

# Step 2b: Unnest Order separately (keep rows with non-NA Order)
df_order <- df_countries %>%
  filter(!is.na(Order)) %>%
  mutate(Order = str_split(Order, ";")) %>%
  unnest(Order) %>%
  select(Countries, taxa = Order)

# Step 3: Bind Class and Order taxa together
df_taxa <- bind_rows(df_class, df_order) %>%
  distinct()

print(df_taxa)


#Sankey/alluvial plot



# Prepare data for sankey: counts of each Country-Taxa pair (strength)
df_sankey <- df_taxa %>%
  count(Countries, taxa)

ggplot(df_sankey,
       aes(y = n, axis1 = Countries, axis2 = taxa)) +
  geom_alluvium(aes(fill = Countries), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Countries", "Taxa"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Country - Taxa connections")


#class plot

# Start from your original df and Countries splitted
df_countries <- df %>%
  mutate(Countries = str_split(Countries, ";")) %>%
  unnest(Countries)

# Prepare Class data
df_class <- df_countries %>%
  filter(!is.na(Class)) %>%
  mutate(Class = str_split(Class, ";")) %>%
  unnest(Class) %>%
  group_by(Countries, Class) %>%
  tally(name = "count") %>%
  ungroup()

# Filter Class taxa with > 20 occurrences (across all countries)
top_class <- df_class %>%
  group_by(Class) %>%
  summarise(total = sum(count)) %>%
  filter(total > 20) %>%
  pull(Class)

df_class_filtered <- df_class %>%
  filter(Class %in% top_class)

# Plot Class alluvial
ggplot(df_class_filtered,
       aes(y = count, axis1 = Countries, axis2 = Class)) +
  geom_alluvium(aes(fill = Countries), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Countries", "Class"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Country - Class connections (taxa with > 10 occurrences)")


#orders plot
# Prepare Order data
df_order <- df_countries %>%
  filter(!is.na(Order)) %>%
  mutate(Order = str_split(Order, ";")) %>%
  unnest(Order) %>%
  group_by(Countries, Order) %>%
  tally(name = "count") %>%
  ungroup()

# Filter Order taxa with > 10 occurrences
top_order <- df_order %>%
  group_by(Order) %>%
  summarise(total = sum(count)) %>%
  filter(total > 20) %>%
  pull(Order)

df_order_filtered <- df_order %>%
  filter(Order %in% top_order)

# Plot Order alluvial
ggplot(df_order_filtered,
       aes(y = count, axis1 = Countries, axis2 = Order)) +
  geom_alluvium(aes(fill = Countries), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Countries", "Order"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Country - Order connections (taxa with > 10 occurrences)")

#####################################


df <- df %>%
  filter(Ref_pubyr <= 2012)

df <- df %>%
  mutate(Language = if_else(tolower(Language) == "russian", "Russian", Language))

df <- df %>%
  mutate(Order = if_else(tolower(Language) == "Proboscidae", "Proboscoidea", Order))






# Step 1: Unnest countries
df_countries <- df %>%
  mutate(Countries = str_split(Countries, ";")) %>%
  unnest(Countries)
df_countries <- df %>%
  mutate(Countries = str_split(Countries, ";")) %>%
  unnest(Countries) %>%
  mutate(Countries = str_trim(Countries))  # trim whitespace

# Step 2: Filter countries with more than 20 publications
country_pub_counts <- df_countries %>%
  count(Countries) %>%
  filter(n > 20) %>%
  pull(Countries)

df_countries_filtered <- df_countries %>%
  filter(Countries %in% country_pub_counts)

# --------- CLASS ---------

# Prepare Class data
df_class <- df_countries_filtered %>%
  filter(!is.na(Class)) %>%
  mutate(Class = str_split(Class, ";")) %>%
  unnest(Class) %>%
  group_by(Countries, Class) %>%
  tally(name = "count") %>%
  ungroup()
df_class <- df_class %>%
  mutate(Class = str_split(Class, ";")) %>%
  unnest(Class) %>%
  mutate(Class = str_trim(Class))  # trim whitespace
# Filter Class taxa with more than 20 occurrences overall
top_class <- df_class %>%
  group_by(Class) %>%
  summarise(total = sum(count)) %>%
  filter(total > 20) %>%
  pull(Class)

df_class_filtered <- df_class %>%
  filter(Class %in% top_class)

# Plot Class alluvial
class=ggplot(df_class_filtered,
             aes(y = count, axis1 = Countries, axis2 = Class)) +
  geom_alluvium(aes(fill = Countries), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Countries", "Class"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Country - Class connections (taxa with > 10 occurrences, countries with > 20 publications)")

# --------- ORDER ---------

# Prepare Order data
df_order <- df_countries_filtered %>%
  filter(!is.na(Order)) %>%
  mutate(Order = str_split(Order, ";")) %>%
  unnest(Order) %>%
  group_by(Countries, Order) %>%
  tally(name = "count") %>%
  ungroup()
df_order <- df_order %>%
  mutate(Order = str_split(Order, ";")) %>%
  unnest(Order) %>%
  mutate(Order = str_trim(Order))  # trim whitespace
# Filter Order taxa with more than 20 occurrences overall
top_order <- df_order %>%
  group_by(Order) %>%
  summarise(total = sum(count)) %>%
  filter(total > 20) %>%
  pull(Order)

df_order_filtered <- df_order %>%
  filter(Order %in% top_order)

# Plot Order alluvial
order=ggplot(df_order_filtered,
             aes(y = count, axis1 = Countries, axis2 = Order)) +
  geom_alluvium(aes(fill = Countries), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Countries", "Order"), expand = c(.05, .05)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Country - Order connections (taxa with > 10 occurrences, countries with > 20 publications)")

grid.arrange(class, order, ncol = 2)

########################################################
#pie charts of post 2014 specimens distribution by collection



View(ua)
# Load required libraries

# Function to prepare top 5 + Others (removes NAs)
prepare_top5 <- function(data, column_name, type_value, top_n = 5) {
  data %>%
    filter(Type == type_value) %>%
    filter(!is.na(!!sym(column_name))) %>%      # Remove NA
    count(!!sym(column_name)) %>%
    arrange(desc(n)) %>%
    mutate(
      !!sym(column_name) := ifelse(row_number() <= top_n, as.character(!!sym(column_name)), "Others")
    ) %>%
    group_by(!!sym(column_name)) %>%
    summarise(n = sum(n), .groups = "drop")
}

# Function to create pie chart
create_pie <- function(data, column_name, title_text) {
  ggplot(data, aes(x = "", y = n, fill = !!sym(column_name))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(title = title_text, fill = column_name) +
    theme_void() +
    theme(legend.position = "right")
}

# Prepare Material deposition plots
material_mainland <- prepare_top5(ua, "Material deposition", "Mainland")
material_crimea   <- prepare_top5(ua, "Material deposition", "Crimea")

# Create Material deposition pies
p1 <- create_pie(material_mainland, "Material deposition", "Material deposition - Mainland")
p2 <- create_pie(material_crimea,   "Material deposition", "Material deposition - Crimea")

# Arrange Material deposition plots in 1 row
grid.arrange(p1, p2, ncol = 2)

# Prepare Country of deposition plots
country_mainland <- prepare_top5(ua, "Country of deposition", "Mainland")
country_crimea   <- prepare_top5(ua, "Country of deposition", "Crimea")

# Create Country of deposition pies
p3 <- create_pie(country_mainland, "Country of deposition", "Country of deposition - Mainland")
p4 <- create_pie(country_crimea,   "Country of deposition", "Country of deposition - Crimea")

# Arrange Country of deposition plots in 1 row
grid.arrange(p3, p4, ncol = 2)


###############################################################################################
###############################################################################################
#Network and chord diagramm computing

#=========================
#  1. CLEAN & PREPARE DATA
#=========================
crimea <- crimea %>%
  mutate(
    Countries = strsplit(Countries, ";"),
    Countries = map(Countries, ~ trimws(.)),
    Countries = map(
      Countries,
      ~ str_replace_all(., regex("^belgium$", ignore_case = TRUE), "Belgium")
    )
  )
#=========================
 # 2. BUILD EDGE LIST (COLLABORATIONS)
#=========================
  pairs_df <- crimea %>%
  filter(lengths(Countries) >= 2) %>%
  rowwise() %>%
  mutate(pairs = list(as.data.frame(t(combn(Countries, 2))))) %>%
  unnest(pairs) %>%
  ungroup() %>%
  rename(from = V1, to = V2) %>%
  select(from, to, Type) %>%
  count(from, to, Type, name = "n", sort = TRUE)

pairs_df_filtered <- pairs_df %>%
  filter(Type %in% c("Crimea", "Mainland")) %>%
  distinct()
#=========================
 # 3. CREATE CHORD-READY EDGE TABLE (KEY STEP)
#=========================
  edges_by_type <- pairs_df_filtered %>%
  transmute(
    Source = from,
    Target = to,
    Weight = n,
    Type
  )


#=========================
 # 4. NETWORK FUNCTION (LOUVAIN + CENTRALITY)
#=========================
plot_network_by_type <- function(df_edges, title) {

  g <- igraph::graph_from_data_frame(
    df_edges %>% select(from = Source, to = Target, weight = Weight),
    directed = FALSE
  )

  # Node size
  V(g)$size <- igraph::degree(g) * 3 + 5

  # Louvain communities
  communities <- igraph::cluster_louvain(g)
  V(g)$community <- as.numeric(igraph::membership(communities))

  # Centrality table
  centrality <- data.frame(
    Node        = V(g)$name,
    Degree      = igraph::degree(g, normalized = TRUE),
    Betweenness = igraph::betweenness(g, normalized = TRUE),
    Eigenvector = igraph::eigen_centrality(g)$vector,
    Community   = V(g)$community,
    stringsAsFactors = FALSE
  )

  # Plot
  p <- ggraph(g, layout = "stress") +
    geom_edge_link(aes(width = weight), alpha = 0.6) +
    geom_node_point(aes(size = size, color = as.factor(community))) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    theme_void() +
    labs(title = title, color = "Community") +
    scale_edge_width(range = c(0.5, 4))

  list(graph = g, plot = p, centrality = centrality)
}

#=========================
 # 5. COMPUTE NETWORKS
#=========================
  network_results <- list()

for (t in unique(edges_by_type$Type)) {
  df_edges <- edges_by_type %>% filter(Type == t)
  network_results[[t]] <- plot_network_by_type(
    df_edges,
    paste("Country Collaboration Network —", t)
  )
}

#=========================
 # 6. CHORD DIAGRAM FUNCTION
#=========================
  plot_chord <- function(edges, top_n = 15, transparency = 0.2) {

    node_weights <- bind_rows(
      edges %>% select(Node = Source, Weight),
      edges %>% select(Node = Target, Weight)
    ) %>%
      group_by(Node) %>%
      summarise(Total_Weight = sum(Weight), .groups = "drop") %>%
      arrange(desc(Total_Weight))

    top_nodes <- head(node_weights$Node, top_n)

    edges_top <- edges %>%
      filter(Source %in% top_nodes & Target %in% top_nodes)

    mat <- matrix(
      0,
      length(top_nodes),
      length(top_nodes),
      dimnames = list(top_nodes, top_nodes)
    )

    for (i in seq_len(nrow(edges_top))) {
      s <- edges_top$Source[i]
      t <- edges_top$Target[i]
      w <- edges_top$Weight[i]
      mat[s, t] <- mat[s, t] + w
      mat[t, s] <- mat[t, s] + w
    }

    palette <- colorRampPalette(c(
      "#f3e5d8",
      "#e6c3a1",
      "#d9a066",
      "#c97a3a",
      "#b25a1b"
    ))

    grid_colors <- setNames(palette(length(top_nodes)), top_nodes)

    circos.clear()
    circos.par(start.degree = 90, gap.degree = 4)

    chordDiagram(
      mat,
      grid.col = grid_colors,
      transparency = transparency,
      annotationTrack = "grid",
      preAllocateTracks = list(track.height = 0.12)
    )

    circos.trackPlotRegion(
      track.index = 1,
      panel.fun = function(x, y) {
        sector <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(
          mean(xlim),
          ylim[1] + 0.1,
          sector,
          facing = "clockwise",
          niceFacing = TRUE,
          adj = c(0, 0.5),
          cex = 0.8
        )
      },
      bg.border = NA
    )
  }

#=========================
 # 7. FINAL PLOTS
#=========================
  # Networks
  network_results[["Crimea"]]$plot
network_results[["Mainland"]]$plot

# Chord diagrams
par(mfrow = c(1, 2))

plot_chord(
  edges_by_type %>% filter(Type == "Crimea"),
  transparency = 0.2
)

plot_chord(
  edges_by_type %>% filter(Type == "Mainland"),
  transparency = 0.1
)

# Run function and store results
network_res <- plot_network_by_type(df_edges, "Country Collaboration Network")

# Access igraph object
g <- network_res$graph

# Access plot
network_plot <- network_res$plot

# Access centrality table
centrality_table <- network_res$centrality

