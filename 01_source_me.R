library(tidyverse)
bins <- 10

regional_districts <- sf::st_read(here::here(
  "map_data",
  "Regional Districts",
  "ABMS_RD_polygon.shp"
)) %>%
  janitor::clean_names() %>%
  select(aa_name, geometry)
one_large_municipality <- sf::st_read(here::here(
  "map_data",
  "Municipalities",
  "ABMS_MUNI_polygon.shp"
)) %>%
  janitor::clean_names() %>%
  filter(area_sqm == max(area_sqm)) %>%
  select(aa_name, geometry)
regional_districts <- bind_rows(regional_districts, one_large_municipality) %>%
  mutate(
    aa_name = janitor::make_clean_names(aa_name),
    aa_name = str_replace(aa_name, "regional_district_of_", ""),
    aa_name = str_replace(aa_name, "_regional_district", ""),
    aa_name = str_replace(aa_name, "_region_unincorporated", ""),
    aa_name = str_replace(aa_name, "_regional_municipality", "")
  )

column_names <- c(
  "Region",
  "District",
  "Income Assistance: raw data",
  "Income Assistance: per capita",
  "Building Permits: raw data",
  "Building Permits: per 1,000 capita",
  "Job Postings: raw data",
  "Job Postings: per capita",
  "MPI Estimated Cost: raw data",
  "MPI Estimated Cost: per 1,000 capita",
  "Small Business Registrations: raw data",
  "Population 2020: raw data",
  "Population 2021: raw data",
  "Population: growth",
  "x1",
  "x2",
  "Income Assistance: index",
  "Building Permits: index",
  "Job Postings: index",
  "MPI Estimated Cost: index",
  "Small Business Registration: index",
  "Population: index",
  "Overall: index"
)
district_conditions <- readxl::read_excel(here::here("raw_data", "2022 update data.xlsx"),
  sheet = "RD Current Conditions Monitor",
  range = "B6:X42",
  col_names = column_names,
  na = c("n/a", "n.a.", "n.a")
) %>%
  select(-x1, -x2) %>%
  fill(Region) %>%
  filter(!is.na(District)) %>%
  mutate(
    District = janitor::make_clean_names(District),
    District = str_replace(District, "greater_vancouver", "metro_vancouver"),
    District = str_replace(District, "powell_river", "qathet"),
    District = str_replace(District, "skeena_queen_charlotte", "north_coast")
  ) %>%
  select(Region, District, ends_with("raw data")) %>%
  mutate(
    `Income Assistance: per capita` = `Income Assistance: raw data` / `Population 2021: raw data`,
    `Building Permits: per capita` = `Building Permits: raw data` / (`Population 2021: raw data`),
    `Job Postings: per capita` = `Job Postings: raw data` / `Population 2021: raw data`,
    `MPI Estimated Cost: per capita` = `MPI Estimated Cost: raw data` / (`Population 2021: raw data`),
    `Small Business Registrations: per capita` = `Small Business Registrations: raw data` / (`Population 2021: raw data`),
    `Population: growth` = `Population 2021: raw data` / `Population 2020: raw data` - 1
  )

pca <- district_conditions %>%
  select(District, contains("per capita"), contains("growth")) %>%
  column_to_rownames(var = "District") %>%
  as.matrix() %>%
  nipals::nipals(fitted = TRUE)

class(pca) <- "princomp"
pca$sdev <- sqrt(pca$eig)
pca$n.obs <- dim(pca$scores)[1]

pca_index <- as_tibble(pca$scores, rownames = "district") %>%
  select(district, `PCA: index` = PC1)

district_conditions <- full_join(district_conditions, pca_index, by = c("District" = "district"))%>%
  mutate(
    `Income Assistance: decile` = ntile(-`Income Assistance: per capita`, bins), # negative because lower is better.
    `Building Permits: decile` = ntile(`Building Permits: per capita`, bins),
    `Job Postings: decile` = ntile(`Job Postings: per capita`, bins),
    `MPI Estimated Cost: decile` = ntile(`MPI Estimated Cost: per capita`, bins),
    `Small Business Registrations: decile` = ntile(`Small Business Registrations: per capita`, bins),
    `Population: decile` = ntile(`Population: growth`, bins)
  ) %>%
  rowwise() %>%
  mutate(`Overall: index` = mean(c(
    `Income Assistance: decile`,
    `Building Permits: decile`,
    `Job Postings: decile`,
    `MPI Estimated Cost: decile`,
    `Small Business Registrations: decile`,
    `Population: decile`
  ), na.rm = TRUE)) %>%
  pivot_longer(cols = -c(Region, District), names_to = "name", values_to = "value")%>%
  full_join(regional_districts, by = c("District" = "aa_name")) %>%
  na.omit()%>%
  mutate(name2=name,
         District= str_to_title(str_replace_all(District, "_", " ")))%>%
  separate(name2, into=c("thing", "type"), sep = ": ")%>%
  mutate(type=case_when(type=="per capita"~"normalized",
                        type=="growth"~"normalized",
                        TRUE~type))

saveRDS(district_conditions,here::here("processed_data","district_conditions.rds"))
saveRDS(pca,here::here("processed_data","pca.rds"))
