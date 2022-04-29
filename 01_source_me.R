# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
library(tidyverse)
source(here::here("R","functions.R"))
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
  "Population 2020: raw data",
  "Population 2021: raw data",
  "Population: growth",
  "x1",
  "x2",
  "Income Assistance: index",
  "Building Permits: index",
  "Job Postings: index",
  "MPI Estimated Cost: index",
  "Population: index",
  "Overall: index"
)
district_conditions <- readxl::read_excel(here::here("raw_data", "2022 update data.xlsx"),
                                          range = "B6:V42",
                                          col_names = column_names,
                                          na = c("n/a","n.a.","n.a"))%>%
  select(-x1,-x2)%>%
  fill(Region)%>%
  select(Region,
         District,
         `Overall: index`,
         contains("index"),
         contains("capita"),
         contains("growth"),
         everything())%>%
  filter(!is.na(District))%>%
  mutate(District=janitor::make_clean_names(District),
         District=str_replace(District, "greater_vancouver", "metro_vancouver"),
         District=str_replace(District, "powell_river", "qathet"),
         District=str_replace(District, "skeena_queen_charlotte", "north_coast")
  )%>%
  pivot_longer(cols = -c(Region, District), names_to = "name", values_to = "value")%>%
  janitor::clean_names()%>%
  mutate(name2 = name)%>%
  separate(name2, into = c("thing", "type"), sep = ": ")%>%
  mutate(type = str_replace_all(type, "per capita", "normalized"),
         type = str_replace_all(type, "per 1,000 capita", "normalized"),
         type = str_replace_all(type, "growth", "normalized")
  )


regional_districts <- sf::st_read(here::here("map_data",
                                             "Regional Districts",
                                             "ABMS_RD_polygon.shp"))%>%
  janitor::clean_names()%>%
  select(aa_name, geometry)
one_large_municipality <- sf::st_read(here::here("map_data",
                                                 "Municipalities",
                                                 "ABMS_MUNI_polygon.shp"))%>%
  janitor::clean_names()%>%
  filter(area_sqm == max(area_sqm))%>%
  select(aa_name, geometry)
regional_districts <- bind_rows(regional_districts, one_large_municipality)%>%
  mutate(aa_name = janitor::make_clean_names(aa_name),
         aa_name = str_replace(aa_name, "regional_district_of_", ""),
         aa_name = str_replace(aa_name, "_regional_district", ""),
         aa_name = str_replace(aa_name, "_region_unincorporated", ""),
         aa_name = str_replace(aa_name, "_regional_municipality", "")
  )


df <- full_join(regional_districts, district_conditions, by=c("aa_name"="district"))

choropleths <- tibble(name=unique(district_conditions$name))%>%
  filter(!grepl('raw data', name))%>%
  mutate(the_map=map(name, static_map, df))

saveRDS(choropleths, here::here("processed_data", "choropleths.rds"), compress = FALSE)
saveRDS(district_conditions, here::here("processed_data", "district_conditions.rds"), compress = FALSE)
