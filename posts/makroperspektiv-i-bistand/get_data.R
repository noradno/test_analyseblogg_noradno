library(tidyverse)
library(WDI)

oda <- WDI(indicator = c("DT.ODA.ODAT.CD", "NY.GNP.MKTP.CD", "SP.POP.TOTL"), country = "all", start = 1960, end = 2022, extra = TRUE) 

selection <- c("Low income", "Lower middle income", "Upper middle income", "High income")
labels_no <- c("Lav\ninnt.", "Lavere\nm.innt.", "Høyere\nm.innt.", "Høy\ninnt.")

oda_data <- oda |> 
  group_by(iso3c) |> 
  filter(!is.na(DT.ODA.ODAT.CD)) |> 
  filter(year == max(year),
         country %in% selection) |> 
  mutate(oda_gni = DT.ODA.ODAT.CD / NY.GNP.MKTP.CD,
         oda_pop = DT.ODA.ODAT.CD / SP.POP.TOTL,
         country = case_when(country == "Low income" ~ "Lav\ninnt.",
                             country == "Lower middle income" ~ "Lavere\nm.innt.",
                             country == "Upper middle income" ~ "Høyere\nm.innt.",
                             country == "High income" ~ "Høy\ninnt.",
                             TRUE ~ NA_character_),
         country = factor(country, levels = labels_no))

saveRDS(oda_data, "posts/makroperspektiv-i-bistand/oda_data.rds")
