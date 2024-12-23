library(dplyr)

# Get income groups and regions
wdi <- WDI::WDI(country = "all", 
                start = 2019, 
                indicator = 
                  c("NY.GDP.PCAP.KD"), 
                extra = TRUE) |> 
  select(iso3c, income, region) |> 
  distinct()

saveRDS(wdi, "posts/pengepolitikk-afrika/wdi.rds")