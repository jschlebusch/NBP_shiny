library(sf)
library(ggplot2)
library(tidyverse)
library(haven)


#df_admin <- st_read("world-administrative-boundaries.shp")
df_cshapes <- st_read("CShapes-2.0.shp")
df_nbp <- read_dta("EPR2NBP_Emre2.dta")%>%
  mutate(across(starts_with(c("anydown", "anyupgrade")), as.factor)) %>%
  rename_with(~ paste0("nbp_",.), starts_with(c("anydown", "anyupgrade"))) 

df_nbp <- df_nbp %>%
  select(Year, Country, iso3, ReliFreedom, Secular, nbp_anydown_1, nbp_anyupgrade_1)

#df_cshapes_annual <- df_cshapes %>%
#  mutate(gwsyear = as.numeric(format(gwsyear, "%Y")),
#         gweyear = as.numeric(format(gweyear, "%Y")))

df_cshapes <- df_cshapes %>%
  select(c(cntry_name, gwcode, gwsyear, gweyear, geometry))

df_cshapes_expanded <- df_cshapes %>%
  rowwise() %>%
  mutate(year_range = list(gwsyear:gweyear)) %>%
  unnest(year_range) %>%
  rename(year = year_range)

df_cshapes_expanded <- df_cshapes_expanded %>%
  filter(year >= 1945)

summary(df_cshapes_expanded$gwcode)
print(df_cshapes_expanded$gwcode)

ggplot() +
  geom_sf(data = df_cshapes_expanded %>%
            filter(year == 1962)) + 
  theme_minimal()


st_write(df_cshapes_expanded, "cshapes_annual.geojson", driver = "GeoJSON")













df_shapes_add <- df_cshapes %>%
  filter(cntry_name %in% c("Czechoslovakia", "Yugoslavia", "Russia (Soviet Union)"),
         gwsdate %in% c("1920-06-04", "1947-02-10", "1991-12-21"))


ggplot(data = df_cshapes %>% filter(gwsyear == 2014)) +
  coord_sf(crs = "+proj=longlat +datum=WGS84 +lon_0=180") +
  geom_sf(color = "black")




df_admin <- df_admin %>%
  select(c(color_code, name, geometry))

ggplot(data = df_admin) +
  geom_sf() + 
  theme_minimal()
