####----------------------------------------------------------------------------
#### NBP SHINY APP - DATA PREPARATION
#### 
#### Jan
####
#### 01/2025
####----------------------------------------------------------------------------

###---- PACKAGES ---------------------------------------------------------------
library(haven)
library(sf)
library(ggplot2)
library(tidyverse)
#library(countrycode)
library(states)
library(cshapes)

###---- DATA -------------------------------------------------------------------
df_cshapes <- st_read("CShapes-2.0.shp")

#df_cshapes20 <- cshp(useGW = TRUE, dependencies = TRUE)

df_complete <- read_dta("EPR2NBP_Emre2.dta")%>%
  mutate(across(starts_with(c("anydown", "anyupgrade")), as.factor)) %>%
  rename_with(~ paste0("nbp_",.), starts_with(c("anydown", "anyupgrade"))) %>%
  rename(iso3c = iso3)

df_cc <- countrycode::codelist %>%
  select(iso3c, gwn)

df_cc <- df_cc %>%
  rename(gwcode = gwn)

#df_states <- states::gwstates %>%
#  select(c(gwcode, gwc, country_name))

###---- DATA PREP --------------------------------------------------------------

## ANNUAL SHAPEFILES

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

df_2019 <- df_cshapes_expanded %>%
  filter(year == 2019)

df_2020 <- df_2019 %>%
  mutate(year = 2020)

df_cshapes_expanded <- bind_rows(df_cshapes_expanded, df_2020)

ggplot() +
  geom_sf(data = df_cshapes_expanded %>%
            filter(year == 2020)) + 
  theme_minimal()

## PREPARE NBP COUNTRY-LEVEL VARIABLES - MONOLINGUAL EDU. AND HI

# MONOLINGUAL DUMMY

df_languages <- df_complete %>%
  select(c(Year, Country, iso3c, Group, 
           starts_with("SubGroup"),
           Lang1, Lang2,Lang3, StandardArabicLang,
           OtherSpokenLang,
           starts_with("AddiLang"),
           starts_with("LOI"),
           starts_with("LC"),
           starts_with("AddiLoI")))%>%
  mutate(
    across(everything(), as.factor)
  )

df_languages_long <- df_languages %>%
  pivot_longer(
    cols = c(Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang, AddiLang,
             AddiLangCountry1, AddiLangCountry2, AddiLangCountry3, AddiLangCountry4),
    names_to = "LangVariable",
    values_to = "Language"
  ) %>%
  pivot_longer(
    cols = c(LOIPrimary1, LOIPrimary2, LOIPrimary3, LOIPrimaryStArabic, LOIPrimaryOtherLang, LOIPrimaryAddiLang,
             AddiLOI1, AddiLOI2, AddiLOI3, AddiLOI4),
    names_to = "LOIVariable",
    values_to = "LOIScore"
  ) %>%
  mutate(Match = case_when(
    LangVariable == "Lang1" & LOIVariable == "LOIPrimary1" ~ TRUE,
    LangVariable == "Lang2" & LOIVariable == "LOIPrimary2" ~ TRUE,
    LangVariable == "Lang3" & LOIVariable == "LOIPrimary3" ~ TRUE,
    LangVariable == "StandardArabicLang" & LOIVariable == "LOIPrimaryStArabic" ~ TRUE,
    LangVariable == "OtherSpokenLang" & LOIVariable == "LOIPrimaryOtherLang" ~ TRUE,
    LangVariable == "AddiLang" & LOIVariable == "LOIPrimaryAddiLang" ~ TRUE,
    LangVariable == "AddiLangCountry1" & LOIVariable == "AddiLOI1" ~ TRUE,
    LangVariable == "AddiLangCountry2" & LOIVariable == "AddiLOI2" ~ TRUE,
    LangVariable == "AddiLangCountry3" & LOIVariable == "AddiLOI3" ~ TRUE,
    LangVariable == "AddiLangCountry4" & LOIVariable == "AddiLOI4" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(Match & !is.na(Language) & !is.na(LOIScore) & Language != "") %>%
  select(-Match) %>%
  mutate(Language = case_when(
    Language == "Northern Pashto" ~ "Pashto",
    Language == "Southern Pashto" ~ "Pashto",
    Language == "Standard Arabic" ~ "Arabic",
    Language %in% c("German", "German, Standard", "German Standard", "Bavarian") ~ "German",
    Language %in% c("Kurdish, Northern", "Kurdish, Southern", "Kurdish, Central") ~ "Kurdish",
    str_detect(Language, "alagasy") ~ "Malagasy",
    TRUE ~ Language  
  ))

df_languages_summarised <- df_languages_long %>%
  group_by(Country, Year, Language) %>%
  summarise(
    TotalNational = sum(LOIScore == 1, na.rm = TRUE),
    TotalLocal = sum(LOIScore == 2, na.rm = TRUE),
    TotalNotTaught = sum(LOIScore == 0, na.rm = TRUE),
    TotalUnknown = sum(LOIScore == 99, na.rm = TRUE),
    TotalFailedState = sum(LOIScore == 100, na.rm = TRUE),
    .groups = "drop"
  )

df_monolingual <- df_languages_summarised %>%
  group_by(Country, Year) %>%
  summarise(
    Monolingual = ifelse(
      sum(TotalNational >= 1) == 1 & 
        all((TotalNational)[TotalNational == 0] == 0),
      1,
      0
    ),
    .groups = "drop"
  )

summary(as.factor(df_monolingual$Monolingual))

df_monolingual <- df_monolingual %>%
  mutate(Country = as.character(Country),
         Year = as.numeric(as.character(Year)))

df_complete <- df_complete %>%
  left_join(df_monolingual, by = c("Country", "Year"))

summary(as.factor(df_complete$Monolingual))

# MONOLINGUAL STRICT

df_monolingual_countries <- df_complete %>%
  filter(Monolingual == 1) %>%
  select(c(Country, Year, Group, 
           starts_with("SubGroup"),
           Lang1, Lang2,Lang3, StandardArabicLang,
           OtherSpokenLang,
           starts_with("AddiLang"),
           starts_with("LOI"),
           starts_with("LC"),
           starts_with("AddiLoI"),
           Monolingual))

summary(df_monolingual_countries)

df_monolingual_strict <- df_monolingual_countries %>%
  mutate(strictML_gr = ifelse(
    apply(select(., starts_with("LOI")), 1, function(x) all(is.na(x) | x %in% c(0, 1))) &
      apply(select(., starts_with("LC")), 1, function(x) all(is.na(x) | x == 0)) &
      apply(select(., starts_with("AddiLOI")), 1, function(x) all(is.na(x) | x %in% c(0, 1))),
    1, 0
  )) %>%
  group_by(Country, Year) %>% 
  summarise(MonolingualStrict = ifelse(all(strictML_gr == 1), 1, 0), .groups = "drop")

summary(as.factor(df_monolingual_strict$MonolingualStrict))

df_complete <- df_complete %>%
  left_join(df_monolingual_strict, by = c("Country", "Year"))

df_complete <- df_complete %>%
  mutate(MonolingualStrict = ifelse(is.na(MonolingualStrict), 0, MonolingualStrict))

summary(as.factor(df_complete$Monolingual))
summary(as.factor(df_complete$MonolingualStrict))

# HI INDEX

df_HI_score <- df_complete %>%
  select(iso3c, Group, Year, Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang, 
         LOIPrimary1, LOIPrimary2, LOIPrimary3, LOIPrimaryStArabic, LOIPrimaryOtherLang,
         LOISecondary1, LOISecondary2, LOISecondary3, LOISecondaryStArabic, LOISecondaryOtherLang,
         LCPrimary1, LCPrimary2, LCPrimary3, LCPrimaryStArabic, LCPrimaryOtherLang,
         LCSecondary1, LCSecondary2, LCSecondary3, LCSecondaryStArabic, LCSecondaryOtherLang)

summary(df_HI_score)

loi_cols <- c("LOIPrimary1", "LOIPrimary2", "LOIPrimary3", 
              "LOIPrimaryStArabic", "LOIPrimaryOtherLang",
              "LOISecondary1", "LOISecondary2", "LOISecondary3", 
              "LOISecondaryStArabic", "LOISecondaryOtherLang")

lc_cols <- c("LCPrimary1", "LCPrimary2", "LCPrimary3", 
             "LCPrimaryStArabic", "LCPrimaryOtherLang",
             "LCSecondary1", "LCSecondary2", "LCSecondary3", 
             "LCSecondaryStArabic", "LCSecondaryOtherLang")

df_HI_score <- df_HI_score %>%
  mutate(across(all_of(c(loi_cols, lc_cols)), 
                ~if_else(.x %in% c(99, 100), NA_real_, .x))) # does NA makes sense here or should we treat this as 0?

summary(df_HI_score)

swap_values <- function(x) ifelse(x == 1, 2, ifelse(x == 2, 1, x))

df_HI_score <- df_HI_score %>%
  mutate(across(all_of(c(loi_cols, lc_cols)), swap_values))

df_HI_score <- df_HI_score %>%
  mutate(across(c(Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang), ~ na_if(.x, "")))

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(
    num_languages = length(na.omit(c_across(c(Lang1, Lang2, Lang3, StandardArabicLang, OtherSpokenLang))))) %>%
  ungroup()

summary(df_HI_score$num_languages)

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(LOIsum = sum(c_across(starts_with("LOI")), na.rm = TRUE) * 2) %>% # if you don't want to weight LOI double against LC, we can just remove it here
  ungroup()

df_HI_score <- df_HI_score %>%
  mutate(LOIsum_adj = LOIsum / num_languages)

summary(df_HI_score$LOIsum)
summary(df_HI_score$LOIsum_adj)

df_HI_score <- df_HI_score %>%
  rowwise() %>%
  mutate(LCsum = sum(c_across(starts_with("LC")), na.rm = TRUE)) %>%
  ungroup()

df_HI_score <- df_HI_score %>%
  mutate(LCsum_adj = LCsum / num_languages)

summary(df_HI_score$LCsum)
summary(df_HI_score$LCsum_adj)

df_HI_score <- df_HI_score %>%
  mutate(group_edu_score = LOIsum_adj + LCsum_adj)

summary(df_HI_score$group_edu_score)

df_HI_score <- df_HI_score %>%
  group_by(iso3c, Year) %>%
  mutate(country_edu_score = sum(group_edu_score)) %>%
  ungroup()

summary(df_HI_score$country_edu_score)

# and the HI index as |g- -G)
df_HI_score <- df_HI_score %>%
  mutate(HI = abs(group_edu_score - (country_edu_score - group_edu_score)))

summary(df_HI_score$HI)

hist(df_HI_score$HI)
hist(log(df_HI_score$HI))

df_HI_clean <- df_HI_score %>%
  select(iso3c, Year, Group, group_edu_score, country_edu_score, HI) %>%
  distinct()

df_complete <- df_complete %>%
  left_join(df_HI_clean, by = c("iso3c", "Year", "Group"))

summary(df_complete$HI)

# COUNTRY-LEVEL VARS DF

df_country_vars <- df_complete %>%
  select(c(Country, iso3c, Year, HI, Monolingual, MonolingualStrict)) %>%
  distinct(Country, Year, .keep_all = TRUE)

df_country_vars <- df_country_vars %>%
  left_join(df_cc, by = "iso3c")

summary(as.factor(df_country_vars$gwcode))

df_cv_gwcmissing <- df_country_vars %>%
  filter(is.na(gwcode)) %>%
  distinct(Country)

missing_countries <- c("Czechoslovakia", "Vietnam", "German", "Yugoslavia", "Soviet", "Yemen", "Kosovo")

df_cshapesequiv <- df_cshapes_expanded %>%
  filter(str_detect(cntry_name, paste(missing_countries, collapse = "|"))) 

summary(as.factor(df_cshapesequiv$gwcode))

df_country_vars <- df_country_vars %>%
  mutate(
    gwcode = case_when(
      iso3c == "CSK" ~ 315,
      iso3c == "DDR"  ~ 265,
      iso3c == "XKX" ~ 347,
      iso3c == "YPR" ~ 680,
      iso3c == "YAR" ~ 678,
      iso3c == "VDR" ~ 816,
      iso3c == "SUN" ~ 365,
      iso3c == "YUG" ~ 345,
      TRUE ~ gwcode
    ))

df_country_vars <- df_country_vars %>%
  rename(year = Year)

df_mapping <- df_cshapes_expanded %>%
  left_join(df_country_vars, by = c("gwcode", "year"))

ggplot() +
  geom_sf(data = df_mapping %>%
            filter(year == 1962), aes(fill = HI)) + 
  theme_minimal()


st_write(df_mapping, "nbp_map_data.geojson", driver = "GeoJSON")

