# Databricks notebook source
# indlæs R pakker
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "tidyr" )
library( "ggplot2" )
install.packages( "patchwork" )
library( "patchwork" )
install.packages( "miscTools" )
library( "miscTools" )
install.packages("stargazer")
library("stargazer")

# COMMAND ----------


# definér stivej til det klargjorte data
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output/"

# indlæs det forberedte økonomidatasæt
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_SEGES.rds" ) )

# indlæs dataframe med bedrifter hvor omkostninger ikke kan fordeles
datOMK <- read.csv2( file.path( dirPath, "check_data/OMK_kan_ikke_fordeles.csv" ) )
datU <-read.csv2(file.path(dirPath, "check_data/U_kan_ikke_fordeles.csv"))

display( dat ) 
#display( datOMK ) 
#display(datU)

# COMMAND ----------

# Undersøger cutoff 
# antal bedrifter med < 10  367
# antal bedrifter med < 5  362
# antal bedrifter med < 3 360 
dat %>%
  mutate(
    # omregn til antal dyr
    smågrise_antal = PG_19g * 100,
    høns_antal = PG_19f * 1000,
    
    # omregn til DE
    DE_smågrise = smågrise_antal,
    DE_høns = høns_antal / 10,
    
    # hvis PG_19m, PG_19k, PG_19s allerede er i DE, så læg dem direkte til
    DE_total = PG_19m + PG_19k + PG_19s + DE_smågrise + DE_høns
  ) %>%
  filter(DE_total < 6) %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

dat %>%
  mutate(
    DE_total = PG_19m + PG_19k + PG_19s +
               (PG_19g * 100) / 2 +
               (PG_19f * 1000) / 167
  ) %>%
  filter(DE_total == 0) %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

dat %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

# Undersøger cutoff 
# antal bedrifter med < 10  371
# antal bedrifter med < 5  363
# antal bedrifter med < 3 361  
dat %>%
  mutate(DE = PG_19m + PG_19k + PG_19s + PG_19g + PG_19f) %>%
  filter(DE < 5) %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

dat %>%
  mutate(
    DE = PG_19k + PG_19s + PG_19f,
    kvæg = PG_19k > 0,
    svin = PG_19s > 0,
    høns = PG_19f > 0,
    
    husdyr_type = case_when(
      kvæg & !svin & !høns  ~ "Kun kvæg",
      !kvæg & svin & !høns  ~ "Kun svin",
      !kvæg & !svin & høns  ~ "Kun høns",
      kvæg | svin | høns    ~ "Kombination",
      TRUE                  ~ "Ingen dyreenheder"
    )
  ) %>%
  filter(husdyr_type == "Kun høns", DE < 20) %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

dat_summary <- dat %>%
  mutate(
    kvæg = PG_19k > 0,
    svin = PG_19s > 0,
    høns = PG_19f > 0,
   
    
    husdyr_type = case_when(
       kvæg & !svin & !høns  ~ "Kun kvæg",
      !kvæg & svin & !høns ~ "Kun svin",
      !kvæg & !svin & høns  ~ "Kun høns",
      kvæg | svin | høns  ~ "Kombination",
      TRUE ~ "Ingen dyreenheder"
    )
  ) %>%
  group_by(husdyr_type) %>%
  summarise(antal_bedrifter = n(), .groups = "drop")

dat_summary

# COMMAND ----------

#Overblik over bedrifter med og uden husdyr udspecificeret på husdyrtype:

dat_summary <- dat %>%
  mutate(
    # Opret indikatorer for om bedriften har de forskellige dyretyper
    kvæg = PG_19k > 0,
    svin = PG_19s > 0,
    høns = PG_19f > 0,
    får = får > 0,
    
    # Kategori baseret på hvilke typer der findes
    husdyr_type = case_when(
      kvæg & !svin & !høns ~ "Kun kvæg",
      !kvæg & svin & !høns ~ "Kun svin",
      !kvæg & !svin & høns ~ "Kun høns",
      kvæg | svin | høns ~ "Kombination",   # hvis mere end én type
      !kvæg & !svin & !høns ~ "Ingen dyreenheder"
    )
  ) %>%
  group_by(husdyr_type) %>%
  summarise(antal_bedrifter = n(), .groups = "drop")

dat_summary

# COMMAND ----------

# MAGIC %md
# MAGIC **Undersøger bedrifter med husdyr**

# COMMAND ----------

# Undersøger husdyrsammensætning for de bedrifte med kombination af dyreenheder

# Filtrer kun de bedrifter med kombination
kombinationer <- dat %>%
  mutate(
    
    kvæg = PG_19k > 0,
    svin = PG_19s > 0,
    høns = PG_19f > 0,
    husdyr_type = case_when(
      kvæg & !svin & !høns ~ "Kun kvæg",
      !kvæg & svin & !høns ~ "Kun svin",
      !kvæg & !svin & høns ~ "Kun høns",
      kvæg | svin | høns ~ "Kombination",
      !kvæg & !svin & !høns ~ "Ingen dyreenheder"
    )
  ) %>%
  filter(husdyr_type == "Kombination") %>%
  select(cvr, PG_19k, PG_19s, PG_19f)

kombinationer

# COMMAND ----------


# Tilføj husdyrtype og filtrer kun bedrifter med én dyretype og udbytte >0
dat_enkeltdyr <- dat %>%
  mutate(
    landbrugstype = case_when(
      EKOKODE == 2 ~ "Økologisk",
      EKOKODE == 1 ~ "Konventionel",
      TRUE ~ NA_character_
    ),
    kvæg = PG_19k > 0,
    svin = PG_19s > 0,
    høns = PG_19f > 0,
    husdyrtype = case_when(
      kvæg & !svin & !høns ~ "Kun kvæg",
      !kvæg & svin & !høns ~ "Kun svin",
      !kvæg & !svin & høns ~ "Kun høns",
      TRUE ~ NA_character_
    ),
    # Beregn total udbytte og procentandel
    total = U_P + U_H + U_A,
    pct_U_P  = ifelse(total > 0, U_P / total * 100, NA),
    pct_U_H  = ifelse(total > 0, U_H / total * 100, NA),
    pct_U_A  = ifelse(total > 0, U_A / total * 100, NA)
  ) %>%
  # Kun bedrifter med én dyretype og total udbytte >0
  filter(!is.na(husdyrtype), total > 0)

# Opsummering per husdyrtype og landbrugstype
fordeling_enkeltdyr <- dat_enkeltdyr %>%
  group_by(husdyrtype, landbrugstype) %>%
  summarise(
    n = n(),
    pct_U_P_med  = median(pct_U_P, na.rm = TRUE),
    pct_U_H_med  = median(pct_U_H, na.rm = TRUE),
    pct_U_A_med  = median(pct_U_A, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Tilføj samlet række pr. husdyrtype
  group_by(husdyrtype) %>%
  bind_rows(
    summarise(.,
              landbrugstype = "Alle",
              n = sum(n),
              pct_U_P_med  = mean(pct_U_P_med, na.rm = TRUE),
              pct_U_H_med  = mean(pct_U_H_med, na.rm = TRUE),
              pct_U_A_med  = mean(pct_U_A_med, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

# Vis samlet tabel
stargazer(fordeling_enkeltdyr, type = "text", summary = FALSE, rownames = FALSE,
          title = "Bedrifter med kun én husdyrtype - Fordeling på udbyttetype")

# COMMAND ----------

# Bedrifternes fordeling af antal husdyr (kun kvæg og svin)

# Opret landbrugstype og husdyr-kategorier

dat <- dat %>%
  mutate(
    landbrugstype = case_when(
      EKOKODE == 2 ~ "Økologisk",
      EKOKODE == 1 ~ "Konventionel",
      TRUE ~ NA_character_
    ),
    husdyr_kat = cut(
      PG_19k + PG_19s,  # summen af PG_19k og PG_19s
      breaks = c(-Inf, 0, 10, 100, Inf),
      labels = c("0", "0-10", "10-100", ">100"),
      right = TRUE
    )
  )

# Tabel med antal per kategori
fordeling <- dat %>%
  group_by(landbrugstype, husdyr_kat) %>%
  summarise(Antal = n(), .groups = "drop") %>%
  pivot_wider(names_from = husdyr_kat, values_from = Antal, values_fill = 0)

# Tilføj total-række
fordeling_total <- fordeling %>%
  bind_rows(
    summarise(.,
              landbrugstype = "Alle",
              across(where(is.numeric), sum))
  )

# Lav stargazer-tabel
stargazer(fordeling_total, type = "text", summary = FALSE, rownames = FALSE,  title = "Bedrifternes fordeling af husdyr (kvæg og svin stk) per bedrift")

# Bedrifternes fordeling af antal husdyr inkl høns

# Opret landbrugstype og husdyr-kategorier

dat <- dat %>%
  mutate(
    landbrugstype = case_when(
      EKOKODE == 2 ~ "Økologisk",
      EKOKODE == 1 ~ "Konventionel",
      TRUE ~ NA_character_
    ),
    husdyr_kat = cut(
      PG_19k + PG_19s + PG_19f,  # summen af PG_15 og PG_16 og PG_17 (obs - PG_19f er i 1000 høns) )
      breaks = c(-Inf, 0, 10, 100, Inf),
      labels = c("0", "0-10", "10-100", ">100"),
      right = TRUE
    )
  )

# Tabel med antal per kategori
fordeling <- dat %>%
  group_by(landbrugstype, husdyr_kat) %>%
  summarise(Antal = n(), .groups = "drop") %>%
  pivot_wider(names_from = husdyr_kat, values_from = Antal, values_fill = 0)

# Tilføj total-række
fordeling_total <- fordeling %>%
  bind_rows(
    summarise(.,
              landbrugstype = "Alle",
              across(where(is.numeric), sum))
  )

# Lav stargazer-tabel
stargazer(fordeling_total, type = "text", summary = FALSE, rownames = FALSE,  title = "Bedrifternes fordeling af husdyr (kvæg og svin i stk og høns i 1000 stk) per bedrift")

# COMMAND ----------

# MAGIC %md
# MAGIC **Undersøger rene planteproducenter**

# COMMAND ----------

# Filtrer kun bedrifter uden husdyr
dat_uden_husdyr <- dat %>% filter(U_H == 0)

# Beregn andele
dat_uden_husdyr <- dat_uden_husdyr %>%
  mutate(
    total = U_P + U_H + U_A,
    pct_U_P  = U_P / total * 100,
    pct_U_H  = U_H / total * 100,
    pct_U_A  = U_A / total * 100
  )

fordeling_uden_husdyr <- dat_uden_husdyr %>%
  group_by(landbrugstype) %>%
  summarise(
    n = n(),
    pct_U_P_med  = median(pct_U_P, na.rm = TRUE),
    pct_U_H_med  = median(pct_U_H, na.rm = TRUE),
    pct_U_A_med  = median(pct_U_A, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Tilføj "Alle" række
  bind_rows(
    summarise(.,
              landbrugstype = "Alle",
              n = sum(n),
              pct_U_P_med  = mean(pct_U_P_med, na.rm = TRUE),
              pct_U_H_med  = mean(pct_U_H_med, na.rm = TRUE),
              pct_U_A_med  = mean(pct_U_A_med, na.rm = TRUE))
  ) %>%
  # Rund til 1 decimal
  mutate(across(where(is.numeric), ~round(.x, 1)))

stargazer(fordeling_uden_husdyr, type = "text", summary = FALSE, rownames = FALSE, digits = 1, title = "Bedrifter uden husdyr - Fordeling på udbyttetype")

# COMMAND ----------



# Filtrer kun bedrifter uden husdyr (baseret på PG)
dat_uden_husdyr <- dat %>%
  filter(PG_19k == 0, PG_19s == 0, PG_19f == 0)

# Beregn procentandel fra U_P
dat_uden_husdyr <- dat_uden_husdyr %>%
  mutate(
    total = U_P + U_H + U_A,
    pct_U_P = ifelse(total > 0, (U_P / total) * 100, NA)
  )

# Kategoriser i 10%-intervaller
dat_uden_husdyr <- dat_uden_husdyr %>%
  mutate(
    pct_kat = cut(
      pct_U_P,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
                 "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
    )
  )

# Tæl antal CVR’er per interval og landbrugstype
fordeling_pct_U_P_uden_husdyr <- dat_uden_husdyr %>%
  group_by(landbrugstype, pct_kat) %>%
  summarise(Antal = n(), .groups = "drop") %>%
  pivot_wider(names_from = pct_kat, values_from = Antal, values_fill = 0)

# Tilføj total række
fordeling_pct_U_P_uden_husdyr <- fordeling_pct_U_P_uden_husdyr %>%
  bind_rows(
    summarise(.,
              landbrugstype = "Alle",
              across(where(is.numeric), sum))
  )

# Stargazer tabel
stargazer(fordeling_pct_U_P_uden_husdyr, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE,
          title = "Bedrifter uden husdyr (PG): Procentandel af udbytte fra Planteproduktion")


# CVR-numre for bedrifter uden husdyr med <30% fra planteproduktion
dat_uden_husdyr_plante30 <- dat_uden_husdyr %>%
  filter(pct_U_P < 30) %>%
  select(cvr, pct_U_P)

dat_uden_husdyr_plante30

dat_U_30 <- dat %>%
  semi_join(dat_uden_husdyr_plante30, by = "cvr") %>%
  select(cvr, landbrugsareal_ha, starts_with("U_"),starts_with("P_"), starts_with("DE_"), starts_with("OMK_")) %>%
  arrange(desc(landbrugsareal_ha))

display(dat_U_30)

# COMMAND ----------

hist( with( dat, U_A / (U_T) ), 30 )

# COMMAND ----------

# Bedrifter, hvor U_a/U_T*100>40%

# Sørg for at U_T er beregnet
dat <- dat %>%
  mutate(
    U_T = U_P + U_H + U_A + U_21,
    pct_U_A = ifelse(U_T > 0, U_A / U_T * 100, NA),
    landbrugstype = case_when(
      EKOKODEa == 2 ~ "Økologisk",
      EKOKODEa == 1 ~ "Konventionel",
      TRUE ~ NA_character_
    )
  )

# Filtrer bedrifter med U_A/U_T > 50%
dat_maj_UA <- dat %>%
  filter(pct_U_A > 40)

# Liste med cvr
liste_maj_UA <- dat_maj_UA %>%
  select(cvr, landbrugstype, U_A, U_T, pct_U_A) %>%
  arrange(desc(pct_U_A)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

stargazer(liste_maj_UA, type = "text", summary = FALSE, rownames = FALSE,
          title = "Bedrifter med >40% af udbytte i U_A")

# Opsummering pr. landbrugstype
fordeling_maj_UA <- dat_maj_UA %>%
  group_by(landbrugstype) %>%
  summarise(
    n = n(),
    U_A_med = median(U_A, na.rm = TRUE),
    pct_U_A_med = median(pct_U_A, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    summarise(.,
      landbrugstype = "Alle",
      n = sum(n),
      U_A_med = median(dat_maj_UA$U_A, na.rm = TRUE),
      pct_U_A_med = median(dat_maj_UA$pct_U_A, na.rm = TRUE)
    )
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

stargazer(fordeling_maj_UA, type = "text", summary = FALSE, rownames = FALSE,
          title = "Opsummering af bedrifter med > 40% U_A")





# COMMAND ----------

# Udbytter, der ikke kan fordeles
display(datU)

#datU1 <- datU %>%
 #filter(U_value > 10000)


#datU1 <- datU1 %>% collect()

#display(datU1)

# COMMAND ----------

cvr_søger <- "26777372"

dat_cvrsøger <- dat %>%
  filter(cvr == cvr_søger)

# Vis hele rækken   # ved tjek af de tre første cvr ses stort udbytte i andre husdyr! U22
dat_cvrsøger

# COMMAND ----------



# Antal unikke CVR-numre
antal_unikke_cvr <- datU1 %>%
  summarise(n_cvr = n_distinct(cvr)) %>%
  pull(n_cvr)

print(paste("Antal unikke CVR-numre med U_value > 10000:", antal_unikke_cvr))

# Hvis du vil se listen over dem
unikke_cvr_liste <- datU1 %>%
  distinct(cvr) %>%
  arrange(cvr)

display(unikke_cvr_liste)

# COMMAND ----------

# Data
afgroeder <- c(
  "Vårbyg",
  "Vinterhvede",
  "Græs m. kløver/lucerne",
  "Silomajs m. græsudlæg",
  "Vinterraps",
  "Andet"
)

procenter <- c(
  18.7,
  16.6,
  10.8,
  8.4,
  7.4,
  100 - (18.7 + 16.6 + 10.8 + 8.4 + 7.4)
)

labels <- paste0(afgroeder, ": ", procenter, "%")

farver <- c(
  "#8DAA91",
  "#A3B18A",
  "#588157",
  "#B08968",
  "#D4A373",
  "#DAD7CD"
)

# 🔥 Giv ekstra plads + tillad tegning udenfor
par(mar = c(5, 4, 4, 10), xpd = TRUE)

# Pie uden labels
pie(
  procenter,
  col = farver,
  labels = NA,
  main = "Fordeling af afgrødetyper"
)

# 🔥 Placér legend udenfor (juster tal hvis nødvendigt)
legend(
  x = 1.2, y = 1,
  legend = labels,
  fill = farver,
  cex = 0.8,
  bty = "n"
)

# COMMAND ----------

library(dplyr)
library(stargazer)

# 1️⃣ Tilføj husdyrindikatorer
dat_summary <- dat %>%
  mutate(
    kvæg = PG_19k > 0,
    svin  = PG_19s > 0,
    høns  = PG_19f > 0,
    
    husdyr_type = case_when(
      kvæg & !svin & !høns  ~ "Kun kvæg",
      !kvæg & svin & !høns  ~ "Kun svin",
      !kvæg & !svin & høns  ~ "Kun høns",
      kvæg | svin | høns     ~ "Kombination",
      TRUE                   ~ "Ingen dyreenheder"
    ),
    
    landbrugstype = case_when(
      EKOKODE == 2 ~ "Økologisk",
      EKOKODE == 1 ~ "Konventionel",
      TRUE         ~ NA_character_
    ),
    
    # Indikator for ren planteproduktion (ingen husdyr)
    plantekun = !kvæg & !svin & !høns,
    
    # Indikator for husdyrproduktion
    har_husdyr = kvæg | svin | høns
  )

# 2️⃣ Summér antallet af bedrifter
summary_table <- dat_summary %>%
  group_by(landbrugstype) %>%
  summarise(
    Kun_plante = sum(plantekun, na.rm = TRUE),
    Husdyr = sum(har_husdyr, na.rm = TRUE),
    Total = n()
  ) %>%
  ungroup()

# 3️⃣ Omform til “stargazer-venlig” data.frame
summary_table_df <- as.data.frame(summary_table)

# 4️⃣ Lav stargazer tabel
stargazer(
  summary_table_df,
  type = "text",
  summary = FALSE,
  rownames = FALSE,
  title = "Oversigt over bedrifter"
)
library(dplyr)

# Tæl samtlige unikke CVR
antal_cvr <- dat %>%
  summarise(antal = n_distinct(cvr)) %>%
  pull(antal)

# Print resultat
print(paste("Samlede antal unikke CVR:", antal_cvr))

# COMMAND ----------

display( dat_summary )

# COMMAND ----------

ifelse( dat, )