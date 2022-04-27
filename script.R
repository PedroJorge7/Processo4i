## Script Pedro Jorge Holanda Alves
# Processo seletivo 4i


# Installing/Loading All Libraries
packages <- c("ggplot2"      ,"tidyverse"        ,"data.table",
              "rnaturalearth","rnaturalearthdata","viridis",
              "ggspatial"    ,"kableExtra")  

# Install if necessary and already load the packages
to_install <- packages[! packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))
rm("packages","to_install")


### 1.1 World data --------------

# Read data
data_base <- rbind(fread("./inputs/DataJobID-2314661_2314661_Report.csv"),
                   fread("./inputs/DataJobID-2314662_2314662_Report.csv"),
                   fread("./inputs/DataJobID-2314678_2314678_Report.csv"),
                   fread("./inputs/DataJobID-2314679_2314679_Report.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(reporter_name   = ifelse(reporter_name   == "Russian Fed","Rússia","Ucrânia"),
         trade_flow_name = ifelse(trade_flow_name == "Gross Exports","Exportação","Importação")) %>% 
  filter(!(partner_name %in% c("World",""))) %>% 
  filter(product_code != 9999) # Removing unknown product category

### Mapa --------------

# World map
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(c(name = adm0_a3_is))

# Geometry only por Rússia
rus <- world %>% filter(name == "RUS")
# Geometry only por Ukraine
ukr <- world %>% filter(name == "UKR")

# Adapt data_base for left join
world <- left_join(world,data_base %>% 
                         group_by(reporter_name,partner_iso3,trade_flow_name) %>% 
                         dplyr::summarise(total = sum(trade_value_in_1000_usd)) %>% 
                         rename(name = partner_iso3) %>% 
                         group_by(reporter_name,trade_flow_name) %>% 
                         mutate(perc = (total/sum(total))*100))

## Map Rússia
ggplot() +
  geom_sf(data = world %>% select(name),
          fill = "grey64", color=  "black") + 
  geom_sf(data = subset(world, reporter_name == "Rússia"),
          aes(fill=perc), color = "black") +
  geom_sf(data = rus, fill = "lightblue", color=  "black") + 
  theme_minimal() +
  scale_fill_viridis(option = "viridis", direction = -1,
                     limits = c(0, 26),
                     oob = scales::squish,
                     name = "Participação nas exportações e\n  importações da Rússia",
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) + 
  # annotation_scale(location = "bl", width_hint = 0.4,
  #                  pad_y = unit(0.2, "in"),
  #                  pad_x = unit(0.6, "in")) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(9, "in"),
  #                        pad_y = unit(3, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  #xlab("Longitude") + ylab("Latitude")  +
  facet_wrap(.~trade_flow_name, ncol = 1) +
  theme(legend.position = "bottom")

ggsave("./outputs/Participação nas exportações e importações da Rússia.png", dpi = 1000,
       height = 12.5, width = 20, units = "cm")


# Map Ucrania
ggplot() +
  geom_sf(data = world %>% select(name),
          fill = "grey64", color=  "black") + 
  geom_sf(data = subset(world, reporter_name == "Ucrânia"),
          aes(fill=perc), color = "black") +
  geom_sf(data = ukr, fill = "lightblue", color=  "black") + 
  theme_minimal() +
  scale_fill_viridis(option = "viridis", direction = -1,
                     limits = c(0, 15),
                     oob = scales::squish,
                     name = "Participação nas exportações\n e importações da Ucrânia",
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) + 
  # annotation_scale(location = "bl", width_hint = 0.4,
  #                  pad_x = unit(0.8, "in")) +
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(9, "in"),
  #                        pad_y = unit(3, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # xlab("Longitude") + ylab("Latitude")  +
  facet_wrap(trade_flow_name~., ncol = 1) +
  theme(legend.position = "bottom")

ggsave("./outputs/Participação nas exportações e importações da Ucrânia.png", dpi = 1000,
       height = 12.5, width = 20, units = "cm")

### Tabela ------------

tabela1 <- data_base %>% 
  group_by(reporter_name,product_code,trade_flow_name) %>% 
  dplyr::summarise(total = sum(trade_value_in_1000_usd)) %>% 
  group_by(reporter_name,trade_flow_name) %>% 
  mutate(perc = (total/sum(total))*100) %>% 
  slice_max(total, n = 5) %>% 
  mutate(product = ifelse(product_code == 2709,"Petróleto Bruto",
                   ifelse(product_code == 2710,"Petróleto Refinado",
                   ifelse(product_code == 7108,"Alumínio",
                   ifelse(product_code == 2701,"Hulhas; briquetes, bolas e combustíveis sólidos semelhantes",
                   ifelse(product_code == 1001,"Trigo",
                   ifelse(product_code == 7110,"Platina",
                   ifelse(product_code == 8525,"Aparelhos transmissores para radiotelefonia",
                   ifelse(product_code == 8708,"Acessório de veículo",
                   ifelse(product_code == 8703,"Motor de veiculo",
                   ifelse(product_code == 8471,"Máquinas automáticas de processamento de dados",
                   ifelse(product_code == 2601,"Minério de ferro",
                   ifelse(product_code == 1512,"Oléo de girassol",
                   ifelse(product_code == 1005,"Milho",
                   ifelse(product_code == 7207,"Produtos semimanufaturados de ferro ou aço não ligado",
                   ifelse(product_code == 2711,"Gás de petróleo",
                   ifelse(product_code == 3004,"Medicamentos",
                          NA)))))))))))))))))

tabela2 <- data_base %>% filter(product_code %in% tabela1$product_code) %>% 
  group_by(reporter_name,product_code,partner_name,trade_flow_name) %>% 
  dplyr::summarise(total = sum(trade_value_in_1000_usd)) %>% 
  group_by(reporter_name,product_code,trade_flow_name) %>% 
  mutate(perc = (total/sum(total))*100) %>% 
  slice_max(total, n = 5) %>% 
  mutate(partner = paste0(partner_name, " (",round(perc,0),"%)")) %>% 
  group_by(product_code,reporter_name,trade_flow_name) %>% summarise(partners=paste(partner, collapse=", "))

tabela <- left_join(tabela1,tabela2) %>% ungroup() %>% 
  select(c(reporter_name,product,trade_flow_name,total,perc,partners)) %>% 
  mutate(perc  = round(perc,2),
         total = round(total/1000000,2))
rm(tabela1,tabela2)


tabela %>%
  rename("Principais produtos exportados" = product,
         "Valor das exportações\n(em milhões de US$)" = total,
         "% do total exportado" = perc,
         "Principais países que exporta" = partners) %>% 
  filter(trade_flow_name == "Exportação") %>% 
  select(-c(trade_flow_name,reporter_name)) %>% 
  kbl(caption = "Principais produtos exportados pela Rússia e Ucrânia e seus parceiros comerciais",
      align = c('l','c','c','l'), format = 'latex') %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "All Data" = 2, "55-59" = 2, "60-64" = 2, "65-69" = 2)) %>% 
  # add_header_above(c(" " = 3, "By age" = 6)) %>% 
  pack_rows("Rússia (2020)", 1, 5) %>%
  pack_rows("Ucrânia (2021)",6,10) %>%
  save_kable("./outputs/tabela exportação.tex")

tabela %>%
  rename("Principais produtos importados" = product,
         "Valor das importações\n(em milhões de US$)" = total,
         "% do total exportado" = perc,
         "Principais países que importa" = partners) %>% 
  filter(trade_flow_name == "Importação") %>% 
  select(-c(trade_flow_name,reporter_name)) %>% 
  kbl(caption = "Principais produtos importados pela Rússia e Ucrânia e seus parceiros comerciais",
      align = c('l','c','c','l'), format = 'latex') %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "All Data" = 2, "55-59" = 2, "60-64" = 2, "65-69" = 2)) %>% 
  # add_header_above(c(" " = 3, "By age" = 6)) %>% 
  pack_rows("Rússia (2020)", 1, 5) %>%
  pack_rows("Ucrânia (2021)",6,10) %>%
  save_kable("./outputs/tabela importação.tex")

## 1.2 Brasil ------------
rm(list = ls())

tab_br <- readxl::read_excel("./inputs/EXP_IMP.xlsx") %>% 
  janitor::clean_names()

tab_br <- tab_br %>% 
  select(-c(x2021_quilograma_liquido)) %>% 
  group_by(paises,detalhamento) %>% 
  rename(valor = x2021_valor_fob_us) %>% 
  pivot_wider(names_from = paises, values_from = valor) %>% 
  mutate(perc_russia = (Rússia/Total)*100,
         perc_ucrania = (Ucrânia/Total)*100)

tab_russia <- tab_br %>% filter(!is.na(Rússia)) %>% 
  select(-c(Ucrânia)) %>% 
  group_by(detalhamento) %>% 
  slice_max(Rússia, n = 5) %>%  
  mutate(descricao_ncm = ifelse(codigo_ncm == "12019000","Soja",
                         ifelse(codigo_ncm == "02071400","Galos/galinhas\n congelados",
                         ifelse(codigo_ncm == "09011110","Café em grão",
                         ifelse(codigo_ncm == "12024200","Amendoin",
                         ifelse(codigo_ncm == "17011400","Outros Açúcares\n de cana",
                         ifelse(codigo_ncm == "31042090","Outros cloretos\n de potássio",
                         ifelse(codigo_ncm == "31054000","Diidrogeno-ortofosfato\n de amônio",
                         ifelse(codigo_ncm == "31021010","Ureia",
                         ifelse(codigo_ncm == "27011200","Hulha betuminosa",
                         ifelse(codigo_ncm == "31023000","Nitrato de amônio",NA)))))))))))

# or, order by prevalence:
tab_russia$descricao_ncm <- factor(tab_russia$descricao_ncm, levels = tab_russia$descricao_ncm[order(tab_russia$Rússia)])

ggplot(subset(tab_russia),
       aes(x=descricao_ncm, y=Rússia/1000000000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + 
  geom_text(
    aes(x = descricao_ncm, y = Rússia/1000000000, label = paste0(round(perc_russia,0),"%")),
    position = position_dodge(width = 1),
    vjust = 0.5, size = 3
  ) + 
  coord_flip() +
  facet_wrap(.~detalhamento, scales = "free") +
  labs(x = "Descrição",y = "Percentual (em bilhões)")

ggsave("./outputs/importados da Rússia.png", dpi = 1000,
       height = 12.5, width = 20, units = "cm")


tab_ucrania <- tab_br %>% filter(!is.na(Ucrânia)) %>% 
  select(-c(Rússia)) %>% 
  group_by(detalhamento) %>% 
  slice_max(Ucrânia, n = 5) %>% 
  mutate(descricao_ncm = ifelse(codigo_ncm == "12024200","Amendoins",
                         ifelse(codigo_ncm == "26060011","Bauxita\n(médio de alumínio)",
                         ifelse(codigo_ncm == "17011400","Outros açúcares\n de cana",
                         ifelse(codigo_ncm == "21011110","Café solúvel",
                         ifelse(codigo_ncm == "84244900","Outros pulverizadores",
                         ifelse(codigo_ncm == "72071110","Billets de ferro",
                         ifelse(codigo_ncm == "39041010","Cloreto de vinila",
                         ifelse(codigo_ncm == "72083990","Outros produtos\n  laminados planos",
                         ifelse(codigo_ncm == "72139190","Outros fios-máquinas\n de ferro",
                         ifelse(codigo_ncm == "30043100","Medicamentos que\n contenham insulina",NA)))))))))))


# or, order by prevalence:
tab_ucrania$descricao_ncm <- factor(tab_ucrania$descricao_ncm, levels = tab_ucrania$descricao_ncm[order(tab_ucrania$Ucrânia)])


ggplot(subset(tab_ucrania),
       aes(x=descricao_ncm, y=Ucrânia/1000000000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + 
  geom_text(
    aes(x = descricao_ncm, y = Ucrânia/1000000000, label = paste0(round(perc_ucrania,0),"%")),
    position = position_dodge(width = 1),
    vjust = 0.5, size = 3
  ) + 
  coord_flip() +
  facet_wrap(.~detalhamento, scales = "free") +
  labs(x = "Descrição",y = "Valor (em bilhões)")

ggsave("./outputs/importados da Ucrânia.png", dpi = 1000,
       height = 12.5, width = 20, units = "cm")

### 2. Exercício prático ----------
rm(list = ls())

# Install package
install.packages("remotes")
remotes::install_github("4intelligence/faas4i-pub", force = TRUE)
library("faas4i")

# Login 
faas4i::login()

# Open data
dataset_2 <- readxl::read_excel("./inputs/producao_auto-step2.xlsx") %>% 
  rename(producao_automoveis = producaodeautomoveisnacionaloriginalmensalnivel) %>% 
  select(-c(3:11,17:19,22)) %>% 
  filter(as.numeric(substr(data_tidy,1,4))>=2000)

dataset_3 <- dataset_2 %>% 
  filter(as.numeric(substr(data_tidy,1,4))>=2010)

dataset_1 <- dataset_2 %>% 
  select(c(data_tidy,producao_automoveis))

dataset_4 <- dataset_2 %>% 
  mutate(pdiesel_lag1 = lag(fs_pdiesel,1),petanol_lag1 = lag(fs_petanol,1),pgasoline_lag1 = lag(fs_pgasoline,1),
         pdiesel_lag2 = lag(fs_pdiesel,2),petanol_lag2 = lag(fs_petanol,2),pgasoline_lag2 = lag(fs_pgasoline,2),
         pdiesel_lag3 = lag(fs_pdiesel,3),petanol_lag3 = lag(fs_petanol,3),pgasoline_lag3 = lag(fs_pgasoline,3))

dataset_5 <- dataset_2 %>% 
  mutate(pdiesel_lag1 = lag(fs_pdiesel,1),petanol_lag1 = lag(fs_petanol,1),pgasoline_lag1 = lag(fs_pgasoline,1),
         pdiesel_lag2 = lag(fs_pdiesel,2),petanol_lag2 = lag(fs_petanol,2),pgasoline_lag2 = lag(fs_pgasoline,2),
         pdiesel_lag3 = lag(fs_pdiesel,3),petanol_lag3 = lag(fs_petanol,3),pgasoline_lag3 = lag(fs_pgasoline,3)) %>% 
  filter(as.numeric(substr(data_tidy,1,4))>=2010)


# dataset_1 <- dataset_1 %>% filter(as.numeric(substr(data_tidy,1,4)) >= 2010)

# Basic model -------

data_list <-  list(dataset_1)
names(data_list) <- c("producao_automoveis")

# Default settings
date_variable <- 'data_tidy'
date_format   <- '%Y-%m-%d'

# Model Specification
model_spec <- list(n_steps = 22,
                   n_windows = 12,
                   n_best = 20,
                   exclusions = list(),
                   fill_forecast = TRUE,
                   log = TRUE,
                   seas.d = TRUE)

# Project Name
project_name <- "basic_model"


# Verification of arguments
faas4i::validate_models(data_list    = data_list  , date_variable = date_variable,
                        date_format  = date_format, model_spec    = model_spec,
                        project_name = project_name)

# Run model
faas4i::run_models(data_list    = data_list  , date_variable = date_variable,
                   date_format  = date_format, model_spec    = model_spec,
                   project_name = project_name)

# Basic model -------

data_list <-  list(dataset_2)
names(data_list) <- c("producao_automoveis")

# Default settings
date_variable <- 'data_tidy'
date_format   <- '%Y-%m-%d'

# Model Specification
model_spec <- list(n_steps = 22,
                   n_windows = 12,
                   n_best = 20,
                   exclusions = list(),
                   fill_forecast = TRUE,
                   log = TRUE,
                   seas.d = TRUE)

# Project Name
project_name <- "covariates_model"


# Verification of arguments
faas4i::validate_models(data_list    = data_list  , date_variable = date_variable,
                        date_format  = date_format, model_spec    = model_spec,
                        project_name = project_name)

# Run model
faas4i::run_models(data_list    = data_list  , date_variable = date_variable,
                   date_format  = date_format, model_spec    = model_spec,
                   project_name = project_name)

# Basic model -------

data_list <-  list(dataset_3)
names(data_list) <- c("producao_automoveis")

# Default settings
date_variable <- 'data_tidy'
date_format   <- '%Y-%m-%d'

# Model Specification
model_spec <- list(n_steps = 22,
                   n_windows = 12,
                   n_best = 20,
                   exclusions = list(),
                   fill_forecast = TRUE,
                   log = TRUE,
                   seas.d = TRUE)

# Project Name
project_name <- "restricton_model"


# Verification of arguments
faas4i::validate_models(data_list    = data_list  , date_variable = date_variable,
                        date_format  = date_format, model_spec    = model_spec,
                        project_name = project_name)

# Run model
faas4i::run_models(data_list    = data_list  , date_variable = date_variable,
                   date_format  = date_format, model_spec    = model_spec,
                   project_name = project_name)

# Basic model -------

data_list <-  list(dataset_4)
names(data_list) <- c("producao_automoveis")

# Default settings
date_variable <- 'data_tidy'
date_format   <- '%Y-%m-%d'

# Model Specification
model_spec <- list(n_steps = 22,
                   n_windows = 12,
                   n_best = 20,
                   exclusions = list("fs_pdiesel","fs_petanol","fs_pgasoline",
                                     "pdiesel_lag1","petanol_lag1","pgasoline_lag1",
                                     "pdiesel_lag2","petanol_lag2","pgasoline_lag2",
                                     "pdiesel_lag3","petanol_lag3","pgasoline_lag3"),
                   fill_forecast = TRUE,
                   log = TRUE,
                   seas.d = TRUE)

# Project Name
project_name <- "with_lag_model"


# Verification of arguments
faas4i::validate_models(data_list    = data_list  , date_variable = date_variable,
                        date_format  = date_format, model_spec    = model_spec,
                        project_name = project_name)

# Run model
faas4i::run_models(data_list    = data_list  , date_variable = date_variable,
                   date_format  = date_format, model_spec    = model_spec,
                   project_name = project_name)

# Restrict and  lag model -------

data_list <-  list(dataset_5)
names(data_list) <- c("producao_automoveis")

# Default settings
date_variable <- 'data_tidy'
date_format   <- '%Y-%m-%d'

# Model Specification
model_spec <- list(n_steps = 22,
                   n_windows = 12,
                   n_best = 20,
                   exclusions = list("fs_pdiesel","fs_petanol","fs_pgasoline",
                                     "pdiesel_lag1","petanol_lag1","pgasoline_lag1",
                                     "pdiesel_lag2","petanol_lag2","pgasoline_lag2",
                                     "pdiesel_lag3","petanol_lag3","pgasoline_lag3"),
                   fill_forecast = TRUE,
                   log = TRUE,
                   seas.d = TRUE)

# Project Name
project_name <- "restrict_lag_model"


# Verification of arguments
faas4i::validate_models(data_list    = data_list  , date_variable = date_variable,
                        date_format  = date_format, model_spec    = model_spec,
                        project_name = project_name)

# Run model
faas4i::run_models(data_list    = data_list  , date_variable = date_variable,
                   date_format  = date_format, model_spec    = model_spec,
                   project_name = project_name)


## Unzip projcets -----------

download_zip(project_id = list_projects()[[1]]$id,
             path = "D:/Pedro Jorge/4i",
             filename = "processo_seletivo_4i")
unzip("forecast-processo_seletivo_4i.zip")

##  results -----------
results <- readRDS("forecast_1_producao_automoveis_626727210db5bff231ffe00e_20220425_225712.rds")

results$infos

# Acurácia (MAPE)
results[[13]][[1]]

## graph
results[[19]][[1]]

forecast <- results[[21]][[8]]
a <- forecast %>% mutate(mape = abs((y_orig-y_fit)/y_orig))
mean(a$mape, na.rm = T)*100

forecast <- forecast %>% 
  select(c(data_tidy,y_orig,y_fit)) %>% 
  pivot_longer(!data_tidy,names_to = "Curva", values_to = "value") %>% 
  mutate(Curva = ifelse(Curva == "y_orig","Original","Fitted"))

ggplot(forecast, aes(x=data_tidy, y=value, group = Curva, color=Curva)) +
  geom_line(aes(linetype=Curva))+
  scale_linetype_manual(values=c("twodash","solid"))+
  geom_point(aes(shape=Curva)) +
  labs(title="Previsão da produção de automoveis",
       x="Mêses", y = "Total da produção",
       color = "Curva") +
  theme_classic() + 
  scale_color_manual(values=c('gray60','#E69F00')) +
  theme(legend.position="bot")
