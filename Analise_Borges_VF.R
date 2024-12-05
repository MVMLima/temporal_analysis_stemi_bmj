# Carregar bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(readxl)
library(gtsummary)

# Carregar o arquivo CSV combinado
df <- read_excel("Final Database_Alt_MVML.xlsx")

# Alterar os nomes da coluna
df <- clean_names(df)

# Exibir inforNULL# Exibir informações básicas sobre o conjunto de dados
glimpse(df)

# Verificar duplicidades
duplicated_rows <- df[duplicated(df), ]

# Remover duplicidades
df_unique <- df %>% distinct()

# Mostrar o número de linhas antes e depois de remover duplicidades
cat("Número de linhas antes de remover duplicidades: ", nrow(df), "\n")
cat("Número de linhas depois de remover duplicidades: ", nrow(df_unique), "\n")


#Tabelas das variáveis
table(df_unique$final_coding)
table(df_unique$level_of_evidence)
table(df_unique$class_of_recommendation)
table(df_unique$type_of_recommendation)
table(df_unique$pharmacological)
table(df_unique$non_pharmacological)

####

###NP-PAC-50 <- ????


# Converter a coluna 'Year' para factor
df_unique <- df_unique %>% 
  mutate(year = as.factor(year))

# Criar uma função para contar valores nulos por coluna
count_na <- function(df_unique) {
  sapply(df_unique, function(x) sum(is.na(x)))
}

# Aplicar a função ao DataFrame e visualizar o resultado
na_counts <- count_na(df_unique)
print(na_counts)

# Substituir valores nulos por "NA"
df_unique[is.na(df_unique)] <- "N/A"

# Plotando o número de recomendações por ano
ggplot(df_unique, aes(x = year)) +
  geom_bar(color = "skyblue", fill = "skyblue") +
  labs(title = "Number of Recommendations per Year", x = "Year", y = "Number of Recommendations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tabela de Frequencias por Ano

df_unique %>% tabyl(year) %>% 
  adorn_totals(where = c("row")) %>%
  adorn_pct_formatting()

df_unique %>% 
  select(year) %>% 
  tbl_summary(
    label = year ~ "Year") %>% 
    modify_header(label ~ "**Variable**")

df_unique %>% 
  select(level_of_evidence) %>% 
  tbl_summary(
    label = level_of_evidence ~ "Level of Evidence") %>% 
  modify_header(label ~ "**Variable**")

df_unique %>% 
  select(class_of_recommendation) %>% 
  tbl_summary(
    label = class_of_recommendation ~ "Class of Evidence") %>% 
  modify_header(label ~ "**Variable**")

# Preparar dados para plotagem
class_per_year <- df_unique %>% 
  count(year, class_of_recommendation ) %>% 
  spread(key = class_of_recommendation , value = n, fill = 0)

# Plotando o número de recomendações por ano e classe de recomendação
class_per_year <- clean_names(class_per_year)

# Transformar o DataFrame para formato longo (long format)
df_long <- class_per_year %>%
  pivot_longer(cols = starts_with("class"), names_to = "Class", values_to = "Count")

# Normalizar os dados para que cada barra some 100%
df_long <- df_long %>%
  group_by(year) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Definir cores personalizadas
custom_colors <- c("class_i" = "#1f77b4", "class_ii_a" = "#ff7f0e", "class_ii_b" = "#2ca02c", "class_iii" = "#d62728", "mixed" = "#9467bd")

# Criar o gráfico de barras empilhadas horizontal com cores personalizadas
ggplot(df_long, aes(x = factor(year), y = Count, fill = Class)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  labs(title = "Recommendations per Year by Class of Recommendation",
       x = "Year",
       y = "Proportion (%)",
       fill = "Class") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

# Preparar dados para plotagem level
level_per_year <- df_unique %>% 
  count(year, level_of_evidence ) %>% 
  spread(key = level_of_evidence , value = n, fill = 0)

# Plotando o número de recomendações por ano e level de recomendação
level_per_year <- clean_names(level_per_year)

# Transformar o DataFrame para formato longo (long format)
df_long <- level_per_year %>%
  pivot_longer(cols = starts_with("loa"), names_to = "Level", values_to = "Count")

# Normalizar os dados para que cada barra some 100%
df_long <- df_long %>%
  group_by(year) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Definir cores personalizadas
custom_colors <- c("loa_a" = "#1f77b4", "loa_b" = "#ff7f0e", "loa_c" = "#2ca02c")

# Criar o gráfico de barras empilhadas horizontal com cores personalizadas
ggplot(df_long, aes(x = factor(year), y = Count, fill = Level)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  labs(title = "Level per Year by Class of Recommendation",
       x = "Year",
       y = "Proportion (%)",
       fill = "Level") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

###### Separado o Banco de Dados

df_phar <- df_unique %>% filter(type_of_recommendation == "Pharmacological")

df_phar$non_pharmacological <- NULL

# Preparar dados para plotagem level
phar_per_year <- df_phar %>% 
  count(year, pharmacological ) %>% 
  spread(key = pharmacological , value = n, fill = 0)

# Calcular o percentual de cada level_of_evidence por ano
df_summary <- df_phar %>%
  group_by(year, level_of_evidence, class_of_recommendation) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = (count / total) * 100)

# Transformar o DataFrame para formato largo (wide format) para heatmap
df_wide <- df_summary %>%
  select(year, level_of_evidence, class_of_recommendation, percent) %>%
  spread(key = class_of_recommendation, value = percent, fill = 0)

# Para simplificar, assumindo que level_of_evidence tem 3 níveis: LOA-A, LOA-B, LOA-C
# Caso tenha mais níveis, ajuste o código conforme necessário

df_wide <- clean_names(df_wide)

# Remover a coluna Mixed se ela existir
df_wide <- df_wide %>% select(-mixed)

# Transformar o DataFrame para formato longo (long format)
df_long <- df_wide %>%
  pivot_longer(cols = starts_with("Class"), names_to = "Class", values_to = "Percentage")

# Filtrar apenas as combinações de ano e recomendação farmacológica que têm valores não nulos
df_long <- df_long %>%
  filter(level_of_evidence != "NA")

# Criar o heatmap
ggplot(df_long, aes(x = factor(year), y = level_of_evidence, fill = Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "skyblue", high = "blue", name = "Percentual") +
  labs(title = "Heatmap Level of evidence for Year",
       x = "Year",
       y = "Level of Evidence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Criar a tabela de resumo
summary_table <- df %>%
  group_by(class_of_recommendation, type_of_recommendation, level_of_evidence) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = level_of_evidence, values_from = count, values_fill = list(count = 0))

# Visualizar a tabela de resumo
print(summary_table)

# Criar a tabela de resumo com tabyl
summary_table <- df %>%
  tabyl(type_of_recommendation, class_of_recommendation, level_of_evidence) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

# Visualizar a tabela de resumo
print(summary_table)


# Dados baseados na tabela da imagem (removendo "Mixed" e "NA")
data <- data.frame(
  class_of_recommendation = rep(c("Class I", "Class II-a", "Class II-b", "Class III"), each = 2),
  type_of_recommendation = rep(c("Non-Pharmacological", "Pharmacological"), 4),
  LOA_A = c(111, 165, 20, 37, 5, 21, 15, 17),
  LOA_B = c(184, 148, 84, 87, 37, 43, 31, 51),
  LOA_C = c(124, 156, 113, 80, 62, 42, 32, 29)
)

# Converter os dados para long format
data_long <- data %>%
  gather(key = "Evidence_Level", value = "Count", LOA_A, LOA_B, LOA_C)

# Gráfico de Barras Empilhadas
ggplot(data_long, aes(x = interaction(class_of_recommendation, type_of_recommendation), y = Count, fill = Evidence_Level)) +
  geom_bar(stat = "identity") +
  labs(x = "Class and Type of Recommendation", y = "Count", fill = "Level of Evidence",
       title = "Distribution of Evidence Levels by Class and Type of Recommendation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Gráfico de Barras Empilhadas Normalizado (Porcentagem)
data_long <- data_long %>%
  group_by(class_of_recommendation, type_of_recommendation) %>%
  mutate(Percent = Count / sum(Count) * 100)


#Tabela cruzamento

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()


df_unique %>%
  select(level_of_evidence, class_of_recommendation) %>% 
  filter(class_of_recommendation != "Mixed") %>%
  filter(level_of_evidence != "NA") %>% 
  tbl_cross(
    row = level_of_evidence,
    col = class_of_recommendation,
    label = level_of_evidence ~ "Level of Evidence",
    percent = "col"
  ) %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Class of Recomendation**") %>% 
  bold_labels() %>% 
  add_p()


#Gráfico 
ggplot(data_long, aes(x = interaction(class_of_recommendation, type_of_recommendation), y = Count, fill = Evidence_Level)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Class and Type of Recommendation", y = "Count", fill = "Level of Evidence",
       title = "Percentage Distribution of Evidence Levels by Class and Type of Recommendation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(ggplot2)
library(reshape2)

# Dados agregados para o heatmap
data_heatmap <- df_unique %>%
  count(class_of_recommendation, level_of_evidence) %>% 
  filter(level_of_evidence != "NA")

# Mapa de Calor
ggplot(data_heatmap, aes(x = class_of_recommendation, y = level_of_evidence, fill = n)) +
  geom_tile() +
  labs(x = "Class of Recommendation", y = "Type of Recommendation", fill = "Count",
       title = "Heatmap of Recommendation Frequencies") +
  theme_minimal()

# Dados agregados para o heatmap
data_heatmap <- df_unique %>%
  count(class_of_recommendation, level_of_evidence) %>%
  group_by(class_of_recommendation) %>%
  filter(level_of_evidence != "NA") %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))

# Mapa de Calor usando porcentagens
ggplot(data_heatmap, aes(x = class_of_recommendation, y = level_of_evidence, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = percentage), color = "black", size = 6) +
  labs(x = "Class of Recommendation", y = "Level of Evidence", fill = "Percentage",
       title = "Heatmap of Level of Evidence Frequencies (Percentage)") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "green")

df_unique %>% 
  select(level_of_evidence) %>% 
  tbl_summary(
    label = level_of_evidence ~ "Level of Evidence") %>% 
  modify_header(label ~ "**Variable**")

df_unique %>% 
  select(class_of_recommendation) %>% 
  tbl_summary(
    label = class_of_recommendation ~ "Class of Recommendation") %>% 
  modify_header(label ~ "**Variable**")

df_unique %>% 
  select(type_of_recommendation) %>% 
  tbl_summary(
    label = type_of_recommendation ~ "Type of Recommendation") %>% 
  modify_header(label ~ "**Variable**")

df_unique %>% 
  select(pharmacological) %>%
  filter(pharmacological != "N/A") %>%
  tbl_summary(
    label = pharmacological ~ "Recommendation - Pharmacological ") %>% 
  modify_header(label ~ "**Variable**")

df_unique %>% 
  select(non_pharmacological) %>%
  filter(non_pharmacological != "N/A") %>%
  tbl_summary(
    label = non_pharmacological ~ "Recommendation - Non-Pharmacological ") %>% 
  modify_header(label ~ "**Variable**")

library(writexl)

write_csv(df_unique, "df_unique2.csv")

df_unique_2 <- df_unique %>% filter(level_of_evidence != "NA")


# Contagem total de recomendações
total_recommendations <- nrow(df_unique_2)

# Contagem de cada level_of_evidence
loe_counts <- df_unique_2 %>% count(level_of_evidence)

# Proporções de cada level_of_evidence
loe_proportions <- loe_counts %>%
  mutate(proportion = n / total_recommendations * 100)

# Calcular a mediana para LOE A
median_loeb <- loe_proportions %>% filter(level_of_evidence == "LOA-B") %>% pull(proportion)

# Calcular os percentis
percentiles <- loe_proportions %>% summarize(
  p25 = quantile(proportion, 0.25),
  p50 = quantile(proportion, 0.50),
  p75 = quantile(proportion, 0.75)
)

# Resultados
median_loeb
percentiles


###Análise do Dia 01/07/2024

# Filtrar os dados com valores não nulos em level_of_evidence
df_evidence <- df_unique %>% filter(level_of_evidence != "NA")

# Calcular as proporções de cada nível de evidência por ano
evidence_proportions <- df_evidence %>%
  group_by(year, level_of_evidence) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Plotar as proporções ao longo dos anos
ggplot(evidence_proportions, aes(x = year, y = count, color = level_of_evidence, group = level_of_evidence)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Recommendations by Level of Evidence Over the Years",
       x = "Year",
       y = "Count") +
  theme_minimal()

df_count_loe <- df_evidence %>% 
  count(year, level_of_evidence) %>% 
  spread(key = level_of_evidence, value = n, fill = 0) %>% 
  mutate()

# Preparar dados para teste qui-quadrado
level_of_evidence_counts <- df_evidence %>%
  group_by(year, level_of_evidence) %>%
  summarise(count = n()) %>%
  spread(level_of_evidence, count, fill = 0)

level_of_evidence_counts$year <- NULL

# Teste Qui-Quadrado# Teste df_evidenceQui-Quadrado
chisq.test(as.matrix(level_of_evidence_counts))

# Calcular proporção de recomendações farmacológicas e não farmacológicas ao longo do tempo
pharm_nonpharm_counts <- df_unique %>%
  group_by(year, type_of_recommendation) %>%
  summarise(count = n()) %>%
  spread(type_of_recommendation, count, fill = 0)

pharm_nonpharm_counts$year <- NULL

# Teste Qui-Quadrado para recomendações farmacológicas e não farmacológicas
chisq.test(as.matrix(pharm_nonpharm_counts))


# Calcular contribuições para o teste qui-quadrado nas subáreas farmacológicas
pharm_subareas_counts <- df_unique %>%
  filter(type_of_recommendation == "Pharmacological") %>%
  group_by(year, pharmacological) %>%
  summarise(count = n()) %>%
  spread(pharmacological, count, fill = 0)

pharm_subareas_counts$year <- NULL

chisq_pharm <- chisq.test(as.matrix(pharm_subareas_counts))

# Calcular contribuições para o teste qui-quadrado nas subáreas não farmacológicas
nonpharm_subareas_counts <- df %>%
  filter(type_of_recommendation == "Non-Pharmacological") %>%
  group_by(year, non_pharmacological) %>%
  summarise(count = n()) %>%
  spread(non_pharmacological, count, fill = 0)

nonpharm_subareas_counts$year <- NULL

chisq_nonpharm <- chisq.test(as.matrix(nonpharm_subareas_counts))
                                       
# Subáreas farmacológicas com maior contribuição
contrib_pharm <- (chisq_pharm$observed - chisq_pharm$expected)^2 / chisq_pharm$expected
contrib_pharm_sum <- colSums(contrib_pharm)
contrib_pharm_sum[order(-contrib_pharm_sum)][1:10]

# Subáreas não farmacológicas com maior contribuição
contrib_nonpharm <- (chisq_nonpharm$observed - chisq_nonpharm$expected)^2 / chisq_nonpharm$expected
contrib_nonpharm_sum <- colSums(contrib_nonpharm)
contrib_nonpharm_sum[order(-contrib_nonpharm_sum)][1:10]

#### Preenchimento das Tabelas Resultados 03/07/2024

df_unique %>% 
  group_by(pharmacological) %>% 
  count(level_of_evidence) %>% 
  print(n = 90) %>% 
  filter(level_of_evidence == "LOA-A") %>% 
  print(n = 30)

df_unique %>% 
  group_by(pharmacological) %>% 
  count(class_of_recommendation) %>% 
  print(n = 100)

df_unique %>% 
  group_by(non_pharmacological) %>% 
  count(level_of_evidence) %>% 
  print(n = 90)

df_unique %>% 
  group_by(non_pharmacological) %>% 
  count(class_of_recommendation) %>% 
  print(n = 100)

df_unique %>% 
  group_by(year) %>% 
  count(cardiology_society) %>% 
  print(n = 100)

library(tidyverse)
library(ggplot2)
library(reshape2)
library(scales)

# Função para criar e plotar o heatmap para um dado Nível de Evidência
plot_heatmap_for_loe <- function(df, level_of_evidence) {
  # Filtra os dados pelo nível de evidência
  loe_data <- df %>% filter(level_of_evidence == level_of_evidence)
  
  # Agrupa os dados e calcula o tamanho e a porcentagem
  grouped_loe <- loe_data %>%
    group_by(therapeutic_subgroup_3_or_4_digits, year) %>%
    summarise(Count = n()) %>%
    spread(year, Count, fill = 0)
  
  grouped_loe_percentage <- grouped_loe %>%
    mutate_at(vars(-therapeutic_subgroup_3_or_4_digits), funs((. / sum(.)) * 100))
  
  # Transforma os dados em formato longo para o ggplot2
  grouped_loe_long <- grouped_loe_percentage %>%
    gather(key = "year", value = "percentage", -therapeutic_subgroup_3_or_4_digits)
  
  # Plota o heatmap
  ggplot(grouped_loe_long, aes(x = year, y = therapeutic_subgroup_3_or_4_digits, fill = percentage)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red", name = paste0("% ", level_of_evidence)) +
    geom_text(aes(label = round(percentage, 1)), color = "black") +
    labs(title = paste0('Heatmap of ', level_of_evidence, ' Recommendations by Year and Category'),
         x = 'Year',
         y = 'Therapeutic Subgroup (3 or 4-Digits)') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Exemplo de uso da função (considerando que 'df' é o seu dataframe):
plot_heatmap_for_loe(df_unique, "LOA-A")



