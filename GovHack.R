# ===========================
# Digital Confidence – R Script
# Xiaojing Liu, Qiuyue Liu
# Data: GovHack2025/data/digital-project-data_v2.csv
# Goal: Identify and analyze projects related to "Digital Confidence / Safe Online Participation"
# ===========================

# ---- Required packages ----
req_pkgs <- c("tidyverse","readr","stringr","lubridate","scales")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(req_pkgs, library, character.only = TRUE))

# ---- Load dataset ----
df <- read_csv("GovHack2025/data/digital-project-data_v2.csv", show_col_types = FALSE)

cat("==== Dataset structure ====\n")
print(dim(df))
print(names(df))
cat("\nSample rows:\n")
print(head(df, 5))

# ---- Data cleaning: Budget, Date, End Year ----
df <- df %>%
  mutate(
    # budget to numeric
    Budget_m = readr::parse_number(`Budget ($m)*`,
                                   na = c("nfp","NFP","Unavailable","NA","")),
    
    # normalize & parse date
    EndDate_raw  = as.character(`Project End Date`),
    EndDate_norm = stringr::str_replace_all(EndDate_raw, "\\.", "-"),
    EndDate      = lubridate::dmy(EndDate_norm, quiet = TRUE),
    
    # first build Year_End (string)
    Year_End = dplyr::if_else(!is.na(EndDate),
                              as.character(lubridate::year(EndDate)),
                              stringr::str_extract(EndDate_norm, "\\b(19|20)\\d{2}\\b")),
    
    # 标注 ongoing
    Year_End = dplyr::if_else(is.na(Year_End) & grepl("ongoing", tolower(EndDate_raw)),
                              "Ongoing", Year_End),
    
    # clean + numeric version
    Year_End_clean = Year_End,
    Year_End_num   = suppressWarnings(as.integer(Year_End_clean))
  )

year_levels <- c(sort(unique(na.omit(df$Year_End_num))), "Ongoing")


# ---- Define keywords ----
kw_scam     <- c("scam","fraud","anti-scam","phish","sms","spam")
kw_security <- c("security","cyber","intelligence","criminal","risk","safety")
kw_identity <- c("identity","id","digital id","mygov","login","authentication")
kw_privacy  <- c("privacy","consent","trust","data protection","governance")
kw_health   <- c("health","my health record","ehealth","interoperability")
kw_access   <- c("inclusion","access","accessibility","foundations","skills")

detect_any <- function(x, pats){
  str_detect(x, regex(paste(pats, collapse="|"), ignore_case = TRUE))
}

df <- df %>%
  mutate(
    text_all  = paste(`Project Name`, `Project Description`, sep = " | "),
    is_scam   = detect_any(text_all, kw_scam),
    is_sec    = detect_any(text_all, kw_security),
    is_iden   = detect_any(text_all, kw_identity),
    is_priv   = detect_any(text_all, kw_privacy),
    is_health = detect_any(text_all, kw_health),
    is_access = detect_any(text_all, kw_access),
    is_theme  = is_scam | is_sec | is_iden | is_priv | is_health | is_access,
    Tags = paste0(
      ifelse(is_scam,   "Scam;",   ""),
      ifelse(is_sec,    "Security;", ""),
      ifelse(is_iden,   "Identity;", ""),
      ifelse(is_priv,   "Privacy;",  ""),
      ifelse(is_health, "Health;",   ""),
      ifelse(is_access, "Inclusion;", "")
    ) %>% str_replace_all(";$","")
  )

# ---- Summary ----
cat("\n==== Thematic project overview ====\n")
summary_theme <- df %>%
  summarise(
    total_projects = n(),
    theme_projects = sum(is_theme, na.rm = TRUE),
    pct_theme      = round(mean(is_theme, na.rm = TRUE)*100, 1)
  )
print(summary_theme)

# ---- Plot 1: Top 10 projects by budget (thematic) ----
plot_top10 <- df %>%
  filter(is_theme, !is.na(Budget_m)) %>%
  arrange(desc(Budget_m)) %>%
  slice_head(n = 10) %>%
  mutate(ProjectWrap = str_wrap(`Project Name`, width = 35)) %>%  
  ggplot(aes(x = reorder(ProjectWrap, Budget_m), y = Budget_m)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Projects by Budget\n(Theme-related)",          
    x = "Project Name", y = "Budget (millions)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9, lineheight = 0.95))  
print(plot_top10)

# ---- Plot 2: Top 10 agencies by project count (thematic) ----
plot_agency <- df %>%
  filter(is_theme) %>%
  count(Agency, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Agency, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::number_format(accuracy = 1)) +
  labs(title = "Top Agencies by Count of Theme-Related Projects",
       x = "Agency", y = "Project Count") +
  theme_minimal(base_size = 12)
print(plot_agency)

# ---- Plot 3: Projects by end year (thematic) ----
plot_year <- df %>%
  filter(is_theme) %>%
  mutate(Year_End_f = factor(ifelse(is.na(Year_End_num), "Ongoing",
                                    as.character(Year_End_num)),
                             levels = as.character(year_levels))) %>%
  count(Year_End_f) %>%
  ggplot(aes(x = Year_End_f, y = n)) +
  geom_col(fill = "seagreen") +
  labs(title = "Theme-Related Projects by End Year",
       x = "End Year", y = "Project Count") +
  theme_minimal(base_size = 12)
print(plot_year)
# ---- Table: Tag-level summary ----
table_tags <- tibble(
  Tag = c("Scam","Security","Identity","Privacy","Health","Inclusion"),
  Project_Count = c(
    sum(df$is_scam,   na.rm = TRUE),
    sum(df$is_sec,    na.rm = TRUE),
    sum(df$is_iden,   na.rm = TRUE),
    sum(df$is_priv,   na.rm = TRUE),
    sum(df$is_health, na.rm = TRUE),
    sum(df$is_access, na.rm = TRUE)
  ),
  Budget_Sum_m = c(
    sum(df$Budget_m[df$is_scam],   na.rm = TRUE),
    sum(df$Budget_m[df$is_sec],    na.rm = TRUE),
    sum(df$Budget_m[df$is_iden],   na.rm = TRUE),
    sum(df$Budget_m[df$is_priv],   na.rm = TRUE),
    sum(df$Budget_m[df$is_health], na.rm = TRUE),
    sum(df$Budget_m[df$is_access], na.rm = TRUE)
  )
)
cat("\n==== Tag-level summary ====\n")
print(table_tags)

# ---- Table: Example of thematic projects (first 10) ----
cat("\n==== Sample thematic projects ====\n")
print(df %>%
        filter(is_theme) %>%
        select(Agency, Tier, `Project Name`, Budget_m, Year_End, Tags) %>%
        head(10))

# ============================================================
# EXTRA ANALYTICS: Lines, multi-series, quantiles & regression
# ============================================================

library(tidyr)
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
library(broom)

df_tags_long <- df %>%
  mutate(Tags = if_else(is.na(Tags), "", Tags)) %>%
  separate_rows(Tags, sep = ";", convert = FALSE) %>%
  filter(Tags != "")

# How many tags to show in the multi-line plots
top_n <- 5

# ----- C1: Multi-line (yearly budget by tag, top tags by total budget) -----
top_tags_budget <- df_tags_long %>%
  filter(!is.na(Budget_m)) %>%
  group_by(Tags) %>%
  summarise(Total_m = sum(Budget_m), .groups = "drop") %>%
  arrange(desc(Total_m)) %>%
  slice_head(n = top_n) %>%
  pull(Tags)

ts_tag_budget <- df_tags_long %>%
  filter(Tags %in% top_tags_budget, !is.na(Year_End_num), !is.na(Budget_m)) %>%
  group_by(Year_End_num, Tags) %>%
  summarise(Budget_Sum_m = sum(Budget_m), .groups = "drop")

plot_ts_tag_budget <- ggplot(ts_tag_budget,
                             aes(x = Year_End_num, y = Budget_Sum_m, color = Tags)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Yearly Budget by Tag (Top tags)",
       x = "End Year", y = "Total Budget (millions)") +
  theme_minimal(base_size = 12)
print(plot_ts_tag_budget)
# ggsave("plots/lines_budget_by_tag.png", plot_ts_tag_budget, width = 9, height = 5, dpi = 150)

# ----- C2: Multi-line (yearly project count by tag, same tag set) -----
ts_tag_count <- df_tags_long %>%
  filter(Tags %in% top_tags_budget, !is.na(Year_End_num)) %>%
  distinct(`Project Name`, Tags, Year_End_num) %>%  # avoid accidental duplicates
  count(Year_End_num, Tags)

plot_ts_tag_count <- ggplot(ts_tag_count,
                            aes(x = Year_End_num, y = n, color = Tags)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Yearly Project Count by Tag (Top tags)",
       x = "End Year", y = "Project Count") +
  theme_minimal(base_size = 12)
print(plot_ts_tag_count)
# ggsave("plots/lines_count_by_tag.png", plot_ts_tag_count, width = 9, height = 5, dpi = 150)

# ----- C3 (optional): Small multiples (one panel per tag) -----
plot_ts_small <- ggplot(ts_tag_budget,
                        aes(x = Year_End_num, y = Budget_Sum_m, group = Tags)) +
  geom_line(linewidth = 1) + geom_point() +
  facet_wrap(~ Tags, scales = "free_y") +
  labs(title = "Yearly Budget by Tag – Small Multiples",
       x = "End Year", y = "Total Budget (millions)") +
  theme_minimal(base_size = 12)
print(plot_ts_small)

