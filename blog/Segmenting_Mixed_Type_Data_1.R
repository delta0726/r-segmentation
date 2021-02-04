# ***********************************************************************************************
# Title     : Segmenting with Mixed Type Data part-1
# Objective : TODO
# Created by: Owner
# Created on: 2021/02/04
# URL       : https://diegousai.io/2020/04/mixed-type-data-segmentation-initial-data-exploration/
# ***********************************************************************************************


# ＜概要＞
# - クラスタリング用のクリーニングデータセットの作成


# ＜目次＞
# 0 準備
# 1 初期加工
# 2 データ確認
# 3 データの追加処理
# 4 データ保存


# 0 準備 ---------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(skimr)
library(knitr)
library(janitor)


# データ作成
data_raw <-
   read_xlsx(
      path = "blog/data/Subscription Data.xlsx",
      sheet = 'Data',
      trim_ws = TRUE,
      col_names = TRUE,
      guess_max = 2000
   ) %>%
   set_names(names(.) %>% str_to_lower) %>%
   rename_at(vars(contains("cancellation")),
             funs(str_replace_all(., "cancellation", "canc"))) %>%
   rename_at(vars(contains(" ")),
             funs(str_replace_all(., "[ ]", "_")))

# データ確認
data_raw %>% glimpse()
data_raw %>% skim()


# 1 初期加工 ---------------------------------------------------------------------------------

# データ加工
# --- 重複を削除
data_clean <-
 data_raw %>%
   group_by(account_id) %>%
   count() %>%
   filter(n == 1) %>%
   ungroup() %>%
   left_join(data_raw) %>%
   select(-n)


# 2 データ確認 -------------------------------------------------------------------

# プロット作成
# --- 国別のサブスクリプション
data_raw %>%
   group_by(country) %>%
   count() %>%
   ungroup() %>%
   arrange(desc(n)) %>%
   ungroup() %>%
   mutate(country = country %>% as_factor()) %>%
   filter(n > 7) %>%

   ggplot(aes(x = country, y = n)) +
   geom_col(fill = "#E69F00", colour = "red") +
   theme_minimal() +
   labs(title     = 'Number of subscriptions by acquisition country',
         caption  = '',
         x        = 'Country of Residence',
         y        = 'Number of Subscribers') +
   theme(plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


# 3 データの追加処理  -------------------------------------------------------------------

# ステータスを簡単な表現に修正
data_clean <-
   data_clean %>%
   mutate(status =
             case_when(status == 'Lapsed' ~ 'Cancelled',
                       status == 'Pending Cancellation' ~ 'Cancelled',
                       TRUE ~ status)
   )


data_clean <-
   data_clean %>%
   filter(payment_frequency != 'Fixed Term')

data_clean <-
   data_clean %>%
   filter(campaign_code != 'Unknown')

data_clean <-
   data_clean %>%
   mutate(trial_monthly_price =
             case_when(trial_monthly_price == 6.94 ~ 0,
                       TRUE ~ trial_monthly_price),
          monthly_price =
             case_when(monthly_price == 6.94 ~ 6.99,
                       TRUE ~ monthly_price)
          )


data_clean <-
   data_clean %>%
      mutate(
         canc_reason =
          case_when(
            status == 'Active' & canc_reason == 'Unknown' ~ '-',

            # setting all NAs to zero as they're easier to deal with
            is.na(canc_reason)  ~ '-',

            canc_reason == 'App performance' |
            canc_reason == 'No compatible devices' |
            canc_reason == 'Look and feel'   |
            canc_reason == 'Functionality' |
            canc_reason == 'Download'                      ~ 'UX Related',

            canc_reason == 'Failed Credit Card Payment' |
            canc_reason == 'Failed PayPal Payment' |
            canc_reason == 'Failed Direct Debit Payment'   ~ 'Failed Payment',

            canc_reason == 'Political' |
            canc_reason == 'Editorial' |
            canc_reason == 'Lack of content'               ~ 'Editorial',

            canc_reason == 'Competitor' |
            canc_reason == 'Apple news'                    ~ 'Competitor',

            canc_reason == 'Product switch' |
            canc_reason == 'Amendment' |
            canc_reason == 'Duplicate subscription'        ~ 'Other',

            canc_reason == 'Not known' |
            canc_reason == 'Unknown'                       ~ '-',

            TRUE                                           ~ canc_reason)
      )


data_clean <-
   data_clean %>%
   filter(payment_method != 'Unknown')


# 4 データ保存 ------------------------------------------------------------------------------

# データ確認
data_clean %>% skim()
data_clean %>% glimpse()

# データ保存
data_clean %>% saveRDS( "blog/data/data_clean.rds")