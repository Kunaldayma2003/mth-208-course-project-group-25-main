library(dplyr)
data <- read.csv("./../trending_videos_global.csv")

language_vector <- c(
  "unknown" = "Unknown",
  "en" = "English",
  "en-US" = "English",
  "ko" = "Korean",
  "es" = "Spanish",
  "en-CA" = "English",
  "en-GB" = "English",
  "ne" = "Nepali",
  "ja" = "Japanese",
  "es-MX" = "Spanish",
  "ro" = "Romanian",
  "ta" = "Tamil",
  "zxx" = "No linguistic content",
  "de" = "German",
  "de-DE" = "German",
  "tr" = "Turkish",
  "fi" = "Finnish",
  "de-AT" = "German",
  "fr" = "French",
  "fr-FR" = "French",
  "en-IN" = "English",
  "or" = "Odia",
  "hi" = "Hindi",
  "kn" = "Kannada",
  "te" = "Telugu",
  "ml" = "Malayalam",
  "mr" = "Marathi",
  "bh" = "Bhojpuri",
  "bn" = "Bengali",
  "hak-TW" = "Hakka",
  "en-AU" = "English",
  "pt" = "Portuguese",
  "pt-BR" = "Portuguese",
  "ru" = "Russian",
  "uk" = "Ukrainian",
  "zh" = "Chinese",
  "uz" = "Uzbek",
  "kk" = "Kazakh",
  "es-419" = "Spanish",
  "es-ES" = "Spanish",
  "ar" = "Arabic",
  "it" = "Italian",
  "zh-CN" = "Chinese",
  "id" = "Indonesian",
  "jv" = "Javanese",
  "nl" = "Dutch"
)

data <- data %>%
  mutate(language = recode(language, !!!language_vector))

#req.colname <- c("rank","region","title","language","category","channel_title","publish_at","duration_second","likes","views","comments")
req.colname <- c("region","title","language","category","duration_second","likes","views","comments")

global.trendings.dataset <- data[req.colname]
save(global.trendings.dataset, file="./../global-trendings-dataset.RData")

