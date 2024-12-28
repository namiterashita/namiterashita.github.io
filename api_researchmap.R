pacman::p_load(tidyverse, jsonlite)

# 論文API
api_url = paste0("https://api.researchmap.jp/",
                 "kazuhiroterashita/",
                 "published_papers" # 論文リスト
                 )

parsedjson <- fromJSON(api_url)

# 共著者を処理し、1つの文字列として結合する
authors_ja <- sapply(parsedjson$items$authors$ja, 
                  function(df)
                    paste(df$name, collapse = ", "))

authors_en <- sapply(parsedjson$items$authors$en, 
                     function(df)
                       paste(df$name, collapse = ", "))

cinii <- sapply(parsedjson$items$identifiers$cinii_cr_id, 
                 function(x) if (is.null(x)) NA else x, USE.NAMES = FALSE)

doi <- sapply(parsedjson$items$identifiers$doi, 
                function(x) if (is.null(x)) NA else x, USE.NAMES = FALSE)

parsedjson$items$see_also

gyoseki <-
  data.frame(authors_ja = authors_ja,
             authors_en = authors_en,
             year = parsedjson$items$publication_date,
             title_ja = parsedjson$items$paper_title$ja,
             title_en = parsedjson$items$paper_title$en,
             pub_ja = parsedjson$items$publication_name$ja,
             pub_en = parsedjson$items$publication_name$en,
             vol = parsedjson$items$volume,
             no = parsedjson$items$number,
             page_st = parsedjson$items$starting_page,
             page_en = parsedjson$items$ending_page,
             type = parsedjson$items$published_paper_type,
             referee = parsedjson$items$referee,
             cinii = cinii,
             doi = doi) |>
  mutate(year = str_remove_all(year, "-\\d{2,}"),
         referee_ja = case_when(referee == TRUE ~ "査読あり",
                                TRUE ~ ""),
         referee_en = case_when(referee == TRUE ~ "Peer Reviewed",
                                TRUE ~ ""))

gyoseki[is.na(gyoseki)] <- ""

journal_a <- gyoseki |>
  filter(type == "scientific_journal"|
           type == "research_institution") |>
  mutate(list_cv = paste0(  
        authors_en, ". ",year, ". ", title_en, ". ", pub_en, " ",vol, "(",no, "):", page_st, "-",page_en,". ",
        "<br>(=",
        "「",title_ja,"」", "『",pub_ja, "』", vol, "(",no, "):", page_st, "-",page_en,".) "),
        list_cv = str_remove_all(list_cv, "\\(\\)"),
        list_web = paste0(
          authors_ja, ". ",year, ". ",  "「",title_ja,"」", "『",pub_ja, "』", vol, "(",no, "):", page_st, "-",page_en,". "),
        list_web = str_remove_all(list_web, "\\(\\)"),
        list_web = str_remove_all(list_web, "[DOI](https://doi.org/){.btn .btn-outline-success .btn-sm}")
        )


api_misc = paste0("https://api.researchmap.jp/",
                  "kazuhiroterashita/",
                  "misc" # misc
)

miscjson <- fromJSON(api_misc)

# presentationAPI
api_pre = paste0("https://api.researchmap.jp/",
                 "kazuhiroterashita/",
                 "presentations" # presentations
)

prejson <- fromJSON(api_pre)

# 共著者を処理し、1つの文字列として結合する
presenters_ja <- sapply(prejson$items$presenters$ja, 
                     function(df)
                       paste(df$name, collapse = ", "))

presenters_en <- sapply(prejson$items$presenters$en, 
                     function(df)
                       paste(df$name, collapse = ", "))

lang <- sapply(prejson$items$languages, 
                function(x) if (is.null(x)) NA else x, USE.NAMES = FALSE)

presentations <-
  data.frame(presenters_ja = presenters_ja,
             presenters_en = presenters_en,
             year_date = prejson$items$publication_date,
             title_ja = prejson$items$presentation_title$ja,
             title_en = prejson$items$presentation_title$en,
             pub_ja = prejson$items$event$ja,
             pub_en = prejson$items$event$en,
             type = prejson$items$presentation_type,
             location = prejson$items$location,
             lang = lang) |>
  mutate(year_month_j = format(as.Date(year_date), "%Y年%m月"),
         year_month_j = str_replace(year_month_j, "年0", "年"),
         year_month_k = format(as.Date(year_date), "%Y년%m월"),
         year_month_k = str_replace(year_month_k, "년0", "년"),
         year_month_e = format(as.Date(year_date), "%b %Y"),
         year_month = case_when(lang == "eng" ~ year_month_e,
                                lang == "jpn" ~ year_month_j,
                                lang == "kor" ~ year_month_k,
                                TRUE ~ ""))


# awardsAPI
api_awards = paste0("https://api.researchmap.jp/",
                    "kazuhiroterashita/",
                    "awards" # awards
)

awardsjson <- fromJSON(api_awards)



awards <-
  data.frame(year_month = awardsjson$items$award_date,
             award = awardsjson$items$award_name,
             association = awardsjson$items$association,
             lang = awardsjson$items$address_country) |>
  mutate(year_month_j = str_replace(year_month, "-","年"),
         year_month_j = str_c(year_month_j, "月"),
         year_month_j = str_replace(year_month_j, "年0", "年"),
         year_month_k = str_replace(year_month_j, "年", "년"),
         year_month_k = str_replace(year_month_k, "月", "월"),
         year_month_e = str_c(year_month, "-01"),
         year_month_e = format(as.Date(year_month_e), "%b %Y"),
         year_month = case_when(lang == "ENG" ~ year_month_e,
                                lang == "JPN" ~ year_month_j,
                                lang == "KOR" ~ year_month_k,
                                TRUE ~ ""))

# research_projectsAPI
api_research_projects = paste0("https://api.researchmap.jp/",
                    "kazuhiroterashita/",
                    "research_projects" # awards
)


research_projectsjson <- fromJSON(api_research_projects)


# 共著者を処理し、1つの文字列として結合する
investigators_ja <- sapply(research_projectsjson$items$investigators$ja, 
                     function(df)
                       paste(df$name, collapse = ", "))

grant_number <- sapply(research_projectsjson$items$identifiers$grant_number, 
              function(x) if (is.null(x)) NA else x, USE.NAMES = FALSE)


research_projects <-
  data.frame(from_date = research_projectsjson$items$from_date,
             to_date = research_projectsjson$items$to_date,
             research_project = research_projectsjson$items$research_project_title,
             offer_org = research_projectsjson$items$offer_organization,
             sys_name = research_projectsjson$items$system_name,
             investigators.ja = investigators_ja,
             grant_number = grant_number,
             category = research_projectsjson$items$category) |>
  mutate(from_date_j = str_replace(from_date, "-","年"),
         from_date_j = str_c(from_date_j, "月"),
         from_date_j = str_replace(from_date_j, "年0", "年"),
         from_date_k = str_replace(from_date_j, "年", "년"),
         from_date_k = str_replace(from_date_k, "月", "월"),
         from_date_e = str_c(from_date, "-01"),
         to_date_j = str_replace(to_date, "-","年"),
         to_date_j = str_c(to_date_j, "月"),
         to_date_j = str_replace(to_date_j, "年0", "年"),
         to_date_k = str_replace(to_date_j, "年", "년"),
         to_date_k = str_replace(to_date_k, "月", "월"),
         to_date_e = str_c(to_date, "-01"),
         to_date_e = format(as.Date(to_date_e), "%b %Y"),
         co.invest = str_count(investigators.ja, ","),
         co.invest = case_when(co.invest == 0 ~ "単独",
                               TRUE ~ "分担"))

research_projects <- research_projects |>
  mutate(list_web = paste0(
    "1. \\[", co.invest, "\\] ", from_date_j, "-", to_date_j, ": ",
    offer_org.ja, " ", sys_name.ja, 
    " \"",research_project.ja, " \"", ". ")
  )

research_projects <- research_projects |>
  mutate(list_web = format_titles(list_web)
  )


cat(paste(research_projects$list_web,
          collapse = "\r\r"))
