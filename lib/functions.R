get_preprints_openalex <- function(orcid) {
  # Fetch all works by this ORCID that are preprints
  works <- openalexR::oa_fetch(
    entity = "works",
    author.orcid = orcid,
    type = "preprint"
  )

  if (is.null(works) || nrow(works) == 0) {
    return(tibble(
      title = character(),
      doi = character(),
      date = as.Date(character()),
      authors = list(),
      link = character(),
      source = character(),
      status = character()
    ))
  }

  # Process works into the expected format
  # Column names from openalexR: landing_page_url, source_display_name, authorships

  works %>%
    mutate(
      title = display_name,
      date = as.Date(publication_date),
      link = coalesce(landing_page_url, oa_url, paste0("https://doi.org/", doi)),
      # Determine source from URL if source_display_name is NA
      source = case_when(
        !is.na(source_display_name) ~ source_display_name,
        str_detect(link, "psyarxiv|osf\\.io") ~ "PsyArXiv",
        str_detect(link, "biorxiv") ~ "bioRxiv",
        str_detect(link, "medrxiv") ~ "medRxiv",
        str_detect(link, "arxiv\\.org") ~ "arXiv",
        TRUE ~ "Preprint"
      ),
      status = "submitted",
      # Convert author info to expected format
      authors = map(authorships, function(auth_list) {
        if (is.null(auth_list) || nrow(auth_list) == 0) {
          return(tibble(family_name = character(), given_name = character()))
        }
        auth_list %>%
          mutate(
            # Split "First Last" format from display_name
            name_parts = str_split(display_name, "\\s+"),
            given_name = map_chr(name_parts, ~paste(.x[-length(.x)], collapse = " ")),
            family_name = map_chr(name_parts, ~.x[length(.x)])
          ) %>%
          select(family_name, given_name)
      })
    ) %>%
    select(title, doi, date, authors, link, source, status) %>%
    # Keep only the most recent version of each preprint (by title)
    arrange(desc(date)) %>%
    group_by(title) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(desc(date))
}

get_my_osf_preprints <- function(){
  #r=httr::GET("https://api.osf.io/v2/users/dwha7/preprints/")
  #r <- rvest::read_html("https://osf.io/v2/users/dwha7/preprints/")
  r <- RCurl::getURL("https://api.osf.io/v2/users/dwha7/preprints/")
  preprint_ids=jsonlite::fromJSON(r)$data$id
  #preprint_ids=jsonlite::fromJSON(content(r, "text"))$data$id
  
  id="9v5sy_v1" # issue at OSF!
  id="u5j7s_v1"
  map_df(preprint_ids, function(id){
    print(id)
    ## get APA-style citation
    url=sprintf("https://api.osf.io/v2/preprints/%s/citation/apa/", id)
    #r1=GET(url)
    r1=RCurl::getURL(url)
    ## get details
    url=sprintf("https://api.osf.io/v2/preprints/%s/", id)
    #r2=GET(url)
    r2=RCurl::getURL(url)
    tab2=jsonlite::fromJSON(r2)#content(r2, "text"))
    ppdate=lubridate::as_date(tab2$data$attributes$date_published)
    pplink= tab2$data$links$html
    
    ## get authors
    link=tab2$data$relationships$contributors$links$related$href
    auth.tab=NULL
    while(T){ ## sometimes multiple pages (>10 authors)
      #r3=GET(link)
      r3=RCurl::getURL(link, .encoding = "utf-8")
      tab3=jsonlite::fromJSON(r3)#content(r3, "text"))
      tab3$data$embeds$users$data$attributes %>% select(family_name, middle_names, given_name) %>%
        mutate(author=sprintf("%s, %s.", family_name, str_sub(given_name, 1,1))) -> tmp
      auth.tab=bind_rows(auth.tab,tmp)
      link=tab3$links$`next`
      if(is.null(link)){
        break
      }
    }
    
    status="unknown"
    if("submitted" %in% tab2$data$attributes$tags){
      status="submitted"
    }
    if("accepted" %in% tab2$data$attributes$tags){
      status="accepted"
    }
    if("published" %in% tab2$data$attributes$tags){
      status="published"
    }
    
    
    citationstr <- tryCatch({
      jsonlite::fromJSON(r1)$data$attributes$citation  
    }, error = function(e){
      warning(sprintf("Problem in preprint id %s\nError:\n=====\n", id), e)
      ""
    })
    
    ## nice df
    tibble(
      date=ppdate,
      title=tab2$data$attributes$title,
      authors=list(auth.tab),
      #citation=jsonlite::fromJSON(content(r1, "text"))$data$attributes$citation  ,
      citation=citationstr,
      unpublished=(status!="published"),
      status=status,
      link=pplink
    )
  }) -> preprints
  return(preprints)
}

strip_curly_braces <- function(x){
  x=str_replace_all(x, "\\{", "")
  x=str_replace_all(x, "\\}", "")
  return(x)
}

get_citation_info_dimensions <- function(dois){
  map_df(dois, function(doi){
    r=GET(glue("http://metrics-api.dimensions.ai/doi/{doi}"))
    #r=RCurl::getURL(glue("http://metrics-api.dimensions.ai/doi/{doi}"))
    if(r$status_code==404){
      tab=list(doi=doi,times_cited=0, recent_citations=0, relative_citation_ratio=0, field_citation_ratio=0)
    } else {
      tab=jsonlite::fromJSON(content(r, "text"))
    }
    as.tibble(map(tab, function(x){ifelse(is.null(x), 0, x)})) %>% select(doi,times_cited, recent_citations, relative_citation_ratio, field_citation_ratio)
  })
}


get_date_from_doi <- function(dois) {
  map_chr(dois, function(doi){
    #r=GET(glue("http://dx.doi.org/{doi}"))
    r=RCurl::getURL(glue("http://dx.doi.org/{doi}"))
    website=r #content(r,"text")
    
    # looking for this kind of thing for biorxiv
    # <meta name="citation_date" content="2020/01/01" />"
    datestr=stringr::str_match(website, 'citation_date\\" content=\\"(.+?)\\"')[1,2]
  }) %>% lubridate::date()
}

