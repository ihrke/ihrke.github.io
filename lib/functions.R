get_my_osf_preprints <- function(){
  r=GET("https://api.osf.io/v2/users/dwha7/preprints/")
  preprint_ids=jsonlite::fromJSON(content(r, "text"))$data$id
  
  id=preprint_ids[1]
  map_df(preprint_ids, function(id){
    ## get APA-style citation
    r1=GET(sprintf("https://api.osf.io/v2/preprints/%s/citation/apa/", id))
    ## get details
    r2=GET(sprintf("https://api.osf.io/v2/preprints/%s/", id))
    tab2=jsonlite::fromJSON(content(r2, "text"))
    ppdate=lubridate::as_date(tab2$data$attributes$date_published)
    pplink= tab2$data$links$html
    
    ## get authors
    link=tab2$data$relationships$contributors$links$related$href
    auth.tab=NULL
    while(T){ ## sometimes multiple pages (>10 authors)
      r3=GET(link)
      tab3=jsonlite::fromJSON(content(r3, "text"))
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
    
    ## nice df
    tibble(
      date=ppdate,
      title=tab2$data$attributes$title,
      authors=list(auth.tab),
      citation=jsonlite::fromJSON(content(r1, "text"))$data$attributes$citation  ,
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
    r=GET(glue("http://dx.doi.org/{doi}"))
    website=content(r,"text")
    
    # looking for this kind of thing for biorxiv
    # <meta name="citation_date" content="2020/01/01" />"
    datestr=stringr::str_match(website, 'citation_date\\" content=\\"(.+?)\\"')[1,2]
  }) %>% lubridate::date()
}

