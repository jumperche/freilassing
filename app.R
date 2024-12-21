library(shiny)
library(shinyMobile)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(shinyjs)
library(shinyalert)
library(furrr)
library(promises)
library(future)
library(shinyWidgets)


plan(multisession)
# Parallelisierung aktivieren
get_full_article_text <- function(article_url, source) {
  if (is.na(article_url) || article_url == "") {
    return("Kein Link vorhanden")
  }
  
  page <- tryCatch(read_html(article_url), error = function(e) NULL)
  if (is.null(page)) {
    return("Fehler beim Abrufen des Artikels")
  }
  
  if (source == "freilassing") {
    content_node <- page %>% html_node('div.w-post-elm.post_content[itemprop="text"]')
    if (!is.null(content_node)) {
      full_text <- content_node %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    } else {
      full_text <- "Kein Inhalt verfügbar"
    }
  } else if (source == "vdk") {
    content_node <- page %>% html_node('div.teaser-content')
    if (!is.null(content_node)) {
      full_text <- content_node %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    } else {
      full_text <- "Kein Inhalt verfügbar"
    }
  } else {
    full_text <- "Quelle unbekannt"
  }
  
  return(full_text)
}

get_freilassing_articles <- function() {
  url <- "https://www.freilassing.de/neuigkeiten/"
  page <- read_html(url)
  
  articles <- page %>%
    html_nodes(".w-grid-item") %>%
    lapply(function(node) {
      title <- node %>% html_node(".post_title a") %>% html_text(trim = TRUE)
      description <- node %>% html_node(".post_content") %>% html_text(trim = TRUE)
      detail_url <- node %>% html_node(".post_title a") %>% html_attr("href")
      image_url <- node %>% html_node("img") %>% html_attr("src")
      date <- node %>% html_node(".w-post-elm-value") %>% html_text(trim = TRUE)
      
      full_text <- get_full_article_text(detail_url, source = "freilassing")
      
      data.frame(
        title = title,
        description = full_text,
        url = detail_url,
        image = image_url,
        date = dmy(date),
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  
  return(articles)
}

get_vdk_articles <- function() {
  url <- "https://bayern.vdk.de/vor-ort/ov-freilassing/veranstaltungen-und-aktuelles/"
  page <- read_html(url)
  
  articles <- page %>%
    html_nodes(".layout-list--news li") %>%
    lapply(function(node) {
      title <- node %>% html_node(".teaser-news__heading") %>% html_text(trim = TRUE)
      image_url <- node %>% html_node("img") %>% html_attr("src")
      if (!is.na(image_url) && !str_starts(image_url, "http")) {
        image_url <- paste0("https://bayern.vdk.de", image_url)
      }
      description <- node %>% html_node("img") %>% html_attr("alt")
      date <- node %>% html_node(".teaser-news__date") %>% html_text(trim = TRUE)
      detail_url <- node %>% html_node("a") %>% html_attr("href")
      if (!is.na(detail_url) && !str_starts(detail_url, "http")) {
        detail_url <- paste0("https://bayern.vdk.de", detail_url)
      }
      
      full_text <- get_full_article_text(detail_url, source = "vdk")
      
      data.frame(
        title = title,
        description = full_text,
        url = detail_url,
        image = image_url,
        date = dmy(date),
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  
  return(articles)
}

combine_articles <- function() {
  freilassing_articles <- get_freilassing_articles()
  vdk_articles <- get_vdk_articles()
  
  all_articles <- bind_rows(freilassing_articles, vdk_articles)
  
  # Remove duplicates based on title
  unique_articles <- all_articles %>%
    distinct(title, .keep_all = TRUE)
  
  # Sort by date (newest first)
  sorted_articles <- unique_articles %>%
    arrange(desc(date))
  
  return(sorted_articles)
}

# File path for local storage
articles_file <- "articles_data.rds"

# Function to fetch full article details dynamically
get_full_event_text <- function(event_url) {
  if (is.na(event_url) || event_url == "") {
    return(list(description = "Kein Link vorhanden", termin = NA, ort = NA, veranstalter = NA))
  }
  
  page <- tryCatch(read_html(event_url), error = function(e) NULL)
  if (is.null(page)) {
    return(list(description = "Fehler beim Abrufen des Events", termin = NA, ort = NA, veranstalter = NA))
  }
  
  # Extract relevant fields
  termin <- page %>%
    html_node(xpath = "//h6[strong[contains(text(), 'Termin:')]]") %>%
    html_text(trim = TRUE) %>%
    str_remove("Termin:") %>%
    str_trim()
  
  ort <- page %>%
    html_node(xpath = "//h6[strong[contains(text(), 'Ort:')]]") %>%
    html_text(trim = TRUE) %>%
    str_remove("Ort:") %>%
    str_trim()
  
  veranstalter <- page %>%
    html_node(xpath = "//h6[strong[contains(text(), 'Veranstalter:')]]") %>%
    html_text(trim = TRUE) %>%
    str_remove("Veranstalter:") %>%
    str_trim()
  
  description <- page %>%
    html_node('div.w-post-elm.post_content[itemprop="text"]') %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n")
  
  return(list(
    description = ifelse(nchar(description) > 0, description, "Keine Beschreibung verfügbar"),
    termin = termin,
    ort = ort,
    veranstalter = veranstalter
  ))
}



# Function to scrape event data
get_freilassing_events <- function() {
  url <- "https://www.freilassing.de/veranstaltungen/"
  page <- read_html(url)
  
  events <- page %>%
    html_nodes(".w-grid-item") %>%
    lapply(function(node) {
      # Titel und Link
      title <- node %>% html_node(".post_title a") %>% html_text(trim = TRUE)
      link <- node %>% html_node(".post_title a") %>% html_attr("href")
      
      # Bild
      image <- node %>% html_node("img") %>% html_attr("src")
      if (!is.na(image) && !str_detect(image, "\\.jpg$")) {
        image <- NA
      }
      
      # Detailseite laden
      details <- get_full_event_text(link)
      
      # Gebühr und Anmeldung erforderlich
      fee <- NA
      registration_required <- NA
      
      data.frame(
        title = title,
        date = str_extract(details$termin, "\\d{2}\\.\\d{2}\\.\\d{4}") %>% as.Date(format = "%d.%m.%Y"),
        time = str_extract(details$termin, "\\d{2}:\\d{2} Uhr"),
        location = details$ort,
        organizer = details$veranstalter,
        description = details$description,
        link = link,
        image = image,
        fee = fee,
        registration_required = registration_required,
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  
  return(events)
}





get_vdk_events <- function() {
  url <- "https://bayern.vdk.de/vor-ort/ov-freilassing/veranstaltungen-und-aktuelles/"
  page <- read_html(url)
  
  events <- page %>%
    html_nodes("li.layout-list__item") %>%
    lapply(function(node) {
      # Titel des Events
      title <- node %>%
        html_node("h3.teaser-news__heading") %>%
        html_text(trim = TRUE)
      
      # Detail-Link des Events
      detail_url <- node %>%
        html_node("h3.teaser-news__heading") %>%
        html_attr("data-tooltip")
      
      # Datum des Events
      date <- node %>%
        html_node("time.meta-event__day-start") %>%
        html_attr("datetime") %>%
        as.Date(format = "%Y-%m-%d")
      
      # Zeit des Events
      time <- node %>%
        html_node("time.meta-event__time") %>%
        html_text(trim = TRUE)
      
      # Ort des Events
      location <- node %>%
        html_node("div.meta-event__text") %>%
        html_text(trim = TRUE)
      
      # Beschreibung (falls vorhanden, aus Tooltip oder direktem Text)
      description <- node %>%
        html_node("div.teaser-news__teaser") %>%
        html_text(trim = TRUE)
      
      # Bild-URL
      image_url <- node %>%
        html_node("img") %>%
        html_attr("src")
      if (!is.na(image_url) && !str_starts(image_url, "http")) {
        image_url <- paste0("https://bayern.vdk.de", image_url)
      }
      
      # Daten in ein DataFrame konvertieren
      data.frame(
        title = title,
        date = date,
        time = time,
        location = location,
        description = ifelse(nchar(description) > 0, description, "Keine Beschreibung verfügbar"),
        url = detail_url,
        image = image_url,
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  
  return(events)
}

# VHS-Kategorien
vhs_categories <- c("643-CAT-KAT15", "643-CAT-KAT16", "643-CAT-KAT19", "643-CAT-KAT20",
                    "643-CAT-KAT21", "643-CAT-KAT22", "643-CAT-KAT23")

# Funktion zum Abrufen der VHS-Kurse (Hauptseiten)
get_courses_from_category <- function(category) {
  url <- paste0("https://www.vhs-rupertiwinkel.de/p/", category)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(data.frame())
  
  # Scraping der Kursliste
  courses <- page %>%
    html_nodes(".card-list-item-wrap a.card-list-item") %>%
    lapply(function(node) {
      title <- node %>% html_attr("title")
      link <- node %>% html_attr("href")
      full_link <- paste0("https://www.vhs-rupertiwinkel.de", link)
      data.frame(title = title, link = full_link, stringsAsFactors = FALSE)
    }) %>%
    bind_rows()
  
  return(courses)
}

# Funktion zum Abrufen von Kursdetails
get_course_details <- function(course_url) {
  page <- tryCatch(read_html(course_url), error = function(e) NULL)
  if (is.null(page)) {
    return(data.frame(
      title = NA, provider = "VHS", description = NA,
      date = NA, fee = NA, location = NA, link = course_url,
      stringsAsFactors = FALSE
    ))
  }
  
  # Kursinfos extrahieren
  title <- page %>%
    html_node("h1.my-4") %>%
    html_text(trim = TRUE)
  
  description <- page %>%
    html_node(".contentMain") %>%
    html_text(trim = TRUE)
  
  date_time <- page %>%
    html_node(".list-group-item.d-flex.justify-content-between") %>%
    html_text(trim = TRUE)
  start_date <- str_extract(date_time, "\\d{2}\\.\\d{2}\\.\\d{4}") %>%
    dmy()
  
  fee <- page %>%
    html_node("#course_fee .course-priceNumber") %>%
    html_text(trim = TRUE)
  
  location <- page %>%
    html_node("#course_venues small") %>%
    html_text(trim = TRUE)
  
  # Rückgabe der Daten
  return(data.frame(
    title = title,
    provider = "VHS",
    description = description,
    date = start_date,
    fee = fee,
    location = location,
    link = course_url,
    stringsAsFactors = FALSE
  ))
}
get_vhs_events <- function() {
  # Fetch all courses from VHS categories
  all_courses <- lapply(vhs_categories, get_courses_from_category) %>%
    bind_rows()
  
  # Fetch details for each course
  detailed_courses <- lapply(all_courses$link, function(course_url) {
    page <- tryCatch(read_html(course_url), error = function(e) NULL)
    if (is.null(page)) {
      return(data.frame(
        title = NA, date = NA, time = NA, location = NA, organizer = "VHS",
        description = NA, link = course_url, image = NA, fee = NA,
        registration_required = TRUE, stringsAsFactors = FALSE
      ))
    }
    
    # Extract course information
    title <- page %>% html_node("h1.my-4") %>% html_text(trim = TRUE)
    description <- page %>% html_node(".contentMain") %>% html_text(trim = TRUE)
    date_time <- page %>% html_node(".list-group-item.d-flex.justify-content-between") %>% html_text(trim = TRUE)
    date <- str_extract(date_time, "\\d{2}\\.\\d{2}\\.\\d{4}") %>% dmy()
    time <- str_extract(date_time, "\\d{2}:\\d{2} Uhr")
    fee <- page %>% html_node("#course_fee .course-priceNumber") %>% html_text(trim = TRUE)
    location <- page %>% html_node("#course_venues small") %>% html_text(trim = TRUE)
    
    data.frame(
      title = title,
      date = date,
      time = time,
      location = location,
      organizer = "VHS",
      description = description,
      link = course_url,
      image = NA,
      fee = fee,
      registration_required = TRUE,
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
  
  return(detailed_courses)
}

# Funktion zum Abrufen aller VHS-Events
get_vdk_events <- function() {
  url <- "https://bayern.vdk.de/vor-ort/ov-freilassing/veranstaltungen-und-aktuelles/"
  page <- read_html(url)
  
  events <- page %>%
    html_nodes("li.layout-list__item") %>%
    lapply(function(node) {
      # Titel und Link
      title <- node %>% html_node("h3.teaser-news__heading") %>% html_text(trim = TRUE)
      link <- node %>% html_node("h3.teaser-news__heading") %>% html_attr("data-tooltip")
      
      # Datum und Zeit
      date <- node %>% html_node("time.meta-event__day-start") %>% html_attr("datetime") %>%
        as.Date(format = "%Y-%m-%d")
      time <- node %>% html_node("time.meta-event__time") %>% html_text(trim = TRUE)
      
      # Ort und Beschreibung
      location <- node %>% html_node("div.meta-event__text") %>% html_text(trim = TRUE)
      description <- node %>% html_node("div.teaser-news__teaser") %>% html_text(trim = TRUE)
      
      # Bild
      image <- node %>% html_node("img") %>% html_attr("src")
      if (!is.na(image) && !str_starts(image, "http")) {
        image <- paste0("https://bayern.vdk.de", image)
      }
      
      # Gebühr und Anmeldung erforderlich
      fee <- NA
      registration_required <- NA
      
      data.frame(
        title = title,
        date = date,
        time = time,
        location = location,
        organizer = "VDK",
        description = description,
        link = link,
        image = image,
        fee = fee,
        registration_required = registration_required,
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  
  return(events)
}


combine_events <- function() {
  # Fetch events from all sources
  freilassing_events <- get_freilassing_events()
  vdk_events <- get_vdk_events()
  vhs_events <- get_vhs_events()
  
  # Combine all events
  all_events <- bind_rows(freilassing_events, vdk_events, vhs_events)
  
  # Remove past events and sort by date
  current_date <- Sys.Date()
  all_events <- all_events %>%
    filter(!is.na(date) & date >= current_date) %>%
    arrange(date)
  
  return(all_events)
}


# File path for local storage
events_file <- "events_data.rds"


needs_update <- function(file_path, max_age_hours) {
  if (!file.exists(file_path)) {
    return(TRUE) 
  }
  file_mod_time <- file.info(file_path)$mtime
  elapsed_time <- difftime(Sys.time(), file_mod_time, units = "hours")
  return(elapsed_time > max_age_hours) 
}

update_at_fixed_times <- function(update_function, file, data_val, session, status_label, max_age_hours = 8) {
  if (needs_update(file, max_age_hours)) {
 
    if (file.exists(file)) {
      existing_data <- tryCatch(readRDS(file), error = function(e) NULL)
      if (!is.null(existing_data)) {
        data_val(existing_data)
      }
    }
    
    # Update im Hintergrund ausführen
    future::future({
      new_data <- tryCatch({
        update_function()
      }, error = function(e) NULL)
      
      if (!is.null(new_data)) {
        saveRDS(new_data, file)
        new_data
      } else {
        NULL
      }
    }) %...>% {
     
      if (!is.null(.)) {
        session$sendCustomMessage("update_complete", list(status = status_label)) # Optional: Benutzer benachrichtigen
        data_val(.)
      }
    }
  }
  
 
  current_time <- lubridate::now()
  next_times <- c(
    lubridate::today() + hours(7),
    lubridate::today() + hours(14),
    lubridate::today() + hours(22)
  )
  
  next_time <- next_times[next_times > current_time][1]
  if (is.na(next_time)) {
    next_time <- next_times[1] + days(1)
  }
  
  invalidateLater(as.numeric(difftime(next_time, current_time, units = "secs")) * 1000, session = session)
}




ui <- f7Page(
  f7TabLayout(
    navbar = f7Navbar(
      title = "Artikel Übersicht",
      hairline = TRUE,
      shadow = TRUE
    ),
    panels = tagList(
      f7Panel(
        title = "Menü",
        side = "left",
        theme = "light",
        f7Button("update_articles", "Artikel aktualisieren")
      )
    ),
    f7Tabs(
      id = "tabs",
      f7Tab(
        tabName = "Artikel",
        icon = f7Icon("rectangle_stack_fill"),
        active = TRUE,
        uiOutput("articles_ui")
      ),
      f7Tab(
        tabName = "Events",
        icon = f7Icon("calendar"),
        uiOutput("events_ui")
      ),
      f7Tab(
        tabName = "Einstellungen",
        icon = f7Icon("gear_fill"),
        h2("Einstellungen"),
        p("Hier können Einstellungen vorgenommen werden.")
      )
    )
  )
  
  ,
  # Platzhalter für das Popup
  uiOutput("article_popup"),
  uiOutput("event_popup"),
  
  # JavaScript-Code, um das Popup zu öffnen
  tags$script(HTML("
  Shiny.addCustomMessageHandler('close-popup', function(message) {
    var popupId = '#' + message.id;
    console.log('Schließe Popup: ', popupId);
    if (app.popup.get(popupId)) {
      app.popup.close(popupId);
    }
  });

  // Popup öffnen
  Shiny.addCustomMessageHandler('open-popup', function(message) {
    var popupId = '#' + message.id;
    console.log('Öffne Popup: ', popupId);
    if (app.popup.get(popupId)) {
      app.popup.close(popupId);  // Vorheriges schließen, falls noch offen
    }
    app.popup.open(popupId);
  });

"))
  ,
  # CSS-Styling bleibt unverändert
  tags$style(HTML("
  .custom-card {
    border-radius: 12px;
    box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1);
    background-color: #fff;
    padding: 15px;
    margin: 15px;
  }
  .custom-card img {
    border-radius: 8px;
    width: 100%;
    height: auto;
  }
  .custom-card-title {
    font-size: 1.2em;
    font-weight: bold;
    color: #333;
    margin-top: 10px;
  }
  .custom-card-description {
    color: #666;
    margin-top: 10px;
    margin-bottom: 15px;
  }
  .weiterlesen-button {
    display: inline-block;
    padding: 20px 20px;
    background-color: #AEDEF4;
    color: white;
    border-radius: 6px;
    text-align: center;
    text-decoration: none;
    font-weight: bold;
    transition: background-color 0.3s;
    margin: 10px auto;
    cursor: pointer;
  }
  .weiterlesen-button:hover {
    background-color: #AEDEF4; /* Hover-Farbe */
  }
  .tabbar a.tab-link-active .tabbar-label, .tabbar a .tabbar-label {
    color: #6200EA !important;
  }
  .tabbar a.tab-link-active .tabbar-icon, .tabbar a .tabbar-icon {
    color: #6200EA !important;
  }
  .custom-card img {
  border-radius: 8px;
  width: 100% !important; /* Added !important to ensure it overrides */
  height: auto !important;
  }
  .weiterlesen-button .icon
  .weiterlesen-button .fa, /* Font Awesome Icons */
  .weiterlesen-button .i { /* Andere Icons innerhalb des Buttons */
    display: none !important; /* Versteckt das Icon */
  }
   /* Events mit Bild */
  .custom-card-with-image {
    border: 1px solid #ccc;
    border-radius: 8px;
    margin: 15px;
    padding: 0;
    background-color: #fff;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    overflow: hidden;
  }
  .custom-card-with-image img {
    width: 100%;
    height: auto;
    display: block;
  }
  .custom-card-with-image .custom-card-content {
    padding: 15px;
  }

  /* Events ohne Bild */
  .custom-card-no-image {
    border: 1px solid #ccc;
    border-radius: 8px;
    margin: 15px;
    padding: 15px;
    background-color: #f9f9f9;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }
  .custom-card-no-image .custom-card-content {
    text-align: left;
  }
  
               
                 

"))
  
)

server <- function(input, output, session) {
  articles_data <- reactiveVal()
  events_data <- reactiveVal()
  updating <- reactiveVal(FALSE) 
  
 
  # Load or fetch articles
  load_articles <- function() {
    if (file.exists(articles_file)) {
      articles <- readRDS(articles_file)
      articles_data(articles)
    } else {
      showModal(modalDialog("Artikel werden abgerufen...", footer = NULL))
      articles <- combine_articles()
      saveRDS(articles, articles_file)
      articles_data(articles)
      removeModal()
    }
  }
  
  # Load or fetch events
  load_events <- function() {
    if (file.exists(events_file)) {
      events <- readRDS(events_file)
      events_data(events)
     
    } else {
      showModal(modalDialog("Events werden abgerufen...", footer = NULL))
      events <- combine_events()
      saveRDS(events, events_file)
      events_data(events)
      removeModal()
    }
  }
  
    # Daten laden
  load_articles()
  load_events()
  
  
  
  
  
  
  observe({
    update_at_fixed_times(
      update_function = combine_articles,
      file = articles_file,
      data_val = articles_data,
      session = session,
      status_label = "Artikel",
      max_age_hours = 8
    )
  })
  
  observe({
    update_at_fixed_times(
      update_function = combine_events,
      file = events_file,
      data_val = events_data,
      session = session,
      status_label = "Events",
      max_age_hours = 8
    )
  })
  
  
  
#### Manuelles Aktualisieren der Artikel####
    observeEvent(input$update_articles, {
      updating(TRUE)
      new_articles <- tryCatch({
        combine_articles()
      }, error = function(e) NULL)
      if (!is.null(new_articles)) {
        saveRDS(new_articles, articles_file)
        articles_data(new_articles)
      }
      updating(FALSE)
    })
    
    observeEvent(input$update_articles, {
      new_articles <- tryCatch({
        combine_articles()
      }, error = function(e) NULL)
      if (!is.null(new_articles)) {
        saveRDS(new_articles, articles_file)
        articles_data(new_articles)
      }
    })
    
    observeEvent(input$update_events, {
      new_events <- tryCatch({
        combine_events()
      }, error = function(e) NULL)
      if (!is.null(new_events)) {
        saveRDS(new_events, events_file)
        events_data(new_events)
      }
    })
    
#### Artikel anzeigen####
    output$articles_ui <- renderUI({
      articles <- articles_data()
      req(articles)
      
      lapply(1:nrow(articles), function(i) {
        article <- articles[i, ]
        
        # Check if the article has an image
        has_image <- !is.na(article$image) && article$image != ""
        short_description <- ifelse(
          nchar(article$description) > 200,
          paste0(substr(article$description, 1, 200), "..."),
          article$description
        )
        
        # Article date (formatted)
        formatted_date <- if (!is.na(article$date)) format(article$date, "%d.%m.%Y") else "Kein Datum verfügbar"
        
        if (has_image) {
          div(
            class = "custom-card-with-image",
            img(
              src = article$image,
              alt = article$title,
              class = "custom-card-image"
            ),
            div(
              class = "custom-card-content",
              h3(article$title, class = "custom-card-title"),
              p(paste("Datum:", formatted_date), class = "custom-card-date"), # Show date
              p(short_description, class = "custom-card-description"),
              actionButton(
                inputId = paste0("read_article_", i),
                label = "Weiterlesen",
                class = "weiterlesen-button"
              )
            )
          )
        } else {
          div(
            class = "custom-card-no-image",
            div(
              class = "custom-card-content",
              h3(article$title, class = "custom-card-title"),
              p(paste("Datum:", formatted_date), class = "custom-card-date"), # Show date
              p(short_description, class = "custom-card-description"),
              actionButton(
                inputId = paste0("read_article_", i),
                label = "Weiterlesen",
                class = "btn-details"
              )
            )
          )
        }
      })
    })
    
  
  #### Events anzeigen####
    output$events_ui <- renderUI({
      events <- events_data()
      req(events)
      
      # Filter future events
      current_date <- Sys.Date()
      events <- events %>%
        filter(!is.na(date) & date >= current_date)
      
      lapply(1:nrow(events), function(i) {
        event <- events[i, ]
        
        # Short description
        short_description <- ifelse(
          nchar(event$description) > 200,
          paste0(substr(event$description, 1, 200), "..."),
          event$description
        )
        
        # Check if image exists
        has_image <- !is.na(event$image) && nzchar(event$image)
      
        # Organizer
        organizer <- if (!is.na(event$organizer) && nzchar(event$organizer)) {
          event$organizer
        } else {
          "Unbekannter Organisator"
        }
        
        # Event date (formatted)
        formatted_date <- format(event$date, "%d.%m.%Y")
        
        if (has_image) {
          div(
            class = "custom-card-with-image",
            img(src = event$image, alt = event$title, class = "custom-card-image"),
            div(
              class = "custom-card-content",
              h3(event$title, class = "custom-card-title"),
              p(paste("Datum:", formatted_date), class = "custom-card-date"), # Show date
              p(short_description, class = "custom-card-description"),
              if (!is.na(event$fee) && nzchar(event$fee)) p(paste("Gebühren:", event$fee), class = "custom-card-fee"),
              if (!is.na(event$time) && nzchar(event$time)) p(paste("Zeit:", event$time), class = "custom-card-time"),
              p(paste("Organisator:", organizer), class = "custom-card-organizer"),
              actionButton(inputId = paste0("read_event_", i), label = "Details ansehen", class = "weiterlesen-button")
            )
          )
        } else {
          div(
            class = "custom-card-no-image",
            div(
              class = "custom-card-content",
              h3(event$title, class = "custom-card-title"),
              p(paste("Datum:", formatted_date), class = "custom-card-date"), # Show date
              p(short_description, class = "custom-card-description"),
              if (!is.na(event$fee) && nzchar(event$fee)) p(paste("Gebühren:", event$fee), class = "custom-card-fee"),
              if (!is.na(event$time) && nzchar(event$time)) p(paste("Zeit:", event$time), class = "custom-card-time"),
              p(paste("Organisator:", organizer), class = "custom-card-organizer"),
              actionButton(inputId = paste0("read_event_", i), label = "Details ansehen", class = "weiterlesen-button")
            )
          )
        }
      })
    })
    
  
  
#####Artikel "Weiterlesen"-Button####
    observe({
    articles <- articles_data()
    req(articles)
    
    lapply(1:nrow(articles), function(i) {
      local({
        my_i <- i
        article <- articles[my_i, ]
        
        observeEvent(input[[paste0("read_more_", my_i)]], {
          shinyalert(
            title = article$title,
            text = HTML(paste0(
              "<div style='text-align: center;'>",
              if (!is.na(article$image)) paste0(
                "<img src='", article$image, "' style='width: 100%; border-radius: 8px; margin-bottom: 10px;'>"
              ),
              "<p>", article$description, "</p>",
              "</div>"
            )),
            html = TRUE,
            size = "l",
            type = ""
          )
        })
      })
    })
  })
    
  
  
  #### Event "Details"-Button####
    observe({
      events <- events_data()
      req(events)
      
      lapply(1:nrow(events), function(i) {
        local({
          my_i <- i
          event <- events[my_i, ]
          
          observeEvent(input[[paste0("read_event_", my_i)]], {
            shinyalert(
              title = event$title,
              text = HTML(paste0(
                "<div style='text-align: left;'>",
                if (!is.na(event$image)) paste0(
                  "<img src='", event$image, "' style='width: 100%; border-radius: 8px; margin-bottom: 10px;'>"
                ),
                "<p><strong>Beschreibung:</strong> ", event$description, "</p>",
                if (!is.na(event$fee) && nzchar(event$fee)) paste0("<p><strong>Gebühren:</strong> ", event$fee, "</p>"),
                paste0("<p><strong>Datum:</strong> ", format(event$date, "%d.%m.%Y"), "</p>"),
                if (!is.na(event$time) && nzchar(event$time)) paste0("<p><strong>Zeit:</strong> ", event$time, "</p>"),
                paste0("<p><strong>Organisator:</strong> ", 
                       if (!is.na(event$organizer) && nzchar(event$organizer)) event$organizer else "Unbekannter Organisator", 
                       "</p>"),
                paste0(
                  if (!is.na(event$registration_required) && event$registration_required) {
                    "<p><strong>Anmeldung:</strong> Anmeldung notwendig</p>"
                  } else {
                    ""
                  }
                )
                ,
                
                if (!is.na(event$link) && nzchar(event$link)) paste0(
                  "<p><a href='javascript:void(0);' onclick='window.open(\"", event$link, "\", \"_blank\");' style='color: #007bff; text-decoration: underline;'>",
                  "Original-Link öffnen</a></p>"
                ),
                
                
                "</div>"
              )),
              html = TRUE,
              size = "l",
              type = "" 
            )
          })
        })
      })
    })
    
  
}


shinyApp(ui, server)
