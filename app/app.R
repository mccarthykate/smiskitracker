library(shiny)
library(bslib)

# ---- Data -------------------------------------------------------------------
# app/smiskis.csv is built from the Google Sheet by R/update_smiskis.R

smiskis <- read.csv("smiskis.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
smiskis$secret <- as.logical(smiskis$secret)
if (is.null(smiskis$image)) smiskis$image <- ""
smiskis$image[is.na(smiskis$image)] <- ""
smiskis$col_key <- gsub("[^a-z0-9]+", "-", tolower(smiskis$collection))

collections <- unique(smiskis$collection)
totals <- vapply(collections, function(cl) sum(smiskis$collection == cl), integer(1))
n_secrets <- sum(smiskis$secret)

empty_state <- function() {
  list(
    have = setNames(rep(FALSE, nrow(smiskis)), smiskis$id),
    date = setNames(rep("", nrow(smiskis)), smiskis$id)
  )
}

owned_in <- function(st, cl) sum(st$have[smiskis$id[smiskis$collection == cl]])

# Pictures live in images/ next to app/ (not inside it), so the published site
# can load them one at a time instead of bundling all of them into the app.
# On GitHub Pages they're copied to img/, one level above the app's own URL.
# When running locally with shiny::runApp("app"), serve them from ../images.
img_base <- "../img/"
if (dir.exists("../images")) addResourcePath("img", normalizePath("../images"))

# ---- UI pieces --------------------------------------------------------------

figure_row <- function(item, st) {
  id <- item$id
  have <- st$have[[id]]
  tags$li(
    class = paste("figure", if (have) "have"),
    tags$label(
      tags$input(
        type = "checkbox", class = "smiski-check", `data-id` = id,
        checked = if (have) NA
      ),
      tags$span(class = "tick", `aria-hidden` = "true"),
      if (nzchar(item$image)) {
        tags$img(
          class = "thumb", src = paste0(img_base, item$image), alt = "",
          width = 56, height = 56, loading = "lazy", decoding = "async"
        )
      } else {
        tags$span(class = "thumb thumb-empty", `aria-hidden` = "true")
      },
      tags$span(
        class = "text",
        tags$span(class = "name", item$figure),
        if (item$secret) tags$span(class = "secret-badge", "secret")
      )
    ),
    tags$input(
      type = "date", class = "smiski-date", `data-id` = id,
      value = st$date[[id]],
      `aria-label` = paste("Date you got", item$figure),
      title = "Date you got it"
    )
  )
}

collection_card <- function(cl, items, st) {
  owned <- owned_in(st, cl)
  total <- totals[[cl]]
  div(
    class = paste("smiski-card", if (owned == total) "complete"),
    `data-col` = items$col_key[1],
    div(
      class = "smiski-card-head",
      tags$h3(cl),
      tags$span(class = "count", tags$span(class = "owned", owned), " / ", total)
    ),
    div(class = "bar", div(class = "fill", style = sprintf("width:%.1f%%", 100 * owned / total))),
    tags$ul(
      class = "figures",
      lapply(seq_len(nrow(items)), function(i) figure_row(items[i, ], st))
    )
  )
}

stat_tile <- function(title, output_id, sub_id) {
  div(
    class = "stat",
    div(class = "stat-label", title),
    div(class = "stat-value", textOutput(output_id, inline = TRUE)),
    div(class = "stat-sub", textOutput(sub_id, inline = TRUE))
  )
}

# ---- UI ---------------------------------------------------------------------

ui <- page_sidebar(
  title = tagList(
    # Phones only: opens and closes the sidebar as a slide-out menu
    tags$button(
      type = "button", class = "menu-btn", `aria-label` = "Menu",
      `aria-expanded` = "false", `aria-controls` = "filters",
      tags$span(), tags$span(), tags$span()
    ),
    tags$span(class = "brand", tags$span(class = "brand-glow"), "Smiski Tracker")
  ),
  window_title = "Smiski Tracker",
  fillable = FALSE,
  sidebar = sidebar(
    id = "filters",
    width = 290,
    bg = "#EEF4E6",
    # On phones the sidebar becomes a slide-out menu (see tracker.js / styles.css)
    open = list(desktop = "open", mobile = "always"),
    textInput("search", "Search", placeholder = "e.g. Peeking"),
    selectizeInput(
      "collections", "Collections", choices = collections, multiple = TRUE,
      options = list(placeholder = "All collections", plugins = list("remove_button"))
    ),
    radioButtons(
      "status", "Show",
      choices = c("Everything" = "all", "Ones I have" = "have", "Ones I'm missing" = "missing")
    ),
    checkboxInput("secrets_only", "Secrets only", FALSE),
    tags$button(type = "button", class = "btn btn-primary w-100 menu-done", "Show figures"),
    tags$hr(),
    tags$h6("Your collection is saved in this browser"),
    tags$p(
      class = "small text-muted",
      "Download a backup to keep it safe, or to move it to another device."
    ),
    downloadButton("export", "Download backup", class = "btn-outline-primary btn-sm w-100"),
    fileInput("import", NULL, accept = ".csv", buttonLabel = "Restore…",
              placeholder = "backup .csv"),
    actionLink("clear", "Clear my whole collection", class = "small text-danger"),
    tags$hr(),
    tags$p(
      class = "small text-muted about",
      "A fan-made tracker, not affiliated with Smiski or Dreams Inc. Figure list from the ",
      tags$a(
        href = "https://docs.google.com/spreadsheets/d/1U4l4ugaL8UG7wzEfQqQ1h_oYYX0s0tGpqw9XV_cxnms/",
        target = "_blank", rel = "noopener", "Smiski collection sheet"
      ), "."
    )
  ),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;800&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "tracker.js")
  ),
  div(
    class = "stats",
    stat_tile("Figures collected", "stat_owned", "stat_owned_sub"),
    stat_tile("Sets completed", "stat_sets", "stat_sets_sub"),
    stat_tile("Secrets found", "stat_secrets", "stat_secrets_sub")
  ),
  uiOutput("grid")
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  state <- reactiveVal(empty_state())
  loaded <- reactiveVal(FALSE)
  redraw <- reactiveVal(0)
  search <- debounce(reactive(input$search), 300)

  persist <- function(st) {
    ids <- names(st$have)[st$have]
    payload <- setNames(
      lapply(ids, function(id) list(have = TRUE, date = st$date[[id]])),
      ids
    )
    session$sendCustomMessage("smiski_save", list(data = payload))
  }

  send_progress <- function(st, cl, celebrate = FALSE) {
    session$sendCustomMessage("smiski_progress", list(
      key = smiskis$col_key[match(cl, smiskis$collection)],
      owned = owned_in(st, cl), total = totals[[cl]], celebrate = celebrate
    ))
  }

  # Browser sends back what it saved last time
  observeEvent(input$saved_state, {
    st <- empty_state()
    saved <- input$saved_state$data
    if (is.list(saved) && length(saved)) {
      for (id in intersect(names(saved), smiskis$id)) {
        rec <- saved[[id]]
        st$have[[id]] <- isTRUE(rec$have)
        if (st$have[[id]] && is.character(rec$date)) st$date[[id]] <- rec$date
      }
    }
    state(st)
    loaded(TRUE)
  }, once = TRUE)

  # Ticking or unticking a figure
  observeEvent(input$toggle, {
    req(loaded())
    id <- input$toggle$id
    req(is.character(id), id %in% smiskis$id)
    st <- state()
    cl <- smiskis$collection[match(id, smiskis$id)]
    before <- owned_in(st, cl)
    have <- isTRUE(input$toggle$have)
    st$have[[id]] <- have
    st$date[[id]] <- if (have && is.character(input$toggle$date)) input$toggle$date else ""
    state(st)
    persist(st)
    after <- owned_in(st, cl)
    finished <- after == totals[[cl]] && before < after
    send_progress(st, cl, celebrate = finished)
    if (finished) {
      showNotification(paste0("You completed ", cl, "!"), type = "message", duration = 4)
    }
  })

  # Editing the date acquired
  observeEvent(input$date_change, {
    req(loaded())
    id <- input$date_change$id
    req(is.character(id), id %in% smiskis$id)
    st <- state()
    req(st$have[[id]])
    d <- input$date_change$date
    st$date[[id]] <- if (is.character(d)) d else ""
    state(st)
    persist(st)
  })

  output$grid <- renderUI({
    req(loaded())
    redraw()
    st <- isolate(state())
    d <- smiskis

    if (length(input$collections)) d <- d[d$collection %in% input$collections, ]
    q <- tolower(trimws(search() %||% ""))
    if (nzchar(q)) {
      d <- d[grepl(q, tolower(d$figure), fixed = TRUE) |
             grepl(q, tolower(d$collection), fixed = TRUE), ]
    }
    if (isTRUE(input$secrets_only)) d <- d[d$secret, ]
    if (identical(input$status, "have")) d <- d[st$have[d$id], ]
    if (identical(input$status, "missing")) d <- d[!st$have[d$id], ]

    if (!nrow(d)) {
      msg <- if (identical(input$status, "missing") && !nzchar(q)) {
        "Nothing missing here. You have them all!"
      } else {
        "No figures match those filters."
      }
      return(div(class = "empty", msg))
    }

    cards <- lapply(unique(d$collection), function(cl) {
      collection_card(cl, d[d$collection == cl, ], st)
    })
    div(class = "smiski-grid", cards)
  })

  # ---- Summary stats ----
  output$stat_owned <- renderText({
    sprintf("%d / %d", sum(state()$have), nrow(smiskis))
  })
  output$stat_owned_sub <- renderText({
    sprintf("%.0f%% of every figure listed", 100 * mean(state()$have))
  })
  output$stat_sets <- renderText({
    st <- state()
    sprintf("%d / %d", sum(vapply(collections, function(cl) owned_in(st, cl) == totals[[cl]], logical(1))),
            length(collections))
  })
  output$stat_sets_sub <- renderText({
    st <- state()
    started <- sum(vapply(collections, function(cl) owned_in(st, cl) > 0, logical(1)))
    sprintf("%d collection%s started", started, if (started == 1) "" else "s")
  })
  output$stat_secrets <- renderText({
    sprintf("%d / %d", sum(state()$have[smiskis$secret]), n_secrets)
  })
  output$stat_secrets_sub <- renderText("the rare ones")

  # ---- Backup / restore ----
  output$export <- downloadHandler(
    filename = function() paste0("my-smiskis-", Sys.Date(), ".csv"),
    content = function(file) {
      st <- state()
      out <- smiskis[, c("id", "collection", "figure", "secret")]
      out$have <- unname(st$have[out$id])
      out$date_acquired <- unname(st$date[out$id])
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  observeEvent(input$import, {
    d <- tryCatch(
      read.csv(input$import$datapath, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) NULL
    )
    if (is.null(d) || !all(c("id", "have") %in% names(d))) {
      showNotification("That file doesn't look like a Smiski Tracker backup.", type = "error")
      return()
    }
    d <- d[d$id %in% smiskis$id, ]
    have <- toupper(as.character(d$have)) %in% c("TRUE", "T", "1", "YES")
    st <- empty_state()
    st$have[d$id] <- have
    if ("date_acquired" %in% names(d)) {
      dates <- as.character(d$date_acquired)
      dates[is.na(dates) | !have] <- ""
      st$date[d$id] <- dates
    }
    state(st)
    persist(st)
    redraw(redraw() + 1)
    showNotification(sprintf("Restored %d figures from your backup.", sum(have)), type = "message")
  })

  observeEvent(input$clear, {
    showModal(modalDialog(
      title = "Clear your whole collection?",
      "This unticks every figure saved in this browser. Download a backup first if you might want it back.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("clear_confirm", "Clear everything", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$clear_confirm, {
    removeModal()
    st <- empty_state()
    state(st)
    persist(st)
    redraw(redraw() + 1)
  })
}

shinyApp(ui, server)
