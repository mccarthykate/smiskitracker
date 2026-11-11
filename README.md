# Smiski Tracker

A website for tracking your Smiski blind box collection.

**Live site:** https://mccarthykate.github.io/smiskitracker/

Tick off the figures you own, record the date you got each one, and see your
progress for each collection. Your collection is saved in your own browser. Use
**Download backup** / **Restore** to keep it safe or move it to another device.

The site is a [Shiny](https://shiny.posit.co/) app that runs entirely in the
browser using [shinylive](https://posit-dev.github.io/r-shinylive/). The first
visit takes a little while to load because R is downloaded into the browser.

## Project layout

| Path | What it is |
|---|---|
| `app/app.R` | The Shiny app (interface and logic) |
| `app/smiskis.csv` | Every figure: collection, name, and whether it's a secret |
| `app/www/` | Styles (`styles.css`) and browser saving (`tracker.js`) |
| `R/update_smiskis.R` | Rebuilds `smiskis.csv` from the Google Sheet |
| `.github/workflows/deploy.yml` | Publishes the site to GitHub Pages on every push |

## Common tasks

**Run the app on your computer** (open `smiskitracker.Rproj` in RStudio first):

```r
shiny::runApp("app")
```

**Add new figures:** add them to the
[Google Sheet](https://docs.google.com/spreadsheets/d/1U4l4ugaL8UG7wzEfQqQ1h_oYYX0s0tGpqw9XV_cxnms/),
then run

```r
source("R/update_smiskis.R")
```

then commit and push. The site updates by itself a few minutes later.

**Preview the published version locally:**

```r
shinylive::export("app", "site")
httpuv::runStaticServer("site")
```

---

A fan-made project, not affiliated with Smiski or Dreams Inc.
