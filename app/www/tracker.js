// Smiski Tracker: saves each visitor's collection in their own browser
// (localStorage) and passes ticks and dates to the Shiny server.
$(function () {
  var KEY = "smiskitracker-v1";

  function load() {
    try {
      var raw = window.localStorage.getItem(KEY);
      var data = raw ? JSON.parse(raw) : {};
      return data && typeof data === "object" && !Array.isArray(data) ? data : {};
    } catch (e) {
      return {};
    }
  }

  function today() {
    var d = new Date();
    return d.getFullYear() + "-" +
      String(d.getMonth() + 1).padStart(2, "0") + "-" +
      String(d.getDate()).padStart(2, "0");
  }

  // Hand the saved collection to the server once the session is up. The
  // event can fire before or after this script runs, so cover both cases.
  var sent = false;
  function sendSaved() {
    if (sent || !window.Shiny || !Shiny.setInputValue) return;
    sent = true;
    Shiny.setInputValue("saved_state", { data: load() });
  }
  $(document).on("shiny:sessioninitialized shiny:connected", sendSaved);
  if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) sendSaved();

  Shiny.addCustomMessageHandler("smiski_save", function (msg) {
    var data = msg && msg.data && !Array.isArray(msg.data) ? msg.data : {};
    try {
      window.localStorage.setItem(KEY, JSON.stringify(data));
    } catch (e) {
      /* storage blocked (private window); the session still works */
    }
  });

  Shiny.addCustomMessageHandler("smiski_progress", function (m) {
    var card = document.querySelector('.smiski-card[data-col="' + m.key + '"]');
    if (!card) return;
    card.querySelector(".owned").textContent = m.owned;
    card.querySelector(".fill").style.width = (100 * m.owned / m.total) + "%";
    card.classList.toggle("complete", m.owned === m.total);
    if (m.celebrate) {
      card.classList.remove("pop");
      void card.offsetWidth; // restart the animation
      card.classList.add("pop");
    }
  });

  $(document).on("change", ".smiski-check", function () {
    var li = this.closest(".figure");
    var dateEl = li.querySelector(".smiski-date");
    var have = this.checked;
    li.classList.toggle("have", have);
    if (have && !dateEl.value) dateEl.value = today();
    if (!have) dateEl.value = "";
    Shiny.setInputValue(
      "toggle",
      { id: this.dataset.id, have: have, date: dateEl.value },
      { priority: "event" }
    );
  });

  // ---- Phone menu ----------------------------------------------------------
  // On small screens the sidebar slides in over the page. The ☰ button,
  // "Show figures", tapping the dimmed page, or Escape open and close it.
  var phone = window.matchMedia("(max-width: 575.98px)");
  function menuIsOpen() {
    return document.body.classList.contains("menu-open");
  }
  function setMenu(open) {
    var header = document.querySelector(".bslib-page-sidebar > .navbar, .bslib-page-title");
    if (header) {
      document.documentElement.style.setProperty(
        "--menu-top", Math.max(0, header.getBoundingClientRect().bottom) + "px"
      );
    }
    document.body.classList.toggle("menu-open", open);
    document.querySelectorAll(".menu-btn").forEach(function (btn) {
      btn.setAttribute("aria-expanded", open ? "true" : "false");
      btn.setAttribute("aria-label", open ? "Close menu" : "Menu");
      btn.classList.toggle("open", open);
    });
    var sb = document.querySelector(".bslib-sidebar-layout > .sidebar");
    if (sb && open) sb.scrollTop = 0;
  }
  $(document).on("click", ".menu-btn", function () {
    setMenu(!menuIsOpen());
  });
  $(document).on("click", ".menu-done", function () {
    setMenu(false);
  });
  // Tapping the dimmed page behind the menu closes it
  $(document).on("click", ".menu-backdrop", function () {
    setMenu(false);
  });
  $(document).on("keydown", function (e) {
    if (e.key === "Escape" && menuIsOpen()) setMenu(false);
  });
  // Turning a phone sideways into a wide screen shouldn't leave it stuck open
  phone.addEventListener("change", function (e) {
    if (!e.matches) setMenu(false);
  });
  $("body").append('<div class="menu-backdrop" aria-hidden="true"></div>');

  $(document).on("change", ".smiski-date", function () {
    Shiny.setInputValue(
      "date_change",
      { id: this.dataset.id, date: this.value },
      { priority: "event" }
    );
  });
});
