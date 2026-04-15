# =============================================================================
# hr_graphics_pipeline.R
#
# Pipeline: early_scan CSV → computed gainers/losers →
#           self-contained HTML (React + Babel CDN) → Playwright → PNG
#
# Finds the most recent early_scan_*.csv in output/ automatically.
#
# R deps:   readr, dplyr, purrr, glue, processx
# Node deps: playwright
#   Install: npm install playwright
#            npx playwright install --with-deps chromium
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(glue)
  library(processx)
})

# ── Config ────────────────────────────────────────────────────────────────────

OUTPUT_DIR <- "output"   # where HTML + PNGs are written
N_PLAYERS  <- 8          # players per graphic
WAIT_MS    <- 2500       # ms to wait for fonts/animations before screenshot

# ── 1. Find most recent scan CSV ──────────────────────────────────────────────

csv_files <- list.files(OUTPUT_DIR, pattern = "^early_scan_.*\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No early_scan_*.csv files found in ", OUTPUT_DIR, "/")
}

# Sort by modification time descending; take the newest
INPUT_CSV <- csv_files[order(file.mtime(csv_files), decreasing = TRUE)][1]
cat(sprintf("✓ Using CSV: %s\n", INPUT_CSV))

# ── 2. Read & compute ─────────────────────────────────────────────────────────

df <- read_csv(INPUT_CSV, show_col_types = FALSE) |>
  mutate(translated_dhr = predicted_delta_hr_bbe * bbe_y1)

gainers <- df |>
  filter(predicted_delta_hr_bbe > 0) |>
  arrange(desc(translated_dhr)) |>
  slice_head(n = N_PLAYERS) |>
  transmute(
    name = batter_name,
    rate = round(predicted_delta_hr_bbe, 5),
    dhr  = round(translated_dhr, 2)
  )

losers <- df |>
  filter(predicted_delta_hr_bbe < 0) |>
  arrange(translated_dhr) |>
  slice_head(n = N_PLAYERS) |>
  transmute(
    name = batter_name,
    rate = round(predicted_delta_hr_bbe, 5),
    dhr  = round(translated_dhr, 2)
  )

cat(sprintf("✓ Computed %d gainers, %d losers\n", nrow(gainers), nrow(losers)))

# ── 3. Parse date range from filename ─────────────────────────────────────────
# Expects a filename like: early_scan_YYYY-MM-DD_YYYY-MM-DD.csv

date_match <- regmatches(
  basename(INPUT_CSV),
  regexpr("(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})", basename(INPUT_CSV))
)
if (length(date_match) == 1) {
  parts      <- strsplit(date_match, "_")[[1]]
  scan_from  <- format(as.Date(parts[1]), "%b %d")
  scan_to    <- format(as.Date(parts[2]), "%b %d, %Y")
  scan_label <- paste(scan_from, "\u2013", scan_to)   # en-dash
} else {
  scan_label <- "Early Season Scan"
}

cat(sprintf("✓ Scan label: %s\n", scan_label))

# ── 4. Serialise R data frames → JS array literals ───────────────────────────
# Uses sprintf to avoid glue delimiter clashes with JS braces.

df_to_js <- function(tbl) {
  rows <- map_chr(seq_len(nrow(tbl)), \(i)
    sprintf('  { name: "%s", rate: %s, dhr: %s }',
            tbl$name[i], tbl$rate[i], tbl$dhr[i])
  )
  paste0("[\n", paste(rows, collapse = ",\n"), "\n]")
}

gainers_js <- df_to_js(gainers)
losers_js  <- df_to_js(losers)

# ── 5. Build self-contained HTML ──────────────────────────────────────────────
# Uses React 18 + Babel Standalone via CDN: no build step required.
# glue delimiters changed to << >> to avoid escaping JS curly braces.

html <- glue(
  .open  = "<<",
  .close = ">>",
'<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Karla:wght@400;500;600;700&family=DM+Mono:wght@400;500&display=swap" rel="stylesheet">
<script src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
<script src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>
<script src="https://unpkg.com/@babel/standalone/babel.min.js"></script>
<style>
  * { box-sizing: border-box; margin: 0; padding: 0; }
  html, body { background: #080808; width: 600px; }
</style>
</head>
<body>
<div id="root"></div>

<script type="text/babel">
const { useState } = React;

const gainersData = <<gainers_js>>;
const losersData  = <<losers_js>>;
const SCAN_LABEL  = "<<scan_label>>";

const GAINER_COLOR = "#3dffa0";
const LOSER_COLOR  = "#ff5c78";

function fmt(n, d) {
  const sign = n >= 0 ? "+" : "\u2212";
  return sign + Math.abs(n).toFixed(d);
}

function HRRow({ row, i, accent, maxDhr, mode }) {
  const barPct = (Math.abs(row.dhr) / maxDhr) * 100;
  return (
    <div className="hr-row" style={{ animationDelay: `${i * 50}ms` }}>
      <div className="rank">{i + 1}</div>
      <div className="player-name">{row.name}</div>
      <div className="rate-col" style={{ color: accent }}>{fmt(row.rate, 3)}</div>
      <div className="bar-col">
        <div className="bar-track">
          <div className="bar-fill" style={{ width: `${barPct}%`, background: accent }} />
        </div>
        <div className="dhr-num">{fmt(row.dhr, 1)}</div>
      </div>
    </div>
  );
}

function App() {
  const [mode, setMode] = useState("gainers");
  const isGainers = mode === "gainers";
  const data   = isGainers ? gainersData : losersData;
  const accent = isGainers ? GAINER_COLOR : LOSER_COLOR;
  const maxDhr = Math.max(...data.map(d => Math.abs(d.dhr)));

  return (
    <div className="app">
      <div className="toggle-wrap">
        {[["gainers", "\u25b2 Gainers", GAINER_COLOR],
          ["losers",  "\u25bc Losers",  LOSER_COLOR]].map(([v, label, c]) => (
          <button key={v} className="toggle-btn"
            onClick={() => setMode(v)}
            style={{ background: mode === v ? c : "#161616",
                     color:      mode === v ? "#000" : "#555" }}>
            {label}
          </button>
        ))}
      </div>

      <div className="card" id="hr-card">
        <div className="card-header" style={{ borderBottom: `3px solid ${accent}` }}>
          <div className="header-inner">
            <div>
              <div className="card-title">{isGainers ? "HR Gainers" : "HR Losers"}</div>
              <div className="card-sub">Early Season Scan \u00b7 {SCAN_LABEL}</div>
            </div>
            <div className="card-icon" style={{ color: accent }}>
              {isGainers ? "\u2191" : "\u2193"}
            </div>
          </div>
        </div>

        <div className="col-headers">
          <div className="col-label" />
          <div className="col-label">Player</div>
          <div className="col-label right">\u0394 HR/BBE</div>
          <div className="col-label bar-head">Proj \u0394HR</div>
        </div>

        {data.map((row, i) => (
          <HRRow key={`${mode}-${i}`}
            row={row} i={i} accent={accent} maxDhr={maxDhr} mode={mode} />
        ))}

        <div className="card-footer">
          <div className="footer-note">
            \u0394 HR/BBE \u2014 model-predicted rate change vs prior year
            \u00a0\u00b7\u00a0
            Proj \u0394HR \u2014 rate \u00d7 prior-yr BBE volume
          </div>
        </div>
      </div>
    </div>
  );
}

ReactDOM.createRoot(document.getElementById("root")).render(<App />);
</script>

<style>
  .app          { display:flex; flex-direction:column; align-items:center; padding:30px 20px; }
  .toggle-wrap  { display:flex; gap:6px; margin-bottom:22px; }
  .toggle-btn   { padding:7px 20px; border:none; cursor:pointer; font-family:"Karla",sans-serif;
                  font-size:11px; font-weight:700; letter-spacing:1.8px;
                  text-transform:uppercase; border-radius:2px; }
  .card         { width:540px; background:#0d0d0d; border:1px solid #1e1e1e; overflow:hidden; }
  .card-header  { padding:22px 28px 18px; }
  .header-inner { display:flex; justify-content:space-between; align-items:flex-end; }
  .card-title   { font-family:"Bebas Neue",cursive; font-size:34px; color:#f2f2f2;
                  letter-spacing:2.5px; line-height:1; margin-bottom:5px; }
  .card-sub     { font-family:"Karla",sans-serif; font-size:10px; font-weight:600;
                  color:#444; letter-spacing:1.8px; text-transform:uppercase; }
  .card-icon    { font-family:"Bebas Neue",cursive; font-size:56px; line-height:1; opacity:0.12; }
  .col-headers  { display:grid; grid-template-columns:22px 1fr 80px 180px;
                  padding:7px 28px; background:#0a0a0a;
                  border-top:1px solid #1a1a1a; border-bottom:1px solid #1a1a1a; }
  .col-label    { font-family:"Karla",sans-serif; font-size:9px; font-weight:700;
                  letter-spacing:1.5px; color:#363636; text-transform:uppercase; }
  .col-label.right    { text-align:right; }
  .col-label.bar-head { padding-left:14px; }
  @keyframes fadeSlide {
    from { opacity:0; transform:translateX(-6px); }
    to   { opacity:1; transform:translateX(0); }
  }
  .hr-row       { display:grid; grid-template-columns:22px 1fr 80px 180px;
                  padding:11px 28px; border-bottom:1px solid #141414;
                  align-items:center; animation:fadeSlide 0.35s ease both; }
  .hr-row:last-child { border-bottom:none; }
  .rank         { font-family:"DM Mono",monospace; font-size:10px; color:#2e2e2e; font-weight:500; }
  .player-name  { font-family:"Karla",sans-serif; font-size:15px; font-weight:500; color:#e6e6e6; }
  .rate-col     { font-family:"DM Mono",monospace; font-size:12.5px; font-weight:500;
                  text-align:right; letter-spacing:-0.3px; }
  .bar-col      { display:flex; align-items:center; gap:10px; padding-left:14px; }
  .bar-track    { flex:1; height:5px; background:#1c1c1c; border-radius:1px; overflow:hidden; }
  .bar-fill     { height:100%; border-radius:1px; opacity:0.9; }
  .dhr-num      { font-family:"DM Mono",monospace; font-size:14px; font-weight:500;
                  color:#f0f0f0; min-width:44px; text-align:right; letter-spacing:-0.5px; }
  .card-footer  { padding:9px 28px 13px; border-top:1px solid #161616; background:#0a0a0a; }
  .footer-note  { font-family:"Karla",sans-serif; font-size:9px; color:#2e2e2e;
                  letter-spacing:0.3px; line-height:1.6; }
</style>
</body>
</html>')

html_path <- normalizePath(
  file.path(OUTPUT_DIR, "hr_model_graphic.html"),
  mustWork = FALSE
)
writeLines(html, html_path)
cat(sprintf("✓ HTML written: %s\n", html_path))

# ── 6. Write Playwright script ────────────────────────────────────────────────

gainers_png <- normalizePath(file.path(OUTPUT_DIR, "hr_gainers.png"), mustWork = FALSE)
losers_png  <- normalizePath(file.path(OUTPUT_DIR, "hr_losers.png"),  mustWork = FALSE)

# Forward slashes required in Playwright file:// URIs on all platforms
html_uri <- paste0("file:///", gsub("\\\\", "/", html_path))

pw_script <- sprintf(
'const { chromium } = require("playwright");

(async () => {
  const browser = await chromium.launch();
  const page    = await browser.newPage();

  await page.setViewportSize({ width: 600, height: 900 });
  await page.goto("%s");

  // Wait for Google Fonts + row animations to settle
  await page.waitForTimeout(%d);

  const card = page.locator("#hr-card");

  // --- Gainers (default view) ---
  await card.screenshot({ path: "%s" });
  console.log("Saved: %s");

  // --- Switch to Losers ---
  await page.locator("button", { hasText: "Losers" }).click();
  await page.waitForTimeout(600);
  await card.screenshot({ path: "%s" });
  console.log("Saved: %s");

  await browser.close();
})();
',
  html_uri,
  WAIT_MS,
  gsub("\\\\", "/", gainers_png), gainers_png,
  gsub("\\\\", "/", losers_png),  losers_png
)

pw_path <- file.path(OUTPUT_DIR, "screenshot.js")
writeLines(pw_script, pw_path)
cat(sprintf("✓ Playwright script written: %s\n", pw_path))

# ── 7. Run Playwright ─────────────────────────────────────────────────────────

cat("Running Playwright...\n")

result <- processx::run(
  command         = "node",
  args            = pw_path,
  error_on_status = FALSE,
  echo            = TRUE
)

if (result$status == 0) {
  cat("\n✓ Pipeline complete.\n")
  cat(sprintf("  Gainers PNG : %s\n", gainers_png))
  cat(sprintf("  Losers PNG  : %s\n", losers_png))
} else {
  cat("\n✗ Playwright step failed (exit code", result$status, ")\n")
  cat("  Make sure you have run:\n")
  cat("    npm install playwright\n")
  cat("    npx playwright install --with-deps chromium\n")
}
