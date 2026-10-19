# Slide deck / CSS refactor — Handoff

Last updated: 2026-10-19

## Context

Working on slide decks for a USF talk ([usf.qmd](usf.qmd), early Nov) plus shared
presentation styling. Decks are Quarto + reveal.js, published to Posit Connect Cloud
via [publish.sh](publish.sh) (copies `_quarto_uw.yml` → `_quarto.yml`, publishes, cleans up).

Three deck families are in scope:

| Deck(s) | Location | Theme CSS |
|---|---|---|
| usf.qmd | this project root | presentation.css |
| power.qmd | `archive/2026/power/` (uses root config) | presentation.css |
| 8 unit decks + book | `~/github/book_git/` | slides_teaching.css / slides_wide_teaching.css / book.css |

Note: each `book_git/*.qmd` is published **both** as a book chapter (book.css) and as a
slide deck (slides_teaching.css), so its markup must work under both.

## CSS architecture (current state)

We split **theme** (automatic look per context) from **utilities** (opt-in classes used
in `.qmd`). Utilities live in a single shared file duplicated identically in both repos;
each deck family keeps its own distinct theme file.

```
slide_decks/
  presentation.css        theme only (research-talk look: dark title bg, left align, black bold headers)
  utilities.css           SHARED utilities  <-- keep identical to book_git/utilities.css
book_git/
  slides_teaching.css     theme only (teaching look: logo, right align, red headers, 20px)
  slides_wide_teaching.css  theme only (same, 25px, full width)
  book.css                theme only (HTML book: red headers, honeydew code)
  utilities.css           SHARED utilities  <-- keep identical to slide_decks/utilities.css
```

Every Quarto config now loads `[theme.css, utilities.css]`:
- `slide_decks/_quarto_uw.yml` → `[presentation.css, utilities.css]`
- `book_git/_quarto_slides.yml` → `[slides_teaching.css, utilities.css]`
- `book_git/_quarto_slides_wide.yml` → `[slides_wide_teaching.css, utilities.css]`
- `book_git/_quarto_book.yml` → `[book.css, utilities.css]`

### utilities.css contents (the shared file)

- **Colors:** `.uwred` (#c5050c), `.red`, `.green` (= `forestgreen`), `.blue`, `.teal`, `.gray`
- **Spacers:** `.gap-xs` / `.gap-sm` / `.gap-md` / `.gap-lg` (use `<div class="gap-sm"></div>`)
- **Table highlight:** `.hl` (fill) — `<tr class="hl">` for a row, `class="hl"` on cells for a column
- **Callouts:** `.callout-corner` (small fixed bottom-left box) and `.callout-wide` (force full-width).
  Selectors are wrappers: `.callout-corner .callout`, `.callout-wide .callout`.

Selectors use plain `table ...` (no `.reveal` prefix) so they work in both slides and the book.

## What we changed this round

1. **Cleaned presentation.css** — removed an empty rule and a dead commented background block;
   organized into THEME + UTILITY sections.
2. **Created shared utilities.css** (identical in both repos) and pointed all four configs at it.
3. **Stripped the four theme files** down to theme-only (utilities removed; no drift).
4. **Removed unused classes** entirely: `list-gray`, `smaller`, `medium`, `centered`,
   `hlbox`, `rect_outline`, `circ_outline`.
5. **Inverted the callout default.** Previously presentation.css made *every* callout an
   absolute corner box; now callouts render normally and you opt in with `.callout-corner`.
   - usf.qmd: its 4 "Rules of thumb for auROC" callouts were updated to keep the corner look
     (standalone one wrapped in `::: {.callout-corner}`; three in-column ones got
     `.callout-corner` added to the `{.column width="40%"}` fence).
   - power.qmd and book_git decks needed no callout edits (they already wrap in `.callout-wide`).
6. **Converted inline color styles to classes** in all active decks (usf, power, book_git 02 & 03):
   Pandoc `[x]{style="color:..."}` → `[x]{.class}`; raw `<span style="color:...">` →
   `<span class="...">`; power.qmd h4 headers kept `margin-top: 3rem` inline but moved color to `.uwred`.
   No inline `color` styles remain in any active deck.

## Important notes / gotchas

- **Two utilities.css copies must be kept in sync by hand** (separate git repos, no single
  source of truth). If you edit one, copy it to the other. A future option: git submodule or a
  tiny CSS package.
- **`.green` renders `forestgreen`,** not CSS `green` — chosen to preserve existing rendering.
  If you want true green + a separate `.forestgreen`, that's a quick change.
- **Archived 2024 decks** (`archive/2024/1111_etg.qmd`, `archive/2024/1025_bov.qmd`) reference
  `presentation.css` directly. They're out of scope and untouched, but if republished they'd no
  longer get utility classes unless `utilities.css` is added to them.

## Suggested next steps

- [ ] Render usf.qmd to confirm theme, colors, and the `.callout-corner` boxes look right.
- [ ] Render a book_git slide deck AND the book to confirm shared utilities render consistently.
- [ ] Continue substantive content work on usf.qmd for the USF talk.
- [ ] (Optional) Decide on the `.green` vs `.forestgreen` naming.
