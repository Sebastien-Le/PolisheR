# PolisheR 1.1.1

## New Features
- Added support for `sample.pct` in `nail_condes()` and `shiny_nail_condes()` UI.
- Improved prompt cleaning with `clean_text()`.

## Bug Fixes
- Fixed bug in `get_sentences_condes()` where `Estimate` could be non-numeric.

---

# shiny_nail_qda 1.0.0

## New Features
- Introduced the `shiny_nail_qda()` function, which launches an interactive Shiny app for QDA analysis.

## Bug Fixes
- Fixed compatibility with dplyr for the `%>%` operator.
- Resolved missing namespace issue for `req()` and `all_of()`.

## Notes
- Initial release of the package with a fully functional Shiny app.
