# ohwm2 — Active Work Plan

_Last updated: 2026-05-31_

## Current milestone: Stabilize ohwm2 for production

### Completed this session
- [x] Diagnosed root cause of #1 reported bug: first-run Results page hang
  - Cause: `updateSliderInput` firing slider observers before `results_loaded` guard was set, triggering full recomputation with no spinner
  - Fix: `freezeReactiveValue()` before each `updateSliderInput`; `isolate()` on slider input reads in initial computation; move `remove_modal_spinner()` to after `results_loaded(TRUE)`
  - Location: `R/app_server.R` lines ~213–329

### Pending — immediate (stability)
- [ ] Apply and verify Option 3 fix in `app_server.R` (see session notes)
- [ ] Confirm first-run hang is resolved on Posit Connect Cloud deployment
- [ ] Confirm slider observers still fire correctly on user interaction after fix

### Pending — near-term (robustness / Option 4 refactor)
Full server architecture refactor. The current server has several structural problems
identified during the 2026-05-31 session that make it fragile and hard to maintain:

- [ ] Replace imperative `observeEvent(input$view_results)` block with
      `eventReactive(input$view_results, {...})` returning a named results list
- [ ] Eliminate all `<<-` global assignment; use proper reactive values
- [ ] Restructure slider and Manning's n observers as top-level observers
      (not nested inside `view_results`); add `ignoreInit = TRUE`
- [ ] Add `validate()`/`need()` guards before all expensive computation steps
- [ ] Restore `show_modal_spinner` / `remove_modal_spinner` to slider observers
      (present in `ohwm`, removed in `ohwm2`)
- [ ] Evaluate restoring console log footer (present in `ohwm`, removed in `ohwm2`)
- [ ] Evaluate `noUiSliderInput` vs `sliderInput` — was the switch intentional?

### Pending — backport to `ohwm` template
After `ohwm2` is stable:
- [ ] Apply all `ohwm2` improvements to `ohwm`
- [ ] Add full `dev/` governance structure to `ohwm`
- [ ] Update `ohwm` `fluvgeo` pin to `>= 2025.5.3` and `@*release` remote
- [ ] Confirm `ohwm` is a clean starting point for new app development

### Pending — `tieredassessment` replacement
- [ ] Build replacement app from stabilized `ohwm` template
- [ ] Archive / deprecate `tieredassessment`

### Deferred
- VPN/SSL cert fix for USACE development machines (`CURL_CA_BUNDLE` → DoD CA chain)
  — workaround: develop off VPN
- Option C CI drift detection workflow — revisit after backport complete
- Option B + D shared package / parameterized app — medium/long-term architecture goal

## Known environment issues
- New computer setup requires `ARCGIS_CLIENTID`, `ARCGIS_CLIENTSECRET`, `ARCGIS_HOST`
  in `~/.Renviron` (user-level, never committed)
- USACE VPN SSL inspection breaks `download.file` to `elevation.arcgis.com` —
  disconnect from VPN to develop locally
  