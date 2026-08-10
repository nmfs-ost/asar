---
applyTo: "**/*.qmd, **/*.scss, **/*.css, **/*.js, _quarto.yml"
---

# Accessibility Rules

## Semantic HTML
- Use semantic Quarto Markdown & HTML5 tags.
- Headings must be sequential (H1->H2->H3). One H1 per page.
- Use descriptive nav labels in `_quarto.yml`.
- Prefer native elements (`<button>`) over ARIA roles (`<div role="button">`).
- Use landmarks (`<nav>`, `<main>`), not just `<div>`s.

## Media & Data
- Images need alt text: `!description`.
- Plots need `fig-alt`: `#| fig-alt: "description"`.
- Don't use only color to convey info in charts. Use patterns or labels.
- Videos need `<track kind="captions">`. No `autoplay` unless muted.

## Keyboard & Focus
- JS interactives must be keyboard accessible.
- Don't hide focus outlines without a visible (2px) alternative.
- Tab order must be logical.
- Focus must not be obscured by sticky elements (`scroll-padding-top`).
- Modals must trap focus and return it on close.

## ARIA & Screen Readers
- Use descriptive link text (not "click here").
- Toggle `aria-expanded` on collapsibles.
- Announce dynamic content with `aria-live="polite"` (updates) or `assertive` (alerts).
- Never use `aria-hidden="true"` on a focusable element. Use `inert` or remove focusability.

## Color & Contrast
- Colors must meet contrast ratios: Text (4.5:1), Large Text/UI (3:1).
- Check contrast on all elements, including code themes.

## Audits & Links
- Pa11y CI (`axe`, `htmlcs`) runs on the rendered `_site/` directory.
- No duplicate `id` attributes.
- Links need accessible names. Icon-only links need `aria-label`.
- Pa11y ignores in `accessibility_audit.yml` must be narrow and only for generated-code false positives.

## Forms
- In raw HTML forms:
  - `<input>` requires `<label>`.
  - Group fields with `<fieldset>` and `<legend>`.
  - Show errors next to fields with `aria-describedby`.
  - Use ARIA roles as a last resort.