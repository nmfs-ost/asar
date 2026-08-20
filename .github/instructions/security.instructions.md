---
applyTo: "**/*.js, **/*.qmd, **/*.html, .github/workflows/*.yml"
---
# Security Rules

## JavaScript & DOM Manipulation (XSS Prevention)
- **No `innerHTML` or `insertAdjacentHTML`** with unsanitized data. Use `textContent`.
- **No `eval()`**, `setTimeout(string)`, or `setInterval(string)`.
- Sanitize data from `window.location` before use.
- Dynamic links must use secure protocols (`https`, `mailto:`), not `javascript:` URIs.

## Content Security Policy (CSP) & Headers
- Prioritize strict CSP rules in `<meta>` tags and head includes.
- Avoid inline scripts; use external `.js` files.
- Load third-party widgets via HTTPS.

## GitHub Actions & CI/CD Security
- Define minimum `permissions` in all `.yml` workflows (e.g., `contents: read`).
- Pin actions to a full commit SHA, not a tag.
- Use GitHub Secrets for tokens/keys; no hardcoded values.
- Don't pipe `curl` to `bash` without verification.

## External Data & Dependencies
- JS fetching external data must use HTTPS.
- No sensitive data or PII in committed files.
- JS libraries require version pinning and subresource integrity checks.

## Code Generation
- When generating code, comment on security choices (e.g., XSS prevention, workflow permissions).
