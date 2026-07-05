# math

Personal TeX notes published at <https://ultronozm.github.io/math/>.

This repository is a child of the reusable TeX notes site machinery in
`/Users/au710211/work/tex-site`, configured as the `upstream` remote locally.
Content lives here; reusable publishing machinery lives upstream.

## Local Roles

- Content and site-specific files: `*.tex`, `common.tex`, `refs.bib`,
  `images/`, `config.json`, `index.org`, and local style choices.
- Upstream-owned machinery: `.github/workflows/build.yml`,
  `.github/workflows/make-index.yml`, `compile.sh`, `convert.sh`,
  `make-index.sh`, and `tex2html.el`.

To merge future machinery changes:

```sh
git fetch upstream
git merge upstream/main
```

The live site is served from the generated `deploy` branch.  The branch is a
snapshot artifact branch, not source history.
