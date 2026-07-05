# math

Personal TeX and Org notes published at <https://ultronozm.github.io/math/>.

This repository is a child of the reusable TeX notes site machinery at
<https://github.com/ultronozm/tex-site>, configured as the `upstream` remote
locally.  Content lives here; reusable publishing machinery lives upstream.

## Local Roles

- Content and site-specific files: `*.tex`, `*.org`, `common.tex`, `refs.bib`,
  `images/`, `config.json`, `index.org`, and local style choices.
- Upstream-owned machinery: `.github/workflows/build.yml`,
  `.github/workflows/make-index.yml`, `compile.sh`, `convert.sh`,
  `make-index.sh`, and `tex2html.el`.

To merge future machinery changes:

```sh
git fetch upstream
git merge upstream/main
```

## Deployment

The live site is served from the generated `deploy` branch.  The branch is a
snapshot artifact branch, not source history.

The `build` workflow carries forward existing generated artifacts, rebuilds
changed TeX notes and changed Org notes, emits a warning-only TeX link report,
and force-pushes a fresh snapshot.  The `make-index` workflow then refreshes
`listing.json` and `index.html`.

For safe trials, dispatch the `build` workflow manually with
`deploy_branch=deploy-test`, then dispatch `make-index` with the same
`deploy_branch`.  This exercises the snapshot branch without changing the
live Pages branch.
