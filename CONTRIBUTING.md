# Contributing to `asar`

Do you have an idea that would improve `asar`? *Consider making a contribution!* We welcome ideas for improving not only our code, but also our documentation, tutorial, and any other material associated with `asar`. Here are some options and tips for doing so.

Note: To make any contribution, you must agree to abide by the [Code of Conduct](https://github.com/nmfs-ost/asar/blob/main/CODE_OF_CONDUCT.md).

## Contributing ideas: code, documentation, etc.

### Recommended workflow: fork & submit a pull request

The most efficient way to contribute an idea is to fork `asar`, make your 
suggested changes on a local branch, and then submit a pull request to the main branch. 
This will allow the developers to easily evaluate your suggested changes. 
Please see the [GitHub Docs' "Contributing to a project" page](https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project) 
for step-by-step guidance in using this workflow.

We also follow a few other practices that will help us expedite the review process.
After completing the bug fix or feature, please complete the following:

1. Run `devtools::test()` to verify the package checks are passing (this includes tests).

2. If including a feature, please add a test using the 
[`{testthat}` package structure](https://testthat.r-lib.org/reference/test_that.html).
If you are unfamiliar with this approach, simply let us know :smile

3. If including a feature that is a new function, please add documentation using
the [`{roxygen2}` package structure](https://roxygen2.r-lib.org/articles/roxygen2.html).

4. Please increment the package version following our semantic versioning found in @sec-version.

5. Add a short summary or title line in the NEWS.md file indicating the feature 
or fix. This will be how we include recognition for your contribution in our 
monthly release notes.

### Recommended practices

-   Write clear, succinct commit messages ([see some tips here](https://opensource.com/article/22/12/git-commit-message))
-   Limit a commit to a few, rather than many, changes. Smaller commits means more commit messages, which is often helpful for documentation.
-   Ensure your base branch is correct. We typically merge most PRs into "dev", then merge "dev" into "main" upon a new release.
-   Test that your contributed code will function as expected under different circumstances.
-   Add comments to the code if it's not immediately clear what the purpose of the code is, or how it works.

### Semantic Versioning {#sec-version}

We use an approach to semantic versioning for our packages. This includes using 
a typical structure for semantic versioning which follows:

v<MAJOR>.<MINOR>.<PATCH>

where,

- MAJOR = major release (not backwards compatible)
- MINOR = minor release (new features)
- PATCH = hot fixes and bug fixes (aka patches)

A branch in v<MAJOR>.<MINOR>.<PATCH>.9000 indicates development in main branch.

Please use the following "release indicators" for a naming a branch or pull request:

| Indicator | Description |
|-----------|-------------|
| feat | A new feature |
| fix | A bug fix |
| docs | Documentation only changes |
| style | Changes that do not affect the meaning of the code (i.e. white-space, formatting, missing semi-colons) |
| refactor | A code change that neither fixes a bug nor adds a feature |
| perf | A code change that improves performance |
| test | Adding missing or correcting existing tests |
| chore | changes to the build process or auxiliary tools and libraries such as documentation generation |

#### Rules for Indicator Use

- If the body contains the text "BREAKING CHANGE" then MAJOR version is incremented.
- If the type contains feat, then MINOR version is incremented.
- If the type contains a fix, then PATCH version is incremented.
- If the type contains refactor/style/perf/doc/test/chore, then nothing is incremented and no release will be made.


The above content is modeled after an article on ["Automating Versioning and Releases Using Semantic Release"](https://medium.com/agoda-engineering/automating-versioning-and-releases-using-semantic-release-6ed355ede742)
from Agoda Engineering.

## Contributing bugs

Found a bug? Tell us about it on our [Issues page](https://github.com/nmfs-ost/asar/issues). Before you create an issue, please check that it has not already been resolved (i.e., is a "closed" issue) or documented in our [Frequently Asked Questions (FAQ) vignette](https://nmfs-ost.github.io/asar/articles/faqs.html).

If possible, please submit a reproducible example ([reprex](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html)) to help us understand the problem better and, ideally, allow us to reproduce your issue.

## Contributing questions

Have a question? Ask it in our [Discussions page](https://github.com/nmfs-ost/asar/discussions). You can categorize it under General, Ideas, Q&A, and more.
