# Contributing

Development is a community effort, and we welcome participation.

## Issues

* Before posting a new issue or discussion topic, please take a moment to search for existing similar threads in order to avoid duplication.
* For bug reports: if you can, please install the latest GitHub version of `pipr` (i.e. `remotes::install_github("PIP-Technical-Team/pipr")`) and verify that the issue still persists.
* Describe your issue in prose as clearly and concisely as possible.
* For any problem you identify, post a [minimal reproducible example](https://www.tidyverse.org/help/) so the maintainer can troubleshoot. A reproducible example is:
    * **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Development

External code contributions are extremely helpful in the right circumstances. Here are the recommended steps.

1. Prior to contribution, please propose your idea in a discussion topic or issue thread so you and the maintainer can define the intent and scope of your work.
2. [Clone](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository) or [fork](https://help.github.com/articles/fork-a-repo/) the repository.
3. Follow the [GitHub flow](https://guides.github.com/introduction/flow/index.html) to create a new branch, add commits, and open a pull request.
4. Discuss your code with the maintainer in the pull request thread.
5. If everything looks good, the maintainer will merge your code into the project.

Please also follow these additional guidelines.

* Respect the architecture and reasoning of the package.
* If possible, keep contributions small enough to easily review manually. It is okay to split up your work into multiple pull requests.
* For new features or functionality, add tests in `tests`. Tests that can be automated should go in `tests/testthat/`.
* Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) and check your formatting with the `lint_package()` function from the [`lintr`](https://github.com/jimhester/lintr) package.
* Check code coverage with `covr::package_coverage()`. Automated tests should cover all the new or changed functionality in your pull request.
* Run overall package checks with `devtools::check()`.
