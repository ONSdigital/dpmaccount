# Contributing to dpmaccount

We appreciate your input and we want to make contributing to this project as easy and transparent as possible, whether it's:

-   Reporting a bug
-   Submitting a fix
-   Proposing new features
-   Becoming a maintainer
-   Discussing the current state of the code

To maintain consistency and to facilitate efficient, effective, and inclusive contributions please consider both the [CODE_OF_CONDUCT.md](https://github.com/ONSdigital/dpmaccount/blob/main/CODE_OF_CONDUCT.md) together with the following guidance.

## We develop with GitHub

We use GitHub to host code, to track issues and feature requests, as well as accept pull requests.

### Code changes

To maintain a coherent code audit trail contributors must adhere to the following guidance: 

#### Semantic versioning policy

When updating the [CHANGELOG.md](https://github.com/ONSdigital/dpmaccount/blob/main/CHANGELOG.md) with a new version number we prescribe the following semantic versioning policy:

Major version zero (0.y.z) is for initial development. Anything MAY change at any time.

Given a version number MAJOR.MINOR.PATCH, increment the:

- MAJOR version when you make backwards incompatible changes
- MINOR version when you add functionality in a backward compatible manner
- PATCH version when you make backward compatible bug fixes

Additional labels for pre-release and build metadata are available as extensions to the MAJOR.MINOR.PATCH format.

#### Code standards
- If any proposed code changes require testing, ensure unit tests are added.
- Make sure code is in the [tidyverse style](https://style.tidyverse.org/).
- Update the [CHANGELOG.md](https://github.com/ONSdigital/dpmaccount/blob/main/CHANGELOG.md) with the new version number (if applicable)
- If the changes don't require a version update make sure to add descriptions of the changes to the "Pre-release changes/updates" section. 


#### Branching

- Branch the repo and create your **`feature`** branch from **`main`**.
- Use the branch for a specific feature/bug, with an informative name (if possible including a reference to a GitHub issue number) 
- Only use the branch for the initial purpose it was created for (don't use for continuous/subsequent development)

#### Commit messages

- A concise summary of your change is usually sufficient, but remember that it is these commit messages that will be used in the future to understand what changes have been made to your project. You might rely on these to identify where an error has been introduced, so it is important that you write these messages clearly and informatively.

- We recommend contributors aim to commit discrete changes on a regular basis rather than bulking changes into single aggregate commits (e.g. changing a method, reformatting or adding functionality).

- Commit messages typically should be sufficiently descriptive such that other users can easily determine the content of the changes without delving into the specifics of the commit (for further guidance see the [duck book](https://best-practice-and-impact.github.io/qa-of-code-guidance/version_control.html#write-short-and-informative-commits) or [note on commit messages](https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)).


#### Pull requests

Pull requests are the only way to propose changes to the codebase:

1.  Create a pull request, with the pull_request_form filled in (ensure sufficient information/notes have been added to the pull request form, pull requests won't be accepted without a valid request form)
2.  Attach a reviewer to the pull request, pull requests must be reviewed by at-least 1 individual not-involved in the development of the modified/added code being requested
3.  Submit that pull request!
4.  The reviewer will attend to the request as soon as is reasonable and will either respond with a request for additional information/changes or following a successful review they will accept the request and delete the feature branch
5. If the pull request consists of multiple small commits contributing to a small (preferably single) change to the codebase then the commits will be squashed to maintain a cohesive branch history. In specific cases where the requester feels it prudent to keep the individual commits make this clear (as well as the reasoning) in the pull_request_form
6. If a new [version number](https://semver.org/) has been added to the [CHANGELOG.md](https://github.com/ONSdigital/dpmaccount/blob/main/CHANGELOG.md) file make sure to create a new [version release]((https://github.com/ONSdigital/dpmaccount/releases/new)) with the corresponding version tag

## Report bugs/request features using GitHub's [issues](https://github.com/ONSdigital/dpmaccount/issues)

We use GitHub issues to track public bugs/feature requests. Report a bug/feature request by [opening a new issue](https://github.com/ONSdigital/dpmaccount/issues/new/choose) and filling in the relevant attached template (individual templates for [feature request](https://github.com/ONSdigital/dpmaccount/issues/new?assignees=&labels=enhancement&projects=&template=feature_request_template.md)/[bug fix](https://github.com/ONSdigital/dpmaccount/issues/new?assignees=&labels=bug&projects=&template=bug_report_template.md) requests) with all applicable information; it's that easy!

### Complete the correct issue template when submitting issues:

#### Feature requests

Use the [feature request](https://github.com/ONSdigital/dpmaccount/issues/new?assignees=&labels=enhancement&projects=&template=feature_request_template.md) template as a starting point

**As a minimum you should include:**

-   A title
-   Description of the requested feature
-   Desired behaviour
-   Your definition-of-done (DoD) i.e. the changes/implementations required to consider this feature successfully added

There are also options to add additional information (if applicable):

-   Test data (example before/after data to demonstrate desired behaviour)
-   Other dpmaccount function/s affected by the requested function
-   Links to related/relevant information i.e. to similar functionality in other packages
-   dpmaccount package versions you'd like to requested functionality to work with (if other than the most recent)

#### Bug reports

Use the [bug fix](https://github.com/ONSdigital/dpmaccount/issues/new?assignees=&labels=bug&projects=&template=bug_report_template.md) template as a starting point

**As a minimum you should include:**

-   A title
-   Prerequisites checklist
-   A description of the issue
-   Steps to reproduce the bug
    -   Be specific, there's no such thing as too much detail!
    -   Give sample code if you can.
-   What you expected would happen
-   What actually happens
-   Your definition-of-done (DoD) i.e. the changes/implementations required to consider this bug successfully fixed

If applicable you can also add additional information:

-   Any logs, error outputs etc
-   dpmaccount package version/s affected (if the bug affects more than just the most recent release)
-   Environmental information/imported package versions

The more thorough the bug report the more likely it will be resolved swiftly and successfully!

## Use a consistent coding style

We use the [tidyverse style](https://style.tidyverse.org/). There are two R packages which aid consistent styling

-   styler (<https://styler.r-lib.org/>)
-   lintr (<https://github.com/r-lib/lintr>)

Installing 'styler' in RStudio allows for easy access to the styler addin to style sections of code or entire scripts

## Any contributions you make will be under the MIT Software License

In short, when you submit code changes, your submissions are understood to be under the same [MIT License](http://choosealicense.com/licenses/mit/) that covers the project. Feel free to contact the maintainers if that's a concern. For additional information regarding the licensing, and related copyright, of this code please refer to the [LICENSE](https://github.com/ONSdigital/dpmaccount/blob/main/LICENSE.md)
