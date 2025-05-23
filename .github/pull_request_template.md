# Requester section

*To be completed by the pull-requester (assignee)*

## Description
<details><summary>Please include a summary of the changes</summary>
 
  - What is this change?
  - What does it fix?
  - Is this a bug fix or a feature and does it break any existing functionality?
  - How has it been tested?
</details>
 
*This pr introduces....*
 
## Type of change
- [ ] Bug fix - *non-breaking change*
- [ ] New feature - *non-breaking change*
- [ ] Breaking change - *backwards incompatible change, changes expected behaviour*
- [ ] Non-user facing change, structural change, dev functionality, docs ...
 
## Requestor checklist:

*If applicable, if not indicate why in requester comments section*

- [ ] I have performed a self-review of my own code
- [ ] I have commented my code appropriately, focusing on explaining my design decisions (explain why, not how)
- [ ] I have made corresponding changes to the documentation (comments, docstring, etc.) using roxygen (devtools check()/document() etc.)
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] I have updated the change log/version number [CHANGELOG.md](https://github.com/ONSdigital/dpmaccount/blob/main/CHANGELOG.md)
- [ ] I have checked the pipeline runs with test data

### Requester comments
 
*Any additional requester comments/notes/requirements*

<br>
 
# Reviewer section

*To be completed by the pull-request reviewer*

##  Peer review checklist:
Any new code (where appropriate) includes all of the following:

- [ ] **Commit messages**:
  - [ ] The individual commit messages are sufficiently descriptive, when combined with the **pull request description** above, forming a coherent audit trail. If not, add suggested improvements/changes to the reviewer notes below
  - [ ] The commit messages are in-line with the [contributing guidance](https://github.com/ONSdigital/dpmaccount/blob/main/CONTRIBUTING.md)
- [ ] **Data**: All [personally identifiable data](https://ico.org.uk/for-organisations/uk-gdpr-guidance-and-resources/personal-information-what-is-it/what-is-personal-information-a-guide/) has been removed/anonymised
- [ ] **Documentation**: Docstrings, comments have been added/updated
- [ ] **[Style guidelines](https://confluence.ons.gov.uk/display/PP/Style+guidelines)**: New code conforms to the project's contribution guidelines
- [ ] **[Functionality](https://confluence.ons.gov.uk/display/PP/Functionality)**: The code works fully implements the requirements and works as expected, handles expected edge cases, exceptions are handled appropriately
- [ ] **[Complexity](https://confluence.ons.gov.uk/display/PP/Complexity)**: The code is not overly complex, logic has been split into appropriately sized functions, etc.
- [ ] **[Test coverage](https://confluence.ons.gov.uk/display/PP/Test+coverage)**: Unit tests cover essential functions for a reasonable range of inputs and conditions. Added and existing tests pass on my machine.
- [ ] **Vignettes**: Any relevant vignettes have been modified/created.
- [ ] **Version numbering**: The versioning number has been incremented correctly and follows mandated semantic versioning protocol in [contributing guidance](https://github.com/ONSdigital/dpmaccount/blob/main/CONTRIBUTING.md).

*If any of the above are not applicable, include an explanation in the review comments below*

### Review comments
Suggestions should be tailored to the code that you are reviewing. Provide context.
Be critical and clear, but not mean. Ask questions and set actions.
<details><summary>These might include:</summary>
 
- bugs that need fixing (does it work as expected? and does it work with other code that it is likely to interact with?)
- alternative methods (could it be written more efficiently or with more clarity?)
- documentation improvements (does the documentation reflect how the code actually works?)
- additional tests that should be implemented
- Do the tests effectively assure that it works correctly? Are there additional edge cases/negative tests to be considered?
- Code style improvements (could the code be written more clearly?)
</details>
<br>
 
*Further reading: [code review best practices](https://best-practice-and-impact.github.io/qa-of-code-guidance/peer_review.html)*
