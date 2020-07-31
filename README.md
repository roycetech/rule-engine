# rule-engine

A simple rule engine that can evaluate text expressions using the basic logic
 operators 'and', 'or', 'not', and parenthesis.

[![Build Status](https://travis-ci.com/roycetech/rule-engine.svg?branch=master)](https://travis-ci.com/roycetech/rule-engine)
[![Test Coverage](https://api.codeclimate.com/v1/badges/4dd9edeb10d81fee093d/test_coverage)](https://codeclimate.com/github/roycetech/rule-engine/test_coverage)
[![Maintainability](https://api.codeclimate.com/v1/badges/4dd9edeb10d81fee093d/maintainability)](https://codeclimate.com/github/roycetech/rule-engine/maintainability)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.roycetech.rule-engine/rule-engine.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.github.roycetech.rule-engine%22%20AND%20a:%22rule-engine%22)

### To build

`make test`

### Testing

Preference was put on the LogicChecker example, available in another language.


### Release process

- Develop, develop, develop
- Commit any outstanding changes
- Verify build passes with `make test`
- Get the current version number with `make version-get` in preparation for
 updating.
- Update versions to release version with `make version-set VERSION=1.0.0`
- Verify the new version with `git diff`
- Delete the version backup with `make version-commit`
- Commit release version
- Run deployment with: `make release`

### Post Release process

- Update versions to next snapshot version: `make version-set VERSION=1.0.1-SNAPSHOT`
- Delete the version backup with `make version-commit`
- Commit new snapshot version
- Develop, develop, develop and rinse and repeat


### References

- [Working with Apache Maven](https://central.sonatype.org/pages/apache-maven.html)
- [Shunting Yard Algorithm Wikipedia](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
