# Revision history for OllamaHoles

## 0.1.6.0 -- 2024-05-02

* Feature: Add model-options parameter

## 0.1.5.3 -- 2024-05-02

* CI: Add GitHub Actions workflow for building and testing
* CI: Build for all supported GHC versions, only run tests on GHC 9.12
* Improvement: Update prompt and default model
* Update documentation
* Dependency: Remove unused exceptions dependency

## 0.1.5.2 -- 2024-05-01

* Bugfix: rework hole-fit validation, zonk the type and pass it on.

## 0.1.5.1 -- 2024-05-01

* Bugfix: capture suggestion constraints and make sure they're respected
* Bugfix: Don't display errors when validating suggestions

## 0.1.5.0 -- 2024-05-01

* Improve the list of things in scope
* Let user add guidance to the LLM
* Include docs of things in scope with the include-docs flag

## 0.1.4.0 -- 2024-04-30

* Support GHC 9.8, 9.10 and 9.12

## 0.1.3.0 -- 2024-04-30

* More robust handling of fits that cause errors during validation

## 0.1.2.0 -- 2024-04-30

* Add validation of valid-hole fits returned by the LLM

## 0.1.1.0 -- 2024-04-29

* Support calling remote LLMs

## 0.1.0.0 -- 2024-04-25

* First version. Released on an unsuspecting world.
