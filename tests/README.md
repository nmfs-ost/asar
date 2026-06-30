# Tests

This folder contains tests for the functions in {asar}. The tests are located in the sub-folder "testthat". We follow the testing structure and approach found using {testthat} R package. Each function should have its own test. If a new function is made, you can create a new testing script by running the following in the console:

```
usethis::use_test()
```

The above function references whatever function is currently open. From there, use the options for testing in the testthat package, make your own testing function, or reference the current tests in this folder.