## R CMD check results

0 errors | 0 warnings | 1 note

* Updated testing code of elfdata() both in example and test code to allow graceful failing if internet resources create issues on data downloads. This fixed the issue noted in the last CRAN test.
* Added default data for examples to draw from to limit the need to rely on donttest{}. Maintained donttest{} in elfdata.R as the example can take over a minute to run.
