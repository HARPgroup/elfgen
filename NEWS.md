# elfgen 2.3.6
* Moved to use `hydrogeofetch` instead of `nhdPlusTools` after https://github.com/HARPgroup/elfgen/pull/82

# elfgen 2.3.5
* Added example data to make examples easier to run

# elfgen 2.3.4

* A file check was added to `elfdata()` to allow users to "cache" results when developing workflows that may otherwise require multiple reruns of identical `elfdata()`
* `plot_title` and `break_var` were added as explicit arguments to `elfgen()` to allow users to better customize their analysis
* Documentation updated accordingly, including examples now set with donttest to ensure proper documentation
