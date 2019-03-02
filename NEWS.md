# safejoin 0.1.0

Functions :

* Safe versions of all *dplyr* and *fuzzyjoin* join functions
* `eat` function as an improved join to grow data frames

Different types of checks to be use in `check` argument through the following
codes :

* `"c"` to check *c*onflicts of *c*olumns
* `"b"` like *"by"* checks if `by` parameter was given explicitly
* `"u"` like *unique* to check that the join columns form an unique key on `x`
* `"v"` to check that the join columns form an unique key on `y`
* `"m"` like *match* to check that all rows of `x` have a _match_
* `"n"` to check that all rows of `y` have a _match_
* `"e"` like *expand* to check that all combinations of joining columns are 
  present in `x`
* `"f"`  to check that all combinations of joining columns are present in `y`
* `"l"`  like *levels* to check that join columns are consistent in term of 
  factor levels
* `"t"`  like *type* to check that joining columns have same class and type

Solve conflict between columns by applying a function on pairs of conflicted
columns through the `conflict` argument : 

* regular functions like `coalesce` or `tibble`(to nest)
* the special value `"patch"` to keep values from *rhs* only when it matches

Features of `eat`

* It uses the `...` argument to select columns from `.y` and leverages the select helpers from *dplyr*, allowing also things like renaming, negative selection, quasi-quotation...
* It can prefix new columns or rename them in a flexible way
* It can aggregate the *rhs* on the join columns through the argument `.agg`
  before joining
* It can join recursively to a list of tables
* It can `.fill` `NA` values from the *rhs* with a given value

Fuzzy joins special features: 

* `match_fun` supports formula notation
* A simpler syntax renders obsolete the pair of `multi_by` and `multi_match_fun`
 from the *fuzzyjoin* package, the `by` argument should use a formula such as
 `~ X("var1") > Y("var2") & X("var3") < Y("var4")`, it can either return a
 logical vector or a data.frame containing a logical first column and other
 columns that will be added to the result
 

