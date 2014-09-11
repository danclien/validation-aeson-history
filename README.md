# Aeson + Validation
Example of how to use `aeson` and `validation` with error history.

## Experimental
This is still in the experimental phase. There's a few other issues I would like to fix before releasing this into the wild.

## TODO

* Figure out a way to make the errors and env for the JSON parsing and non-parsing more separated.
  * env separation may be able to be done using a `Reader jsonEnv (Reader env (AccValidation err a))`
  * Error handling may be more difficult since there can only be one error type
    * What would happen if there was a `(AccValidation err1 (AccValidation err a))`?

