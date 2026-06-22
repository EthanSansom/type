# typed() prints as expected

    Code
      with_empty_env(typed(function() { }))
    Output
      <typed>
      function () 
      {
      }
      <environment: R_EmptyEnv>
      Returns:
      * `<result>` is an R object.

---

    Code
      with_empty_env(typed(function() { }, returns = t_lgl))
    Output
      <typed>
      function () 
      {
      }
      <environment: R_EmptyEnv>
      Returns:
      * `<result>` is a bare <logical>.

---

    Code
      with_empty_env(typed(function(x) {
        x
      }))
    Output
      <typed>
      function (x) 
      {
          x
      }
      <environment: R_EmptyEnv>
      Arguments:
      * `x` is an R object.
      Returns:
      * `<result>` is an R object.

---

    Code
      with_empty_env(typed(function(x = t_int) {
        x
      }))
    Output
      <typed>
      function (x) 
      {
          x
      }
      <environment: R_EmptyEnv>
      Arguments:
      * `x` is a bare <integer>.
      Returns:
      * `<result>` is an R object.

---

    Code
      with_empty_env(typed(function(x = t_bool, y = t_lgl) {
        x
      }))
    Output
      <typed>
      function (x, y) 
      {
          x
      }
      <environment: R_EmptyEnv>
      Arguments:
      * `x` is a bare <logical>.
      * `x` is size 1.
      * `y` is a bare <logical>.
      Returns:
      * `<result>` is an R object.

---

    Code
      with_empty_env(typed(same_sized(x, y), function(x = t_bool, y = t_lgl) {
        x
      }))
    Output
      <typed>
      function (x, y) 
      {
          x
      }
      <environment: R_EmptyEnv>
      Arguments:
      * `x` is a bare <logical>.
      * `x` is size 1.
      * `y` is a bare <logical>.
      Relations:
      * `x` and `y` must be the same size.
      Returns:
      * `<result>` is an R object.

---

    Code
      with_empty_env(typed(same_sized(x, y), function(x = t_bool, y = t_lgl) {
        x
      }, returns = t_chr))
    Output
      <typed>
      function (x, y) 
      {
          x
      }
      <environment: R_EmptyEnv>
      Arguments:
      * `x` is a bare <logical>.
      * `x` is size 1.
      * `y` is a bare <logical>.
      Relations:
      * `x` and `y` must be the same size.
      Returns:
      * `<result>` is a bare <character>.

