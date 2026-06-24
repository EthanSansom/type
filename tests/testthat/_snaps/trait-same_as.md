# same_as() description and diagnosis are as expected

    Code
      obj_inspect_type(c("a", "b", "c"), t)
    Output
      Object `c("a", "b", "c")` has the expected type.
      v `c("a", "b", "c")` is a vctrs style vector.
      v `c("a", "b", "c")` is the same as: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(c("c", "b", "a"), t)
    Output
      Object `c("c", "b", "a")` does not have the expected type.
      v `c("c", "b", "a")` is a vctrs style vector.
      i `c("c", "b", "a")` must be: `c("a", "b", "c")`.
      x `c("c", "b", "a")` differs at locations `c(1, 3)`:
      * Actual: `c("c", "a")`
      * Expected: `c("a", "c")`

---

    Code
      obj_inspect_type(c("a", "b"), t)
    Output
      Object `c("a", "b")` does not have the expected type.
      v `c("a", "b")` is a vctrs style vector.
      i `c("a", "b")` must be: `c("a", "b", "c")`.
      x `c("a", "b")` is missing 1 element: `"c"`.

---

    Code
      obj_inspect_type(c("a", "x", "c"), t)
    Output
      Object `c("a", "x", "c")` does not have the expected type.
      v `c("a", "x", "c")` is a vctrs style vector.
      i `c("a", "x", "c")` must be: `c("a", "b", "c")`.
      x `c("a", "x", "c")` is missing 1 element: `"b"`.
      x `c("a", "x", "c")` contains 1 unexpected element: `"x"`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.

