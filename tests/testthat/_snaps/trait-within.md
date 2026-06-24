# within() description and diagnosis are as expected

    Code
      obj_inspect_type(c("a", "b"), t)
    Output
      Object `c("a", "b")` has the expected type.
      v `c("a", "b")` is a vctrs style vector.
      v `c("a", "b")` contains only values from: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(c("a", "d", "e"), t)
    Output
      Object `c("a", "d", "e")` does not have the expected type.
      v `c("a", "d", "e")` is a vctrs style vector.
      i `c("a", "d", "e")` may only contain values from: `c("a", "b", "c")`.
      x `c("a", "d", "e")` contains 2 unexpected elements: `c("d", "e")`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.

