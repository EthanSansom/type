# has() description and diagnosis are as expected

    Code
      obj_inspect_type(list(x = 10L), t1)
    Output
      Object `list(x = 10L)` has the expected type.
      v `list(x = 10L)[[1]]` is a bare <integer>.

---

    Code
      obj_inspect_type(mean, t1)
    Output
      Object `mean` does not have the expected type.
      x `mean[[1]]` must return a value, not raise an error.

---

    Code
      obj_inspect_type(list(x = 1, y = 2), t2)
    Output
      Object `list(x = 1, y = 2)` has the expected type.
      v `names(list(x = 1, y = 2))` is a bare <character>.
      v `names(list(x = 1, y = 2))` is size 2.

---

    Code
      obj_inspect_type(list(x = 1), t2)
    Output
      Object `list(x = 1)` does not have the expected type.
      x `names(list(x = 1))` must be size 2, not size 1.

