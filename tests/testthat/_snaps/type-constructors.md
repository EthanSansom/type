# list_type() description and diagnosis are as expected

    Code
      obj_inspect_type(list(x = 1L, y = "a"), t)
    Output
      Object `list(x = 1L, y = "a")` has the expected type.
      v `list(x = 1L, y = "a")` is a bare <list>.
      v `names(list(x = 1L, y = "a"))` is a bare <character>.
      v `names(list(x = 1L, y = "a"))` is the same as: `c("x", "y")`.
      v `list(x = 1L, y = "a")[["x"]]` is a bare <integer>.
      v `list(x = 1L, y = "a")[["y"]]` is a bare <character>.

---

    Code
      obj_inspect_type(list(x = 1L, y = 2L), t)
    Output
      Object `list(x = 1L, y = 2L)` does not have the expected type.
      v `list(x = 1L, y = 2L)` is a bare <list>.
      v `names(list(x = 1L, y = 2L))` is a bare <character>.
      v `names(list(x = 1L, y = 2L))` is the same as: `c("x", "y")`.
      v `list(x = 1L, y = 2L)[["x"]]` is a bare <integer>.
      x `list(x = 1L, y = 2L)[["y"]]` must be a bare <character>, not a bare
        <integer>.

---

    Code
      obj_inspect_type(list(x = 1L), t)
    Output
      Object `list(x = 1L)` does not have the expected type.
      v `list(x = 1L)` is a bare <list>.
      i `names(list(x = 1L))` must be: `c("x", "y")`.
      x `names(list(x = 1L))` is missing 1 element: `"y"`.

# list_of_type() description and diagnosis are as expected

    Code
      obj_inspect_type(list(1L, 2L), t)
    Output
      Object `list(1L, 2L)` has the expected type.
      v `list(1L, 2L)` is a bare <list>.
      v Every element of `list(1L, 2L)` is a bare <integer>.

---

    Code
      obj_inspect_type(list(1L, "a", 3L), t)
    Output
      Object `list(1L, "a", 3L)` does not have the expected type.
      v `list(1L, "a", 3L)` is a bare <list>.
      x `list(1L, "a", 3L)[[2]]` must be a bare <integer>, not a bare <character>.

# dataframe_type() description and diagnosis are as expected

    Code
      obj_inspect_type(data.frame(x = 1.5, y = "a"), t)
    Output
      Object `data.frame(x = 1.5, y = "a")` has the expected type.
      v `data.frame(x = 1.5, y = "a")` inherits from class `data.frame`.
      v `names(data.frame(x = 1.5, y = "a"))` is a bare <character>.
      v `names(data.frame(x = 1.5, y = "a"))` is the same as: `c("x", "y")`.
      v `data.frame(x = 1.5, y = "a")[["x"]]` is a bare <double>.
      v `data.frame(x = 1.5, y = "a")[["y"]]` is a bare <character>.

---

    Code
      obj_inspect_type(data.frame(x = 1L, y = "a"), t)
    Output
      Object `data.frame(x = 1L, y = "a")` does not have the expected type.
      v `data.frame(x = 1L, y = "a")` inherits from class `data.frame`.
      v `names(data.frame(x = 1L, y = "a"))` is a bare <character>.
      v `names(data.frame(x = 1L, y = "a"))` is the same as: `c("x", "y")`.
      x `data.frame(x = 1L, y = "a")[["x"]]` must be a bare <double>, not a bare
        <integer>.

---

    Code
      obj_inspect_type(list(x = 1.5, y = "a"), t)
    Output
      Object `list(x = 1.5, y = "a")` does not have the expected type.
      i `list(x = 1.5, y = "a")` must inherit all classes: `data.frame`.
      x `list(x = 1.5, y = "a")` does not inherit from `data.frame`.

