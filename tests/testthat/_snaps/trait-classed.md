# classed() description and diagnosis are as expected

    Code
      obj_inspect_type(Sys.Date(), t_any_cls)
    Output
      Object `Sys.Date()` has the expected type.
      v `Sys.Date()` inherits from at least one of classes: `Date` or `POSIXct`.

---

    Code
      obj_inspect_type(1L, t_any_cls)
    Output
      Object `1L` does not have the expected type.
      i `1L` must inherit from at least one of classes: `Date` or `POSIXct`.
      x `1L` has class <integer>.

---

    Code
      obj_inspect_type(Sys.time(), t_all_cls)
    Output
      Object `Sys.time()` has the expected type.
      v `Sys.time()` inherits from class `POSIXct` and `POSIXt`.

---

    Code
      obj_inspect_type(1L, t_all_cls)
    Output
      Object `1L` does not have the expected type.
      i `1L` must inherit all classes: `POSIXct` and `POSIXt`.
      x `1L` does not inherit from `POSIXct` and `POSIXt`.

---

    Code
      obj_inspect_type(Sys.Date(), t_one_cls)
    Output
      Object `Sys.Date()` has the expected type.
      v `Sys.Date()` inherits from class `Date`.

---

    Code
      obj_inspect_type(1L, t_one_cls)
    Output
      Object `1L` does not have the expected type.
      i `1L` must inherit all classes: `Date`.
      x `1L` does not inherit from `Date`.

