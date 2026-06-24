# bare_typed() description and diagnosis are as expected

    Code
      obj_inspect_type(list(), t_bare_list)
    Output
      Object `list()` has the expected type.
      v `list()` is a bare <list>.

---

    Code
      obj_inspect_type(mean, t_bare_list)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a bare <list>, not a bare <closure>.

