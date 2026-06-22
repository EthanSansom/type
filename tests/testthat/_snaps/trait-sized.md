# sized() description and diagnosis are as expected

    Code
      obj_inspect_type(10, t_size1)
    Output
      Object `10` has the expected type.
      v `10` is a vctrs style vector.
      v `10` is size 1.

---

    Code
      obj_inspect_type(1:2, t_size1)
    Output
      Object `1:2` does not have the expected type.
      v `1:2` is a vctrs style vector.
      x `1:2` must be size 1, not size 2.

---

    Code
      obj_inspect_type(mean, t_size1)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.

