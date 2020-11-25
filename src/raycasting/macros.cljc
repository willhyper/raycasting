(ns raycasting.macros)

(defmacro three-decimal
  "Truncate `double` to three decimal places."
  [num]
  `(/ (int (* 1000 ~num)) 1000.0))

