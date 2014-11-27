(require 'spice-mode)

(add-hook 'spice-mode-hook 'turn-on-eldoc-mode)

(define-key calc-mode-map (kbd "G") nil)
(define-key calc-mode-map (kbd "G t") 'spice-graph-trans)
(define-key calc-mode-map (kbd "G d") 'spice-graph-dc)
(define-key calc-mode-map (kbd "G a") 'spice-graph-ac)

(setq calc-gnuplot-default-device "qt")

(defun spice<= (&rest xs)
  (every (lambda (x y)
           (let ((a (math-simplify (math-remove-units x)))
                 (b (math-simplify (math-remove-units y))))
             (not (math-lessp b a)))) xs (cdr xs)))

(defun spice>= (&rest xs)
  (every (lambda (x y)
           (let ((a (math-simplify (math-remove-units x)))
                 (b (math-simplify (math-remove-units y))))
             (or (math-lessp b a) (math-equal a b)))) xs (cdr xs)))

(defun calcFunc-vfind (xs ys sought-y &optional edges)
  (unless edges (setq edges '(rising falling)))
  (cons 'vec
        (loop for (x0 x1 . xr) on (cdr xs)
              for (y0 y1 . yr) on (cdr ys)
              while (and x0 x1 y0 y1)
              when (or (and (spice<= y0 sought-y y1) (member 'rising  edges))
                       (and (spice>= y0 sought-y y1) (member 'falling edges)))
              collect (let* ((f (if (math-equal y0 y1) 0
                                  (math-div (math-sub sought-y y0)
                                            (math-sub y1 y0))))
                             (x (math-add x0 (math-mul f (math-sub x1 x0)))))
                        (math-simplify x)))))

(defun calcFunc-vreverse (ys)
  (cons 'vec (reverse (cdr ys))))
