(require 's)
(require 'cl)
(require 'calc)
(require 'calc-graph)

(defstruct spice-raw-file
  title date plotname flags variable-count point-count variables)

(defstruct spice-variable
  name type values)

(defun spice-read-raw-file ()
  "Reads a SPICE3 raw file written in ASCII starting from point."
  (let ((file (make-spice-raw-file)))
    (while (/= (point-max) (point))
      (if (looking-at "\\([^:\n]*?\\):\\([^\n]*?\\)\n")
          (let ((field (match-string 1))
                (value (s-trim (match-string 2))))
            (forward-line)
            (cond
             ((equal field "Title")
              (setf (spice-raw-file-title file) value))
             ((equal field "Date")
              (setf (spice-raw-file-date file) value))
             ((equal field "Plotname")
              (setf (spice-raw-file-plotname file) value))
             ((equal field "Flags")
              (setf (spice-raw-file-flags file) value))
             ((equal field "No. Variables")
              (setf (spice-raw-file-variable-count file)
                    (string-to-number value)))
             ((equal field "No. Points")
              (setf (spice-raw-file-point-count file)
                    (string-to-number value)))
             ((equal field "Variables")
              (setf (spice-raw-file-variables file)
                    (make-vector (spice-raw-file-variable-count file) nil))
              (while (looking-at
                      "\t\\([0-9]+\\)\t\\([^\t\n]+\\)\t\\([^\t\n]+\\)[^\n]*\n")
                (let ((id (string-to-number (match-string 1)))
                      (name (match-string 2))
                      (type (match-string 3)))
                  (aset (spice-raw-file-variables file) id
                        (make-spice-variable
                         :name name :type type
                         :values (make-vector
                                  (spice-raw-file-point-count file) nil))))
                (forward-line)))
             ((equal field "Values")
              (while (looking-at "\\([0-9]+\\)\t")
                (forward-char (length (match-string 0)))
                (let ((id (string-to-number (match-string 1)))
                      (var-id 0))
                  (while (looking-at "\t\\([^\n]+?\\)\n")
                    (aset
                     (spice-variable-values
                      (aref (spice-raw-file-variables file)
                            var-id))
                     id
                     (let* ((num (match-string 1))
                            (components (split-string num ",")))
                       (if (null (cdr components))
                           (math-read-number num)
                         (cons 'cplx
                               (mapcar #'math-read-number components)))))
                    (incf var-id)
                    (forward-line)))))))
        (forward-line)))
    file))

;; Import into Calc

(defun spice-variable-unit (var)
  (let ((type (spice-variable-type var)))
    (cond
     ((equal type "time")      's)
     ((equal type "frequency") 'Hz)
     ((equal type "voltage")   'V)
     ((equal type "current")   'A))))

(defun spice-variable-calc-unit (var)
  (let ((unit (spice-variable-unit var)))
    `(var ,unit ,(intern (concat "var-" (symbol-name unit))))))

(defun spice-variable-calc-name (var)
  (let ((name (spice-variable-name var)))
    (cond
     ((string-match "\\`v(\\([^()]+\\))\\'" name)
      (concat "V" (match-string 1 name)))
     ((string-match "\\`i(v?\\([^()]+\\))\\'" name)
      (concat "I" (match-string 1 name)))
     (t name))))

(defun spice-variable-calc-symbol (var)
  (intern (concat "var-" (spice-variable-calc-name var))))

(defun spice-variable-calc-vector (var)
  (let ((unit (spice-variable-calc-unit var)))
    (cons 'vec
          (loop for value across (spice-variable-values var)
                collecting `(* ,value ,unit)))))

(defun spice-variable-calc-import (var)
  (set
   (spice-variable-calc-symbol var)
   (spice-variable-calc-vector var)))

(defun spice-raw-file-calc-import (file)
  (mapc #'spice-variable-calc-import (spice-raw-file-variables file)))

;; Run ngspice

(defvar spice-output-file nil)

(defun spice-ngspice-sentinel (process event)
  (message "process %s got event %s" process event)
  (cond
   ((equal event "finished\n")
    (with-temp-buffer
      (insert-file-contents spice-output-file)
      (let ((file (spice-read-raw-file)))
        (spice-raw-file-calc-import file)
        (calc))))
   (t
    (message "ngspice: %s" event))))

(defun spice-run-ngspice (&optional buffer)
  (interactive)
  (let ((process-environment
         (cons "SPICE_ASCIIRAWFILE=1" process-environment)))
    (let* ((input (buffer-file-name buffer))
           (output (make-temp-file "ngspice"))
           (process (start-process
                     "ngspice"
                     (concat "*ngspice-" (buffer-name buffer) "*")
                     "ngspice"
                     "-b"
                     "-r" output
                     input)))
      (setq spice-output-file output)
      (set-process-sentinel process #'spice-ngspice-sentinel))))

;; Plot the result of a simulation

(defun spice-calc-graph-axes (axes)
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (or (calc-graph-find-plot nil nil)
	(error "No data points have been set!"))
    (let ((base (point))
	  start
          end)
      (re-search-forward "[,\n]\\|[ \t]+with")
      (setq end (match-beginning 0))
      (goto-char base)
      (if (looking-at "[^,\n]*[^,\n \t]\\([ \t]+axes\\)")
	  (progn
	    (goto-char (match-beginning 1))
	    (delete-region (point) end))
	(goto-char end))
      (insert " axes " axes))))

(defun spice-graph-init-trans ()
  (calc-graph-init)
  (calc-graph-set-command "xlabel"  (prin1-to-string "Time [s]"))
  (calc-graph-set-command "ylabel"  (prin1-to-string "Voltage [V]"))
  (calc-graph-set-command "y2label" (prin1-to-string "Current [A]"))
  (calc-graph-set-command "ytics" "nomirror")
  (calc-graph-set-command "grid" "")
  (calc-graph-set-command "key" "")
  (calc-graph-set-command "y2tics" "")
  (calc-graph-set-command "x2tics")
  (calc-graph-set-command "nox2tics" "")
  (calc-graph-set-command "logscale"))

(defun spice-graph-trans (var)
  (interactive (list (calc-read-var-name "Variable: ")))
  (let* ((data (calc-var-value var))
         (unitless-data (calcFunc-mapr
                         (lambda (x)
                           (math-simplify (math-remove-units x))) data))
         (unit (nth 1 (nth 2 (nth 1 data))))
         (time (calcFunc-mapr (lambda (x)
                                (math-simplify (math-remove-units x)))
                              var-time)))
    (spice-graph-init-trans)
    (calc-graph-add-curve
     (calc-graph-lookup time)
     (calc-graph-lookup unitless-data))
    (calc-graph-set-styles t nil)
    (calc-graph-name (calc-var-name var))
    (when (eql unit 'A)
      (spice-calc-graph-axes "x1y2"))
    (calc-graph-plot 0)))

(defun calcFunc-bodeamplitude (value ref)
  (calcFunc-mapr
   (lambda (x r)
     (calcFunc-dbfield (calcFunc-abs (math-simplify (math-remove-units x)))
                       (calcFunc-abs (math-simplify (math-remove-units r)))))
   value ref))

(defun calcFunc-bodephase (value ref)
  (calcFunc-mapr
   (lambda (x r)
     (math-sub
      (calcFunc-arg (math-simplify (math-remove-units x)))
      (calcFunc-arg (math-simplify (math-remove-units r)))))
   value ref))

(defun spice-graph-init-ac ()
  (calc-graph-init)
  (calc-graph-set-command "xlabel"  (prin1-to-string "Frequency [Hz]"))
  (calc-graph-set-command "ylabel"  (prin1-to-string "Amplitude [dB]"))
  (calc-graph-set-command "y2label" (prin1-to-string "Phase [deg]"))
  (calc-graph-set-command "ytics" "nomirror")
  (calc-graph-set-command "grid" "")
  (calc-graph-set-command "key" "")
  (calc-graph-set-command "x2tics")
  (calc-graph-set-command "nox2tics" "")
  (calc-graph-set-command "y2tics" "")
  (calc-graph-set-command "logscale" "x"))

(defun spice-graph-ac (input output)
  (interactive
   (list (calc-read-var-name "Input: ")
         (calc-read-var-name "Output: ")))
  (let* ((frequency (calcFunc-mapr
                     (lambda (x)
                       (calcFunc-abs (math-simplify (math-remove-units x))))
                     var-frequency))
         (amplitude (calcFunc-mapr
                     (lambda (x)
                       (math-simplify (math-remove-units x)))
                     (calcFunc-bodeamplitude
                      (calc-var-value output) (calc-var-value input))))
         (phase (calcFunc-bodephase
                 (calc-var-value output) (calc-var-value input))))
    (spice-graph-init-ac)

    (calc-graph-add-curve
     (calc-graph-lookup frequency)
     (calc-graph-lookup amplitude))
    (calc-graph-set-styles t nil)
    (calc-graph-name
     (concat "|" (calc-var-name output) "/" (calc-var-name input) "|"))

    (calc-graph-add-curve
     (calc-graph-lookup frequency)
     (calc-graph-lookup phase))
    (calc-graph-set-styles t nil)
    (calc-graph-name
     (concat "arg(" (calc-var-name output) "/" (calc-var-name input) ")"))
    (spice-calc-graph-axes "x1y2")

    (calc-graph-plot 0)))

(defun spice-graph-init-dc ()
  (calc-graph-init)

  (calc-graph-set-command "xlabel"  (prin1-to-string "Input Voltage [V]"))
  (calc-graph-set-command "ylabel"  (prin1-to-string "Output Voltage [V]"))
  (calc-graph-set-command "x2label" (prin1-to-string "Input Current [A]"))
  (calc-graph-set-command "y2label" (prin1-to-string "Output Current [A]"))

  (calc-graph-set-command "ytics" "nomirror")
  (calc-graph-set-command "xtics" "nomirror")
  (calc-graph-set-command "nox2tics")
  (calc-graph-set-command "x2tics" "")

  (calc-graph-set-command "grid" "")
  (calc-graph-set-command "key" "")

  (calc-graph-set-command "logscale"))

(defun spice-graph-dc (input output)
  (interactive
   (list (calc-read-var-name "Input: ")
         (calc-read-var-name "Output: ")))
  (let* ((input-data (calcFunc-mapr
                      (lambda (x)
                        (math-simplify (math-remove-units x)))
                      (calc-var-value input)))
         (input-unit (nth 1 (nth 2 (nth 1 (calc-var-value input)))))
         (output-data (calcFunc-mapr
                       (lambda (x)
                         (math-simplify (math-remove-units x)))
                       (calc-var-value output)))
         (output-unit (nth 1 (nth 2 (nth 1 (calc-var-value output))))))
    (spice-graph-init-dc)

    (calc-graph-add-curve
     (calc-graph-lookup input-data)
     (calc-graph-lookup output-data))
    (calc-graph-set-styles t nil)
    (calc-graph-name
     (concat (calc-var-name output) "(" (calc-var-name input) ")"))
    (spice-calc-graph-axes
     (concat
      (if (eql input-unit 'A) "x2" "x1")
      (if (eql output-unit 'A) "y2" "y1")))

    (calc-graph-plot 0)))

(defvar spice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'spice-run-ngspice)
    map)
  "Key map for `spice-mode'")

(defvar spice-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\* "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\. "_" table)
    table))

(defvar spice-whitespace-regexp
  "[ \t]*\\(?:\n[ \t]*\\+[ \t]*\\(?:\\*[^\n]*\\)?\\)*")

(defvar spice-non-empty-whitespace-regexp
  (concat "\\(?:[ \t]+" spice-whitespace-regexp
          "\\|\\(?:\n[ \t]*\\+[ \t]*\\(?:\\*[^\n]*\\)?\\)+"
          "\\)"))

(defun spice-match-argument (function-regexp id)
  (s-join
   spice-whitespace-regexp
   (cons (concat "\n[ \t]*" (if (zerop id) "\\(" "")
                        function-regexp
                        (if (zerop id) "\\)" ""))
         (loop for i from (1- id) downto 0
               collecting
               (if (zerop i) "\\([^ \t\n]+\\)"
                 "[^ \t\n]+")))))

(defun spice-font-lock-extend-region ()
  (let ((beg (save-excursion
               (goto-char font-lock-beg)
               (beginning-of-line)
               (while (and (looking-at "[ \t]+\\+") (/= (point) (point-min)))
                 (forward-line -1))
               (forward-line -1)
               (point)))
        (end (save-excursion
               (goto-char font-lock-end)
               (forward-line)
               (while (and (looking-at "[ \t]+\\+") (/= (point) (point-max)))
                 (forward-line))
               (forward-line)
               (point))))
    (setq font-lock-beg beg
          font-lock-end end)))

(defvar spice-font-lock-keywords
  `((("\\`[^\n]*$" . font-lock-doc-face) ; Title
     ;; Basic components

     (,(spice-match-argument "R[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "R[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "R[^ \t\n]*" 2) 1 font-lock-variable-name-face)

     (,(spice-match-argument "C[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "C[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "C[^ \t\n]*" 2) 1 font-lock-variable-name-face)

     (,(spice-match-argument "L[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "L[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "L[^ \t\n]*" 2) 1 font-lock-variable-name-face)

     (,(spice-match-argument "K[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "K[^ \t\n]*" 1) 1 font-lock-function-name-face)
     (,(spice-match-argument "K[^ \t\n]*" 2) 1 font-lock-function-name-face)

     ;; Sources

     (,(spice-match-argument "V[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "V[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "V[^ \t\n]*" 2) 1 font-lock-variable-name-face)

     (,(spice-match-argument "I[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "I[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "I[^ \t\n]*" 2) 1 font-lock-variable-name-face)

     (,(spice-match-argument "E[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "E[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "E[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "E[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "E[^ \t\n]*" 4) 1 font-lock-variable-name-face)

     (,(spice-match-argument "G[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "G[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "G[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "G[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "G[^ \t\n]*" 4) 1 font-lock-variable-name-face)

     (,(spice-match-argument "H[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "H[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "H[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "H[^ \t\n]*" 3) 1 font-lock-function-name-face)

     (,(spice-match-argument "F[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "F[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "F[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "F[^ \t\n]*" 3) 1 font-lock-function-name-face)

     ;; Switches

     (,(spice-match-argument "S[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "S[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "S[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "S[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "S[^ \t\n]*" 4) 1 font-lock-variable-name-face)
     (,(spice-match-argument "S[^ \t\n]*" 5) 1 font-lock-function-name-face)

     (,(spice-match-argument "W[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "W[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "W[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "W[^ \t\n]*" 3) 1 font-lock-function-name-face)
     (,(spice-match-argument "W[^ \t\n]*" 4) 1 font-lock-type-face)

     ;; Semiconductor devices

     (,(spice-match-argument "D[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "D[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "D[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "D[^ \t\n]*" 3) 1 font-lock-type-face)

     (,(spice-match-argument "Q[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "Q[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "Q[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "Q[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "Q[^ \t\n]*" 4) 1 font-lock-type-face)

     (,(spice-match-argument "M[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "M[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "M[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "M[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "M[^ \t\n]*" 4) 1 font-lock-variable-name-face)
     (,(spice-match-argument "M[^ \t\n]*" 5) 1 font-lock-type-face)

     (,(spice-match-argument "J[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(spice-match-argument "J[^ \t\n]*" 1) 1 font-lock-variable-name-face)
     (,(spice-match-argument "J[^ \t\n]*" 2) 1 font-lock-variable-name-face)
     (,(spice-match-argument "J[^ \t\n]*" 3) 1 font-lock-variable-name-face)
     (,(spice-match-argument "J[^ \t\n]*" 4) 1 font-lock-type-face)

     ;; Subcircuits

     (,(spice-match-argument "X[^ \t\n]*" 0) 1 font-lock-function-name-face)
     (,(concat "\n[ \t]*X[^ \t\n]*"
               "\\(\\(?:" spice-non-empty-whitespace-regexp "[^ \t\n]+\\)*\\)"
               spice-non-empty-whitespace-regexp "\\([^ \t\n]+\\)")
      1 font-lock-variable-name-face)
     (,(concat "\n[ \t]*X[^ \t\n]*"
               "\\(\\(?:" spice-non-empty-whitespace-regexp "[^ \t\n]+\\)+\\)"
               spice-non-empty-whitespace-regexp "\\([^ \t\n]+\\)")
      2 font-lock-type-face)

     ;; Simulation types

     (,(spice-match-argument "\\.tf" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.tf" 2) 1 font-lock-function-name-face)

     (,(spice-match-argument "\\.op" 0) 1 font-lock-builtin-face)

     (,(spice-match-argument "\\.dc" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.dc" 1) 1 font-lock-function-name-face)

     (,(spice-match-argument "\\.ac" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.ac" 1) 1 font-lock-keyword-face)

     (,(spice-match-argument "\\.tran" 0) 1 font-lock-builtin-face)

     (,(spice-match-argument "\\.sens" 0) 1 font-lock-builtin-face)

     ;; Result

     (,(spice-match-argument "\\.print" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.print" 1) 1 font-lock-keyword-face)
     (,(spice-match-argument "\\.plot" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.plot" 1) 1 font-lock-keyword-face)

     ;; Model

     (,(spice-match-argument "\\.model" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.model" 1) 1 font-lock-type-face)
     (,(spice-match-argument "\\.model" 2) 1 font-lock-keyword-face)

     ;; Subcircuit definition

     (,(spice-match-argument "\\.subckt" 0) 1 font-lock-builtin-face)
     (,(spice-match-argument "\\.subckt" 1) 1 font-lock-type-face)
     (,(concat "\n[ \t]*\\.subckt" spice-whitespace-regexp
               "[^ \t\n]+"
               "\\(\\(?:" spice-whitespace-regexp "[^ \t\n]+\\)+\\)")
      1 font-lock-variable-name-face t)

     (,(spice-match-argument "\\.ends" 0) 1 font-lock-builtin-face)

     ("\\<[sS][iI][nN]\\>" . font-lock-keyword-face)
     ("\\<[pP][wW][lL]\\>" . font-lock-keyword-face)
     ("\\<[pP][uU][lL][sS][eE]\\>" . font-lock-keyword-face))))

(defvar spice-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst (".subckt" insts ".ends")
            ("(" insts ")")
            (id))
      (insts (inst "\n" insts)
             (inst))))))

(defun spice-forward-token ()
  (skip-chars-forward " \t")
  (when (looking-at "\\(\\*[^\n]*\\)?\n[ \t]*\\+[ \t]*")
    (forward-line)
    (while (looking-at "[ \t]*\\+[ \t]*")
      (goto-char (match-end 0))
      (forward-comment 1)))
  (skip-chars-forward " \t")
  (cond
   ((looking-at "\\(?:\\*[^\n]*\\)?\n")
    (goto-char (match-end 0))
    "\n")
   (t (smie-default-forward-token))))

(defun spice-after-line-continuation-p ()
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (and (looking-at "[ \t]*\\+[ \t]*\\(\\*[^\n]+\\)?")
           (>= (match-end 0) pos)
           (match-beginning 0)))))

(defun spice-backward-token ()
  (skip-chars-backward " \t")
  (loop for x = (spice-after-line-continuation-p)
        while (and x (/= (point) (point-min))) do
        (goto-char x)
        (backward-char))
  (let ((old-line-number (line-number-at-pos)))
    (forward-comment (- (point)))
    (cond
     ((/= (line-number-at-pos) old-line-number)
      "\n")
     (t (smie-default-backward-token)))))

(defvar spice-indent-offset 2)

(defun spice-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) spice-indent-offset)
    (`(:after . "\n")
     (if (smie-rule-parent-p ".subckt")
         (smie-rule-parent spice-indent-offset)))))

(defvar spice-eldoc-components
  '(("R" "Npos" "Nneg" "value")
    ("C" "Npos" "Nneg" "value" "IC")
    ("L" "Npos" "Nneg" "value" "IC")
    ("K" "L1" "L2" "coupling")

    ("V" "Npos" "Nneg" "value")
    ("I" "Npos" "Nneg" "value")
    ("E" "Npos" "Nneg" "Cpos" "Cneg" "gain[V/V]")
    ("G" "Npos" "Nneg" "Cpos" "Cneg" "gain[A/V]")
    ("H" "Npos" "Nneg" "Vctrl" "gain[V/A]")
    ("F" "Npos" "Nneg" "Vctrl" "gain[A/A]")

    ("S" "Npos" "Nneg" "Cpos" "Cneg" "model")
    ("W" "Npos" "Nneg" "Vctrl" "model")

    ("D" "Npos" "Nneg" "model")
    ("Q" "collector" "base" "emitter" "model" "parameters...")
    ("M" "drain" "gate" "source" "body" "model" "parameters...")
    ("J" "drain" "gate" "source" "model" "parameters...")

    ("X" "nodes..." "model")))

(defvar spice-eldoc-functions
  '(("sin" "offset" "amplitude" "frequency" "delay" "damping" "phase")
    ("pwl" "T0" "V0" "...")
    ("pulse" "Vinit" "Vpeak" "Tdelay" "Trise" "Tfall" "Ton" "Period")))

(defvar spice-eldoc-instructions
  '((".tf" "variable" "source")
    (".op")
    (".dc" "source" "Vstart" "Vstop" "step" "...")
    (".ac" "[lin/dec/oct]" "Npts" "Fstart" "Fstop")
    (".tran" "Tstep" "Tstop" "Tstart")
    (".sens" "variable")
    (".print" "[ac/dc/tran]" "values...")
    (".plot" "[ac/dc/tran]" "values...")
    (".subckt" "name" "nodes...")
    (".model" "name" "type" "properties...")))

(defun spice-eldoc-to-function-name ()
  (block nil
    (while (/= (point-min) (point))
      (let ((tok (spice-backward-token)))
        (cond
         ((and (equal tok "") (looking-back "("))
          (backward-char)
          (let*
              ((candidate-name (spice-backward-token))
               (description (assoc candidate-name spice-eldoc-functions)))
            (when description
              (spice-forward-token) (spice-forward-token)
              (forward-char)
              (return (list 'function description)))))
         ((equal tok "")
          (let ((depth (car (syntax-ppss))))
            (backward-char)
            (while (> (car (syntax-ppss)) depth)
              (when (equal (spice-backward-token) "") (backward-char)))))
         ((equal tok "\n")
          (let ((candidate-name (spice-forward-token)))
            (while (equal candidate-name "\n")
              (setq candidate-name (spice-forward-token)))
            (let ((as-component (assoc (upcase (substring candidate-name 0 1))
                                       spice-eldoc-components))
                  (as-instruction (assoc (downcase candidate-name)
                                         spice-eldoc-instructions)))
              (spice-backward-token)
              (cond
               (as-component   (return (list 'component   as-component)))
               (as-instruction (return (list 'instruction as-instruction)))
               (t (return nil)))))))))))

(defun spice-eldoc-forward-argument ()
  (let ((tok (spice-forward-token)))
    (when (equal tok "")
      (let ((depth (car (syntax-ppss))))
        (forward-char)
        (while (and (> (car (syntax-ppss)) depth)
                    (/= (point) (point-max)))
          (when (equal (spice-forward-token) "")
            (forward-char))))))
  (when (looking-at "(") (spice-eldoc-forward-argument)))

(defun spice-eldoc-find-arg-id (point)
  (let ((id 0) (last-tok nil))
    (while (and (< (point) point)
                (not (equal last-tok "\n")))
      (setq last-tok (spice-eldoc-forward-argument))
      (incf id))
    (max (1- id) 0)))

(defun spice-eldoc-argument-info ()
  (let ((pos (point))
        (info (spice-eldoc-to-function-name)))
    (when info
      (let ((arg-id (spice-eldoc-find-arg-id pos)))
        (list
         info
         arg-id
         (equal "\n" (spice-forward-token)))))))

(defun spice-eldoc-format-info (info arg-id last-p)
  (let* ((element-type (car info))
         (arguments (nth 1 info))
         (is-x (equal (car arguments) "X"))
         region-beg region-end)
    (with-temp-buffer
      (insert "\n")
      (when (eql element-type 'function)
        (insert (pop arguments) "("))

      (if is-x
          (progn
            (if (= arg-id 0) (setq region-beg (point)))
            (insert "Xname ")
            (if (= arg-id 0) (setq region-end (point)))

            (if (and (> arg-id 0) (not last-p)) (setq region-beg (point)))
            (insert "nodes... ")
            (if (and (> arg-id 0) (not last-p)) (setq region-end (point)))

            (if (and (> arg-id 0) last-p) (setq region-beg (point)))
            (insert "model\n")
            (if (and (> arg-id 0) last-p) (setq region-end (point))))
        (let ((id 0))
          (while arguments
            (let ((arg (pop arguments)))
              (if (or (= arg-id id)
                      (and (null arguments) (< id arg-id)))
                  (setq region-beg (point)))
              (insert arg " ")
              (if (or (= arg-id id)
                      (and (null arguments) (< id arg-id)))
                  (setq region-end (point))))
            (incf id))))

      (when (eql element-type 'function)
        (insert ")"))

      (spice-mode)
      (font-lock-fontify-buffer)
      (when (and region-beg region-end)
        (let ((beg (save-excursion
                     (goto-char region-beg)
                     (skip-chars-forward " \t\n")
                     (point)))
              (end (save-excursion
                     (goto-char region-end)
                     (skip-chars-backward " \t\n")
                     (point))))
          (add-text-properties beg end '(face highlight))))
      (buffer-substring 1 (point-max)))))

(defun spice-eldoc ()
  (save-excursion
    (let ((info (spice-eldoc-argument-info)))
      (when info
        (apply #'spice-eldoc-format-info info)))))

(define-derived-mode spice-mode prog-mode "SPICE"
  "Major mode to edit SPICE netlists.

Key bindings:
\\{spice-mode-map}"
  :syntax-table spice-mode-syntax-table
  (setq font-lock-defaults spice-font-lock-keywords)
  (add-to-list 'font-lock-extend-region-functions
               #'spice-font-lock-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-end) "")
  (smie-setup spice-smie-grammar #'spice-smie-rules
              :forward-token  #'spice-forward-token
              :backward-token #'spice-backward-token)
  (setq-local smie-indent-basic 'spice-indent-offset)
  (setq-local 'eldoc-documentation-function 'spice-eldoc))

(add-to-list 'auto-mode-alist '("\\.spice\\'" . spice-mode))
(add-to-list 'auto-mode-alist '("\\.net\\'" . spice-mode))

(provide 'spice-mode)
