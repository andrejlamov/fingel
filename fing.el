;;; fing.el --- Typing exericses for me -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: andrej.lamov@gmail.com

(require 's)
(require 'dash)

(defvar fingel-input-buffer "*fingel-input*")
(defvar fingel-exercise-buffer "*fingel-exercise*")

(defvar fingel-correct-face '(:background "light green"))
(defvar fingel-wrong-face '(:background "pink"))

(defvar fingel-nr-of-words 60)
(defvar fingel-word-length 10)

(defvar fingel-exercises
  (list
   :ex1 "!@#$%^"
   :ex2 "&*()_+|~`\\'\"][{}.,<>:;"
   :ex1-and-ex2 '(:ex1 :ex2)
   :left-43 "1!qQaAzZ2@wWsSxX"
   :right-43 "9(oOlL.>/?;:pP0)-_[{'\"=+]}"
   :43 '(:left-43 :right-43)))

(defun fingel-split-on-chars (str)
  (->>  str
        (s-split "")
        (cdr)
        (-butlast)))

(defun fingel-diff (a b)
  (-map-indexed
   (-lambda (idx (a . b)) (list idx (s-equals? a b)))
   (-zip (fingel-split-on-chars a)
         (fingel-split-on-chars b))))

(defun fingel-diff-to-regions (diff)
  (-partition-by (-lambda ((_idx is-eq)) is-eq) diff))

(defun fingel-content (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun fingel-create-exercise (domain-string word-length nr-of-words)
  (let ((limit (length domain-string))
        (domain (fingel-split-on-chars domain-string)))
    (->> (-repeat nr-of-words (-repeat word-length ""))
         (-map (lambda (word)
                 (-map (lambda (c) (nth (random limit) domain)) word)))
         (-map (-partial 's-join ""))
         (s-join "\n"))))

(defun fingel-plist-keys (pl)
  (->> pl
       (-partition-all 2)
       (-map 'car)))

(defun fingel-get-text (pl k)
  (let ((v (plist-get pl k)))
    (cond
     ((listp v) (->> v
                     (-map (-partial 'plist-get pl))
                     (s-join "")))
     ((stringp v) v))))

(defun fingel-remove-overlays ()
  (interactive)
  (remove-overlays))

(defun fingel-draw-overlays (&rest _a _b _c)
  (interactive)
  (remove-overlays)
  (let* ((ex (fingel-content fingel-exercise-buffer))
         (in (fingel-content fingel-input-buffer))
         (ex-len (length ex))
         (in-len (length in)))
    (-each
        (->> (fingel-diff ex in)
             (fingel-diff-to-regions))
      (-lambda (v)
        (-let (((first-idx is-correct) (-first-item v))
               ((last-idx _) (-last-item v)))
          (-> (make-overlay (+ first-idx 1) (+ last-idx 2))
              (overlay-put 'face (if is-correct fingel-correct-face fingel-wrong-face))))))
    ;; (-> (make-overlay (+ (min ex-len in-len) 1)
    ;;                   (+ (max ex-len in-len) 1))
    ;;     (overlay-put 'face fingel-wrong-face))
    ))

(defun fingel-draw-overlays-in-input-buffer (&rest _a _b _c)
  (interactive)
  (with-current-buffer fingel-input-buffer
    (fingel-draw-overlays)))

(defun fingel-draw-overlays-in-exercise-buffer (&rest _a _b _c)
  (interactive)
  (with-current-buffer fingel-exercise-buffer
    (fingel-draw-overlays)))

(defun fingel ()
  (interactive)
  (let* ((exercise-key (->> fingel-exercises
                            (fingel-plist-keys)
                            (completing-read "fingel: ")
                            (read)))
         (domain (fingel-get-text fingel-exercises exercise-key))
         (exercise-text (fingel-create-exercise
                         domain
                         fingel-word-length
                         fingel-nr-of-words))
         (in-buffer (get-buffer-create fingel-input-buffer))
         (exer-buffer (get-buffer-create fingel-exercise-buffer)))

    (switch-to-buffer exer-buffer)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert exercise-text)
    (setq buffer-read-only t)
    (goto-char (point-min))

    (switch-to-buffer in-buffer)
    (delete-region (point-min) (point-max))
    (add-hook 'after-change-functions #'fingel-draw-overlays-in-input-buffer t t)
    (add-hook 'after-change-functions #'fingel-draw-overlays-in-exercise-buffer t t)
    (fingel-draw-overlays)))
