;;; fing.el --- Typing exericses for me -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: andrej.lamov@gmail.com

(require 's)
(require 'dash)
(require 'cl)
(require 'generator)

(defvar fing-input-buffer "*fing-input*")
(defvar fing-exercise-buffer "*fing-exercise*")
(defvar fing-correct-face '(:background "light green"))
(defvar fing-wrong-face '(:background "pink"))

(defun fing-random-char (c)
  (let ((i (random (length c))))
    (substring c i (+ i 1))))

(defun fing-random-element (c)
  (let ((i (random (length c))))
    (nth i c)))

(defun fing-steps (step count)
  (->> (-repeat count step)
       (-reductions-r-from (lambda (acc v) (+ acc v)) 0)
       (cdr)
       (reverse)))

(defun fing-plist-keys (pl)
  (->> pl
       (-partition-all 2)
       (-map 'car)))

(iter-defun fing-gen-any (&rest fingers)
  (let ((l (length fingers)))
    (while fingers
      (let* ((i (random l))
             (finger (nth i fingers)))
        (iter-yield
         (if (stringp finger)
             (fing-random-char finger)
           (iter-next finger)))))))

(iter-defun fing-gen-!consecutive (&rest fingers)
  (let* ((l (length fingers))
         (skip (random l)))
    (while fingers
      (let* ((idxs (fing-steps 1 l))
             (idx (->> (-remove-item skip idxs)
                       (fing-random-element)))
             (finger (nth idx fingers)))
        (setq skip idx)
        (iter-yield
         (if (stringp finger)
             (fing-random-char finger)
           (iter-next finger)))))))

(cl-defun fing-gen-words (&key word-count word-len gen)
  (setq res nil)
  (dotimes (i word-count)
    (dotimes (j word-len)
      (setq res (s-append (iter-next gen) res)))
    (setq res (s-append "\n" res)))
  (s-trim res))

(defun fing-split-on-chars (str)
  (->>  str
        (s-split "")
        (cdr)
        (-butlast)))

(defun fing-diff (a b)
  (-map-indexed
   (-lambda (idx (a . b)) (list idx (s-equals? a b)))
   (-zip (fing-split-on-chars a)
         (fing-split-on-chars b))))

(defun fing-diff-to-regions (diff)
  (-partition-by (-lambda ((_idx is-eq)) is-eq) diff))

(defun fing-content (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun fing-remove-overlays ()
  (interactive)
  (remove-overlays))

(defun fing-draw-overlays (&rest _a _b _c)
  (interactive)
  (remove-overlays)
  (let* ((ex (fing-content fing-exercise-buffer))
         (in (fing-content fing-input-buffer))
         (ex-len (length ex))
         (in-len (length in)))
    (-each
        (->> (fing-diff ex in)
             (fing-diff-to-regions))
      (-lambda (v)
        (-let (((first-idx is-correct) (-first-item v))
               ((last-idx _) (-last-item v)))
          (-> (make-overlay (+ first-idx 1) (+ last-idx 2))
              (overlay-put 'face (if is-correct fing-correct-face fing-wrong-face))))))
    ;; (-> (make-overlay (+ (min ex-len in-len) 1)
    ;;                   (+ (max ex-len in-len) 1))
    ;;     (overlay-put 'face fing-wrong-face))
    ))

(defun fing-draw-overlays-in-input-buffer (&rest _a _b _c)
  (interactive)
  (with-current-buffer fing-input-buffer
    (fing-draw-overlays)))

(defun fing-draw-overlays-in-exercise-buffer (&rest _a _b _c)
  (interactive)
  (with-current-buffer fing-exercise-buffer
    (fing-draw-overlays)))

(defvar fing-nr-of-words 40)
(defvar fing-word-length 10)

(setq fing-left-4 "1!qQaAzZ")
(setq fing-left-3 "2@wWsSxX")
(setq fing-left-2 "3#eEdDcC")
(setq fing-left-1 "4$rRfFvVbBgGtT5%")
(setq fing-right-1 "6^yYhHnN7&uUjJmM")
(setq fing-right-2 "8*iIkK,<")
(setq fing-right-3 "9(oOlL.>")
(setq fing-right-4 "0)pP;:/?'\"[{-_=+]}\\|`~")

(setq fing-exercises
      (list
       :left-12-right-34 (fing-gen-!consecutive (fing-gen-!consecutive fing-left-1 fing-left-2)
                                                (fing-gen-!consecutive fing-right-3 fing-right-4))
       :left-right-34 (fing-gen-!consecutive fing-left-3 fing-left-4 fing-right-3 fing-right-4)
       :left-right-12 (fing-gen-!consecutive fing-left-1 fing-left-2 fing-right-1 fing-right-2)))

(defun fingel ()
  (interactive)
  (let* ((exercise-key (->> fing-exercises
                            (fing-plist-keys)
                            (completing-read "fing: ")
                            (read)))
         (exercise-text (->> (plist-get fing-exercises exercise-key)
                             (fing-gen-words :word-count fing-nr-of-words :word-len fing-word-length :gen)))
         (in-buffer (get-buffer-create fing-input-buffer))
         (exer-buffer (get-buffer-create fing-exercise-buffer)))

    (switch-to-buffer exer-buffer)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert exercise-text)
    (setq buffer-read-only t)
    (goto-char (point-min))

    (switch-to-buffer in-buffer)
    (delete-region (point-min) (point-max))
    (add-hook 'after-change-functions #'fing-draw-overlays-in-input-buffer t t)
    (add-hook 'after-change-functions #'fing-draw-overlays-in-exercise-buffer t t)
    (fing-draw-overlays)))

(provide 'fing)
