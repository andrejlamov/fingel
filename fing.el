;;; fing.el --- Typing exericses for me -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: andrej.lamov@gmail.com

(require 's)
(require 'dash)

(defvar fingel-input-buffer "*fingel-input*")
(defvar fingel-exercise-buffer "*fingel-exercise*")

(defvar fingel-correct-face '(:background "light green"))
(defvar fingel-wrong-face '(:background "pink"))

(defvar fingel-nr-of-words 40)
(defvar fingel-word-length 10)

(defvar fingel-exercises)

(setq fingel-exercises
      '(
        :left-hard "!@#$%^"
        :right-hard "&*()_+|~`\\'\"][{}.,<>:;"
        :left-right-hard (:domains (:left-hard :right-hard) :combine "alternate")

        :left-1 "$4rRfFvV5%tTgGbB"
        :left-2 "2@wWsSxX"
        :left-3 "3#eEdDcC"
        :left-4 "1!qQaAzZ"
        :left-42 (:domains (:left-4 :left-2) :combine "concat")
        :left-13 (:domains (:left-1 :left-3) :combine "concat")
        :left-12 (:domains (:left-1 :left-2) :combine "concat")
        :left-23 (:domains (:left-2 :left-3) :combine "concat")
        :left-34 (:domains (:left-3 :left-4) :combine "concat")

        :right-1 "67^&yuYUhjHJnmNM"

        :right-1-left-2 (:domains (:left-2 :right-1) :combine "alternate")))

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

(defun fingel-steps (step count)
  (->> (-repeat count step)
       (-reductions-r-from (lambda (acc v) (+ acc v)) 0)
       (cdr)
       (reverse)))

(defun fingel-diff-to-regions (diff)
  (-partition-by (-lambda ((_idx is-eq)) is-eq) diff))

(defun fingel-content (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun fingel-generate-text (domain-strings &optional word-length nr-of-words)
  (let* ((word-length fingel-word-length)
         (nr-of-words fingel-nr-of-words)
         (domains (-map 'fingel-split-on-chars domain-strings))
         (domain-idxs (->> (fingel-steps 1 (length domains))
                           (-repeat word-length)
                           (-flatten))))
    (->> (-repeat nr-of-words (-repeat word-length ""))
         (-map (lambda (word)
                 (-map (lambda (i) (let ((domain (nth i domains)))
                                     (nth (random (length domain)) domain)))
                       domain-idxs)))
         (-map (-partial 's-join ""))
         (s-join "\n"))))

(defun fingel-plist-keys (pl)
  (->> pl
       (-partition-all 2)
       (-map 'car)))

(defun fingel-exercise-text (pl k)
  (let ((v (plist-get pl k)))
    (cond
     ((listp v) (let ((domains (->> (plist-get v :domains)
                                    (-map (-partial 'plist-get pl)))))
                  (pcase (plist-get v :combine)
                    ("concat" (fingel-generate-text (list (funcall (-partial 's-join "") domains))))
                    ("alternate" (fingel-generate-text domains)))))
     ((stringp v) (fingel-generate-text v)))))

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
         (exercise-text (fingel-exercise-text fingel-exercises exercise-key))
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
