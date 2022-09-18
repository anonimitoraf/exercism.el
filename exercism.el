;;; exercism.el --- Emacs (unofficial) integration for https://exercism.org  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: September 15, 2022
;; Version: 0.0.0
;; Keywords: exercism, convenience
;; Homepage: https://github.com/anonimitoraf/exercism.el
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (a "1.0.0") (request "0.3.2") (async "1.9.6") (async-await "1.1") (persist "0.5") (transient "0.3.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  (Unofficial) Emacs integration for https://exercism.org.
;;  Provides slightly more features then the CLI for a more streamlined experience.
;;
;;; Code:

(require 'dash)
(require 'a)
(require 'request)
(require 'async)
(require 'async-await)
(require 'persist)
(require 'transient)

(defvar exercism--api-token)
(defvar exercism--exercise-slug)
(defvar exercism--track-slug)
(defvar exercism--implementation-file-paths)
(defvar exercism--shell-cmd)

(persist-defvar
 exercism--exercises-by-track (a-list)
 "An `a-list' of exercises grouped by track by suggested order of completion.")

(persist-defvar exercism--current-track nil "Current track.")

(defcustom exercism-executable "exercism"
  "Executable name/location."
  :type 'string
  :group 'exercism)

(defcustom exercism-directory
  (cond ((eq system-type 'darwin) "~/Exercism")
        (t "~/exercism"))
  "File path containing downloaded Exercism files."
  :type 'string
  :group 'exercism)

(defmacro exercism--debug (form)
  "Print out FORM and the evaluation result."
  `(let ((result ,form))
     (message (concat (prin1-to-string ',form) " => %s") result)
     result))

(defun exercism--run-shell-command (shell-cmd &optional callback)
  "Run SHELL-CMD asynchronously, calling CALLBACK if defined.
Otherwise, just echoes the output."
  (setq exercism--shell-cmd shell-cmd)
  (async-start
   `(lambda ()
      ,(async-inject-variables "exercism.*")
      (shell-command-to-string exercism--shell-cmd))
   (lambda (result)
     (if callback (funcall callback result)
       (message "[exercism shell cmd]: %s" result)))))

(defun exercism--configure (api-token)
  "Configure excerism with API-TOKEN."
  (setq exercism--api-token api-token)
  (exercism--run-shell-command (concat exercism-executable
                                       " configure"
                                       " --token " exercism--api-token)
                               (lambda (result) (message "[exercism] configure: %s" result))))

(defun exercism-configure ()
  "Configure exercism."
  (interactive)
  (exercism--configure (read-string "API token: ")))

(defun exercism--download-exercise (exercise-slug track-slug)
  "Download the exercise locally as specified via EXERCISE-SLUG and TRACK-SLUG."
  (promise-new
   (lambda (resolve _)
     (setq exercism--exercise-slug exercise-slug
           exercism--track-slug track-slug)
     (exercism--run-shell-command (concat exercism-executable
                                          " download"
                                          " --exercise=" exercism--exercise-slug
                                          " --track=" exercism--track-slug)
                                  (lambda (result)
                                    (message "[exercism] download exercise: %s" result)
                                    (funcall resolve result))))))

(defun exercism--list-tracks ()
  "List all the tracks."
  (promise-new
   (lambda (resolve _)
     (request
       (concat "https://exercism.org/api/v2/tracks")
       :parser #'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let* ((tracks (a-get data 'tracks))
                          (track-slugs (-map (lambda (it) (a-get it 'slug)) tracks)))
                     (funcall resolve track-slugs))))))))

(defun exercism--list-exercises (track-slug &optional only-unlocked?)
  "List all exercises given TRACK-SLUG.
If ONLY-UNLOCKED? is non-nil, only lists unlocked lessons."
  (promise-new
   (lambda (resolve _)
     (request
       (concat "https://exercism.org/api/v2/tracks/" track-slug "/exercises")
       :parser #'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let* ((exercises (a-get data 'exercises))
                          (exercise-slugs (->> (cl-map #'list #'identity exercises)
                                               ;; TODO Find out how to use web session so we
                                               ;; can correctly filter out only unlocked exercises.
                                               ;; Currently, all exercises are "unlocked"
                                               (-filter (lambda (it)
                                                          (if (not only-unlocked?) t
                                                            (a-get it 'is_unlocked))))
                                               (-map (lambda (it) (a-get it 'slug))))))
                     (funcall resolve exercise-slugs))))))))

(defun exercism--submit (implementation-file-paths &optional open-in-browser-after?)
  "Submit your solution in IMPLEMENTATION-FILE-PATHS.
If OPEN-IN-BROWSER-AFTER? is non-nil, the browser's opened for
you to complete your solution."
  (setq exercism--implementation-file-paths implementation-file-paths)
  (exercism--run-shell-command (format "%s submit %s" exercism-executable exercism--implementation-file-paths)
                               (lambda (result)
                                 (message "[exercism] submit: %s" result)
                                 ;; Result looks something like:
                                 ;; Your solution has been submitted successfully.
                                 ;; View it at:
                                 ;;
                                 ;;
                                 ;; https://exercism.org/tracks/javascript/exercises/hello-world
                                 (when open-in-browser-after?
                                   (when (string-match "\\(https://exercism\\.org.*\\)" result)
                                     (browse-url (match-string 1 result)))
                                   (message "[exercism] submit: %s" result)))))

(defun exercism-submit ()
  "Submit your implementation."
  (interactive)
  (exercism--submit (buffer-file-name)))

(defun exercism-submit-then-open-in-browser ()
  "Submit your implementation then open the submission page in your browser."
  (interactive)
  (exercism--submit (buffer-file-name) t))

(async-defun exercism--track-init (track-slug)
  "Init a track (via TRACK-SLUG).
This is done by downloading the hello-world exercise."
  (message "[exercism] initializing %s... (please wait)" track-slug)
  (let ((result (await (exercism--download-exercise "hello-world" track-slug))))
    (when (string-match "^Error:.*" result)
      (user-error result))))

(async-defun exercism-set-track ()
  "Set the current track that you intend to do exercises for."
  (interactive)
  (let* ((tracks (await (exercism--list-tracks)))
         (track (completing-read "Choose track: " tracks (-const t) t))
         (track-dir (expand-file-name track exercism-directory)))
    (unless (file-exists-p track-dir) (await (exercism--track-init track)))
    (setq exercism--current-track track)
    (message "[exercism] set current track to: %s" track)))

(async-defun exercism-open-exercise ()
  "Open an exercise from the currently selected track."
  (interactive)
  (unless exercism--current-track (exercism-set-track))
  (let* ((track-dir (expand-file-name exercism--current-track exercism-directory))
         (track-exercises (await (exercism--list-exercises exercism--current-track t)))
         (exercise (completing-read (format "Choose an exercise (%s): " exercism--current-track)
                                    track-exercises (cl-constantly t) t))
         (exercise-dir (expand-file-name exercise track-dir)))
    (if (file-exists-p exercise-dir)
        (find-file exercise-dir)
      (progn
        (message "[exercism] downloading %s exercise %s... (please wait)" exercism--current-track exercise)
        (let ((result (await (exercism--download-exercise exercise exercism--current-track))))
          (message "[exercism] download result: %s" result)
          ;; TODO Maybe don't assume that the exercise dir path
          ;; will be the same. Instead retrieve it from the
          ;; download response?
          (when (file-exists-p exercise-dir)
            (find-file exercise-dir)))))))

(transient-define-prefix exercism ()
  "Bring up the Exercism action menu."
  ["Exercism actions"
   ("c" "Configure" exercism-configure)
   ("t" "Set current track" exercism-set-track)
   ("o" "Open an exercise" exercism-open-exercise)
   ("s" "Submit" exercism-submit)
   ;; TODO Use a transient flag instead of a separate prefix
   ("S" "Submit (then open in browser)" exercism-submit-then-open-in-browser)])

;; TODO Command to update CLI
;; TODO Order exercises by suggested order of completion
;; TODO Show each exercise's difficulty and blurb (maybe via marginalia?)

(provide 'exercism)
;;; exercism.el ends here
