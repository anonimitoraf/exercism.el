;;; exercism.el --- Unofficial https://exercism.org integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: September 15, 2022
;; Version: 0.0.0
;; Keywords: exercism, convenience
;; Homepage: https://github.com/anonimitoraf/exercism.el
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (a "1.0.0") (s "1.13.1") (request "0.3.2") (async "1.9.6") (async-await "1.1") (persist "0.5") (transient "0.3.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Do exercism exercises within Emacs!
;;
;;; Code:

(require 'dash)
(require 'a)
(require 's)
(require 'request)
(require 'async)
(require 'async-await)
(require 'persist)
(require 'transient)

(defvar exercism--api-token)
(defvar exercism--exercise-slug)
(defvar exercism--track-slug)
(defvar exercism--shell-cmd)

(defcustom exercism-executable "exercism"
  "Executable name/location."
  :type 'string
  :group 'exercism)

(defcustom exercism-directory
  (expand-file-name
   (cond ((eq system-type 'darwin) "~/Exercism")
         (t "~/exercism")))
  "DEPRECATED: Hardcoded file path containing downloaded Exercism files.
This is only here for backwards-compatibility."
  :type 'string
  :group 'exercism)

(defcustom exercism-config-path
  (expand-file-name "~/.config/exercism/user.json")
  "File path to the exercism CLI configuration file."
  :type 'string
  :group 'exercism)

(persist-defvar exercism--current-track nil "Current track.")
(persist-defvar exercism--current-exercise nil "Current exercise.")
(persist-defvar exercism--workspace exercism-directory "Root dir for all the files")

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

(defun exercism--file-to-string (file-path)
  "Read the contents of FILE-PATH to a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun exercism--configure (api-token)
  "Configure excerism with API-TOKEN."
  (setq exercism--api-token api-token)
  (exercism--run-shell-command (concat (shell-quote-argument exercism-executable)
                                       " configure"
                                       " --token " (shell-quote-argument exercism--api-token))
                               (lambda (result) (message "[exercism] configure: %s" result)
                                 ;; {
                                 ;;   "apibaseurl": "https://api.exercism.io/v1",
                                 ;;   "token": "d4bfe622-92e1-4d36-a0ae-788712e03946",
                                 ;;   "workspace": "/Users/raf/Exercism"
                                 ;; }
                                 (let* ((user-config-path (expand-file-name exercism-config-path))
                                       (user-config-str (exercism--file-to-string user-config-path))
                                       (user-config (json-parse-string user-config-str
                                                                       :object-type 'alist
                                                                       :array-type 'list))
                                       (workspace (alist-get 'workspace user-config)))
                                   (setq exercism--workspace workspace)))))

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
     (exercism--run-shell-command (concat (shell-quote-argument exercism-executable)
                                          " download"
                                          " --exercise=" (shell-quote-argument exercism--exercise-slug)
                                          " --track=" (shell-quote-argument exercism--track-slug))
                                  (lambda (result)
                                    (message "[exercism] download result for %s: %s" exercise-slug result)
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
                          (filtered-exercises (->> (cl-map #'list #'identity exercises)
                                               ;; TODO Find out how to use web session so we
                                               ;; can correctly filter out only unlocked exercises.
                                               ;; Currently, all exercises are "unlocked"
                                               (-filter (lambda (it)
                                                          (if (not only-unlocked?) t
                                                            (a-get it 'is_unlocked)))))))
                     (funcall resolve filtered-exercises))))))))

(defun exercism--get-config (exercise-dir)
  (let* ((config (exercism--file-to-string
                  (expand-file-name "config.json" (concat exercise-dir "/" ".exercism")))))
    (json-parse-string config
                       :object-type 'alist
                       :array-type 'list)))

(defun exercism--get-solution-files (exercise-dir)
  (let* ((config (exercism--get-config exercise-dir))
         (solution-file-paths (a-get-in config '(files solution))))
    solution-file-paths))

(defun exercism--submit (&optional open-in-browser-after?)
  "Submits your solution in the current directory.
If OPEN-IN-BROWSER-AFTER? is non-nil, the browser's opened for
you to complete your solution."
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (exercise-dir (expand-file-name exercism--current-exercise track-dir))
         (solution-files (exercism--get-solution-files exercise-dir))
         (default-directory exercise-dir)
         (submit-command (s-join " " (list
                                      (shell-quote-argument exercism-executable) "submit"
                                      (s-join " " solution-files)))))
    (exercism--run-shell-command submit-command
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
                                     (message "[exercism] submit: %s" result))))))

(defun exercism-submit ()
  "Submit your implementation."
  (interactive)
  (exercism--submit))

(defun exercism-submit-then-open-in-browser ()
  "Submit your implementation then open the submission page in your browser."
  (interactive)
  (exercism--submit t))

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
         (track-dir (expand-file-name track exercism--workspace)))
    (unless (file-exists-p track-dir) (await (exercism--track-init track)))
    (setq exercism--current-track track)
    (message "[exercism] set current track to: %s" track)))

(defvar exercism--longest-exercise-slug-length)
(defvar exercism--longest-exercise-difficulty-length)
(defun exercism--exercises->longest (exercises property)
  "Return the longest PROPERTY length from list of EXERCISES."
  (->> exercises
       (--map (a-get it property))
       (--max-by (> (length it) (length other)))
       (length)))

(defun exercism--color-string (str color)
  "Make a string STR with fg COLOR. Return STR."
  (add-face-text-property 0 (length str) (list :foreground color) nil str)
  str)

(defun exercism--exercise-annotation-fn (exercise)
  "Annotates each EXERCISE option with the difficulty and description.
EXERCISE should be a list with the shape `(slug exercise-data)'."
  (let* ((option (assoc exercise minibuffer-completion-table))
         (data (cadr option))
         (blurb (a-get data 'blurb))
         (difficulty (a-get data 'difficulty)))
    ;; TODO Make annotation colors customizable via faces
    (concat " " (exercism--color-string (s-pad-right exercism--longest-exercise-difficulty-length " " difficulty)
                                          (cond
                                           ((equal difficulty "easy") "green")
                                           ((equal difficulty "medium") "yellow")
                                           ((equal difficulty "hard") "red")
                                           (t "blue")))
            "    " (exercism--color-string blurb "grey50"))))

(async-defun exercism-open-exercise ()
  "Open an exercise from the currently selected track."
  (interactive)
  (unless exercism--current-track (exercism-set-track))
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (track-exercises (await (exercism--list-exercises exercism--current-track t)))
         (_ (setq exercism--longest-exercise-slug-length (exercism--exercises->longest track-exercises 'slug)
                  exercism--longest-exercise-difficulty-length (exercism--exercises->longest track-exercises 'difficulty)))
         (exercise-options (-map (lambda (exercise)
                                   (list (s-pad-right exercism--longest-exercise-slug-length " " (a-get exercise 'slug)) exercise))
                                 track-exercises))
         (completion-extra-properties '(:annotation-function exercism--exercise-annotation-fn))
         (exercise (s-trim (completing-read (format "[exercism] (%s) Choose an exercise: " exercism--current-track)
                                     exercise-options (-const t) t)))
         (exercise-dir (expand-file-name exercise track-dir)))
    (if (file-exists-p exercise-dir)
        (progn (find-file exercise-dir)
               (setq exercism--current-exercise exercise))
      (message "[exercism] downloading %s exercise %s... (please wait)" exercism--current-track exercise)
      (let ((result (await (exercism--download-exercise exercise exercism--current-track))))
        (message "[exercism] download result: %s" result)
        ;; TODO Maybe don't assume that the exercise dir path
        ;; will be the same. Instead retrieve it from the
        ;; download response?
        (when (file-exists-p exercise-dir)
          (find-file exercise-dir))
        (setq exercism--current-exercise exercise)))))

(async-defun exercism-download-all-unlocked-exercises ()
  "Download all the unlocked exercises."
  (interactive)
  (unless exercism--current-track (exercism-set-track))
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (track-exercises (await (exercism--list-exercises exercism--current-track t))))
    (seq-doseq (exercise track-exercises)
      (let* ((slug (a-get exercise 'slug))
             (exercise-dir (expand-file-name slug track-dir)))
        (unless (file-exists-p exercise-dir)
          (message "[exercism] attempting to download %s exercise %s..." exercism--current-track slug)
          (exercism--download-exercise slug exercism--current-track))))))

;; (exercism-download-all-unlocked-exercises)

(defun exercism--list-downloaded-exercises ()
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace)))
    (seq-filter (lambda (file) (not (seq-contains-p '("." "..") file)))
                (directory-files track-dir))))

;; (exercism--list-downloaded-exercises)

(defun exercism-open-exercise-offline ()
  "Select and open an already downloaded exercise from the currently selected track."
  (interactive)
  (unless exercism--current-track (exercism-set-track))
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (downloaded-exercise-slugs (exercism--list-downloaded-exercises))
         (exercise (s-trim (completing-read (format "[exercism]: (%s) Choose a downloaded exercise: " exercism--current-track)
                                     downloaded-exercise-slugs (-const t) t)))
         (exercise-dir (expand-file-name exercise track-dir)))
    (progn (find-file exercise-dir)
           (setq exercism--current-exercise exercise))))

(defun exercism--transient-name ()
  (format "Exercism actions | TRACK: %s | EXERCISE: %s"
          (or exercism--current-track "N/A")
          (or exercism--current-exercise "N/A")))

(defun exercism--semver-to-number (semver)
  "Rudimentary conversion of semvers to a numerical value that can be compared easily.
Turn '3.26.1' into something like: 3_026_001."
  (let ((portions (split-string semver "\\."))
        (portion-idx 0))
    (seq-reduce (lambda (sum n)
                  (prog1 (+ sum (* (expt 1000 portion-idx) (string-to-number n)))
                    (setq portion-idx (1+ portion-idx))))
                (reverse portions) 0)))

;; (exercism--semver-to-number "3.26.1") ;; => 3026001

(defun exercism--compare-semvers (ver1 op ver2)
  "Compares VER1 and VER2 as numerical values with the operator OP."
  (funcall op (exercism--semver-to-number ver1) (exercism--semver-to-number ver2)))

;; (exercism--compare-semvers "3.26.1" #'> "3.3.2") ;; => t
;; (exercism--compare-semvers "3.2.1" #'> "3.10.1") ;; => nil

(defun exercism--cli-version ()
  "Gets out the CLI version."
  (promise-new (lambda (resolve _)
                 (exercism--run-shell-command
                  (concat (shell-quote-argument exercism-executable) " version")
                  (lambda (result)
                    (let ((version (when (string-match "exercism version \\([0-9.]+\\)" result)
                                     (match-string 1 result))))
                      (funcall resolve version)))))))

(async-defun exercism-cli-version ()
  "Prints out the CLI version."
  (interactive)
  (message "[exercism] version: %s" (await (exercism--cli-version))))

(async-defun exercism-run-tests ()
  "Runs the tests for the currently selected exercise."
  (interactive)
  (let* ((version (await (exercism--cli-version)))
         (min-version "3.2.0")
         (track-dir (expand-file-name exercism--current-track exercism--workspace))
         (exercise-dir (expand-file-name exercism--current-exercise track-dir))
         ;; TODO Maybe use a macro that sets the dir? e.g. (with-current-track ...)
         (default-directory exercise-dir)
         (compile-command (concat (shell-quote-argument exercism-executable) " test")))
    (if (exercism--compare-semvers version #'< min-version)
        (message "[exercism] error: running tests is only supported for CLI version %s and above. You are on %s"
                 min-version version)
      (compile compile-command))))

(transient-define-prefix exercism ()
  "Bring up the Exercism action menu."
  [:description exercism--transient-name
   ("v" "Display CLI version" exercism-cli-version)
   ("c" "Configure" exercism-configure)
   ("t" "Set current track" exercism-set-track)
   ("d" "Download all unlocked exercises" exercism-download-all-unlocked-exercises)
   ("e" "Open a downloaded exercise" exercism-open-exercise-offline)
   ("o" "Open an exercise" exercism-open-exercise)
   ("r" "Run tests" exercism-run-tests)
   ("s" "Submit" exercism-submit)
   ;; TODO Use a transient flag instead of a separate prefix
   ("S" "Submit (then open in browser)" exercism-submit-then-open-in-browser)])

;; TODO Command to update CLI
;; TODO Ability to download all exercises
;; TODO Order exercises by suggested order of completion

(provide 'exercism)
;;; exercism.el ends here
