;; -*- lexical-binding: t -*-

(require 'dash)
(require 'a)
(require 'request)
(require 'async)
(require 'persist)

(defvar exercism--api-token)
(defvar exercism--exercise-slug)
(defvar exercism--track-slug)
(defvar exercism--implementation-file-paths)

(persist-defvar
 exercism--exercises-by-track (a-list)
 "An a-list of exercises grouped by track by suggested order of completion.")

(defcustom exercism-executable "exercism"
  "Executable name/location."
  :type 'string
  :group 'exercism)

(defun exercism--configure (api-token)
  "Configure excerism with API-TOKEN."
  (setq exercism--api-token api-token)
  (async-start
   `(lambda ()
      ,(async-inject-variables "exercism.*")
      (shell-command-to-string (concat exercism-executable
                                       " configure"
                                       " --token " api-token)))
   (lambda (result) (message "[exercism] configure: %s" result))))

(defun exercism-configure ()
  "Configure exercism."
  (interactive)
  (exercism--configure (read-string "API token: ")))

(defun exercism--download-exercise (exercise-slug track-slug)
  "Download the exercise locally as specified via EXERCISE-SLUG and TRACK-SLUG."
  (setq exercism--exercise-slug exercise-slug
        exercism--track-slug track-slug)
  (async-start
   `(lambda ()
      ,(async-inject-variables "exercism.*")
      (shell-command-to-string
       (concat exercism-executable
               " download"
               " --exercise=" exercism--exercise-slug
               " --track=" exercism--track-slug) ))
   (lambda (result)
     (message "[exercism] download exercise: %s" result))))

(defun exercism--list-exercises (track-slug on-success)
  "List all exercises given TRACK-SLUG.
ON-SUCCESS is a fn that gets called with the exercise slugs."
  (request
    (concat "https://exercism.org/api/v2/tracks/" track-slug "/exercises")
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((exercises (a-get data 'exercises))
                       (exercise-slugs (--map (a-get it 'slug) exercises)))
                  (funcall on-success exercise-slugs))))))

(defun exercism-download ()
  "Download all the exercises of a track."
  (interactive)
  (let ((track-slug (read-string "Track slug: ")))
    (exercism--list-exercises track-slug
                              (lambda (exercises)
                                (setq exercism--exercises-by-track
                                      (a-assoc exercism--exercises-by-track track-slug exercises))
                                (persist-save 'exercism--exercises-by-track)
                                (--map (exercism--download-exercise it track-slug) exercises)))))

(defun exercism--submit (implementation-file-paths)
  "Submit your solution in IMPLEMENTATION-FILE-PATHS."
  (setq exercism--implementation-file-paths implementation-file-paths)
  (async-start
   `(lambda ()
      ,(async-inject-variables "exercism.*")
      (shell-command-to-string
       (format "%s submit %s" exercism-executable exercism--implementation-file-paths)))
   (lambda (result)
     (message "[exercism] submit: %s" result))))

(defun exercism-submit ()
  "Submit your implementation."
  (interactive)
  (exercism--submit (buffer-file-name)))

;; ---- REPL manual tests ----

(defmacro exercism--comment (&rest _body)
  "Comment out one or more s-expressions."
  nil)

(exercism--comment
 (exercism--list-exercises "erlang" (-partial 'message "exercises %s"))
 (exercism-download))

;; -----

(provide 'exercism)
