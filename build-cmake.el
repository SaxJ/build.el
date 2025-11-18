;;; cmake.el --- Build CMake projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste
;; Copyright (C) 2025  Marco Craveiro

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; URL: https://github.com/27justin/build.el
;; Package-Requires: ((seq))
;; Version: 0.1
;; Keywords: compile, build-system, cmake

;;; Commentary:

;;; Requirements:

(require 'build-api)
(require 'seq)

;;; Code:

(defvar build-cmake-debug-enabled t
  "When non-nil, enable debug logging for CMake functions.")

(defvar build-cmake-debug-buffer "*cmake-build-debug*"
  "When non-nil, enable debug logging for CMake functions.")

(defun build--cmake-debug (format-string &rest args)
  "Log debug message to *cmake-debug* buffer if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when build-cmake-debug-enabled
    (with-current-buffer (get-buffer-create build-cmake-debug-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%Y-%m-%dT%H:%M:%S.%3N")
                      (apply #'format format-string args))))))

(defun build-cmake-clear-debug-buffer ()
  "Clear the *cmake-debug* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create build-cmake-debug-buffer)
    (erase-buffer)))

(defun build-cmake-show-debug-buffer ()
  "Display the *cmake-debug* buffer."
  (interactive)
  (display-buffer "*cmake-debug*"))

(defun build--cmake-list-generators ()
  "Return a list of available CMake generator names by parsing `cmake --help`.
Leaves a buffer named *cmake-help* for inspection."
  (build--cmake-debug "=== STARTING CMake Generator Parsing ===")
  (condition-case err
      (with-temp-buffer
          (erase-buffer)
          (build--cmake-debug "Calling cmake --help process...")
          (call-process "cmake" nil t nil "--help")
          (build--cmake-debug "Process finished. Buffer size: %d chars" (buffer-size))
          (goto-char (point-min))
          (let ((in-generators nil)
                (generators '())
                (line-count 0)
                (found-first-generator nil))
            (while (not (eobp))
              (setq line-count (1+ line-count))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                    (pos (point)))
                (build--cmake-debug "=== Line %d (pos %d) ===" line-count pos)
                (build--cmake-debug "Raw line: '%s'" line)
                (build--cmake-debug "in-generators: %s, found-first-generator: %s"
                                   in-generators found-first-generator)

                (cond
                 ;; Start of generators section
                 ((string-match "^Generators$" line)
                  (build--cmake-debug ">>> FOUND 'Generators' header! Setting in-generators to t")
                  (setq in-generators t)
                  (setq found-first-generator nil))

                 ;; Parse generator line
                 ((and in-generators
                       (string-match "^\\([[:space:]]\\|\\*\\)[[:space:]]+\\([^=]+\\)[[:space:]]*=" line))
                  (build--cmake-debug ">>> MATCHED generator line!")
                  (build--cmake-debug "    Full match: '%s'" (match-string 0 line))
                  (build--cmake-debug "    Group 1 (space/asterisk): '%s'" (match-string 1 line))
                  (build--cmake-debug "    Group 2 (generator name): '%s'" (match-string 2 line))
                  (let ((generator-name (string-trim (match-string 2 line))))
                    (if (string-empty-p generator-name)
                        (build--cmake-debug ">>> SKIPPING empty generator name")
                      (build--cmake-debug ">>> ADDING generator: '%s'" generator-name)
                      (push generator-name generators)
                      (setq found-first-generator t))))

                 ;; End of section: only after we've found at least one generator
                 ((and in-generators found-first-generator
                       (not (string-match "^\\([[:space:]]\\|\\*\\)[[:space:]]+\\([^=]+\\)[[:space:]]*=" line))
                       (not (string-match "^[[:space:]]+[^=]*$" line))
                       (not (string-empty-p (string-trim line))))
                  (build--cmake-debug ">>> ENDING generators section - doesn't look like generator line")
                  (setq in-generators nil))

                 ;; Debug: show why we're skipping lines in generators section
                 ((and in-generators (not found-first-generator)
                       (not (string-match "^\\([[:space:]]\\|\\*\\)[[:space:]]+\\([^=]+\\)[[:space:]]*=" line)))
                  (build--cmake-debug ">>> In generators section, waiting for first generator..."))

                 ;; Debug: show sections we're passing through
                 ((and (not in-generators)
                       (string-match "^\\([A-Z][a-z]+\\)$" line))
                  (build--cmake-debug ">>> Found other section: '%s'" line)))

                (forward-line)))

            (let ((result (nreverse generators)))
              (build--cmake-debug "=== PARSING COMPLETE ===")
              (build--cmake-debug "Total lines processed: %d" line-count)
              (build--cmake-debug "Generators found: %d" (length result))
              (build--cmake-debug "Generator list: %S" result)
              result))

        )
    (error
     (build--cmake-debug "ERROR in build--cmake-list-generators: %S" err)
     nil)))

(defun build--cmake-list-presets ()
  "Return a list of available CMake preset names by parsing `cmake --list-presets`.
Leaves a buffer named *cmake-presets* for inspection."
  (build--cmake-debug "=== STARTING CMake Preset Parsing ===")
  (condition-case err
      (with-temp-buffer
          (erase-buffer)
          (build--cmake-debug "Calling cmake --list-presets process...")
          (call-process "cmake" nil t nil "--list-presets")
          (build--cmake-debug "Process finished. Buffer size: %d chars" (buffer-size))
          (goto-char (point-min))
          (let ((in-presets nil)
                (presets '())
                (line-count 0)
                (found-first-preset nil))
            (while (not (eobp))
              (setq line-count (1+ line-count))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                    (pos (point)))
                (build--cmake-debug "=== Line %d (pos %d) ===" line-count pos)
                (build--cmake-debug "Raw line: '%s'" line)
                (build--cmake-debug "in-presets: %s, found-first-preset: %s"
                                   in-presets found-first-preset)

                (cond
                 ;; Start of presets section
                 ((string-match "^Available configure presets:" line)
                  (build--cmake-debug ">>> FOUND 'Available configure presets:' header! Setting in-presets to t")
                  (setq in-presets t)
                  (setq found-first-preset nil))

                 ;; Parse preset line - matches lines like: "  \"preset-name\"             - Description"
                 ((and in-presets
                       (string-match "^[[:space:]]+\"\\([^\"]+\\)\"[[:space:]]+-[[:space:]]+\\(.*\\)" line))
                  (build--cmake-debug ">>> MATCHED preset line!")
                  (build--cmake-debug "    Full match: '%s'" (match-string 0 line))
                  (build--cmake-debug "    Group 1 (preset name): '%s'" (match-string 1 line))
                  (build--cmake-debug "    Group 2 (description): '%s'" (match-string 2 line))
                  (let ((preset-name (match-string 1 line)))
                    (build--cmake-debug ">>> ADDING preset: '%s'" preset-name)
                    (push preset-name presets)
                    (setq found-first-preset t)))

                 ;; End of section: only after we've found at least one preset
                 ;; and we encounter a line that doesn't look like a preset
                 ((and in-presets found-first-preset
                       (not (string-match "^[[:space:]]+\"\\([^\"]+\\)\"[[:space:]]+-[[:space:]]+\\(.*\\)" line))
                       (not (string-empty-p (string-trim line))))
                  (build--cmake-debug ">>> ENDING presets section - doesn't look like preset line")
                  (setq in-presets nil))

                 ;; Debug: show why we're skipping lines in presets section
                 ((and in-presets (not found-first-preset)
                       (not (string-match "^[[:space:]]+\"\\([^\"]+\\)\"[[:space:]]+-[[:space:]]+\\(.*\\)" line)))
                  (build--cmake-debug ">>> In presets section, waiting for first preset..."))

                 ;; Debug: show other sections we might encounter
                 ((and (not in-presets)
                       (string-match "^\\(Available\\|Presets\\)" line))
                  (build--cmake-debug ">>> Found other preset-related section: '%s'" line)))

                (forward-line)))

            (let ((result (nreverse presets)))
              (build--cmake-debug "=== PARSING COMPLETE ===")
              (build--cmake-debug "Total lines processed: %d" line-count)
              (build--cmake-debug "Presets found: %d" (length result))
              (build--cmake-debug "Preset list: %S" result)
              result)))
    (error
     (build--cmake-debug "ERROR in build--cmake-list-presets: %S" err)
     nil)))

(defun build-cmake-project-p ()
  (build--project-file-exists-p "CMakeLists.txt"))

(defun build--cmake-strip-arguments (list args)
  "Strip elements in `ARGS' from `list` using fuzzy matching.
If an element in `list` starts with any string in `args`, it will be stripped."
  (let ((results '()))
    (seq-do (lambda (item)
              (unless (seq-some (lambda (arg)
                                  (string-prefix-p arg item))
                                args)
                (push item results)))
            list)
    (reverse results)))

(defun build-cmake-build (&optional args)
  "Run CMake build with the provided OPTIONS or default to '--build build'."
  (interactive
   (list (transient-args 'build-cmake-transient)))
  (let* ((default-directory (project-root (project-current)))
         (preset (transient-arg-value "--preset=" args))
         (target (transient-arg-value "--target=" args))
         (build-command (if preset
                            (format "cmake --build --preset %s%s%s"
                                    preset
                                    (if target (format " --target %s" target) "")
                                    (string-join (build--cmake-strip-arguments args '("--preset=" "--target=" "--clean-first")) " "))
                          (format "cmake --build %s %s %s"
                                  (or (transient-arg-value "-B=" args) "build")
                                  (if target (format " --target %s " target) "")
                                  (string-join (build--cmake-strip-arguments args '("-B=" "-S=" "-G=" " -D" "--preset=" "--target=")) " ")))))
    (funcall build--compile build-command)))

(defun build-cmake-generate (&optional args)
  "Run CMake generate with the provided ARGS."
  (interactive
   (list (transient-args 'build-cmake-transient)))
  (let ((default-directory (project-root (project-current)))
        (preset (transient-arg-value "--preset=" args))
        (source-dir (transient-arg-value "-S=" args))
        (build-dir (transient-arg-value "-B=" args)))
    (if preset
        ;; Use preset but honor -S and -B if provided
        (let ((generate-command (format "cmake --preset %s %s %s"
                                        preset
                                        (if source-dir (format " -S %s " source-dir) "")
                                        (if build-dir (format " -B %s " build-dir) ""))))
          (funcall build--compile generate-command))
      ;; Use traditional arguments
      (let ((processed-args (mapcar (lambda (arg)
                                      (if (string-prefix-p "-G=" arg)
                                          ;; Wrap the value after '-G=' in quotes if it's a generator
                                          (concat "-G=\"" (substring arg 3) "\"")
                                        arg))
                                    (build--cmake-strip-arguments args '("--clean-first" "--target" "--preset=")))))
        ;; Join the processed args into a single string and run cmake
        (let ((generate-command (format "cmake %s " (string-join processed-args " "))))
          (funcall build--compile generate-command))))))

(transient-define-prefix build-cmake-transient ()
  "CMake Build Commands"
  ["CMake Options"
   ["Preset"
    ("-p" "Use preset" "--preset="
     :prompt "CMake preset: "
     :choices (build--cmake-list-presets)
     :reader (lambda (prompt _initial _history)
               (let ((presets (build--cmake-list-presets)))
                 (if presets
                     (completing-read prompt presets)
                   (message "No CMake presets available.")))))
    ]
   ["Generating"
    ("-S" "Set source directory" "-S=" :prompt "Path to source: ")
    ("-B" "Set build directory" "-B=" :prompt "Path to build: ")
    ("-D" "Defines" " " :prompt "Set defines: " :class transient-option :always-read t)
    ("-G" "Generator" "-G="
     :prompt "Build generator: "
     :choices (build--cmake-list-generators)
     :reader (lambda (prompt _initial _history)
               (let ((generators (build--cmake-list-generators)))
                 (if generators
                     (completing-read prompt generators)
                   (message "No CMake generators available")))))
    ]
   ["Building"
    ("-C" "Clean first" "--clean-first")
    ("-t" "Target" "--target=" :prompt "Build target: ")
    ("T" "Test" "--target=test")
    ("P" "Package" "--target=package")
    ("X" "Clean" "--target=clean")
    ]
   ]
  ["Build"
   ("g" "Generate" build-cmake-generate)
   ("b" "Build" build-cmake-build)
   ])

(add-to-list 'build--systems '(build-cmake-project-p . build-cmake-transient))
(provide 'build-cmake)
