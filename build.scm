;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Build - https://github.com/substratic/build
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic build)
  (import (gambit))
  (export gsc-command
          make-project
          test-project
          build-project
          run-interactively
          substratic-modules
          gather-dependency-libs)
  (include "string-utils.scm")
  (begin

    ;; NOTE: Until Gambit gains the ability to build a module's whole
    ;; dependency graph, this list will need to be kept up to date
    ;; when new modules are added to the engine.
    (define substratic-modules
      '("substratic/sdl2"

        ;; Core modules
        "substratic/engine/rpc"
        "substratic/engine/loop"
        "substratic/engine/node"
        "substratic/engine/string"
        "substratic/engine/alist"
        "substratic/engine/state"
        "substratic/engine/assets"
        "substratic/engine/engine"
        "substratic/engine/events"
        "substratic/engine/config"
        "substratic/engine/tile-map"
        "substratic/engine/tile-set"
        "substratic/engine/keyboard"
        "substratic/engine/renderer"
        "substratic/engine/collision"
        "substratic/engine/transform"
        "substratic/engine/components"

        ;; Components
        "substratic/engine/components/fps"
        "substratic/engine/components/sprite"
        "substratic/engine/components/collider"
        "substratic/engine/components/messages"
        "substratic/engine/components/movement"
        "substratic/engine/components/position"
        "substratic/engine/components/animation"
        "substratic/engine/components/component"
        "substratic/engine/components/controller"))

    (define is-macos? (equal? (cadr (system-type)) 'apple))

    (define (remove-voids items)
      (fold-right (lambda (item new-list)
                    (if (equal? item #!void)
                        new-list
                        (cons item new-list)))
                  '()
                  items))

    (define (project-ref project field #!optional (default #f))
      (let ((pair (assoc field project)))
        (if pair
            (cdr pair)
            default)))

    (define (make-project #!key
                          (name "Substratic Engine Project")
                          (exe-name "game")
                          (version "0.0.1")
                          (output-path "./dist")
                          (modules '())
                          (files '())
                          (test-files '())
                          (search-paths '()))
      `((name . ,name)
        (exe-name . ,exe-name)
        (version . ,version)
        (output-path . ,output-path)
        (modules . ,(remove-voids modules))
        (files . ,files)
        (test-files . ,test-files)
        (search-paths . ,(remove-voids search-paths))))

    (define current-target
      (cond
       ;; TODO: What's the code for Windows?
       ((equal? (caddr (system-type)) 'linux-gnu) 'Linux)
       ((equal (caddr (system-type)) 'mingw32) 'MingW)
       ((equal (cadr (system-type)) 'apple) 'MacOS)))

    (define (resolve-output-path project)
      (let ((output-path (project-ref project 'output-path)))
        (if (procedure? output-path)
            (output-path current-target)
            output-path)))

    (define (resolve-files project mode)
      (let ((files (project-ref project 'files)))
        (remove-voids
          (if (procedure? files)
              (files mode current-target)
              files))))

    (define (resolve-test-files project)
      (let ((test-files (project-ref project 'test-files)))
        (remove-voids
          (if (procedure? test-files)
              (test-files current-target)
              test-files))))

    (define (gsc-command project #!key (gambit-path "./gambit")
                                       (userlib-path "./lib/gambit")
                                       (debug #f))
      (string-join
        (list
          (path-normalize (path-expand "bin/gsc" gambit-path))
          " -:"
          "~~bin=" gambit-path "/bin,"
          "~~include=" gambit-path "/include,"
          "~~lib=" gambit-path "/lib,"
          "~~userlib=" userlib-path
          " "
          (when debug
            "-debug-source ")
          (when is-macos?
            "-ld-options \"-mmacosx-version-min=10.6\" ")
          "-exe -o " (path-expand (project-ref project 'exe-name)
                                  (resolve-output-path project))
          " -nopreload"
          (string-join (project-ref project 'search-paths))
          (string-join (append substratic-modules (project-ref project 'modules)))
          (string-join (resolve-files project 'build)))
       delimiter: ""))

    (define (build-project project #!key (gambit-path "./gambit")
                                         (userlib-path "./lib/gambit")
                                         (debug #f))
      (println "\nBuilding for \033[1;32m" current-target "\033[0m\n")
      ;; TODO: Create the output path if it doesn't exist
      (let ((build-command (gsc-command project
                                        gambit-path: gambit-path
                                        userlib-path: userlib-path
                                        debug: debug)))
        (println "Executing build command:\n\n" build-command "\n\n")
        (shell-command build-command)))
        ;; (when (equal? current-target 'MingW)
        ;;   (gather-dependency-libs deft-agent))))

    (define (test-project project)
      (for-each (lambda (test-file) (load test-file))
                (resolve-test-files project))
      (println "\n\033[1;32mTest execution complete!\n\033[0m"))

    (define (run-interactively project)
      (for-each (lambda (file) (load file))
                (resolve-files project 'run)))

    (define (gather-dependency-libs project)
      ;; TODO: Fix me
      (let* ((deps (shell-command "ldd ./dist/deft-agent.exe" #t))
             (deps-to-copy (fold (lambda (dep deps-to-copy)
                                   (let ((lib-path (caddr (string-split dep #\space))))
                                     (if (string-starts-with? lib-path "/mingw64/bin/")
                                         (cons lib-path deps-to-copy)
                                         deps-to-copy)))
                                 '()
                                 (string-split (cdr deps) #\newline))))
        (for-each (lambda (dep-path)
                    (println "Copying dependency: " dep-path)
                    (copy-file (path-normalize (string-append "c:/msys64" dep-path)
                                (path-normalize (string-append "./dist/" (path-strip-directory dep-path))))))
                  deps-to-copy)))))
