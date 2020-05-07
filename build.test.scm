;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Build - https://github.com/substratic/build
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(include "string-utils.scm")

(import (_test)
        (substratic build))

(define test-modules '("game/logic" "game/components"))

(define test-project
  (make-project
    name: "Test Project"
    output-path: "./output"
    search-paths: '("src/" "lib/")
    modules: test-modules
    files: '("release-vars.scm" "main.scm")))

(test-group "gsc-command"
  (test-group "generates a command that will build the project"
    (let ((expected (string-append
                      (path-normalize "../../../gambit/bin/gsc")
                      " -:"
                      "~~bin=../../../gambit/bin,"
                      "~~include=../../../gambit/include,"
                      "~~lib=../../../gambit/lib,"
                      "~~userlib=./lib/gambit "
                      "-exe -o ./output/test-game -nopreload "
                      "src/ lib/"
                      (string-join (append substratic-modules test-modules))
                      " release-vars.scm main.scm")))
      (test-equal expected
                  (gsc-command test-project exe-name: "test-game" gambit-path: "../../../gambit")))))
