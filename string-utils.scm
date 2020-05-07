;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Build - https://github.com/substratic/build
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (string-join list #!key (delimiter " ")
                                (formatter (lambda (item) item)))
  (fold (lambda (item string)
          (if (equal? item #!void)
              string
              (string-append string delimiter (formatter item))))
        ""
        list))
