# Substratic Build

Substratic Build is a set of build APIs for games written with [Substratic
Engine](https://github.com/substratic/engine), a game engine written in Gambit
Scheme.

A simple example of a `build.scm` file for a game using Substratic Engine and Build:

```scheme
@; ;; Tell Gambit to pass along remaining command line args to this script

(import (substratic build))

(define super-mapio-brothers
  (make-project
    name: "Super Mapio Brothers"
    exe-name: "super-mapio"
    version: "1.0.0"

    output-path:
      (lambda (target)
        (if (equal? target 'MacOS)
            "./dist/super-mapio-brothers.app/Contents/MacOS"
            "./dist"))

    search-paths: '("src/" "lib/")

    files:
      (lambda (mode target)
        (if (equal? mode 'build)
            '("src/release.scm")
            '("src/dev.scm")))

    modules: '(;; Components
               mapio/components/jump

               ;; Entities
               mapio/entitites/gormba
               mapio/entitites/looigi

               ;; Game-specific code
               "mapio/game"

               ;; Program entrypoint
               "mapio/main")

    test-files: '("test/components/jump.test.scm")))

(cond
 ((member "--dev" (command-line)) (run-interactively super-mapio-brothers))
 ((member "--test" (command-line)) (test-project super-mapio-brothers))
 (else (build-project super-mapio-brothers)))
```

## License

Substratic Build is licensed under the [Mozilla Public License
2.0](https://www.mozilla.org/MPL/2.0/), see [LICENSE](LICENSE) for more details.
