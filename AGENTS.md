We are using the Jolt programming language. For more reference, see:

- The jolt source code is vendored at ./reference/jolt/ for reference.
- The jolt documentation is vendored at ./reference/jolt-lang.github.io/docs/docs/
  (start with `repl-driven-development.html`, "Differences from Clojure", and `reference/jolt/llms.txt`).

## Running code

- Run tests with `jolt -M:test` (see `deps.edn` for `-i`/`-e :integration` filters).
- Jolt is Clojure-like, not Clojure: check "Differences from Clojure" before assuming a JVM/clojure.core behaviour.
- The app is a GUI (SDL); don't launch it from the nREPL. Evaluate pure functions and state transitions only.

## nREPL evaluation (start and stop within the same task)

- Never leave an nREPL running: start it, use it, kill it, all in the same piece of work.
- Start on a fixed non-default port in the background, from the project root (so `deps.edn` source roots and native libs load):
  - `jolt nrepl-server 7899 > $TMPDIR/nrepl.log 2>&1 & echo $! > $TMPDIR/nrepl.pid`
  - Wait for `started on port` in the log before evaluating (poll the log; don't blind-sleep).
  - It writes `.nrepl-port` in the project dir; it's removed on exit; delete it if it lingers and never commit it.
- Evaluate with `clj-nrepl-eval --port 7899 '(+ 1 2)'` (or a heredoc for multi-line code).
  - Sessions persist per port, so `require`s and `def`s carry across calls; `--reset-session` clears them.
  - Output is `=> value`; `println` output and errors (e.g. `Divide by zero`) print inline.
  - Pass `-t MS` for slow forms; the default timeout is 120s.
- Dev mode: redefining a var takes effect on the next call. After editing a file, reload it with `(require 'my.ns :reload)` or `(load-file "src/...")`, then re-test.
- Prefer small REPL probes (check an unfamiliar core fn, try a fix) before editing files or running the full suite.
- Always stop it at the end: `kill $(cat $TMPDIR/nrepl.pid)`; confirm with `clj-nrepl-eval --discover-ports` and `ls -a | grep nrepl`.
- Also stop it on failure paths (timeouts, errors); don't abandon a hung server.

## Paren repair

- Don't run `clj-paren-repair` routinely; it also reformats code. Use it only when you hit a paren/delimiter balancing problem (reader error, unbalanced edit).
- Then run `clj-paren-repair FILE...` (rewrites in place) rather than hand-counting parens, and review the diff afterwards.
- With no args it filters stdin to stdout, handy for fixing a snippet before eval.

## Formatting

- Keep code consistent with `cljfmt`; run it on the files you changed, not the whole tree (e.g. `cljfmt check src/hoatzin/app.clj`).
  - `cljfmt check PATHS...` reports formatting diffs without touching files; run it before finishing a task.
  - `cljfmt fix PATHS...` rewrites in place; use it on files you edited, then review the diff.
- Don't `cljfmt fix` files you haven't touched: it creates noisy, unrelated diffs.
- If the tree isn't already clean, fix only your own changes and don't reformat existing code.

## Gotchas

- `jolt` keeps the process alive ~60s after `-main` if a future ran; `-main` must call `shutdown-agents`.
- `.jolt/`, `.cpcache/` and `reference/` are gitignored; don't edit `reference/`.
