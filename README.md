# Hoatzin

A small modal text editor for macOS, written in [Jolt](https://github.com/jolt-lang), with SDL3 for the window and CoreText for text.

![Hoatzin](docs/app.png)

## Features

- Normal and insert modes (in normal mode, `k` deletes forwards and `cmd+k` the line), with a `:` command line (`:open`, `:write`, `:save`, `:quit`, `:buffers`, `:new`, `:close`, `:revert`, `:cd`, `:mode`, `:settings`)
- Multiple buffers
- Wrapped text, input-method composition, smooth scrolling
- Settings window (`:settings`) for the editor and UI fonts and the theme, saved as you change them

## Requirements

- macOS
- [Jolt](https://github.com/jolt-lang)
- SDL3 (`brew install sdl3`)

## Running

```
jolt -m hoatzin.core
```

## Modes

As in Emacs, each buffer can be in a mode, which decides how its file is read and written, and can add commands or rebind normal mode's keys. Modes are Clojure, evaluated by [SCI](https://github.com/babashka/sci) outside the editor: see `src/hoatzin/app/modes.clj` for what a mode is.

A file takes the mode for its extension as it is opened or saved; `:mode name` chooses one (`:mode text` for none). The editor's own modes are in `resources/modes`:

- **auk**, for `.auk` files: notes kept as EDN. Each string of `:content` is a line of the text, and each `{:type :section :ref id}` a section from `:sections`, shown as a box of text of its own. A section's `:content` is the same, so sections hold sections:

  ```clojure
  {:content  ["A line." {:type :section :ref 1} "Another."]
   :sections [{:id 1 :content ["In a box." {:type :section :ref 2}]}
              {:id 2 :content ["In a box in a box."]}]}
  ```

  Lists and checklists sit in the text the same way, an item to a line, saved as `{:type :list :content [{:text "an item"}]}` and `{:type :checklist :content [{:text "an item" :checked? true}]}`; a list's `:content` may hold lists too, after the item they are below. A section may have a `:title`.

  In normal mode:

  | key | does |
  | --- | --- |
  | `cmd+s` | adds a section below the line, in the text the caret is in, with a new line after it, and moves into it |
  | `cmd+shift+k` | deletes the section or list the caret is in, once you answer `y` (`cmd+k` deletes the line, as everywhere) |
  | `space` | folds the section the caret is in, leaving the caret over it; over a folded one, unfolds it and moves in |
  | `cmd+r` | renames the section in its header: return, up or down keep the name, esc gives it up |
  | `l` / `ctrl+l` | adds a list / a checklist, as `cmd+s` adds a section; in a list, a sublist below the item |
  | `t` | ticks or unticks the checklist item the caret is in (or click its box) |
  | `k` | deletes forwards, or the selection; not a section or list after the line |
  | `tab` | in a list: indents the item, then takes it out to the list holding its list, then puts it back |

  On an empty line, a section or list takes the line's place, the line coming after it.

  In insert mode, `cmd+l` and `cmd+ctrl+l` add a list and a checklist, `tab` is as in normal mode, `return` on an empty last item leaves the list for a new line after it, and `shift+return` leaves every list the caret is in.

  Up and down move into and out of sections, and over folded ones, a section shows ten lines and scrolls past that, and clicking its header folds it.

Your own go in `modes` in `hoatzin` under `$XDG_CONFIG_HOME`, one `.clj` file each.

### Minor modes

A buffer can also be in any number of minor modes at once, which add keys and commands as a mode does (see `src/hoatzin/app/modes.clj`). `:minor name` turns one on or off in the buffer; `:minor` lists them. The editor's own:

- **variants**, on by default: other wordings of a part of the text. In normal mode, `v` with a selection starts a variant of it: the text goes, the caret becomes an underline, and what you type is the variant (`return` keeps it, `esc` puts the text back). Over a variant, `n` shows its next wording, and `v` adds another. Each variant shows a dot (up to three) for each wording, below its start. They're kept as EDN beside the file, written when it is, in `hoatzin/modes/variants/` under `$XDG_CONFIG_HOME`, named for the file's absolute path with `/` as `-` (`/notes/a.txt` → `-notes-a.txt.edn`), and read without waiting when the file is opened.

## Settings

Settings are saved to `settings.json` in `hoatzin` under `$XDG_CONFIG_HOME` (`~/.config` by default).

## Tests

```
jolt -M:test                          # everything
jolt -M:test -e :integration          # unit tests only
jolt -M:test -i :integration          # golden-image tests only
UPDATE_GOLDEN=1 jolt -M:test -i :integration   # regenerate the goldens
```
