# Hoatzin

A small modal text editor for macOS, written in [Jolt](https://github.com/jolt-lang), with SDL3 for the window and CoreText for text.

![Hoatzin](docs/app.png)

## Features

- Normal and insert modes, with a `:` command line (`:open`, `:write`, `:save`, `:quit`, `:buffers`, `:new`, `:close`, `:revert`, `:cd`, `:settings`)
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

## Settings

Settings are saved to `settings.json` in `hoatzin` under `$XDG_CONFIG_HOME` (`~/.config` by default).

## Tests

```
jolt -M:test                          # everything
jolt -M:test -e :integration          # unit tests only
jolt -M:test -i :integration          # golden-image tests only
UPDATE_GOLDEN=1 jolt -M:test -i :integration   # regenerate the goldens
```
