# wt-tab-status

Shows the state of every Claude Code session on its Windows Terminal tab, so you can tell at a glance which tabs need you.

| Tab icon      | Meaning                                                                          |
| ------------- | -------------------------------------------------------------------------------- |
| Spinning ring | Claude is working in at least one pane of the tab                                |
| Bell          | Claude finished while you were looking elsewhere; cleared when you focus the tab |
| Normal icon   | Nothing new                                                                      |

It works on renamed tabs, where a renamed title hides the spinner Claude Code writes into the window title, and on tabs split into several panes, where Windows Terminal combines the progress of all of them.

## Install

```sh
claude plugin marketplace add zompinc/sync-method-generator --sparse .claude-plugin plugins
claude plugin install wt-tab-status@zomp
```

Sessions already running pick it up after a restart.

## How it works

Four hooks write terminal sequences through the hook output's `terminalSequence` field:

- `UserPromptSubmit` starts an indeterminate progress ring (`OSC 9;4;3`).
- `Stop` and `StopFailure` clear the ring (`OSC 9;4;0`) and ring the bell (`BEL`). Windows Terminal marks an unfocused tab with a bell icon until it is focused.
- `SessionEnd` clears the ring.

The hooks run through `bash`, which on Windows means Git Bash.

## Notes

- With Windows Terminal's default `bellStyle` the bell is also audible. For a silent marker, set `"bellStyle": "taskbar"` under `profiles.defaults`, which flashes the taskbar instead.
- The ring keeps spinning while Claude waits on a permission prompt.
- Claude Code's own progress sequences (`terminalProgressBarEnabled`) can clear the ring partway through a turn. Turn that setting off if the ring stops early.
- Other terminals may ignore the progress sequence; the bell then beeps or flashes as that terminal does.
