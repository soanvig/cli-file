# cli-file

*Alpha version, API subjected to change*

Tool for easy and programming language-agnostic command-line interfaces for executing commands.
It's built to provide accessible and extensible interface to building projects, running tests,
managing deployments and so on, without relying on poor tools like `npm scripts`.

It offers domain-specific language - configuration file containing lines with commands like:

```
myEcho :: value = World -> echo Hello $value
```

that can be executed using

`cli-file myEcho` resulting in `Hello World`

or

`cli-file myEcho John` resulting in `Hello John`

Any shell command can be called that way.

## How to use

By default `cli-file` looks for a file `commands.cli` in current directory.
That is the file containing configured commands.

Syntax:

1. Command without any arguments: `myEcho -> echo Hello World`
2. Command with required argument: `myEcho :: value -> echo Hello $value`
3. Command with optional argument: `myEcho :: value = World -> echo Hello $value`
4. Command with multiple arguments: `myEcho :: first, second = World -> echo $first $second`

At this point `cli-file` doesn't support default values longer than one word (on TODO list)

### Switches

1. `-c <file location>` or `--commands <file location>` - change default commands file location (e.g. `cli-file -c /tmp/commands myEcho John`)
2. `-h` or `--help` - (needs to be first) show help
3. `-v` or `--version` - (needs to be first) print version